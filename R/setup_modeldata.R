#' setup modeldata based on fsca or rcn as predictor
#'
#' @export
#' @param snoteltoday.sp spatialpoints data frame of the snotel data for the simulation date
#' @param phvsnotel dataframe containing the physiographic values for the pixels that contain snotel stations
#' @param simfsca raster. modscag image of fsca for the simulation date
#' @param SNOW_VAR character string identifying the snow related regression variable. this should have been setup in the runfile
#' @param PHV_VARS formula with the independent physiographic variables for the regression
#' @param PATH_RCNDOWNLOAD path for the location of recondata*.nc files used with SNOW_VAR is 'rcn'
#' @return a list with 3 items. 1. dataframe with all variables needed to fit the regression model 2. dataframe with all variables  needed to predict on the same grid as the fsca image 3. a formula for predicting


setup_modeldata <- function(snoteltoday.sp,phvsnotel,simfsca,SNOW_VAR,PHV_VARS,PATH_RCNDOWNLOAD){
	if(nargs()<6) {
		stop('not enough arguments to setup_modeldata()')
	} else if(nargs()>6){
		stop('too many arguments to setup_modeldata()')
	}

	myformula <- as.formula(paste0('snotel ~ ',paste(as.character(PHV_VARS)[2],SNOW_VAR,sep=' + ')))

	if(SNOW_VAR=='rcn'){
		rcn_nc_files=dir(PATH_RCNDOWNLOAD,glob2rx('^recondata*.nc$'))
		if(length(rcn_nc_files)==0){
			stop('you haven\'t provided recondata files in the path specified by PATH_RCNDOWNLOAD (this should be defined at the top of your run files).')
		}
		ryrs=as.numeric(sub(pattern='.nc',replacement='',sapply(strsplit(rcn_nc_files,split='[_]'),'[',3)))
		snow_raster=stack(map(ryrs,get_rcn_nc))

	} else if(SNOW_VAR=='fsca') {

		snowpred_raster=simfsca

	} else {

		stop('you haven\'t selected rcn or fsca as a predictor')

		}

	# perform some basic quality control between satellite fsca and snotel

	## extract fsca data for the station pixels ----
	newnames=c(names(snoteltoday.sp),'fsca')
	snotel_fsca <-
		raster::extract(simfsca,snoteltoday.sp,sp=T) %>%
		as.data.frame() %>%
		tbl_df %>%
		dplyr::select(-Longitude,-Latitude) %>%
		setNames(newnames) %>%
		mutate_if(is.factor,as.character)

	## join station fsca and swe data with station phv data ----
	## take care of some inconsistencies between fsca and pillow
	doidata <-
		inner_join(snotel_fsca,phvsnotel,by=c('Site_ID')) %>%
		mutate(fsca=ifelse(fsca>100 & snotel>0,100,fsca)) %>% #if the station is obscured but snotel>0 then 100% coverage
		mutate(fsca=ifelse(fsca==0 & snotel>0,15,fsca)) %>% #if pixel shows no snow but snotel>0, then 15% coverage (modscag detection limit)
		filter(fsca<=100) #remove stations where pixel is obscured and snotel isn't recording snow>0

	# setup prediction dataframe and add rcn variable to doidata if applicable

	if(SNOW_VAR=='rcn'){

		snotel_rcn <-
			raster::extract(snow_raster,snoteltoday.sp,sp=T) %>%
			as.data.frame() %>%
			tbl_df %>%
			dplyr::select(-Longitude, -Latitude, -snotel) %>% #remove snotel so we don't run into troubel when we join below and scale the snotel value
			mutate_if(is.factor,as.character) %>%
			gather(rdate,rcn,-Site_ID:-dy)
		# setNames(c(names(snoteltoday.sp),SNOW_VAR)) %>%

		selectrcn_data <-
		doidata <- doidata %>%
			mutate(snotel=snotel*fsca/100) %>% #in the paper we showed that scaling the snotel reading by fsca of the pixel improves the regression estimates. but it is statistically invalid for phvfsca estimate
			left_join(snotel_rcn)


		print('selecting the best recon date...')

		extract_cvm <- function(data,myformula){
			cvglmnet_object <- gnet_phvfsca(data,myformula)
			cvglmnet_object$cvm[which(cvglmnet_object$lambda==cvglmnet_object$lambda.1se)]
		}

		selectrcn_cvm <-
			selectrcn_data %>%
			# filter(rdate=='X20000301' | rdate=='X20010301') %>%
			group_by(rdate) %>%
			nest() %>%
			mutate(
				cvm=map_dbl(data,extract_cvm,myformula))


		# selectrcn_cvm <-
		# 	selectrcn_data %>%
		# 	# filter(rdate=='X20000301' | rdate=='X20010301') %>%
		# 	group_by(rdate) %>%
		# 	nest() %>%
		# 	mutate(cv_obj=map(data,gnet_phvfsca,myformula),
		# 				 cvm=map_dbl(cv_obj,extract_cvm))

		bestrdate <-
			selectrcn_cvm %>%
			ungroup() %>%
			summarize(best_rdate=rdate[which.min(cvm)]) %>%
			as.character()

		## get best recon raster
		snowpred_raster <- snow_raster[[grep(bestrdate,names(snow_raster))]]
		rscaled <- scale(values(snowpred_raster))
		avg=attr(rscaled,'scaled:center')
		std=attr(rscaled,'scaled:scale')
		values(snowpred_raster) <- rscaled

		## add only best recon data for model fitting data
		doidata <- selectrcn_data %>%
			filter(rdate == bestrdate) %>% #selectrcn_data already contains fsca-scaled snotel
			mutate(rcn=(rcn-avg)/std)

	}

	## check for na in predictor variables
	row.has.na <- apply(doidata, 1, function(x){any(is.na(x))})
	if(any(row.has.na)) {
		warning('Something is wrong. There are NAs in your predictor dataframe (doidata). Does your predictor raster have an NA where they shouldn\'t? Proceeding without the station(s) with NA values.')
		doidata <- na.omit(doidata)
	}


	## combine snow variable with phv data for the domain for subsequent prediction ----
	predictdF <- bind_cols(ucophv,raster::as.data.frame(snowpred_raster) %>% setNames(SNOW_VAR)) %>% tbl_df

	return(list(doidata,predictdF,myformula))
}
