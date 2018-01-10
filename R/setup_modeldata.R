#' setup modeldata based on fsca or rcn as predictor
#'
#' @export
#' @param snoteltoday.sp spatialpoints data frame of the snotel data for the simulation date
#' @param snoteltoday regular dataframe  of the snotel data for the simulation date
#' @param phvsnotel dataframe containing the physiographic values for the pixels that contain snotel stations
#' @param simfsca raster. modscag image of fsca for the simulation date
#' @param SNOW_VAR character string identifying the snow related regression variable. this should have been setup in the runfile
#' @param PHV_VARS formula with the independent physiographic variables for the regression
#' @param PATH_RCNDOWNLOAD path for the location of recondata*.nc files used with SNOW_VAR is 'rcn'
#' @return a list with 3 items. 1. dataframe with all variables needed to fit the regression model 2. dataframe with all variables  needed to predict on the same grid as the fsca image 3. a formula for predicting


setup_modeldata <- function(snoteltoday.sp,snoteltoday,phvsnotel,simfsca,SNOW_VAR,PHV_VARS,PATH_RCNDOWNLOAD){

	myformula <- as.formula(paste0('snotel ~ ',paste(as.character(PHV_VARS)[2],SNOW_VAR,sep=' + ')))

	if(SNOW_VAR=='rcn'){
		rcn_nc_files=dir(PATH_RCNDOWNLOAD,glob2rx('^recondata*.nc$'))
		if(length(rcn_nc_files)==0){
			print('you haven\'t provided recondata files in the path specified by PATH_RCNDOWNLOAD (this should be defined at the top of your run files).')
			stop()
		}
		ryrs=as.numeric(sapply(strsplit(rcn_nc_files,split='[_.]'),FUN='[',2))
		snow_raster=stack(map(ryrs,get_rcn_nc))

	} else if(SNOW_VAR=='fsca') {

		snow_raster=simfsca

	} else {

		print('you haven\'t selected rcn or fsca as a predictor')
		stop()

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

	## join station fsca and swe data with station data ---
	swedata=inner_join(snotel_fsca,phvsnotel,by=c('Station_ID','Site_ID'))

	## merge today's snotel data and phv data ----
	doidata <-
		inner_join(snoteltoday,phvsnotel,by=c('Station_ID','Site_ID')) %>%
		inner_join(snotel_fsca) %>%
		mutate(fsca=ifelse(fsca>100 & snotel>0,100,fsca)) %>% #if the station is obscured but snotel>0 then 100% coverage
		mutate(fsca=ifelse(fsca==0 & snotel>0,15,fsca)) %>% #if pixel shows no snow but snotel>0, then 15% coverage (modscag detection limit)
		filter(fsca<=100) %>% #remove stations where pixel is obscured and snotel isn't recording snow>0

	# setup prediction dataframe and add rcn variable to doidata if applicable

	if(SNOW_VAR=='fsca'){

		## combine fsca with phv data for the domain for subsequent prediction ----
		predictdF <- bind_cols(ucophv,as.data.frame(simfsca) %>% setNames(fsca)) %>% tbl_df

	} else if(SNOW_VAR=='rcn'){

		snotel_rcn <-
			raster::extract(snow_raster,snoteltoday.sp,sp=T) %>%
			as.data.frame() %>%
			tbl_df %>%
			dplyr::select(-Longitude, -Latitude) %>%
			mutate_if(is.factor,as.character) %>%
			gather(rdate,rcn,-Station_ID:-dy)
		# setNames(c(names(snoteltoday.sp),SNOW_VAR)) %>%

		selectrcn_data <- doidata %>%
			mutate(snotel=snotel*fsca/100) %>% #in the paper we showed that scaling the snotel reading by fsca of the pixel improves the regression estimates. but it is statistically invalid for phvfsca estimate
			left_join(snotel_rcn)

		extract_cvm <- function(cvglmnet_object){
			cvglmnet_object$cvm[which(cvglmnet_object$lambda==cvglmnet_object$lambda.1se)]
		}

		selectrcn_cvm <-
			selectrcn_data %>%
			# filter(rdate=='X20000301' | rdate=='X20010301') %>%
			group_by(rdate) %>%
			nest() %>%
			mutate(cv_obj=map(data,gnet_phvfsca,myformula),
						 cvm=map_dbl(cv_obj,extract_cvm))

		bestrdate <-
			selectrcn_cvm %>%
			ungroup() %>%
			summarize(best_rdate=rdate[which.min(cvm)]) %>%
			as.character()

		doidata <- left_join(doidata,snotel_rcn %>% filter(rdate == bestrdate))

		## combine rcn with phv data for the domain for subsequent prediction ----
		rcn_raster <- snow_raster[[grep(bestrdate,names(snow_raster))]]
		predictdF <- bind_cols(ucophv,raster::as.data.frame(rcn_raster) %>% setNames('rcn')) %>% tbl_df

	}

	return(list(doidata,predictdF,myformula))
}
