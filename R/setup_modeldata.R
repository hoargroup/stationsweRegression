#' setup modeldata based on fsca or rcn as predictor
#'
#' @export
#' @param snoteltoday.sp spatialpoints data frame of the pillow data for the simulation date
#' @param phvsnotel dataframe containing the physiographic values for the pixels that contain pillow stations
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

	# create complete formula for regression by adding 'swe' as the y variable and 'recon' or 'fsca' as the additional x variable
	myformula <- as.formula(paste0('swe ~ ',paste(as.character(PHV_VARS)[2],SNOW_VAR,sep=' + ')))

	if(SNOW_VAR=='rcn'){
		# get all the netcdf reconstruction files that start with "recondata" in the directory specified in the run file
		rcn_nc_files=dir(PATH_RCNDOWNLOAD,glob2rx('^recondata*.nc$'),full.names=T)
		if(length(rcn_nc_files)==0){
			stop('you haven\'t provided recondata files in the path specified by PATH_RCNDOWNLOAD (this should be defined at the top of your run files).')
		}
		# extract the year of the reconstrcution from the filename. specifically, the year will be the digits between the last '_' and the extension (after the '.')
		ryrs=as.numeric(sub(pattern='.nc',replacement='',sapply(strsplit(basename(rcn_nc_files),split='[_]'),'[',3)))
		# stack the raster layers from the netcdf files. check get_rcn_nc to see which dates. currently only 1st and 15th of the month
		snow_raster=stack(map(ryrs,get_rcn_nc,rcn_nc_files))
		# check extent with watermask to catch any errors
		correct_extent <- compareRaster(snow_raster,watermask, extent=T,rowcol=T, crs=T, res=T, rotation=T,stopiffalse = F)
		if(!correct_extent){
			stop('the extents of the reconstruction files you provided do not match those of the watermask. did you specify the correct PATH_RCNDOWNLOAD?')
		}


	} else if(SNOW_VAR=='fsca') {

		snowpred_raster=simfsca

	} else {

		stop('you haven\'t selected rcn or fsca as a predictor')

		}

	# perform some basic quality control between satellite fsca and pillow swe

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
		mutate(fsca=ifelse(fsca>100 & swe>0,100,fsca)) %>% #if the station is obscured but swe>0 then 100% coverage
		mutate(fsca=ifelse(fsca==0 & swe>0,15,fsca)) %>% #if pixel shows no snow but swe>0, then 15% coverage (modscag detection limit)
		filter(fsca<=100) #remove stations where pixel is obscured and swe isn't recording snow>0

	num_phv <- nrow(phvsnotel)
	num_fsca <- nrow(doidata)

	## check for na in predictor variables
	row.has.na <- apply(doidata, 1, function(x){any(is.na(x))})
	if(any(row.has.na)) {
		warning('Something is wrong. There are NAs in your predictor dataframe (doidata). Does your predictor raster have an NA where they shouldn\'t? Proceeding without the station(s) with NA values.')
		doidata <- na.omit(doidata)
	}

	num_pred <- nrow(doidata)

	# setup prediction dataframe and add rcn variable to doidata if applicable

	if(SNOW_VAR=='rcn'){

		snotel_rcn <-
			raster::extract(snow_raster,snoteltoday.sp,sp=T) %>%
			as.data.frame() %>%
			tbl_df %>%
			dplyr::select(-Longitude, -Latitude, -swe) %>% #remove swe so we don't run into troubel when we join below and scale the swe value
			mutate_if(is.factor,as.character) %>%
			gather(rdate,rcn,-Site_ID:-dy)
		# setNames(c(names(snoteltoday.sp),SNOW_VAR)) %>%

		selectrcn_data <-
			doidata %>%
			mutate(swe=swe*fsca/100) %>% #in the paper we showed that scaling the swe reading by fsca of the pixel improves the regression estimates. but it is statistically invalid for phvfsca estimate
			left_join(snotel_rcn, by=c("Site_ID", "dte", "yr", "doy", "yrdoy", "dy"))


		print(' - selecting the best recon date...')

		extract_cvm <- function(data,myformula){
			cvglmnet_object <- gnet_phvfsca(data,myformula)
			cvglmnet_object$cvm[which(cvglmnet_object$lambda==cvglmnet_object$lambda.1se)]
		}

		selectrcn_cvm <-
			selectrcn_data %>%
			group_by(rdate) %>%
			nest() %>%
			mutate(
				cvm=map_dbl(data,extract_cvm,myformula))


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
			filter(rdate == bestrdate) %>% #selectrcn_data already contains fsca-scaled pillow swe
			mutate(rcn=(rcn-avg)/std)

		if(any(is.na(doidata$rcn))){
			stop(paste0('There are nodata values in your reconstruction at snow pillow locations for the best recon date (',bestrdate,'). This should not be.'))
		}
	}

	## Print some informational statements and save modeling data to a file
	line1=paste0(' - Stations online: ',num_phv)
	line2=paste0(' - Stations removed for which the modscag pixel is not reporting data (clouds, not run, or otherwise) and the snow pillow is reporting 0 swe: ',num_phv-num_fsca)
	line3=paste0(' - Stations removed because the predictor data include nodata values- you should fix this: ',num_fsca-num_pred)

	print(line1)
	print(line2)
	print(line3)

	forFile <- function(ch){
		gsub(pattern = ' - ',replacement = '# ',ch)
	}

	options(warn=-1)
	fileConn <- file(file.path(PATH_OUTPUT,paste0('modeldata_',datestr,'.txt')), open='w')
	writeLines(paste0('# SWE Regression for ',simdate,' in the ',RUNNAME,'domain'),fileConn)
	writeLines(forFile(line1), fileConn)
	writeLines(forFile(line2), fileConn)
	writeLines(forFile(line3), fileConn)
	writeLines('#',fileConn)
	writeLines('# This is the model data:', fileConn)
	write.table(doidata, file=fileConn, sep='\t', col.names=T,row.names=F, quote=F,append=T)
	close(fileConn)
	options(warn = 0)

	## combine snow variable with phv data for the domain for subsequent prediction ----
	predictdF <- bind_cols(ucophv,raster::as.data.frame(snowpred_raster) %>% setNames(SNOW_VAR)) %>% tbl_df

	return(list(doidata,predictdF,myformula))
}
