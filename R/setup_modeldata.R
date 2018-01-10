#' setup modeldata based on fsca or rcn as predictor
#'
#' @export
#' @param snoteltoday.sp spatialpoints data frame of the snotel data for the simulation date
#' @param snoteltoday regular dataframe  of the snotel data for the simulation date
#' @param phvsnotel dataframe containing the physiographic values for the pixels that contain snotel stations
#' @param myformula formula for regression
#' @param simfsca raster. modscag image of fsca for the simulation date
#' @param snow_var character string identifying the snow related regression variable
#' @param PATH_RCNDOWNLOAD path for the location of recondata*.nc files used with snow_var is 'rcn'


setup_modeldata <- function(snoteltoday.sp,snoteltoday,phvsnotel,myformula,simfsca,snow_var,PATH_RCNDOWNLOAD){

	if(snow_var=='rcn'){
		rcn_nc_files=dir(PATH_RCNDOWNLOAD,glob2rx('^recondata*.nc$'))
		if(length(rcn_nc_files)==0){
			print('you haven\'t provided recondata files in the path specified by PATH_RCNDOWNLOAD (this should be defined at the top of your run files).')
			stop()
		}
		ryrs=as.numeric(sapply(strsplit(rcn_nc_files,split='[_.]'),FUN='[',2))
		snow_raster=stack(map(ryrs,get_rcn_nc))

	} else if(snow_var=='fsca') {

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
		mutate(swe=snotel)#*fsca/100)

	# setup prediction dataframe and add rcn variable to doidata if applicable

	if(snow_var=='fsca'){

		## combine fsca with phv data for the domain for subsequent prediction ----
		predictdF <- bind_cols(ucophv,as.data.frame(simfsca) %>% setNames(fsca)) %>% tbl_df

	} else if(snow_var=='rcn'){

		snotel_rcn <-
			raster::extract(snow_raster,snoteltoday.sp,sp=T) %>%
			as.data.frame() %>%
			tbl_df %>%
			dplyr::select(-Longitude, -Latitude) %>%
			mutate_if(is.factor,as.character) %>%
			gather(rdate,rcn,-Station_ID:-dy)
		# setNames(c(names(snoteltoday.sp),snow_var)) %>%

		selectrcn_data <- doidata %>% left_join(snotel_rcn)

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
		predictdF <- bind_cols(ucophv,as.data.frame(rcn_raster) %>% setNames('rcn')) %>% tbl_df

	}

	return(list(doidata,predictdF))
}
