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
		nameLen=function(nm){
			noext <- strsplit(basename(nm),split='.nc',fixed=T)
			as.numeric(substr(noext,nchar(noext)-3,nchar(noext)))
		}
		ryrs <- purrr::map_dbl(rcn_nc_files,nameLen)
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
	newnames=c('Site_ID','longitude','latitude','dte','pillowswe','modscag')
	snotel_fsca <-
		raster::extract(simfsca,snoteltoday.sp,sp=T) %>%
		tbl_df %>%
		setNames(newnames) %>%
		mutate_if(is.factor,as.character)

	## take care of some inconsistencies between fsca and pillow
	## will export data4gis later after adding rcn (if applicable)
	data4gis <-
		snotel_fsca %>%
		mutate(fsca=modscag,
					 fsca=ifelse(fsca>100 & pillowswe>0,100,fsca),  #if the station is obscured but swe>0 then 100% coverage
					 fsca=ifelse(fsca==0 & pillowswe>0,15,fsca), #if pixel shows no snow but swe>0, then 15% coverage (modscag detection limit)
					 fsca=ifelse(!is.na(modscag) & is.na(fsca), modscag,fsca)) #fix result when pillowswe is NA


	## join station fsca and swe data with station phv data
	doidata <-
		data4gis %>%
		dplyr::select(-longitude,-latitude) %>% #don't need these columns anymore
		inner_join(phvsnotel,by=c('Site_ID'))

	# setup prediction dataframe and add rcn variable to doidata if applicable

	if(SNOW_VAR=='rcn'){

		snotel_rcn <-
			raster::extract(snow_raster,snoteltoday.sp,sp=T) %>%
			tbl_df %>%
			dplyr::select(-Longitude, -Latitude, -pillowswe) %>% #remove pillowswe so we don't run into troubel when we join below and scale the swe value
			mutate_if(is.factor,as.character) %>%
			gather(rdate,rcn,-Site_ID:-dte)
		# setNames(c(names(snoteltoday.sp),SNOW_VAR)) %>%

		selectrcn_data <-
			doidata %>%
			mutate(swe=pillowswe*fsca/100) %>% #in the paper we showed that scaling the swe reading by fsca of the pixel improves the regression estimates. but it is statistically invalid for phvfsca estimate
			left_join(snotel_rcn, by=c("Site_ID", "dte"))


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

		print(paste0('   ... best historical recon date: ',strptime(bestrdate,'X%Y%m%d')))

		## get best recon raster
		snowpred_raster <- snow_raster[[grep(bestrdate,names(snow_raster))]]
		rscaled <- scale(values(snowpred_raster))
		avg=attr(rscaled,'scaled:center')
		std=attr(rscaled,'scaled:scale')
		values(snowpred_raster) <- rscaled

		## add only best recon data for model fitting data
		doidata <-
			selectrcn_data %>%
			filter(rdate == bestrdate) %>%  #selectrcn_data already contains fsca-scaled pillow swe
			mutate(rcn=(rcn-avg)/std)

		## combine fsca, pillow, recon data without any filtering to output to gpkg
		data4gis <-
			data4gis %>%
			mutate(swe=pillowswe*fsca/100) %>% #need to do this again
			left_join(snotel_rcn %>%
									filter(rdate==bestrdate),
								by=c('Site_ID','dte'))

		if(any(is.na(doidata$rcn))){
			stop(paste0('There are nodata values in your reconstruction at snow pillow locations for the best recon date (',bestrdate,'). This should not be.'))
		}

	}
	else if(SNOW_VAR=='fsca'){
		doidata <-
			doidata %>%
			mutate(swe=pillowswe)
		bestrdate = NA
	}


	## export data4gis with all downloaded stations
	coordinates(data4gis) <- ~longitude+latitude
	proj4string(data4gis) <- proj4string(snoteltoday.sp)
	data4gis <- extract(phvstack,data4gis,sp=T)#add phv values for each station
	snotelfilename=paste0(PATH_OUTPUT,'/pillow-',strftime(simdate,'%d%b%Y'),'.gpkg')
	# if(!file.exists(snotelfilename)){
		writeOGR(data4gis,dsn=snotelfilename,layer='pillow_data',driver='GPKG',overwrite_layer=TRUE)
	# }

	## find out which stations are missing data and filter
	### statiosn not reporting
	pillowNA <- is.na(doidata$pillowswe)
	num_pillowNA <- length(which(pillowNA))

	### remove stations where pixel is obscured and swe isn't recording snow>0 (could be NA or 0). left over from previous filtering
	fscaNA <- doidata$fsca>100 | is.na(doidata$fsca)#also check for na in fsca, just in case
	num_fscaNA <- length(which(fscaNA))

	doidata <-
		doidata %>%
		filter(!pillowNA,!fscaNA) #remove nodata. these should catch nodata in swe column too

	## check for na in predictor variables
	row.has.na <- apply(doidata, 1, function(x){any(is.na(x))})
	if(any(row.has.na)) {
		warning('Something is wrong. There are NAs in your predictor dataframe (doidata). Does your predictor raster have an NA where they shouldn\'t? Proceeding without the station(s) with NA values.')
		pred_dropped <- doidata[row.has.na,]$Site_ID
		numDropped <- length(pred_dropped)
		doidata <- na.omit(doidata)
	} else {
		pred_dropped <- 'none'
		numDropped <- 0
	}

	## Print some informational statements and save modeling data to a file
	line1=paste0(' - Stations in inventory: ',nrow(phvsnotel))
	line2=paste0(' - Stations removed for which the modscag pixel is not reporting data (clouds, not run, or otherwise) and the snow pillow is reporting no swe: ',num_fscaNA)
	line3=paste0(' - Stations removed because the predictor data include nodata values- you should fix this: ', numDropped)
	line4=paste0(' - Stations used in analysis: ',nrow(doidata))
	line5=paste0(' - Bad predictor data for Site ID: ',paste(pred_dropped, collapse=', '), ' (any dropped stations are still present in gpkg).')

	print(line1)
	print(line4)
	print(line2)
	print(line3)
	print(line5)
	print(' - See modeldata details in the modeldata_<date>.txt file in your output folder.')

	forFile <- function(ch){
		gsub(pattern = ' - ',replacement = '# ',ch)
	}

	# save model information to file
	options(warn=-1)
	fileConn <- file(file.path(PATH_OUTPUT,paste0('modeldata_',datestr,'.txt')), open='w')
	writeLines(paste0('# SWE Regression for ',simdate,' in the ',RUNNAME,' domain'),fileConn)
	writeLines(paste0('# predicted with SNOW_VAR: ',SNOW_VAR),fileConn)
	writeLines(paste0('# historical recon date used: ', strptime(bestrdate,'X%Y%m%d')), fileConn)
	writeLines(forFile(line1), fileConn)
	writeLines(forFile(line2), fileConn)
	writeLines(forFile(line3), fileConn)
	writeLines(forFile(line4), fileConn)
	writeLines(forFile(line5), fileConn)
	writeLines('#',fileConn)
	writeLines('# This is the model data (columns lat/lon are for the pixel):', fileConn)
	readr::write_tsv(data4gis %>% tbl_df %>% filter(!(Site_ID %in% pred_dropped)), path=fileConn, col_names=T, append=T)
	close(fileConn)
	options(warn = 0)

	## combine snow variable with phv data for the domain for subsequent prediction ----
	predictdF <- bind_cols(ucophv,raster::as.data.frame(snowpred_raster) %>% setNames(SNOW_VAR)) %>% tbl_df

	return(list(doidata,predictdF,myformula))
}
