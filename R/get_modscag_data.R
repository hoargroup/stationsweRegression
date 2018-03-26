#' Function to get MODSCAG data.
#'
#' The function will check for existing processed fsca images in the imagepath. If it doesn't exist, it will check for raw, unprocessed files and mosaic them. If no file exist it will attempt to download them from snowserver and mosaic them.
#'
#' @param doy day of year
#' @param yr year
#' @param type 'NRT' or 'historic'. near realtime download from snowdav or archived data
#' @param imagepath the directory where the mosaiced fsca .tif files exist
#' @param modscagfn 'snow_fraction' or 'snow_fraction_canadj'; portion of the filename before the extension to identify canopy corrected or not corrected modscag;
#' @param fveg_correction logical, default TRUE. should fsca be corrected by modscag fveg?
#' @param reso resolution of fsca
#' @param xmin minimum x coordinate of domain
#' @param xmax maximum x coordinate of domain
#' @param ymin minimum y coordinate of domain
#' @param ymax maximum y coordinate of domain
#' @export
#' @return rasterImage of fsca for the simulation date
#' @details called for its side effect of producing a mosaicked fsca image of the domain

get_modscag_data=function(doy,
													yr,
													type,
													imagepath,
													modscagfn='snow_fraction',
													fveg_correction=TRUE,
													reso,
													xmin,
													xmax,
													ymin,
													ymax){

	if(modscagfn=='snow_fraction_canadj' & fveg_correction) stop('You specified the canopy adjusted modscag fsca and the fveg correction. It doesn\'t make sense to do both.')

	PATH_DOY=paste0(PATH_MODSCAGDOWNLOAD,'/',yr,'/',doy)
	# dir.create(PATH_DOY, showWarnings = FALSE, rec=TRUE)

	simfscafilename=paste0(imagepath,'/modscag_',modscagfn,'_fvegcorrection_',yr,doy,'.tif')
	fscafilename=paste0(imagepath,'/modscag_',modscagfn,'_',yr,doy,'.tif')

	if(!fveg_correction){
		simfscafilename=fscafilename
	}

	#check if domain fsca file exists already
	if(file.exists(simfscafilename)) {
		print(' - fsca image exists. nothing to do. returning fsca.')
		simfsca=raster(simfscafilename)
		# corners=SpatialPoints(rbind(c(xmin,ymin),c(xmin,ymax),c(xmax,ymax),c(xmax,ymin)))
		# correct_grid <- rgeos::gCovers(as(extent(simfsca), "SpatialPolygons"), as(extent(corners), "SpatialPolygons"))
		correct_extent <- compareRaster(simfsca,raster(xmn=xmin,xmx=xmax,ymn=ymin,ymx=ymax), extent=T,rowcol=F, crs=F, res=F, rotation=F,stopiffalse = F)
		if(!correct_extent){
			stop(paste0('the extents of the existing fsca image do not match the extents provided in the run file. are you using the wrong RUNNAME? or, if you previously used the wrong extents try deleting ',simfscafilename))
		} else {
			return(simfsca)
		}

		#if file doesn't exist, check from fveg files if using fveg correction
	} else if(fveg_correction){

		fveg_files=dir(PATH_DOY,paste0('*NRT.vegetation_fraction.tif$'),full.names = TRUE)
		if(length(fveg_files)==0){
			stop('The NRT fveg files do not exist on your computer. please download them from snow-dav using get_NRTmodscag.sh or otherwise (get_NRTmodscag.sh does not exist as part of this package because it uses password protected login for downloads).')
		}
		fvegfilename <- paste0(imagepath,'/modscag_vegetation_fraction_',yr,doy,'.tif')
		if(!file.exists(fvegfilename)){
			fveg=gdalUtils::gdalwarp(fveg_files,fvegfilename,output_Raster = TRUE,t_srs = '+proj=longlat +datum=WGS84',te = c(xmin,ymin,xmax,ymax),tr=c(reso,reso),r='near',dstnodata='-99',ot='Int32')#make sure this is cast at least as a signed integer for -99
		} else {
			fveg=raster(fvegfilename)
		}
	}

	# now look for fsca files
	if(type=='NRT'){

		tiffiles=dir(PATH_DOY,paste0('*NRT.',modscagfn,'.tif$'),full.names = TRUE)

		if(length(tiffiles)==0 ){
			stop('The NRT fsca files do not exist on your computer. please download them from snow-dav using get_NRTmodscag.sh or otherwise (get_NRTmodscag.sh does not exist as part of this package because it uses password protected login for downloads).')
		}

	} else if(type=='historic'){

		tiffiles=dir(PATH_DOY,paste0('*',modscagfn,'.tif$'),full.names = TRUE)
		tiffiles=tiffiles[!grepl(paste0('NRT.',modscagfn,'.tif'), tiffiles)]

		if(length(tiffiles)==0) {
			print(' - no files found for this date. downloading from snowserver.')
			dir.create(PATH_DOY,rec=TRUE)
			system(paste0('scp -i ~/.ssh/snowserver_rsa snowserver.colorado.edu:/data/hydroData/WestUS_Data/MODSCAG/modscag-historic/',yr,'/',doy,'/*',modscagfn,'.tif ', PATH_DOY,'/'))
			tiffiles=dir(PATH_DOY,paste0('*',modscagfn,'.tif$'),full.names = TRUE)
			if(length(tiffiles)==0){
				stop('The historic fsca files do not exist on your computer at PATH_MODSCAGDOWNLOAD and I was unable to download them from snowserver.')
			}
		}
	}

	print(' - creating fsca image for the domain')
	if(!file.exists(fscafilename)){
		fsca=gdalUtils::gdalwarp(tiffiles,fscafilename,output_Raster = TRUE,t_srs = '+proj=longlat +datum=WGS84',te = c(xmin,ymin,xmax,ymax),tr=c(reso,reso),r='near',dstnodata='-99',ot='Int32')#make sure this is cast at least as a signed integer for -99
		correct_extent <- compareRaster(fsca,watermask, extent=T,rowcol=T, crs=T, res=T, rotation=F,stopiffalse = F)
		if(!correct_extent){
			stop('The fsca raster created from the given extents does not match the watermask raster.  Are you using the wrong RUNNAME or the wrong domain extents?')
		}

	} else {
		fsca=raster(fscafilename)
	}

	if(fveg_correction){
		fveg[fveg==100] <- 99 #avoid divide by 0
		simfsca <- fsca/(100-fveg)*100
		simfsca[fsca>100] <- fsca[fsca>100] #preserve cloud and no data flags.
		writeRaster(simfsca,simfscafilename,NAflag=-99)
	} else {
		simfsca=fsca
	}

	return(simfsca)

}

