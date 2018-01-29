#' Function to get MODSCAG data.
#'
#' The function will check for existing processed fsca images in the imagepath. If it doesn't exist, it will check for raw, unprocessed files and mosaic them. If no file exist it will attempt to download them from snowserver and mosaic them.
#'
#' @param doy day of year
#' @param yr year
#' @param type 'NRT' or 'historic'. near realtime download from snowdav or archived data
#' @param imagepath the directory where the mosaiced fsca .tif files exist
#' @param modscagfn 'snow_fraction' or 'snow_fraction_canadj'; portion of the filename before the extension to identify canopy corrected or not corrected modscag;
#' @param reso resolution of fsca
#' @param xmin minimum x coordinate of domain
#' @param xmax maximum x coordinate of domain
#' @param ymin minimum y coordinate of domain
#' @param ymax maximum y coordinate of domain
#' @export
#' @return rasterImage of fsca for the simulation date
#' @details called for its side effect of producing a mosaicked fsca image of the domain

get_modscag_data=function(doy=NULL,
													yr=NULL,
													type=NULL,
													imagepath=NULL,
													modscagfn='snow_fraction_canadj',
													reso,
													xmin,
													xmax,
													ymin,
													ymax){

	simfscafilename=paste0(imagepath,'/modscag_',modscagfn,'_',yr,doy,'.tif')

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

	} else if(type=='NRT'){

		PATH_DOY=paste0(PATH_MODSCAGDOWNLOAD,'/',yr,'/',doy)
		dir.create(PATH_DOY, showWarnings = FALSE, rec=TRUE)
		tiffiles=dir(PATH_DOY,paste0('*NRT.',modscagfn,'.tif$'),full.names = TRUE)

		if(length(tiffiles)==0){
			stop('The NRT fsca files do not exist on your computer. please download them from snow-dav using get_NRTmodscag.sh or otherwise (get_NRTmodscag.sh does not exist as part of this package because it uses password protected login for downloads).')
		}

	} else if(type=='historic'){

		PATH_DOY=paste0(PATH_MODSCAGDOWNLOAD,'/',yr,'/',doy)
		dir.create(PATH_DOY, showWarnings = FALSE, rec=TRUE)
		tiffiles=dir(PATH_DOY,paset0('*',modscagfn,'.tif$'),full.names = TRUE)
		tiffiles=tiffiles[!grepl(paste0('NRT.',modscagfn,'.tif'), tiffiles)]

		if(length(tiffiles)==0) {
			print(' - no files found for this date. downloading from snowserver.')
			system(paste0('scp -i ~/.ssh/snowserver_rsa snowserver.colorado.edu:/data/hydroData/WestUS_Data/MODSCAG/modscag-historic/',yr,'/',doy,'/*fraction.tif ', PATH_DOY,'/'))
			tiffiles=dir(PATH_DOY,paste0('*',modscagfn,'.tif$'),full.names = TRUE)
			if(length(tiffiles)==0){
				stop('The historic fsca files do not exist on your computer at PATH_MODSCAGDOWNLOAD and I was unable to download them from snowserver.')
			}
		}
	}

	print(' - creating fsca image for the domain')
	simfsca=gdalUtils::gdalwarp(tiffiles,simfscafilename,output_Raster = TRUE,t_srs = '+proj=longlat +datum=WGS84',te = c(xmin,ymin,xmax,ymax),tr=c(reso,reso),r='near',dstnodata='-99',ot='Int32')#make sure this is cast at least as a signed integer for -99

	correct_extent <- compareRaster(simfsca,watermask, extent=T,rowcol=T, crs=T, res=T, rotation=F,stopiffalse = F)
	if(correct_extent){
		return(simfsca)
	} else {
		stop('The fsca raster created from the given extents does not match the watermask raster.  Are you using the wrong RUNNAME or the wrong domain extents?')
	}

}

