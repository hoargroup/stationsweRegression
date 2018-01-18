#' Function to get MODSCAG data.
#'
#' The function will check for existing processed fsca images in the imagepath. If it doesn't exist, it will check for raw, unprocessed files and mosaic them. If no file exist it will attempt to download them from snowserver and mosaic them.
#'
#' @param doy day of year
#' @param yr year
#' @param type 'NRT' or 'historic'. near realtime download from snowdav or archived data
#' @param imagepath the directory where the mosaiced fsca .tif files exist
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
													reso,
													xmin,
													xmax,
													ymin,
													ymax){

	simfscafilename=paste0(imagepath,'/modscag_fsca_',yr,doy,'.tif')
	if(file.exists(simfscafilename)) {
		print('fsca image exists. nothing to do. returning fsca.')
		simfsca=raster(simfscafilename)
		# corners=SpatialPoints(rbind(c(xmin,ymin),c(xmin,ymax),c(xmax,ymax),c(xmax,ymin)))
		# correct_grid <- rgeos::gCovers(as(extent(simfsca), "SpatialPolygons"), as(extent(corners), "SpatialPolygons"))
		correct_extent <- compareRaster(simfsca,raster(xmn=xmin,xmx=xmax,ymn=ymin,ymx=ymax), extent=T,rowcol=F, crs=T, res=F, rotation=F,stopiffalse = F)
		if(!correct_extent){
			stop('the extents of the existing fsca image do not match the extents provided in the run file. are you using the wrong RUNNAME?')
		} else {
			return(simfsca)
		}

	}

	if(type=='NRT'){
		PATH_DOY=paste0(PATH_MODSCAGDOWNLOAD,'/',yr,'/',doy)
		dir.create(PATH_DOY, showWarnings = FALSE, rec=TRUE)
		tiffiles=dir(PATH_DOY,'*NRT.snow_fraction.tif$',full.names = TRUE)

		if(length(tiffiles)==0){
			stop('The NRT fsca files do not exist on your computer. please download them from snow-dav using get_NRTmodscag.sh or otherwise (get_NRTmodscag.sh does not exist as part of this package because it uses password protected login for downloads).')
		}
	}

	if(type=='historic'){
		# if(is.null(imagepath)) print('please provide a path'); stop()
		# print(getwd())
		PATH_DOY=paste0(PATH_MODSCAGDOWNLOAD,'/',yr,'/',doy)
		dir.create(PATH_DOY, showWarnings = FALSE, rec=TRUE)
		tiffiles=dir(PATH_DOY,'*snow_fraction.tif$',full.names = TRUE)
		tiffiles=tiffiles[!grepl("NRT.snow_fraction.tif", tiffiles)]

		if(length(tiffiles)==0) {
			print('no files found for this date. downloading from snowserver.')
			system(paste0('scp -i ~/.ssh/snowserver_rsa snowserver.colorado.edu:/data/hydroData/WestUS_Data/MODSCAG/modscag-historic/',yr,'/',doy,'/*fraction.tif ', PATH_DOY,'/'))
			tiffiles=dir(PATH_DOY,'*snow_fraction.tif$',full.names = TRUE)
			if(length(tiffiles)==0){
				stop('The historic fsca files do not exist on your computer at PATH_MODSCAGDOWNLOAD and I was unable to download them from snowserver.')
			}
		}
	}

	simfsca=gdalUtils::gdalwarp(tiffiles,simfscafilename,output_Raster = TRUE,t_srs = '+proj=longlat +datum=WGS84',te = c(xmin,ymin,xmax,ymax),tr=c(reso,reso),r='near',dstnodata='-99',ot='Int32')#make sure this is cast at least as a signed integer for -99

	return(simfsca)

}

