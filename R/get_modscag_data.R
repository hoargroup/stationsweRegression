#' Function to get MODSCAG data
#' Either download the near realtime data from snowdav system or use archived historic data
#'
#' @param source 'NRT' or 'historic'. near realtime download from snowdav or archived data
#' @param doy day of year
#' @param yr year
#' @export
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
		return(simfsca)
	}

	if(type=='NRT'){
		print('not available right now')
		return()
	}

	if(type=='historic'){
		# if(is.null(imagepath)) print('please provide a path'); stop()
		# print(getwd())
		PATH_DOY=paste0(PATH_MODSCAGDOWNLOAD,'/',yr,'/',doy)
		dir.create(PATH_DOY, showWarnings = FALSE, rec=TRUE)
		tiffiles=dir(PATH_DOY,'*snow_fraction.tif$',full.names = TRUE)
		if(length(tiffiles)==0) {
			print('no files found for this date. downloading from snowserver.')
			system(paste0('scp -i ~/.ssh/snowserver_rsa snowserver.colorado.edu:/data/hydroData/WestUS_Data/MODSCAG/modscag-historic/',yr,'/',doy,'/*fraction.tif ', PATH_DOY,'/'))
			tiffiles=dir(PATH_DOY,'*snow_fraction.tif$',full.names = TRUE)
			if(length(tiffiles)==0){
				print('unable to download files from snowserver.')
				return(raster())
			}
		}
		simfsca=gdalUtils::gdalwarp(tiffiles,simfscafilename,output_Raster = TRUE,t_srs = '+proj=longlat +datum=WGS84',te = c(xmin,ymin,xmax,ymax),tr=c(reso,reso),r='near',dstnodata='-99')

	}

}

