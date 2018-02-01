#' function to extract the 1st and 15th of each month from a single year of reconstruction
#'
#' @param ryr the year of reconstruction
#' @param nc_fns filenames of the netcdf reconstruction
#' @details It's assumed that the .nc file has dimensions Lat, Long, time. The variable is "swe" in meters. Values of the time dimension are stored YYYYMMDD.
#' @return rasterstack of reconstructed swe for the 1st and 15th of each year present in recondata*.nc files
#' @export

get_rcn_nc <- function(ryr,nc_fns){
	nc_filename=nc_fns[grep(pattern=ryr,x=nc_fns,fixed=T)]
	nc_stack <- stack(nc_filename)
	nc_names <- names(nc_stack)
	rdates <- strptime(nc_names,format='X%Y%m%d',tz='MST')
	rdays <- as.numeric(strftime(rdates,'%d'))
	keep_layers <- which(rdays==1 | rdays==15)
	return(nc_stack[[keep_layers]])
}
