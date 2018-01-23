#' Load the watermask
#'
#' This function will load the watermask for the given RUNNAME and compare extents vs given extents.
#'
#' @param runname RUNNAME specified in the run file
#' @export
#' @return rasterImage of watermask

load_watermask <- function(runname){
	watermask <- raster(paste0(runname,'/data/gis/',toupper(runname),'_watermask.tif'))
	correct_extent <- compareRaster(watermask,raster(xmn=EXTENT_WEST,xmx=EXTENT_EAST,ymn=EXTENT_SOUTH,ymx=EXTENT_NORTH), extent=T,rowcol=F, crs=F, res=F, rotation=F,stopiffalse = F)
	if(!correct_extent){
		stop('the extents of the watermask do not match the extents provided in the run file. are you using the wrong RUNNAME or is there a typo in the EXTENTS?')
	} else {
		return(watermask)
	}
}
