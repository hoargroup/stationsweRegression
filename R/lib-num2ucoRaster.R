#' num2ucoRaster
#'
#' function to convert vector to raster
#' @param v numeric vector
#' @param nr number of rows, default 2580
#' @return raster object for UCO domain
#' @export

num2ucoRaster=function(v, nr=2580){
     rpred=raster(matrix(v,nrow=nr,byrow=T))
     projection(rpred)='+proj=longlat +datum=WGS84'#CRS(projection(ucophv.stack))
     extent(rpred)=extent(c(-112.25,-104.125,33,43.75))#extent(ucophv.stack)
     return(rpred)
}
