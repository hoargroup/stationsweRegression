num2ucoRaster=function(v){
     rpred=raster(matrix(v,nrow=2580,byrow=T))
     projection(rpred)='+proj=longlat +datum=WGS84'#CRS(projection(ucophv.stack))
     extent(rpred)=extent(c(-112.25,-104.125,33,43.75))#extent(ucophv.stack)
     return(rpred)
}