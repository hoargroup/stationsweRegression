readgrads=function(t,fid=fid,nc=1950,nr=2580){
require(raster)
numel=nc*nr
# print(numel)
dtype=4
tindex=t*numel-numel+1
print(tindex/numel+1)
#print(dtype*tindex-dtype)
seek(fid, where = dtype*tindex-dtype,origin = "start") ## seek is 0 index based!
num=readBin(fid,size=dtype,n=numel,what='numeric')
print(length(num))
maty=matrix(num,byrow=T,ncol=nc)
mat=maty[nrow(maty):1,]
r=raster(mat)
projection(r)='+init=epsg:4326'
extent(r)=c(-112.25,-104.125,33,43.75)
return(r)
}
