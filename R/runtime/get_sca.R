get_sca=function(dte,var){
    yr=strftime(dte,'%Y')
    doy=strftime(dte,'%j')
if(var=='fsca'){
    r=raster(paste0('data/modscagdownloads/',yr,doy,'/mosaicked/modscag_fsca_',yr,doy,'.tif'))
    vals=getValues(r)
    valind=which(vals<=100)
    if(length(valind)>1) vals[valind]=vals[valind]/100
} else if(var=='swe'){
# print(as.numeric(strftime(dte,'%m')))
# print(max(snotelrecon$date))
if(dte<max(snotelrecon$date) && as.numeric(strftime(dte,'%m')) >=3){   
 	rfn=paste0('data/recon_',recon.version,'/recondata_',yr,'_',recon.version,'.nc')
     ncstack=stack(rfn)
     stckdate=strftime(dte,'X%Y%m%d')
     rind=grep(stckdate,names(ncstack))
     vals=getValues(ncstack[[rind]])
 } 
     # else {
#      print('using modscag fsca for masking. may not be completely cloudfree')
#      sfn=file.path('data','selectdates','modscag',paste0('fsca',yr,'.nc'))
#      ncstack=stack(sfn)
#      stckdate=strftime(dte,'X%Y%j')
#     rind=grep(stckdate,names(ncstack))
#     vals=tryCatch({  getValues(ncstack[[rind]])},error=function(x) {#this error should never happen
#     print(paste0('the date ',dte,' was not available in the selected sca image'))})
#     valind=which(vals<=100)
#     if(length(valind)>1) vals[valind]=vals[valind]/100
# }
}
 return(vals)
}
