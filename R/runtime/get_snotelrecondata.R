get_snotelrecondata<- function(rversion,ryrs){
    tmp=new.env()
    for(fn in paste0('data/recon_',rversion,'/snotelrecondata_',ryrs,'_',rversion,'.RData')){
        load(fn,envir=tmp)
    }
    snotelrecon=do.call(rbind,lapply(ls(pattern='snotelrecon20',envir=tmp),get,envir=tmp))
    snotelrecon$date=as.POSIXct(snotelrecon$date)
    return(snotelrecon)
}
