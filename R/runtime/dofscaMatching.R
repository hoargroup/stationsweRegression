dofscaMatching=function(mdate,doydF,sca,style){

	cost='rmse'
	if(cost=='rmse') {
          costfun<-function(y,yhat) sqrt(mean((yhat-y)^2,na.rm=T))
          flag='min'
     }

ryrs=unique(recondata[!is.na(recondata$recon),'yr'])
if(style=='real-time') {
  myr=unique(doydF$yr)#get year of model sim
  ryrs=ryrs[ryrs<myr]#subset recondates to feed to fitting process so only previous years are available.
}
registerDoMC(3)
rdates=ldply(ryrs,.parallel=T,function(yr){
		rfn=paste0('data/recon_',recon.version,'/reconfscadata_',yr,'_',recon.version,'.nc')
    	ncstack=stack(rfn)
    	dates2test=paste0('X',yr,as.vector(outer(c('03','04','05'),c('01','15'),paste0)))
    	ind=which(names(ncstack) %in% dates2test)
		ncstack2=ncstack[[ind]]
    
	fscadates=ldply(1:nlayers(ncstack2),function(n){
		rvals=getValues(ncstack2[[n]])
		rbad=rvals>1
		fbad=sca>1
		sca=sca[!rbad & !fbad]
		rvals=rvals[!rbad & !fbad]
		costobj=costfun(sca,rvals)
		rdate=strptime(names(ncstack2[[n]]),'X%Y%m%d',tz='MST')
		data.frame(rdate,mdate,costobj)
	})
	return(fscadates)
})
rdate=rdates[which.min(rdates$costobj),'rdate']
return(rdate)

}