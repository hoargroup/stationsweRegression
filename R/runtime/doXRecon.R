#takes static model and fits all iterations of dyn model to get cross validated predictions.
doXRecon <- function(doydF,static_mdl,style,rdate=NULL){
if(!is.null(rdate)){
          doyfits=CVwrapper(rdate,doydF,static_mdl)
          return(doyfits)
     } else {

     rdates=unique(recondata[!is.na(recondata$recon),'recondate'])
     if(style=='real-time') {
          myr=unique(doydF$yr)#get year of model sim
          ryr=as.numeric(strftime(rdates,'%Y'))#vector of year of recon dates
          rdates=rdates[ryr<myr]#subset recondates to feed to fitting process so only previous years are available.
     }
     # print(paste(style,'rdates:',length(rdates)))

     doyfits=data.frame()
     if( length(rdates)!=0 ) {
          doyfits=ldply(as.list(rdates),CVwrapper,doydF,static_mdl)#, .parallel=F,.inform=F)
     }
          return(doyfits)
     }
  }
