
# produce cross val predictions for all dyn iterations for a given doy.
CVwrapper <- function(xrdate,doydF,static_mdl,reconvec=NULL){
#    print(paste0('--recondate: ',xrdate))
#    phvrcn.out=withOptions(list(warn=2),doPHVRCNfit(xrdate,doydF,static_mdl))
     xrecon=scale(get_sca(xrdate,'swe'))
	 rstats=list(attr(xrecon,'scaled:center'),attr(xrecon,'scaled:scale'))
     phvrcn.out=doPHVRCNfit(xrdate,doydF,static_mdl,rstats,reconvec=reconvec)
     dyn_mdl=phvrcn.out[[1]]
     rcn.sig.phv=phvrcn.out[[2]]
     doy_rdoydF=phvrcn.out[[3]]
     cvpreds=doXVAL(doy_rdoydF,static_mdl,dyn_mdl,rcn.sig.phv)
     return(cvpreds)
}
