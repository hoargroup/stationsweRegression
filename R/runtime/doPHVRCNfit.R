doPHVRCNfit <- function(xrdate,doydF,static_mdl,rstats=NULL,reconvec=NULL){
     oldopt=options()
#      print(options('warn'))
     on.exit(options(oldopt))
     options(warn=2)

     mdldata=mdldata[mdldata$Station_ID %in% doydF$Station_ID,]
     if(is.null(reconvec)){
     #get appropriate recon data
     doydF$recon=mdldata[mdldata$recondate==xrdate,'recon']
     doydF$recondate=mdldata[mdldata$recondate==xrdate,'recondate']
     # doydF$recon=scale(doydF$recon)
     doydF$recon=(doydF$recon - rstats[[1]])/rstats[[2]]
     
     } else {
          # doydF$recon=scale(reconvec)
          doydF$recon=(reconvec-rstats[[1]])/rstats[[2]]
          doydF$recondate='opt'
     }
	attr(doydF$recon,'scaled:center')=rstats[[1]]
	attr(doydF$recon,'scaled:scale')=rstats[[2]]


     # fix snotel record so there are no zeros
     snotelmdl=doydF$snotel
     snotelmdl=snotelmdl+runif(1,0,0.0001) # doesn't converge usually if there are 0s
#     snotelmdl=log(snotelmdl)
     doydF$snotel=snotelmdl
     #
     ## PHV and RECON
     newformula=paste(formula(static_mdl)[2], formula(static_mdl)[1], formula(static_mdl)[3],' + recon')
#   print(newformula)
     mdl.wrecon=try(
          glm(newformula,data=doydF,family=gaussian(link='identity')),
          TRUE)

     # print(mdl.wrecon)
     failed=inherits(mdl.wrecon,'try-error')
     # print(paste('failed:',failed))
     # print(paste('mdl.recon converged:',mdl.wrecon$converged))
     if(!failed & mdl.wrecon$converged) {
          if(mdl.wrecon$aic < static_mdl$aic){
               rcn.sig.phv=1
          } else {
               rcn.sig.phv=0
          }

          # mdl.anova=tryCatch({
          #      anova(static_mdl,mdl.wrecon,test='F')},error=function(e) mdl.anova=NA)
          #      print(mdl.anova)

          # ### Check significance of regression models
          # rcn.sig.phv=tryCatch({
          #      if(mdl.anova$P[2]<0.05){
          #           rcn.sig.phv=1
          #      } else {
          #           rcn.sig.phv=0}
          # }, error=function(e){
          #      print('----anova did not converge')
          #      rcn.sig.phv=NA})
     } else {
          mdl.wrecon=NA
          rcn.sig.phv=NA
     }
     # print(mdl.wrecon)
     # print(paste('rcn.sig.phv',rcn.sig.phv))
     return(list(mdl.wrecon,rcn.sig.phv,doydF))
}
