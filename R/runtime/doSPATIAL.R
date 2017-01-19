#takes estimates residuals at snotel locations and fits krige model. substracts krige estimates from predictions (either at snotel snotellocs.usgs or for entire domain)
doSPATIAL=function(dF, snotellocs.usgs, covrange, app, newdatapreds=NULL,newdatalocs.agg.usgs=NULL){

    # host <- system("hostname", intern = TRUE)
    # cat("MC inner loop, recondate: ",as.character(unique(dF$recondate))," running on", host, ".\n")

     dF=mutate(dF,
          phvresid=phv.predictions-snotel,#
          phvrcnresid=phvrcn.predictions-snotel,
          # reconresid=recon-snotel,
          reconrtresid=reconrt-snotel)

if(grepl('idp',covrange)){
		covrange=as.numeric(gsub('idp','',covrange))
	} else if(grepl('km',covrange)){
	     covrange=as.numeric(gsub('km','',covrange))*1000
	}

## idw
snotellocs.usgs$phvresid=dF$phvresid
snotellocs.usgs$phvrcnresid=dF$phvrcnresid
snotellocs.usgs$reconrtresid=dF$reconrtresid
##

# ## kriging
#      locsmat=as.matrix(coordinates(snotellocs.usgs))
#      rlambda=summary(Krig(locsmat,dF$reconrtresid,theta=covrange,cov.function="wendland.cov"))$lambda
#      rresidkr=mKrig(locsmat,dF$reconrtresid,theta=covrange,lambda=rlambda,cov.function="wendland.cov")
#      slambda=summary(Krig(locsmat,dF$phvresid,theta=covrange,cov.function="wendland.cov"))$lambda
#      sresidkr=mKrig(locsmat,dF$phvresid,theta=covrange,lambda=slambda,cov.function="wendland.cov")
#      dlambda=summary(Krig(locsmat,dF$phvrcnresid,theta=covrange,cov.function="wendland.cov"))$lambda
#      dresidkr=mKrig(locsmat,dF$phvrcnresid,theta=covrange,lambda=dlambda,cov.function="wendland.cov")
# ##

    if(app=='xval'){

#library(pryr)
#print('---')

registerDoMC(10)
snotelpreds=ldply(unique(dF$Station_ID),.parallel=T,function(sta) {
#    print(sta)
    cvdF=dF[dF$Station_ID==sta,]
    staind=snotellocs.usgs$Station_ID==sta
    sta2pred=snotellocs.usgs[staind,]
    snotellocs.usgs=snotellocs.usgs[!staind,]

## idw
if(length(unique(dF$phv.predictions))!=1){
## idw
phv.fullpred=cvdF$phv.predictions-idw(phvresid~1,locations=snotellocs.usgs,newdata=sta2pred,idp=covrange,debug.level=0)$var1.pred
} else {
  phv.fullpred=rep(NA,nrow(dF))
}
if(length(unique(dF$phvrcn.predictions))!=1){
  phvrcn.fullpred=cvdF$phvrcn.predictions-idw(phvrcnresid~1,locations=snotellocs.usgs,newdata=sta2pred,idp=covrange,debug.level=0)$var1.pred
} else {
  phvrcn.fullpred=rep(NA,nrow(dF))
}
reconrt.fullpred=cvdF$reconrt-idw(reconrtresid~1,locations=snotellocs.usgs,newdata=sta2pred,idp=covrange,debug.level=0)$var1.pred

##
# ## kriging
#         phv.fullpred=dF$phv.predictions-predict(sresidkr,x=locsmat)
#         phvrcn.fullpred=dF$phvrcn.predictions-predict(dresidkr,x=locsmat)
#         reconrt.fullpred=dF$reconrt-predict(rresidkr,x=locsmat)
# ##

#         snotelpreds= data.frame(
#                         date=dF$date,
#                         Station_ID=dF$Station_ID,
#                         yrdoy=dF$yrdoy,
#                         snotel=dF$snotel,
#                         recondate=dF$recondate,
#                         reconrt=dF$reconrt,
#                         phvresid=dF$phvresid,
#                         phvrcnresid=dF$phvrcnresid,
#                         reconrtresid=dF$reconrtresid,
#                         phv.pred=dF$phv.predictions,
#                         phvrcn.pred=dF$phvrcn.predictions,
#                         phv.fullpred,
#                         phvrcn.fullpred,
#                         reconrt.fullpred)
    snotelpreds=data.frame(
        date=cvdF$date,
        Station_ID=cvdF$Station_ID,
        yrdoy=cvdF$yrdoy,
        snotel=cvdF$snotel,
        recondate=cvdF$recondate,
        reconrt=cvdF$reconrt,
        phvresid=cvdF$phvresid,
        phvrcnresid=cvdF$phvrcnresid,
        reconrtresid=cvdF$reconrtresid,
        phv.pred=cvdF$phv.predictions,
        phvrcn.pred=cvdF$phvrcn.predictions,
        phv.fullpred,
        phvrcn.fullpred,
        reconrt.fullpred)

  return(snotelpreds)
})

#print(mem_used())
return(snotelpreds)
     }

    if(app=='predict'){

## idw
coordinates(newdatapreds)=c('x','y')
proj4string(newdatapreds)=proj4string(newdatalocs.usgs)
# snotellocs.usgs=snotellocs.usgs[order(snotellocs.usgs$Station_ID),]
print(paste('num stations used:',length(unique(snotellocs.usgs$Station_ID))))
# snotelidw=idw(phvresid~1,locations=snotellocs.usgs,newdata=snotellocs.usgs,idp=covrange,debug=0)
# phvfullsnotel=dF$phv.predictions-snotelidw$var1.pred
# # print(snotelidw)
# print(data.frame(dF$snotel,dF$phv.predictions,dF$phvresid,snotellocs.usgs$phvresid,snotelidw$var1.pred,phvfullsnotel))

# write.table(as.data.frame(snotellocs.usgs),paste0('dospatial_snotellocs_',unique(dF$yrdoy),'.txt'),sep='\t',row.names=F, quote=F)
# phvidw=
# print(str(phvidw))
phv.fullpred=newdatapreds$phv.predictions-idw(phvresid~1,locations=snotellocs.usgs,newdata=newdatapreds,idp=covrange,debug.level=0)$var1.pred
phvrcn.fullpred=newdatapreds$phvrcn.predictions-idw(phvrcnresid~1,locations=snotellocs.usgs,newdata=newdatapreds,idp=covrange,debug.level=0)$var1.pred
reconrt.fullpred=newdatapreds$reconrt.predictions-idw(reconrtresid~1,locations=snotellocs.usgs,newdata=newdatapreds,idp=covrange,debug.level=0)$var1.pred
##

# ## kriging
#           xpred=as.data.frame(coordinates(newdatalocs.agg.usgs))
#           grid.list=list(x=newdatapreds[,1],y=newdatapreds[,2])
#
#           predagg=predict(sresidkr,x=xpred)
#           obj=list(x=xpred[,1],y=xpred[,2],z=predagg)
#           phv.fullpred=newdatapreds$phv.predictions-interp.surface.grid(obj,grid.list)
#
#           predagg=predict(dresidkr,x=xpred)
#           obj=list(x=xpred[,1],y=xpred[,2],z=predagg)
#           phvrcn.fullpred=newdatapreds$phvrcn.predictions-interp.surface.grid(obj,grid.list)
# ##


          return(
               data.frame(
                    phv.predictions=newdatapreds$phv.predictions,
                    phvrcn.predictions=newdatapreds$phvrcn.predictions,
                    reconrt.predictions=newdatapreds$reconrt.predictions,
                    phv.fullpred,
                    phvrcn.fullpred,
                    reconrt.fullpred)
               )
     }
}
