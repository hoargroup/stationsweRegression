doCOST=function(mdlpreds,cost,recon.version){
     #pass in predictions and a cost function as character  (cor, rmse etc)

     if(cost=='cor') { #statistics to maximize
          costfun<-function(y,yhat) cor(y,yhat)
          flag='max'
     } else if(cost=='r2') {
#           costfun<-function(y,yhat) 1-sum((y-yhat)^2)/sum((y-mean(y,na.rm=T))^2)
          costfun<-function(y,yhat) cor(y,yhat)^2
          flag='max'
     } else if(cost=='mae') { #statistics to minimize
          costfun<-function(y, yhat) mean(abs(yhat-y),na.rm=T)
          flag='min'
     } else if(cost=='rmse') {
          costfun<-function(y,yhat) sqrt(mean((yhat-y)^2,na.rm=T))
          flag='min'
     }


     daystats=ddply(mdlpreds,.(yrdoy,recondate),function(x){
          summarise(x,
                    phv.cor=costfun(snotel,phv.pred),
                    phvrcn.cor=costfun(snotel,phvrcn.pred),
                    phvfull.cor=costfun(snotel,phv.fullpred),
                    phvrcnfull.cor=costfun(snotel,phvrcn.fullpred),
                    reconrt.cor=costfun(snotel,reconrt),
                    reconrtfull.cor=costfun(snotel,reconrt.fullpred))
     })
     daystats$date=as.POSIXct(strptime(daystats$yrdoy,'%Y%j'),tz='MST')

     if(flag=='max'){
     whichrdate=ddply(daystats,.(date),summarise,
                    yr=strftime(unique(date),'%Y'),
                    yrdoy=strftime(unique(date),'%Y%j'),
                    phvrcn_recondate=recondate[which.max(phvrcnfull.cor)],
                    recon_costdate=recondate[which.max(reconrtfull.cor)],
                    skill_phv=max(phv.cor),
                    skill_phvrcn=max(phvrcn.cor),
                    skill_recon=max(reconrt.cor),
                    skill_phvfull=max(phvfull.cor),
                    skill_phvrcnfull=max(phvrcnfull.cor),
                    skill_reconfull=max(reconrtfull.cor))
     } else if(flag=='min') {
          whichrdate=ddply(daystats,.(date),summarise,
                    yr=strftime(unique(date),'%Y'),
                    yrdoy=strftime(unique(date),'%Y%j'),
                    phvrcn_recondate=recondate[which.min(phvrcnfull.cor)],
                    recon_costdate=recondate[which.min(reconrtfull.cor)],
                    skill_phv=min(phv.cor),
                    skill_phvrcn=min(phvrcn.cor),
                    skill_recon=min(reconrt.cor),
                    skill_phvfull=min(phvfull.cor),
                    skill_phvrcnfull=min(phvrcnfull.cor),
                    skill_reconfull=min(reconrtfull.cor))
     }
     return(whichrdate)
}
