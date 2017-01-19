#does cross validation for a doy and an iteration
doXVAL <- function(doy_rdoydF,static_mdl,dyn_mdl,rcn.sig.phv){
     #
     if(!is.na(static_mdl) && !is.na(dyn_mdl)){
          ## cross validation
          myglm= function(formula,data) {
               glm(formula,data,family=gaussian(link='identity'))
          }
          mypredict.glm <- function(object, newdata){
               predict(object, newdata,type='response')
          }
#           print('static mdl')
#           print(formula(static_mdl))
#           print(str(doy_rdoydF))

          #PHV only surface
          phv=errorest(formula=formula(static_mdl),data=doy_rdoydF, model=myglm, predict=mypredict.glm, estimator='cv',est.para=control.errorest(k=nrow(doy_rdoydF), predictions=T))
          #PHV and RECON surface
          phvrcn=errorest(formula=formula(dyn_mdl),data=doy_rdoydF, model=myglm, predict=mypredict.glm, estimator='cv',est.para=control.errorest(k=nrow(doy_rdoydF), predictions=T))

     } else {
          phv=data.frame(predictions=rep(NA,nrow(doy_rdoydF)))
          phvrcn=data.frame(predictions=rep(NA,nrow(doy_rdoydF)))
     }
     #
     #print(str(doy_rdoydF))
     return(     
     data.frame(
          date=doy_rdoydF$date,
          yrdoy=doy_rdoydF$yrdoy,
          recondate=doy_rdoydF$recondate,
          Station_ID=doy_rdoydF$Station_ID,
          snotel=doy_rdoydF$snotel,
          phv.predictions=phv$predictions,
          phvrcn.predictions=phvrcn$predictions,
          reconrt=unscale_vec(doy_rdoydF$recon),
          sig=rcn.sig.phv)
     )
}
