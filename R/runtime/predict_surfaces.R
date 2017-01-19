#use static model and best dynamic model to produce surface estimates for a doy. iterate by year. returns rasters  for stacking
predict_surfaces=function(rdatedF,snotellocs,snotellocs.usgs,newdata,newdatalocs,newdatalocs.agg.usgs,spatialblend,bpth,scalesnotel,fordensource,dateflag){
  #    rdatedF=which_recon_date[2,]#for testing only!

  # get reconyear and load the stack with that recon data.
  mdate=rdatedF$date
  rdate=rdatedF$phvrcn_recondate
  yr=rdatedF$yr
  print(paste0('-- model date: ',mdate))
  print(paste0('---- recon date: ',rdate))

  # subset mdldata for date
  doydF=doidata[doidata$date==mdate,]
  if(dateflag=='surveyvalidation'){
    if(mdate==as.POSIXct('2008-03-16',tz='MST')) doydF=subset(doydF,Station_ID!='07M29S')   #Lizard Head
    if(mdate==as.POSIXct('2008-04-03',tz='MST')) doydF=subset(doydF,Station_ID!='05J37S')   #Joe Wright
    if(mdate==as.POSIXct('2008-04-04',tz='MST')) doydF=subset(doydF,Station_ID!='06J01S')   #Dry Lake
    if(mdate==as.POSIXct('2008-04-05',tz='MST')) doydF=subset(doydF,Station_ID!='06H19S')   #South Brush Creek
    if(mdate==as.POSIXct('2008-04-07',tz='MST')) doydF=subset(doydF,Station_ID!='05J42S')   #Niwot
    if(mdate==as.POSIXct('2008-05-01',tz='MST')) doydF=subset(doydF,Station_ID!='05J37S')
    if(mdate==as.POSIXct('2008-05-02',tz='MST')) doydF=subset(doydF,Station_ID!='06J01S')
    if(mdate==as.POSIXct('2008-05-05',tz='MST')) doydF=subset(doydF,Station_ID!='05J42S')
    if(mdate==as.POSIXct('2009-01-31',tz='MST')) doydF=subset(doydF,Station_ID!='05J37S')
    if(mdate==as.POSIXct('2009-02-28',tz='MST')) doydF=subset(doydF,Station_ID!='05J37S')
    if(mdate==as.POSIXct('2009-03-01',tz='MST')) doydF=subset(doydF,Station_ID!='06H19S')
    if(mdate==as.POSIXct('2009-03-06',tz='MST')) doydF=subset(doydF,Station_ID!='05J42S')
    if(mdate==as.POSIXct('2009-03-17',tz='MST')) doydF=subset(doydF,Station_ID!='10F09S')
    if(mdate==as.POSIXct('2009-03-28',tz='MST')) doydF=subset(doydF,Station_ID!='06J01S')   #Dry Lake
    if(mdate==as.POSIXct('2009-03-29',tz='MST')) doydF=subset(doydF,Station_ID!='06H19S')   #South Brush Creek
    if(mdate==as.POSIXct('2009-04-02',tz='MST')) doydF=subset(doydF,Station_ID!='05J37S')   #Joe Wright
    if(mdate==as.POSIXct('2009-04-03',tz='MST')) doydF=subset(doydF,Station_ID!='05J42S')   #Niwot
    if(mdate==as.POSIXct('2009-05-02',tz='MST')) doydF=subset(doydF,Station_ID!='05J37S')   #Joe Wright
    if(mdate==as.POSIXct('2001-04-27',tz='MST')) doydF=subset(doydF,Station_ID!='06M23S')   #Lily Pond
    if(mdate==as.POSIXct('2002-04-03',tz='MST')) doydF=subset(doydF,Station_ID!='06M23S')   #Lily Pond
    if(mdate==as.POSIXct('2001-04-22',tz='MST')) doydF=subset(doydF,Station_ID!='07M30S')   #Slumgullion
    if(mdate==as.POSIXct('2002-04-06',tz='MST')) doydF=subset(doydF,Station_ID!='07M30S')   #Slumgullion
    if(mdate==as.POSIXct('2001-04-23',tz='MST')) doydF=subset(doydF,Station_ID!='06M03S')   #Upper San Juan
    if(mdate==as.POSIXct('2002-04-05',tz='MST')) doydF=subset(doydF,Station_ID!='06M03S')   #Upper San Juan
    if(mdate==as.POSIXct('2001-04-24',tz='MST')) doydF=subset(doydF,Station_ID!='06M17S')   #Wolf Creek Summit
    if(mdate==as.POSIXct('2002-04-04',tz='MST')) doydF=subset(doydF,Station_ID!='06M17S')   #Wolf Creek Summit

    snotel2predict=snotellocs[!(snotellocs$Station_ID %in% doydF$Station_ID),]
    # GLV surveys, remove uvcc?
    # '2007-05-10','2006-05-11','2005-05-10','2004-05-12','2003-05-14','2002-05-01','2001-05-09')
  }

  snotellocs=snotellocs[snotellocs$Station_ID %in% doydF$Station_ID,]#this is incase we do another year such as fassnacht years. keep in mind that only years availble 2000-2012 will be included regardless if there wre others.
  snotellocs.usgs=snotellocs.usgs[snotellocs.usgs$Station_ID %in% doydF$Station_ID,]#this is incase we do another year such as fassnacht years. keep in mind that only years availble 2000-2012 will be included regardless if there wre others.
  recondata=recondata[recondata$Station_ID %in% doydF$Station_ID,]

  if(!is.na(rdate)){

    sca=get_sca(mdate,'fsca')
    scaraster=num2ucoRaster(sca)#raster(matrix(sca,nrow=2580,byrow=T),xmn=-112.25,xmx=-104.125,ymn=33,ymx=43.75)
    #projection(scaraster)='+proj=longlat +datum=WGS84'
    snotelsca=raster::extract(scaraster, snotellocs,cellnum=T)#get values at snotel pixel locations. named sca so we can use its length later
    #
    forden=get_forden(fordensource,yr)
    snotelforden=raster::extract(forden,snotellocs)
    snotelforden[snotelforden==1]=0.999
    #--
    snotelsca[,2]=snotelsca[,2]/(1-snotelforden) #if 1-forden is > sca from satellite, then resultant sca is >1. different products don't always match
    snotelsca[snotelsca[,2]>1,'layer']=1 ##if clouded, assume 100%sca
    tmp=data.frame(snotelsca,snotel=doydF$snotel)
    which(tmp$layer==0)
    which(tmp$snotel>0)
    ind=which(tmp$snotel > 0 & tmp$layer==0)
    sca[tmp[ind,'cells']]=0.1#change sca value at locations with snotel to 0.1 where they are 0 if snotel swe > 0
    tmp[ind,'layer']=0.1
    snotelsca=tmp[,'layer']
    if(scalesnotel=='scale'){
      doydF$snotel=doydF$snotel*snotelsca
    }

    if(output!='points'){
      recon=get_sca(rdate,'swe')
      #		sca=get_sca(mdate,'swe')##better to use for masking because doesn't drop to zero randomly
      #	    sca[tmp[ind,'cells']]=0.1#change sca value at locations with snotel to 0.1 where they are 0 if snotel swe > 0

      #save indices of sca to insert predictions
      scaind=which(!is.na(sca) & sca!=0 & sca<200)#modscag is 0-100 but scaled 0-1 in get_sca, recon fsca is 0-1
      cloudind=which(sca>200)
      print(paste0('num of clouds: ',length(cloudind)))

      #combine ucophv dataframe and recon data
      newdata2=cbind(newdata,recon)
      newdata2$recon=scale(newdata2$recon)#convoluted but necessary order of operations
      rstats=list(attr(newdata2$recon,'scaled:center'),attr(newdata2$recon,'scaled:scale'))
      newdata2$recon=as.numeric(newdata2$recon)
      newdata2=newdata2[scaind,]

    } else {
      newdata2=mdldata[mdldata$date==rdatedF$date,]
      newdatalocs.usgs=data.frame(as.data.frame(snotellocs.usgs)[,c('Station_ID','x','y')],recon=newdata2$recon)
      newdata2$recon=scale(newdata2$recon)
      sca=snotelsca
      scaind=which(sca>0)#convert to binary. if cloudy then assume sca>0. this should only be jan-feb if it happens
      cloudind=NULL
      newdata2=newdata2[scaind,]
    }

    # fit models for phv and phvrcn for recondate in rdatedF
    static_mdl=doPHVfit(doydF)
    if(length(static_mdl)>1){
      dyn_mdl=doPHVRCNfit(rdate,doydF,static_mdl,rstats)[[1]]
      if(length(dyn_mdl)>1){

        #predict best models for whole domain
        static.pred=predict(static_mdl,newdata2,type='response')#
        dyn.pred=predict(dyn_mdl,newdata2,type='response')

        #setup dataframes for spatial analysis
        newdatapreds=data.frame(
          coordinates(newdatalocs.usgs)[scaind,],
          phv.predictions=static.pred,#newdata was already subset for sca above
          phvrcn.predictions=dyn.pred,#
          reconrt.predictions=get_sca(rdatedF$recon_costdate,'swe')[scaind])

        locpreds=data.frame(
          date=doydF$date,
          yrdoy=doydF$yrdoy,
          phv.predictions=predict(static_mdl,type='response'),
          phvrcn.predictions=predict(dyn_mdl,type='response'),
          snotel=doydF$snotel,
          reconrt=recondata[recondata$recondate==rdatedF$recon_costdate,'recon'])
        # print('preds')
        # print(data.frame(snotel=doydF$snotel,phv=locpreds$phv.predictions))

        newdatapredsfull=data.frame(phv.predictions=rep(0,length(sca)),phvrcn.predictions=0,phv.fullpred=0,phvrcn.fullpred=0,reconrt.predictions=0,reconrt.fullpred=0)
        #geostatistical blending of residuals
        if(spatialblend=='blend'){
          fullpreds=doSPATIAL(locpreds,snotellocs.usgs, covrange, 'predict', newdatapreds,newdatalocs.agg.usgs)# newdatapreds has newdata locations and regression predictions.
          fullpreds[fullpreds<0]=0.001
          #       print(str(fullpreds))
          newdatapredsfull$phv.predictions[scaind]=fullpreds$phv.predictions
          newdatapredsfull$phvrcn.predictions[scaind]=fullpreds$phvrcn.predictions
          newdatapredsfull$reconrt.predictions[scaind]=fullpreds$reconrt.predictions
          newdatapredsfull$phv.fullpred[scaind]=fullpreds$phv.fullpred
          newdatapredsfull$phvrcn.fullpred[scaind]=fullpreds$phvrcn.fullpred
          newdatapredsfull$reconrt.fullpred[scaind]=fullpreds$reconrt.fullpred

          # phvfull=num2ucoRaster(newdatapredsfull$phv.fullpred)
          # 	  print(data.frame(snotel=doydF$snotel,phvfull=extract(phvfull,snotellocs)))

          newdatapredsfull$phv.fullpred[cloudind]=sca[cloudind]
          newdatapredsfull$phvrcn.fullpred[cloudind]=sca[cloudind]
          newdatapredsfull$reconrt.fullpred[cloudind]=sca[cloudind]
        } else {
          newdatapredsfull$phv.predictions[scaind]=newdatapreds$phv.predictions
          newdatapredsfull$phvrcn.predictions[scaind]=newdatapreds$phvrcn.predictions
          newdatapredsfull$reconrt.predictions[scaind]=newdatapreds$reconrt.predictions
          newdatapredsfull$phv.predictions[newdatapredsfull$phv.predictions<0]=0.001#individual masking because newdatapreds needs x y
          newdatapredsfull$phvrcn.predictions[newdatapredsfull$phvrcn.predictions<0]=0.001
          newdatapredsfull$reconrt.predictions[newdatapredsfull$reconrt.predictions<0]=0.001
          # newdatapredsfull=data.frame(phv.fullpred=0,phvrcn.fullpred=0,as.data.frame(newdatalocs.usgs))
          # newdatapredsfull$phv.fullpred[scaind]=NA
          # newdatapredsfull$phvrcn.fullpred[scaind]=NA
          # newdatapredsfull$reconrt.fullpred[scaind]=NA
        }

        newdatapredsfull$phv.predictions[cloudind]=sca[cloudind]
        newdatapredsfull$phvrcn.predictions[cloudind]=sca[cloudind]
        newdatapredsfull$reconrt.predictions[cloudind]=sca[cloudind]


        return(newdatapredsfull)
      } else {
        print(paste(mdate,'empty model'))
        return(data.frame())
      }
    } else {
      print(paste(mdate,'empty model'))
      return(data.frame())
    }
  } else {
    print(paste(mdate,'empty model'))
    return(data.frame())
  }
}
