doDOYfit<-function(doydF,cost,style,optflag,scalesnotel,fscaMatch,fordensource,dateflag) {  ## from ddply

mdate=unique(doydF$date)
print(paste0('model date: ',mdate))

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

# GLV surveys, remove uvcc?
# '2007-05-10','2006-05-11','2005-05-10','2004-05-12','2003-05-14','2002-05-01','2001-05-09')
}

     #set up empty dataframes to output below if there are no results.
     emptydf=function(yrdoy,dte){
          colvec=c('yrdoy','date','yr','phvrcn_recondate','recon_costdate','skill_phv','skill_phvrcn','skill_recon','skill_phvfull','skill_phvrcnfull','skill_reconfull')
          c1=data.frame(matrix(NA,ncol=length(colvec)))
          colnames(c1)=colvec
          c1$yrdoy=yrdoy
          c1$date=dte
          colvec=c('date','Station_ID','yrdoy','snotel','recondate','reconrt','phvresid','phvrcnresid','reconrtresid','phv.pred','phvrcn.pred','phv.fullpred','phvrcn.fullpred','reconrt.fullpred','phvrcn.opt','reconopt')
          c3=data.frame(matrix(NA,ncol=length(colvec)))
          colnames(c3)=colvec
          c3$yrdoy=yrdoy
          c3$date=dte
          colvec=c('date','ryr','Station_ID','historical_apcp','historical_cumdegday','apcp','cumdegday','dist','weight','mth','dy','recondate','recon','reconopt')
          c4=data.frame(matrix(NA,ncol=length(colvec)))
          colnames(c4)=colvec
          c4$date=dte

          return(list(c1,c3,c4))
     }
#
#      withOptions <- function(optionList, expr) {
#           oldOpts <- options(optionList)
#           on.exit(options(oldOpts))
#           expr # lazily evaluate
#      }

#      static_mdl=withOptions(list(warn=2),doPHVfit(doydF))

 snotellocs=snotellocs[snotellocs$Station_ID %in% doydF$Station_ID,]#this is incase we do another year such as fassnacht years. keep in mind that only years availble 2000-2012 will be included regardless if there wre others.
 snotellocs.usgs=spTransform(snotellocs,CRS('+init=epsg:5070'))

if(!identical(doydF$snotel,rep(0,nrow(doydF)))) {


#scale snotel
    sca=get_sca(mdate,'fsca')
    if(scalesnotel=='scale'){
      scaraster=raster(matrix(sca,nrow=2580,byrow=T),xmn=-112.25,xmx=-104.125,ymn=33,ymx=43.75)
      projection(scaraster)='+proj=longlat +datum=WGS84'
      snotelsca=raster::extract(scaraster, snotellocs,cellnum=T)#get values at snotel pixel locations. named sca so we can use its length later

      forden=get_forden(fordensource,unique(doydF$yr))
      snotelforden=raster::extract(forden,snotellocs)
      snotelforden[snotelforden==1]=0.999# avoid divide by zero
    #--
      snotelsca[,2]=snotelsca[,2]/(1-snotelforden)#if 1-forden is > sca from satellite, then resultant sca is >1. different products don't always match
      snotelsca[snotelsca[,2]>1,'layer']=1##if clouded, assume 100%sca
      tmp=data.frame(snotelsca,snotel=doydF$snotel)
      ind=which(tmp$snotel > 0 & tmp$layer==0)
	  sca[tmp[ind,'cells']]=0.1#change sca value at locations with snotel to 0.1 where they are 0 if snotel swe > 0
	  tmp[ind,'layer']=0.1
	  snotelsca=tmp[,'layer']
      doydF$snotel=doydF$snotel*snotelsca
    }

          static_mdl<-doPHVfit(doydF)
          if(length(static_mdl)!=1){
#               print(formula(static_mdl))
              if(fscaMatch=='fsca'){
                rdate=dofscaMatching(mdate,doydF,sca,style)
              } else {
                rdate=NULL
              }
              cvpreds=doXRecon(doydF,static_mdl,style,rdate)#give all relevant dates to do xrecon. realtime only uses dates from previous years.
              if(!empty(cvpreds)){

            fullpreds=ddply(cvpreds,.(recondate),doSPATIAL,snotellocs.usgs,covrange,'xval',.inform=F,.parallel=F)#,.paropts=list(.export=ls(),.packages=.packages(all=T)))
            which_recon_date=doCOST(fullpreds,cost,recon.version)#
#             print('after docost')
            bestind=which(fullpreds$recondate %in% which_recon_date$phvrcn_recondate)
            fullpreds=fullpreds[bestind,]

if(optflag=='opt'){
               optlist=doOptRecon(doydF,static_mdl,style)
			         optpreds=optlist[[1]]
               wmat=optlist[[2]]
#
               fullpreds$phvrcn.opt=optpreds$phvrcn.predictions
               fullpreds$reconopt=optpreds$reconrt
} else {
wmat=emptydf(unique(doydF$yrdoy),unique(doydF$date))[[3]]
fullpreds$phvrcn.opt=NA
fullpreds$reconopt=NA
}
#                 print('--fullpreds:')
#                print(str(fullpreds))
#                print('--wrd')
# print(str(which_recon_date))
# print('--wmat')
# print(str(wmat))

               return(list(which_recon_date,fullpreds,wmat))
               }
          } else {
               print('--phv model didn\'t converge')
               return(emptydf(unique(doydF$yrdoy),unique(doydF$date)))
          }
     } else {
          print('--no snow left at SNOTEL stations')
          return(emptydf(unique(doydF$yrdoy),unique(doydF$date)))
     }
}
