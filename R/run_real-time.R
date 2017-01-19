# setwd('~/GoogleDrive/real-time_interpolation')
# library(tidyr)
# library(plyr)
# library(dplyr)
# library(readr)
# library(raster)
# library(MASS)
# library(ipred)
# library(doMC)
# library(gstat)
# library(ncdf4)
# library(rgdal)

lapply(list.files('src/lib',pattern='.R',full.names=T),source)
lapply(list.files('src/runtime',pattern='.R',full.names=T),source)

## ----- set up runtime flags  --> in reports/web_report.Rmd
# recon.version='v3.1'
# cost='r2'
# style='real-time'
# optflag='noopt'
# scalesnotel='scale'
# fscaMatch='wofsca'
# spatialblend='noblend'#flag to do geostatistical blending or not (prediction stage only; always does in the CV stage).
# output='surface'#points' #'surface' #just predict at snotel pixels #for 'points' spatialblend must also be 'blend'
# covrange='idp1'#
# fordensource='umd_forden'#'nlcd_forden'
# dateflag='C'

## ------ set up folders
configpath=paste0('diagnostics/rswe_',recon.version,'/covrange',covrange,'/snotel',scalesnotel,'/',dateflag,'/')
system(paste0('mkdir -p ', configpath,'fullpreds/xval'))
system(paste0('mkdir -p ', configpath,'fullpreds/',cost,'/netcdf/',style))

### --- which date are we simulating? best cloud free image from https://worldview.earthdata.nasa.gov
# --> in reports/web_report.rmd
# simdate=as.POSIXct('2015-04-29',tz='MST')
# yr=strftime(simdate,'%Y')
# doy=strftime(simdate,'%j')

## --- snotel inventory downloaded from http://www.wcc.nrcs.usda.gov/nwcc
nwccinv=tbl_df(read.csv('data/nwcc_snotel_inventory.csv'))

## ---- read more accurate locations, fix station names and create spatial points dF
snotellocs=tbl_df(read.csv('data/SNOTEL_MASTER.csv',header=T,stringsAsFactors=F))
snotellocs=tbl_df(merge(snotellocs,nwccinv,by.x='Site_ID',by.y='station.id'))
snotellocs.df=snotellocs %>%
		dplyr::select(Station_ID,Site_ID,site_name, State,Latitude,Longitude,Elevation_m,end_date) %>%
		mutate(
			end_date=as.POSIXct(as.character(end_date)),
			site_name=as.character(site_name),
			site_name=gsub('#','no.',site_name,fixed=T),#ifelse(grepl('#',site_name),gsub('#','no.',site_name,fixed=T),site_name),
			site_name=gsub('-',' ',site_name,fixed=T), #site_name=ifelse(grepl('-',site_name),gsub('-',' ',site_name,fixed=T),site_name),
			site_name=gsub('\'','',site_name,fixed=T),# site_name=ifelse(grepl('\'',site_name),gsub('\'','',site_name,fixed=T),site_name),
			Station_ID=gsub(' ','',Station_ID)) %>%
		dplyr::filter(Latitude>=33, Latitude<=43.75, Longitude<= -104.125, Longitude>= -112.25)

snotellocs=as.data.frame(snotellocs.df)
coordinates(snotellocs)= ~Longitude+Latitude
coordnames(snotellocs)=c('x','y')
proj4string(snotellocs)='+proj=longlat +datum=WGS84'
snotellocs.usgs=spTransform(snotellocs,'+init=epsg:5070')

## ---  for testing before there is snow
# # site_id=663
# for(site_id in ucrb$station.id){
# system(paste('source src/runtime/getcurrent_snotel.sh', site_id, 'STAND Daily YEAR'))
# }
#
# fns=list.files(path=paste0('data/snoteldownloads'),pattern='YEAR.csv')
# allfiles=lapply(fns,
# 	function(fname) {
# 	read.csv(paste0('data/snoteldownloads/',fname),skip=2,header=T,check.names=F,stringsAsFactors=F)
# })
# dat=rbind_all(allfiles)
# dat=dat[,-ncol(dat)]
# colnames(dat)=c('Site_ID','date','time','swe','pcp','tobs','tmax','tmin','tavg','snwd')
# str(dat)
## --------------

# ## runtime download snotel data from last week
if( as.numeric(Sys.Date()-as.Date(simdate)) > 7) {
  # for(site_id in snotellocs.df$Site_ID){
  #   system(paste('source src/runtime/gethistoric.sh', site_id, 'STAND Daily', yr, 'CY'))
  # }
  fns=list.files(path=paste0('data/snoteldownloads'),pattern=paste0(yr,'CY.csv'))
} else {
   # system('chmod 775 src/runtime/getcurrent_snotel.sh')
# for(site_id in snotellocs.df$Site_ID){
# system(paste('source src/runtime/getcurrent_snotel.sh', site_id, 'STAND Daily WEEK'))
# }
  fns=list.files(path=paste0('data/snoteldownloads'),pattern='WEEK.csv')
}

allfiles=lapply(fns,
	function(fname) {
	read.csv(paste0('data/snoteldownloads/',fname),skip=2,header=T,check.names=F,stringsAsFactors=F)
})
dat=bind_rows(allfiles)
dat=dat[,-ncol(dat)]
colnames(dat)=c('Site_ID','date','time','swe','pcp','tobs','tmax','tmin','tavg','snwd')
dat <- tbl_df(dat)
# ## -------

## --- get modscag NRT image
# system(paste('source src/runtime/get_modscag.sh ',doy, yr))

## merge snotellocs with new snotel data in dat
dat2=full_join(dplyr::select(snotellocs.df,Station_ID,Site_ID,Longitude,Latitude),dat,by=c('Site_ID'))
# str(dat2)
snoteldata=dat2 %>%
	# filter(time=='') %>%
	dplyr::select(Station_ID,Site_ID,Longitude, Latitude,date,swe,snwd) %>%
	mutate(
		date=as.POSIXct(date,tz='MST'),
		snotel=replace(swe,swe<0,  NA),
		snotel=snotel*2.54/100,#convert inches to meters
		snwd=replace(snwd,snwd<0,  NA),
		snwd=snwd*2.54/100)


## ----- subset snotel data for simulation date
snoteltoday=snoteldata %>%
	dplyr::select(-snwd,-swe) %>%
	filter(date==simdate) %>%
	mutate(
		yr=strftime(date,'%Y'),
		doy=strftime(date,'%j'),
		yrdoy=strftime(date,'%Y%j'),
		dy=strftime(date,'%d'))

## save current snotel as spatial vector file
snoteltoday.sp=data.frame(snoteltoday)
coordinates(snoteltoday.sp)=~Longitude+Latitude
proj4string(snoteltoday.sp)='+proj=longlat +datum=WGS84'
if(!file.exists(paste0('output/real-time/snotel-',strftime(simdate,'%d%b%Y'),'.gpkg'))){
  writeOGR(snoteltoday.sp,dsn=paste0('output/real-time/snotel-',strftime(simdate,'%d%b%Y'),'.gpkg'),layer='snotel_swe',driver='GPKG',overwrite_layer=TRUE)
}
## ------ select recon version
ryrs=seq(2000,2012) #years should match whats availble for recon.version
snotelrecon=get_snotelrecondata(recon.version,ryrs)

## ---- merge snotelrecon data with snotel swe data and phv data
phvstack=stack('data/ucophv_variables_stack')
phvsnotel=raster::extract(phvstack,snotellocs,df=T)
phvsnotel=phvsnotel %>%
  tbl_df %>%
	mutate(
		Site_ID=snotellocs$Site_ID,
		Station_ID=snotellocs$Station_ID) %>%
	dplyr::select(-ID)

swedata=full_join(dplyr::select(snotelrecon,Station_ID,Site_ID,date,recon),phvsnotel,by=c('Station_ID','Site_ID'))
swedata=arrange(swedata,date,Station_ID) %>%
	mutate(
		yr=strftime(date,'%Y'),
		yrdoy=strftime(date,'%Y%j'),
		dy=strftime(date,'%d'),
		doy=strftime(date,'%j'),
		mth=strftime(date,'%m'))
# head(swedata)

### ------ get modelling data
mdldata=swedata
mdldata$recondate=mdldata$date

# select recon dates to evaluate model with
#recondata is used to iterate through recondates.
#these dates can be different than olddata/mdldata if wanted but must include at least 1 date before each mdl date if used in real-time mode
recondata=subset(mdldata, (dy==1 | dy==15) )

## ------ scale all data using domain avg and sd
ucophv=readRDS('data/ucophv.rds')
ucophvsc=scale(ucophv)
avg=attr(ucophvsc,'scaled:center')
std=attr(ucophvsc,'scaled:scale')
varind=which(names(phvsnotel) %in% names(ucophv))
for(i in varind){
  ucoind=which(names(avg) %in% names(phvsnotel)[i])
  phvsnotel[,i]=(phvsnotel[,i]-avg[ucoind])/std[ucoind]
}
varind=which(names(mdldata) %in% names(ucophv))
for(i in varind){
  ucoind=which(names(avg) %in% names(mdldata)[i])
  mdldata[,i]=(mdldata[,i]-avg[ucoind])/std[ucoind]
}

## ***** TODO -- FIGUREOUT WHERE MDLDATA IS USED. DOES IT NEED TO BE?
## ---- merge today's snotel data and phv data and fit model
doidata=merge(snoteltoday,phvsnotel,by=c('Station_ID','Site_ID'))
doylist=doDOYfit(doidata,cost,style,optflag,scalesnotel,fscaMatch,fordensource,dateflag)

## --- write fitting results to disk
which_recon_date=doylist[[1]]
xvalpreds=doylist[[2]]
# optweights=ldply(doylist,'[[',3)
# #
write.table(which_recon_date,paste0(configpath,style,'_recondate_selection_',fscaMatch,'_',cost,'-',as.character(simdate),'.txt'),sep='\t',row.names=F,quote=F)
write.table(xvalpreds,paste0(configpath,'fullpreds/xval/',style,'_snotel_xval_bestpreds_',fscaMatch,'_',cost,'-',as.character(simdate),'.txt'),sep='\t',row.names=F,quote=F)
# write.table(optweights,paste0('diagnostics/rswe_',recon.version,'/covrange',covrange,'/snotel',scalesnotel,'/',style,'_optmodel_',cost,'.txt'),sep='\t',row.names=F,quote=F)
# #

## ---- setup prediction
# which_recon_date=read.table(paste0('diagnostics/rswe_',recon.version,'/covrange',covrange,'/snotel',scalesnotel,'/',dateflag,'/',style,'_recondate_selection_',fscaMatch,'_',cost,'-',as.character(simdate),'.txt'),sep='\t',header=T,stringsAsFactors=F)
# which_recon_date$date=as.POSIXct(strptime(which_recon_date$date,'%Y-%m-%d',tz='MST'))
# which_recon_date$phvrcn_recondate=as.POSIXct(strptime(which_recon_date$phvrcn_recondate,'%Y-%m-%d',tz='MST'))
# which_recon_date$recon_costdate=as.POSIXct(strptime(which_recon_date$recon_costdate,'%Y-%m-%d',tz='MST'))
# which_recon_date$mth=as.numeric(strftime(which_recon_date$date,'%m'))
# which_recon_date$dy=as.numeric(strftime(which_recon_date$date,'%d'))
# which_recon_date=which_recon_date[!is.na(which_recon_date$phvrcn_recondate),]
#####

newdata=as.data.frame(ucophvsc)#
newdatalocs=SpatialPoints(ucophv[,c('Long','Lat')])
proj4string(newdatalocs)='+proj=longlat +datum=WGS84'
coordnames(newdatalocs)=c('x','y')
newdatalocs.usgs=spTransform(newdatalocs,CRS('+init=epsg:5070'))

# predict_wrapper(which_recon_date,style,newdata,newdatalocs.usgs,newdatalocs.agg.usgs,spatialblend,scalesnotel,fscaMatch,cost,fordensource,dateflag)
# system("gdal_translate -of GTiff -a_srs "+proj=longlat +datum=WGS84"  preds-phvrcn_2016_blend-fsca.nc preds_phvrcn_2016_blend-fsca.tif")
