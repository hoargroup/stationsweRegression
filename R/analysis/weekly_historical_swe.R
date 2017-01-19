library(ggplot2)
library(raster)
library(rasterVis)
library(tidyr)
library(plyr)
library(dplyr)
library(readr)
library(raster)
library(MASS)
library(ipred)
library(doMC)
library(gstat)
library(viridis)
library(dplyr)
library(cowplot)
library(ncdf4)
library(rgdal)
library(purrr)

#load functions
lapply(list.files('src/lib',pattern='.R',full.names=T),source)
lapply(list.files('src/runtime',pattern='.R',full.names=T),source)


# simulation date
simdate=as.POSIXct('2015-04-29',tz='MST')
dte=simdate
yr=strftime(dte,'%Y')
dy=strftime(dte,'%d')
month=strftime(dte,'%B')
mnth=strftime(dte,'%b')
mth=strftime(dte,'%m')
weeknum=strftime(dte,'%W')


# # set up runtime flags
# recon.version='v3.1'
# covrange='idp1'
# scalesnotel='scale'
# snotelscale='scale'
# product='phvrcn'
# cost='r2'
# optflag='noopt'
# output='surface'#points' #'surface' #just predict at snotel pixels #for 'points' spatialblend must also be 'blend'
# fscaMatch='wofsca'
# style='real-time'
# residblending='unblended'
# spatialblend='noblend'#flag to do geostatistical blending or not (prediction stage only; always does in the CV stage).
# fordensource='umd_forden'#'nlcd_forden'
# dateflag='C'
# analysisunits='metric'
# config=''

configpath=paste0('diagnostics/rswe_',recon.version,'/covrange',covrange,'/snotel',scalesnotel,'/',dateflag,'/')
inpath=file.path('fullpreds',cost,'geotif',style)

#

fn_phvrcn=dir('/Volumes/Dominik/UCO_snotelregression_sweproduct/',pattern=glob2rx('^preds_phvrcn_X*.tif$'),full.names=TRUE)
dte_str=sapply(strsplit(x=basename(fn_phvrcn),split='[_//.]'),'[',3)
dtes=as.Date(strptime(dte_str,'X%Y%m%d'))
woy=strftime(dtes,'%W')

phvrcn_stack <- stack(fn_phvrcn)
outpath='data/historical_median_phvrcn'
if(!dir.exists(outpath)) dir.create(outpath,recursive=TRUE)

## week of year medians by pixel ----
# statfun=function(x,na.rm=TRUE){ median(x[x<200],na.rm=na.rm)}
# beginCluster(3)
# woy_med <- clusterR(phvrcn_stack,stackApply,args=list(indices=as.integer(woy),fun=statfun))#stackApply returns index layers in order of the first appearance in the index value.
# endCluster()
# names(woy_med) <- paste('woy',unique(as.integer(woy)),sep='')
# writeRaster(woy_med,file.path(outpath,'phvrcn_weekmedian.tif'),NAflag=-99,bylayer=TRUE,suffix='names')
#
# ## date specific median
# mthdy='0501'
# mnthdy='May01'
# mthdyind=which(strftime(dtes,'%m%d')==mthdy)
# mthdy_stack=phvrcn_stack[[mthdyind]]
# beginCluster(3)
# mthdy_med <- clusterR(mthdy_stack,calc,args=list(fun=median, na.rm=TRUE))
# endCluster()
# writeRaster(mthdy_med,file.path(outpath,paste0('phvrcn_',mnthdy,'_median.tif')),NAflag=-99)


#
## create raster layer that has unique values for every unique combo of huc4raster and elevband ----
huc4raster=raster('data/gis/UpperCRB_rasterized.tif')
huc4=readOGR('data/gis','UpperCRB',verbose=F)
huc4df=as.data.frame(huc4[,c('HUC_4','HU_4_Name')]) %>%
  rename(huc4=HUC_4,huc4name=HU_4_Name) %>%
  mutate(huc4=as.numeric(as.character(huc4)),
         huc4name=as.character(huc4name))

dem.m=raster('data/gis/gmted_uco_recongrid.tif')
ucomask=raster('data/gis/UpperCRB_mask.tif')
demm.uco=mask(dem.m,ucomask)
demm.uco[demm.uco<1000]=1000
demm.uco[demm.uco>1000 & demm.uco <=1500]=1500
demm.uco[demm.uco>1500 & demm.uco <=2000]=2000
demm.uco[demm.uco>2000 & demm.uco <=2500]=2500
demm.uco[demm.uco>2500 & demm.uco <=3000]=3000
demm.uco[demm.uco>3000 & demm.uco <=3500]=3500
demm.uco[demm.uco>3500 & demm.uco <=4000]=4000
demm.uco[demm.uco>4000 & demm.uco <=4500]=4500

s=stack(huc4raster,demm.uco)
sdf=data.frame(unique(s))
names(sdf)=c('huc4','elevband')
u <- arrange(sdf,elevband) %>% filter(!is.na(elevband))
u$newVal <- seq(nrow(u))
zoneLayer <- subs(s, u, by = 1:2, which = 3)

## mean swe by huc4 and elev ----
aggfun <- function(r,fxn){
  as.data.frame(zonal(r, zoneLayer, fxn) ) %>%
    rename_(sweval=fxn) %>%
    full_join(u,.,by=c('newVal'='zone')) %>%
    dplyr::select(-newVal) %>%
    full_join(huc4df)
}


dte=sapply(strsplit(names(phvrcn_stack),'_'),'[',3)

swe_avgs <-
  data_frame(dte,ii=seq_along(dte)) %>%
  group_by(dte) %>%
  do(
    aggfun(phvrcn_stack[[.$ii]],'mean')
  ) %>%
  ungroup %>%
  mutate(dte=as.Date(strptime(dte,'X%Y%m%d')))
## don't do this. needs too much memory
# swe_avgs <-
#   aggfun(phvrcn_stack) %>%
#   gather(dte,swe,-huc4,-elevband,-huc4name)
saveRDS(swe_avgs,file.path(outpath,'phvrcn_alldates_elev_huc4_avgswe.rds'))

## weekly medians
woy_med <-
  swe_avgs %>%
  ungroup %>%
  mutate(woy=strftime(dte,'%W')) %>%
  group_by(woy,huc4,huc4name,elevband) %>%
  summarise(
    swe_med=median(sweval,na.rm=T)
  )
saveRDS(woy_med,file.path(outpath,'phvrcn_weekmedian_elev_huc4_avgswe.rds'))


## apr1 and may1 medians
mthdy_med <-
  swe_avgs %>%
  ungroup %>%
  mutate(dte=as.Date(strptime(dte,'X%Y%m%d')),
         mthdy=strftime(dte,'0401')) %>%
  filter(mthdy=='0401' | mthdy=='0501') %>%
  group_by(huc4,huc4name,elevband,mthdy) %>%
  summarise(
    swe_med=median(sweval,na.rm=TRUE)
  )
saveRDS(mthdy_med,file.path(outpath,'phvrcn_Apr01_May01_median_elev_huc4_avgswe.rds'))

## swe volume medians ----
swe_vols <-
  data_frame(dte,ii=seq_along(dte)) %>%
  group_by(dte) %>%
  do(
    aggfun(phvrcn_stack[[.$ii]],'sum')
  ) %>%
  ungroup %>%
  mutate(dte=as.Date(strptime(dte,'X%Y%m%d')))
## don't do this. needs too much memory
# swe_avgs <-
#   aggfun(phvrcn_stack) %>%
#   gather(dte,swe,-huc4,-elevband,-huc4name)
saveRDS(swe_vols,file.path(outpath,'phvrcn_alldates_elev_huc4_avgvol.rds'))

## weekly medians
woy_med <-
  swe_vols %>%
  ungroup %>%
  mutate(woy=strftime(dte,'%W')) %>%
  group_by(woy,huc4,huc4name,elevband) %>%
  summarise(
    swe_med=median(sweval,na.rm=TRUE)
  )
saveRDS(woy_med,file.path(outpath,'phvrcn_weekmedian_elev_huc4_avgvol.rds'))

## apr1 and may1 medians
mthdy_med <-
  swe_vols %>%
  ungroup %>%
  mutate(dte=as.Date(strptime(dte,'X%Y%m%d')),
         mthdy=strftime(dte,'0401')) %>%
  filter(mthdy=='0401' | mthdy=='0501') %>%
  group_by(huc4,huc4name,elevband,mthdy) %>%
  summarise(
    swe_med=median(sweval,na.rm=TRUE)
  )
saveRDS(mthdy_med,file.path(outpath,'phvrcn_Apr01_May01_median_elev_huc4_avgvol.rds'))




# snotel historic ----
invall=read.csv('data/historical_median_snotel/nwcc_inventory.csv',stringsAsFactors=F,header=T) %>% tbl_df
# View(invall)
# nrow(invall)
inv=read.csv('data/historical_median_snotel/uco_snotel_inventory.csv',stringsAsFactors = F, header=T) %>% tbl_df
# nrow(inv)
fns=dir('/Volumes/hydroData/WestUS_Data/snotel/nwcc/',pattern='allyears.csv',full.names=T)
fn_sta.id=substr(sapply(strsplit(basename(fns),'_'),'[',2),5,8)

# setdiff(inv$station.id,fn_sta.id[fn_sta.id %in% inv$station.id])

snotellocs=read.csv('data/SNOTEL_MASTER.csv')

snotelrec <- map_df(.x=fns[fn_sta.id %in% inv$station.id],.f=read.csv,header=T) %>%
  mutate(dte=as.Date(date),
         swe=swe_mm/1000) %>%
  dplyr::select(siteid,dte,swe) %>%
  filter(dte >= as.Date('1979-10-01'), dte < as.Date('2012-10-01'))

snotelrec <-
  snotelrec %>%
  group_by(siteid) %>%
  do({
    # print(.$siteid[1])
    # print(.$dte[1])
    # print(.$dte[nrow(.)])
    if(.$dte[1] > as.Date('1979-10-01')){
      return(data_frame())
    } else if(.$dte[nrow(.)] < as.Date('2012-09-30')){
      return(data_frame())
    } else {
      startind=which(.$dte==as.Date('1979-10-01'))
      endind=which(.$dte==as.Date('2012-09-30'))
      return(.[startind:endind,])
    }
  })


## Apr1 and May 1 median ----
mthdy_med <-
  snotelrec %>%
  mutate(yr=strftime(dte,'%Y'),
         mthdy=strftime(dte,'%m%d')) %>%
  filter(yr<2011,mthdy=='0401' | mthdy=='0501') %>% #limit 1980-2010 per official median
  group_by(siteid,mthdy) %>%
  summarise(
    mthdy_med=median(swe,na.rm=TRUE)
  )
saveRDS(mthdy_med,'data/historical_median_snotel/snotel_Apr01_May01_median_1980-2010.rds')


mthdy_med2 <-
  snotelrec %>%
  mutate(yr=strftime(dte,'%Y'),
         mthdy=strftime(dte,'%m%d')) %>%
  filter(yr>1999,yr<2013,mthdy=='0401' | mthdy=='0501') %>% #limit 2000-2012 to match phvrcn record
  group_by(siteid,mthdy) %>%
  summarise(
    mthdy_med=median(swe,na.rm=TRUE)
  )
saveRDS(mthdy_med2,'data/historical_median_snotel/snotel_Apr01_May01_median_2000-2012.rds')

## week median ----
woy_med <-
  snotelrec %>%
  mutate(yr=strftime(dte,'%Y')) %>%
  filter(yr>=2000,yr<2013) %>%
  mutate(woy=strftime(dte,'%W')) %>%
  group_by(siteid,woy) %>%
  summarise(
    woy_med=median(swe,na.rm=TRUE)
  )
saveRDS(woy_med,'data/historical_median_snotel/snotel_weekmedian_2000-2012.rds')

## join huc4, elevband and snotel data
snotelrec_locs <-
  inner_join(snotelrec,snotellocs %>% dplyr::select(siteid=Site_ID,lat=Latitude,lon=Longitude,Elevation_m,sitename=Site_Name))
snotelrec.sp <-
  snotelrec_locs %>%
  as.data.frame

coordinates(snotelrec.sp) <- ~lon+lat
proj4string(snotelrec.sp) <- '+proj=longlat +datum=WGS84'

snotelrec_locs <-
  snotelrec_locs %>%
  ungroup %>%
  mutate(zonelayer = raster::extract(zoneLayer,snotelrec.sp)) %>%
  filter(!is.na(zonelayer)) %>%
  full_join(u,by=c('zonelayer'='newVal')) %>%
  full_join(huc4df)
saveRDS(snotelrec_locs,'data/historical_median_snotel/snotel_alldates_with-locinfo.rds')

## get daily mean for each huc4 elevation band
snotelrec_zonelayerstats <-
  snotelrec_locs %>%
  mutate(woy=strftime(dte,'%W'),
         yr=strftime(dte,'%Y')) %>%
  group_by(huc4,huc4name,elevband,dte) %>%
  summarise(avgswe=mean(swe,na.rm=T),
            nstations=n())
saveRDS(snotelrec_zonelayerstats,'data/historical_median_snotel/snotel_alldates_elev_huc4_avgswe.rds')

## calc Apr1 and May 1 historic median
# 1980-2010
snotel_mthdy_med <-
  snotelrec_zonelayerstats %>%
  mutate(mthdy=strftime(dte,'%m%d'),
         yr=strftime(dte,'%Y')) %>%
  filter(yr<2011, mthdy=='0401' | mthdy=='0501') %>%
  group_by(huc4,huc4name,elevband,mthdy) %>%
  summarise(swe_med=median(avgswe,na.rm=T))
saveRDS(snotel_mthdy_med,'data/historical_median_snotel/snotel_Apr01_May01_median_elev_huc4_1980-2010.rds')

# 2000-2012
snotel_mthdy_med <-
  snotelrec_zonelayerstats %>%
  mutate(mthdy=strftime(dte,'%m%d'),
         yr=strftime(dte,'%Y')) %>%
  filter(yr>1999,yr<2013, mthdy=='0401' | mthdy=='0501') %>%
  group_by(huc4,huc4name,elevband,mthdy) %>%
  summarise(swe_med=median(avgswe,na.rm=T))
saveRDS(snotel_mthdy_med,'data/historical_median_snotel/snotel_Apr01_May01_median_elev_huc4_2000-2012.rds')



## calc weekly historic median
# 1980-2010
snotel_med <-
  snotelrec_zonelayerstats %>%
  mutate(woy=strftime(dte,'%W'),
         yr=strftime(dte,'%Y')) %>%
  filter(yr<2011) %>%
  group_by(huc4,huc4name,elevband,woy) %>%
  summarise(swe_med=median(avgswe,na.rm=T))
#dont have phvrcn to compare to....#saveRDS(snotel_med,'data/historical_median_snotel/snotel_weekmedian_1980-2010')

# 2000-2012
snotel_med <-
  snotelrec_zonelayerstats %>%
  mutate(woy=strftime(dte,'%W'),
         yr=strftime(dte,'%Y')) %>%
  filter(yr>1999,yr<2013) %>%
  group_by(huc4,huc4name,elevband,woy) %>%
  summarise(swe_med=median(avgswe,na.rm=T))
saveRDS(snotel_med,'data/historical_median_snotel/snotel_weekmedian_elev_huc4_2000-2012.rds')

