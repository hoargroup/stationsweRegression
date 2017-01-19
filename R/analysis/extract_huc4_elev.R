# setwd('~/GoogleDrive/real-time_interpolation')
library(tidyr)
library(plyr)
library(dplyr)
library(readr)
library(raster)
library(rgdal)
library(MASS)
library(ipred)
library(doMC)
library(ggplot2)
library(gstat)
library(ncdf4)

if(residblending=='unblended') {
    resid=''
  } else if(residblending=='blended') {
    resid='full'
  }

#simdate=as.POSIXct('2015-03-30',tz='MST')
# args=commandArgs(trailing=T)
# datein=args[1]
# print(datein)
# if(!is.na(datein)) simdate=as.POSIXct(strptime(datein,'%Y%m%d',tz='MST'))
print(simdate)

# ## ----- set up runtime flags
# recon.version='v3.1'
# cost='r2'
# style='real-time'
# optflag='noopt'
# scalesnotel='scale'
# fscaMatch='fsca'
# spatialblend='blend'#flag to do geostatistical blending or not (prediction stage only; always does in the CV stage).
# output='surface'#points' #'surface' #just predict at snotel pixels #for 'points' spatialblend must also be 'blend'
# covrange='idp1'#
# fordensource='umd_forden'#'nlcd_forden'
# dateflag='C'

## ------ set up folders
configpath=paste0('diagnostics/rswe_',recon.version,'/covrange',covrange,'/snotel',scalesnotel,'/',dateflag,'/')
inpath=file.path('fullpreds',cost,'geotif',style)

# s=stack(paste0(configpath,inpath,'/preds-phvrcn_2015_blend-fsca.nc'))
# ind=grep(strftime(simdate,'%Y%m%d'),names(s))
# r=s[[ind]]

dem.m=raster('data/gis/gmted_uco_recongrid.tif')
ucomask=raster('data/gis/UpperCRB_mask.tif')
demm.uco=mask(dem.m,ucomask)
huc4=readOGR('data/gis','UpperCRB',verbose=T)
huc4raster=raster('data/gis/UpperCRB_rasterized.tif')
fordenstack=stack('data/forestdensity/umd_forden.nc')

huc4df=as.data.frame(huc4[,c('HUC_4','HU_4_Name')]) %>%
  rename(huc4=HUC_4,huc4name=HU_4_Name) %>%
  mutate(huc4=as.numeric(as.character(huc4)),
         huc4name=as.character(huc4name))


demm.uco[demm.uco<1000]=1000
demm.uco[demm.uco>1000 & demm.uco <=1500]=1500
demm.uco[demm.uco>1500 & demm.uco <=2000]=2000
demm.uco[demm.uco>2000 & demm.uco <=2500]=2500
demm.uco[demm.uco>2500 & demm.uco <=3000]=3000
demm.uco[demm.uco>3000 & demm.uco <=3500]=3500
demm.uco[demm.uco>3500 & demm.uco <=4000]=4000
demm.uco[demm.uco>4000 & demm.uco <=4500]=4500

elev_area=freq(demm.uco) %>%
  as.data.frame %>%
  filter(!is.na(value)) %>%
  rename(elevband=value,numcells=count)
totalcells=elev_area %>%
  summarise(
    n=sum(numcells))
elev_area=elev_area %>%
  mutate(
    pctarea=numcells/totalcells$n
  )
huc4_area=freq(huc4raster) %>%
  as.data.frame %>%
  filter(value!=0) %>%
  rename(huc4=value,numcells=count)
totalcells=huc4_area %>%
  summarise(
    n=sum(numcells))
huc4_area=huc4_area %>%
  mutate(
    pctarea=numcells/totalcells$n
  ) %>%
  full_join(huc4df)



dte=simdate
yr=strftime(dte,'%Y')
dy=strftime(dte,'%d')
month=strftime(dte,'%B')
mnth=strftime(dte,'%b')
mth=strftime(dte,'%m')


## get swe raster to extract basin/elev stats ----
predfile=paste0('diagnostics/rswe_',recon.version,'/covrange',covrange,'/snotel',snotelscale,'/',dateflag,'/fullpreds/',cost,'/geotif/',style,'/',config,'/',resid,'preds-',product,'_',as.numeric(strftime(simdate,'%Y%m%d')),'_',spatialblend,'-',fscaMatch,'.tif')
r=raster(predfile)
r[r>100]=NA
# r[r==250]=NA
r=mask(r,ucomask)
# plot(r)
# projection(r)='+init=epsg:4326'

## - forest density
# forden=get_forden('umd_forden',yr)
# basinforden=as.data.frame(zonal(forden,huc4raster,'mean'))
# basinforden=merge(as.data.frame(huc4[,c('HUC_4','HU_4_Name')]),basinforden,by.x='HUC_4',by.y='zone')
# colnames(basinforden)=c('zone','basin','fordenavg')
	# basinforden$fordenavg=floor(basinforden$fordenavg*100)/100

	## create raster layer that has unique values for every unique combo of huc4raster and elevband ----
	s=stack(huc4raster,demm.uco)
	sdf=data.frame(unique(s))
	names(sdf)=c('huc4','elevband')
	u <- arrange(sdf,elevband) %>% filter(!is.na(elevband))
	u$newVal <- seq(nrow(u))
	zoneLayer <- subs(s, u, by = 1:2, which = 3)

	## mean swe by huc4 and elev ----
	swe_avg=as.data.frame(zonal(r, zoneLayer, 'mean') ) %>%
	  rename(avgswe=mean) %>%
	  full_join(u,.,by=c('newVal'='zone')) %>%
	  dplyr::select(-newVal) %>%
	  full_join(huc4_area[,c('huc4','huc4name')])

	write.table(format(swe_avg,digits=2,nsmall=0,scientific=F),paste0('output/',style,'/',snotelscale,'/upperCRB_huc4_elevbands',residblending,'sweavg_table_',analysisunits,'_',dy,mnth,yr,'-',product,'-',covrange,'-',cost,'-',fscaMatch,'.txt'),sep='\t',quote=F,row.names=F)
	saveRDS(swe_avg,paste0('output/',style,'/',snotelscale,'/upperCRB_huc4_elevbands_',residblending,'sweavg_table_',analysisunits,'_',dy,mnth,yr,'-',product,'-',covrange,'-',cost,'-',fscaMatch,'.rds'))

	## mean swe by huc 4 ----
	huc4sweavg=zonal(r,huc4raster,'mean') %>%
	  as.data.frame %>%
	  filter(zone!=0) %>%
	  rename(huc4=zone,sweavg=mean) %>%
	  full_join(huc4_area)

	write.table(format(huc4sweavg,digits=2,nsmall=0,scientific=F),paste0('output/',style,'/',snotelscale,'/upperCRB_huc4_',residblending,'sweavg_table_',analysisunits,'_',dy,mnth,yr,'-',product,'-',covrange,'-',cost,'-',fscaMatch,'.txt'),sep='\t',quote=F,row.names=F)
	saveRDS(huc4sweavg,paste0('output/',style,'/',snotelscale,'/upperCRB_huc4_',residblending,'sweavg_table_',analysisunits,'_',dy,mnth,yr,'-',product,'-',covrange,'-',cost,'-',fscaMatch,'.rds'))

	## mean swe by elev band ----
	elevbandsweavg=zonal(r,demm.uco,'mean') %>%
	  as.data.frame %>%
	  filter(zone!=0) %>%
	  rename(elevband=zone,sweavg=mean) %>%
	  full_join(elev_area)



	write.table(format(elevbandsweavg,digits=2,nsmall=0,scientific=F),paste0('output/',style,'/',snotelscale,'/upperCRB_elevband_',residblending,'sweavg_table_',analysisunits,'_',dy,mnth,yr,'-',product,'-',covrange,'-',cost,'-',fscaMatch,'.txt'),sep='\t',quote=F,row.names=F)
	saveRDS(elevbandsweavg,paste0('output/',style,'/',snotelscale,'/upperCRB_elevband_',residblending,'sweavg_table_',analysisunits,'_',dy,mnth,yr,'-',product,'-',covrange,'-',cost,'-',fscaMatch,'.rds'))


	## volume swe by huc4 and elev ----
	zonesum=as.data.frame(zonal(r,zoneLayer,'sum')) %>%
	  rename(vol=sum) %>%
	  tbl_df
	swe_vol=full_join(u,zonesum,by=c('newVal'='zone')) %>%
	  dplyr::select(-newVal)

	write.table(format(swe_vol,digits=2,nsmall=0,scientific=F),paste0('output/',style,'/',snotelscale,'/upperCRB_huc4_elevbands_',residblending,'swevol_table_',analysisunits,'_',dy,mnth,yr,'-',product,'-',covrange,'-',cost,'-',fscaMatch,'.txt'),sep='\t',quote=F,row.names=F)
	saveRDS(swe_vol,paste0('output/',style,'/',snotelscale,'/upperCRB_huc4_elevbands_',residblending,'swevol_table_',analysisunits,'_',dy,mnth,yr,'-',product,'-',covrange,'-',cost,'-',fscaMatch,'.rds'))

	huc4swevol=swe_vol %>%
	  group_by(huc4) %>%
	  summarise(
	    vol=sum(vol)
	  ) %>%
	  full_join(huc4_area)

	write.table(format(huc4swevol,digits=2,nsmall=0,scientific=F),paste0('output/',style,'/',snotelscale,'/upperCRB_huc4_',residblending,'swevol_table_',analysisunits,'_',dy,mnth,yr,'-',product,'-',covrange,'-',cost,'-',fscaMatch,'.txt'),sep='\t',quote=F,row.names=F)
	saveRDS(huc4swevol,paste0('output/',style,'/',snotelscale,'/upperCRB_elevbands_',residblending,'swevol_table_',analysisunits,'_',dy,mnth,yr,'-',product,'-',covrange,'-',cost,'-',fscaMatch,'.rds'))

elevbandswevol=swe_vol %>%
  group_by(elevband) %>%
  summarise(
    vol=sum(vol)
  )

write.table(format(elevbandswevol,digits=2,nsmall=0,scientific=F),paste0('output/',style,'/',snotelscale,'/upperCRB_elevbands_',residblending,'swevol_table_',analysisunits,'_',dy,mnth,yr,'-',product,'-',covrange,'-',cost,'-',fscaMatch,'.txt'),sep='\t',quote=F,row.names=F)
saveRDS(elevbandswevol,paste0('output/',style,'/',snotelscale,'/upperCRB_elevbands_',residblending,'swevol_table_',analysisunits,'_',dy,mnth,yr,'-',product,'-',covrange,'-',cost,'-',fscaMatch,'.rds'))



