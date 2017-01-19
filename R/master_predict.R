library('ProjectTemplate')
#setwd('~/GoogleDrive/snotel-regression_project')
#setwd('~/Documents/snotel-regression_project')
#library(doMC)
#library(doMPI)
load.project()

## model options
#cost='r2'#cor, r2, mae, rmse
style='reanalysis'#real-time'#'real-time','reanalysis'
spatialblend='blend'#flag to do geostatistical blending or not (prediction stage only; always does in the CV stage). blending takes long time for the entire domain..
output='surface'#points' #'surface' #just predict at snotel pixels #for 'points' spatialblend must also be 'blend'
covrange='idp1'
fordensource='umd_forden'#'nlcd_forden'
dateflag='surveyvalidation'##
## 'surveyvalidation' simulate survey dates and drops the station the survey was around
## 'B2' will simulate survey dates without dropping sites. #use this if doing survey validation simulations. will drop station of survey
## 'B' will simulate all selected dates from modscag plus weekly mar 1 to jun 22
## 'A' will simulate only selcted dates from modscag
## 'F' simulate weekly in 1995, 1998, 1999 to emulate fassnacht
# scalesnotel='noscale'#'scale' or 'noscale'
# fscaMatch='fsca'#fsca or wofsca


args=commandArgs(trailing=T)
if(length(args)<4 & length(args)!=0) {
print('not enough arguments')
stop()
}
if(length(args)==0) args=c('2001','scale','wofsca','r2')
mdlyr=args[1]
scalesnotel=args[2]
fscaMatch=args[3]
cost=args[4]
# cost=args[2]
# scalesnotel=args[2]
# covrange=args[3]
# fscaMatch=args[4]
print(paste('mdlyr:', mdlyr))

# select recon version ----------------------------------------------------
recon.version='v3.1'
ryrs=seq(2000,2012)
snotelrecon=get_snotelrecondata(recon.version,ryrs)

# generate swedata dataframe
swedata=generate_swedata(snotelrecon)
swedata=arrange(swedata,date,Station_ID)
#subset snotel locations to those we have. snotelrec is loaded from cached files and should have only the stations we are using
swedata=swedata[swedata$Station_ID %in% unique(snotelrec$Station_ID),]
snotellocs=snotellocs[snotellocs$Station_ID %in% unique(swedata$Station_ID),]
snotelrecon=snotelrecon[snotelrecon$Station_ID %in% unique(swedata$Station_ID),]

# if(nzchar(Sys.getenv('PBS_NODEFILE'))) {
# # setup parallel processing -----------
#  getnodes <- function() {
#         f <- Sys.getenv('PBS_NODEFILE')
#         x <- if (nzchar(f)) readLines(f) else rep('localhost', 2)
#         as.data.frame(table(x), stringsAsFactors=FALSE)
#        }
#    nodes <- getnodes()
# #cl <- startMPIcluster(verbose=T)
# #    cl <- makeSOCKcluster(nodes$x,outfile='')
#
# print(cl)
# print(nodes)
# #registerDoSNOW(cl)
# #registerDoMPI(cl)
#
# # setcores <- function(cl, nodes) {
# #     cores=6#nodes$Freq
# #     f <- function(cores) assign('allocated.cores', cores, envir=.GlobalEnv)
# #     clusterApply(cl, nodes$Freq, f)
# #   }
# # setcores(cl, nodes)
# }

# get modelling data --------------------------------------------------------
mdldata=swedata[swedata$mth<7,]#if changing mth<7, need to change dates2model too
mdldata$recondate=mdldata$date
mdldata=arrange(mdldata,date,Station_ID)#!!!important

# scale all data using domain avg and sd
ucophvsc=scale(ucophv)
varind=which(names(mdldata) %in% names(ucophv))
avg=attr(ucophvsc,'scaled:center')
std=attr(ucophvsc,'scaled:scale')
for(i in varind){
	ucoind=which(names(avg) %in% names(mdldata)[i])
	mdldata[,i]=(mdldata[,i]-avg[ucoind])/std[ucoind]
}

#define domain for prediction -------
snotellocs=snotellocs[order(snotellocs$Station_ID),]#make sure its in order
#
ucophv.stack=stack('data/ucophv_variables_stack.grd')
cds=ucophv.stack[[2:1]]
raster_snotellocs=as.data.frame(extract(cds,snotellocs))
snotellocs.df=as.data.frame(snotellocs)
snotellocs.df$x=raster_snotellocs$Long
snotellocs.df$y=raster_snotellocs$Lat
snotellocs=snotellocs.df
coordinates(snotellocs)=~x+y
proj4string(snotellocs)=CRS('+init=epsg:4326')
#
snotellocs.usgs=spTransform(snotellocs,CRS('+init=epsg:5070'))
newdata=as.data.frame(ucophvsc)#ucophv is automatically loaded with project and  contains newdata for prediction to domain
newdatalocs=SpatialPoints(ucophv[,c('Long','Lat')])
proj4string(newdatalocs)='+proj=longlat +datum=WGS84'
coordnames(newdatalocs)=c('x','y')
newdatalocs.usgs=spTransform(newdatalocs,CRS('+init=epsg:5070'))
# newdatalocs.agg=SpatialPoints(raster('data/gmted_combined_uco6km.tif'))
# proj4string(newdatalocs.agg)='+proj=longlat +datum=WGS84'
# newdatalocs.agg.usgs=spTransform(newdatalocs.agg,CRS('+init=epsg:5070'))
#
#geope=projectExtent(ucophv.stack[[1]],crs=projection(ucophv.stack))
#
#rskel=ucophv.stack[[1]]
#values(rskel)=F
#names(rskel)=NULL
#rskel=projectRaster(rskel,crs=CRS('+init=epsg:5070'))

# select recon dates to evaluate model with
#recondata is used to iterate through recondates.
#these dates can be different than olddata/mdldata if wanted but must include at least 1 date before each mdl date if used in real-time mode
recondata=subset(mdldata, (dy==1 | dy==15) )

print(cost)
print(style)
print(covrange)
print(scalesnotel)
print(fscaMatch)
print(fordensource)

which_recon_date=read.table(paste0('diagnostics/rswe_',recon.version,'/covrange',covrange,'/snotel',scalesnotel,'/',dateflag,'/',style,'_recondate_selection_',fscaMatch,'_',cost,'-',mdlyr,'.txt'),sep='\t',header=T,stringsAsFactors=F)
which_recon_date$date=as.POSIXct(strptime(which_recon_date$date,'%Y-%m-%d',tz='MST'))
which_recon_date$phvrcn_recondate=as.POSIXct(strptime(which_recon_date$phvrcn_recondate,'%Y-%m-%d',tz='MST'))
which_recon_date$recon_costdate=as.POSIXct(strptime(which_recon_date$recon_costdate,'%Y-%m-%d',tz='MST'))
which_recon_date$mth=as.numeric(strftime(which_recon_date$date,'%m'))
which_recon_date$dy=as.numeric(strftime(which_recon_date$date,'%d'))
which_recon_date=which_recon_date[!is.na(which_recon_date$phvrcn_recondate),]
#####



# which_recon_date=which_recon_date[c(1,2,32,33),]
#registerDoMC(allocated.cores)

# save predicted surfaces to netcdf by year.
which_recon_date=subset(which_recon_date,yr==mdlyr)
#which_recon_date=subset(which_recon_date, (dy==1 & mth==4))
#str(which_recon_date)
predict_wrapper(which_recon_date,style,newdata,newdatalocs.usgs,newdatalocs.agg.usgs,spatialblend,scalesnotel,fscaMatch,cost,fordensource,dateflag)

# wrd=subset(which_recon_date,mth=3)
# predict_wrapper(wrd,style,newdata,newdatalocs.usgs,newdatalocs.agg.usgs,spatialblend)
# d_ply(which_recon_date,.(yr),predict_wrapper,style,newdata,newdatalocs.usgs,newdatalocs.agg.usgs,spatialblend,.inform=F,.parallel=F)#,.paropts=list(.export=ls(), .packages=.packages(all=T)))
# #d_ply(wrd,.(yr),predict_wrapper,style,newdata,newdatalocs.usgs,newdatalocs.agg.usgs,spatialblend,.inform=T,.parallel=F,.paropts=list(.export=ls(), .packages=.packages(all=T)))


# if(exists('cl')) stopCluster(cl)
# quit(save='no')
if(exists('cl')) closeCluster(cl)
#mpi.quit()
library(pryr)
print(mem_used())
quit('no')
