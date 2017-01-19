#allows iteration of predict_surfaces by year to create yearly stacks and ncdf output
predict_wrapper <- function(rdtsdF,style,newdata,newdatalocs,newdatalocs.agg.usgs,spatialblend,scalesnotel,fscaMatch,cost,fordensource,dateflag){

     # registerDoMC(allocated.cores)
     print(paste('**',unique(rdtsdF$yr),'**'))
     bpth=paste0('diagnostics/rswe_',recon.version,'/covrange',covrange,'/snotel',scalesnotel,'/',dateflag,'/fullpreds')

	system(paste0('mkdir -p ',bpth,'/',cost,'/netcdf/',style))
    watermask=getValues(raster('data/cs_NHD_MOD44_water_mask.tif'))
    waterind=which(is.na(watermask))

# 	nx=0.00416666667
# 	ny=0.00416666667
# 	xdim=-112.25+nx/2+nx*(seq(1,1950)-1)
# 	ydim=43.75-ny/2-ny*(seq(1,2580)-1)
# 	tdim=as.numeric(strftime(strptime(rdtsdF$yrdoy,'%Y%j',tz='MST'),'%Y%m%d'))
#
#      #output rasters of each model type.
#      dim1=ncdim_def('Long','degree',xdim)
#      dim2=ncdim_def('Lat','degree',ydim)
#      dim3=ncdim_def('time','yrdoy',unlim=T,vals=tdim)
#      var=ncvar_def('swe','meters',dim=list(dim1,dim2,dim3),missval=-99,longname='snow water equivalent',compression=9,chunksizes=c(1950,2580,1))
#      yr=unique(rdtsdF$yr)
#
#      phv.fn=file.path(bpth,cost,'netcdf',style,paste0('preds-phv_',yr,'_',spatialblend,'-',fscaMatch,'.nc'))
#      if(file.exists(phv.fn)) file.remove(phv.fn)
#
#      phvfull.fn=file.path(bpth,cost,'netcdf',style,paste0('fullpreds-phv_',yr,'_',spatialblend,'-',fscaMatch,'.nc'))
#      if(file.exists(phvfull.fn)) file.remove(phvfull.fn)
#
#      phvrcn.fn=file.path(bpth,cost,'netcdf',style,paste0('preds-phvrcn_',yr,'_',spatialblend,'-',fscaMatch,'.nc'))
#      if(file.exists(phvrcn.fn)) file.remove(phvrcn.fn)
#
#      phvrcnfull.fn=file.path(bpth,cost,'netcdf',style,paste0('fullpreds-phvrcn_',yr,'_',spatialblend,'-',fscaMatch,'.nc'))
#      if(file.exists(phvrcnfull.fn)) file.remove(phvrcnfull.fn)
#
#      reconrt.fn=file.path(bpth,cost,'netcdf',style,paste0('preds-reconrt_',yr,'_',spatialblend,'-',fscaMatch,'.nc'))
#      if(file.exists(reconrt.fn)) file.remove(reconrt.fn)
#
#      reconrtfull.fn=file.path(bpth,cost,'netcdf',style,paste0('fullpreds-reconrt_',yr,'_',spatialblend,'-',fscaMatch,'.nc'))
#      if(file.exists(reconrtfull.fn)) file.remove(reconrtfull.fn)
#
#      phv.nc=nc_create(phv.fn,var)
#      phvfull.nc=nc_create(phvfull.fn,var)
#      phvrcn.nc=nc_create(phvrcn.fn,var)
#      phvrcnfull.nc=nc_create(phvrcnfull.fn,var)
#      reconrt.nc=nc_create(reconrt.fn,var)
#      reconrtfull.nc=nc_create(reconrtfull.fn,var)
#
#      ncatt_put(phv.nc,0,'grid_mapping_name','latitude_longitude')
#      ncatt_put(phvfull.nc,0,'grid_mapping_name','latitude_longitude')
#      ncatt_put(phvrcn.nc,0,'grid_mapping_name','latitude_longitude')
#      ncatt_put(phvrcnfull.nc,0,'grid_mapping_name','latitude_longitude')
#      ncatt_put(reconrt.nc,0,'grid_mapping_name','latitude_longitude')
#      ncatt_put(reconrtfull.nc,0,'grid_mapping_name','latitude_longitude')
#
#      ncatt_put(phv.nc,0,'proj4string','+proj=longlat +datum=WGS84')
#      ncatt_put(phvfull.nc,0,'proj4string','+proj=longlat +datum=WGS84')
#      ncatt_put(phvrcn.nc,0,'proj4string','+proj=longlat +datum=WGS84')
#      ncatt_put(phvrcnfull.nc,0,'proj4string','+proj=longlat +datum=WGS84')
#      ncatt_put(reconrt.nc,0,'proj4string','+proj=longlat +datum=WGS84')
#      ncatt_put(reconrtfull.nc,0,'proj4string','+proj=longlat +datum=WGS84')
#
#      ncatt_put(phv.nc,0,'waterbody','value of 253')
#      ncatt_put(phvfull.nc,0,'waterbody','value of 253')
#      ncatt_put(phvrcn.nc,0,'waterbody','value of 253')
#      ncatt_put(phvrcnfull.nc,0,'waterbody','value of 253')
    #      ncatt_put(reconrt.nc,0,'waterbody','value of 253')
    #      ncatt_put(reconrtfull.nc,0,'waterbody','value of 253')
    # doyinfo=rdtsdF
    d_ply(rdtsdF,.(yrdoy),function(doyinfo){
      dF=predict_surfaces(doyinfo,snotellocs,snotellocs.usgs,newdata,newdatalocs,newdatalocs.agg.usgs,spatialblend,bpth,scalesnotel,fordensource,dateflag)#,.inform=F,.parallel=F)#,.paropts=list(.export=ls(), .packages=.packages(all=T)))
      # tind=which(as.numeric(strftime(doyinfo$date,'%Y%m%d'))==tdim)
      # print(as.numeric(strftime(doyinfo$date,'%Y%m%d')))
      # print(tdim)
      # print(str(doyinfo))
      # # print(unique(doyinfo$yrdoy))
      # print(tind)
      #      d_ply(newdatapredsfull,.(yrdoy),function(dF){
      #for dim1 and dim2 min to max, data should be left to right, bottom to top
      # print(nrow(dF))
      # print(length(watermask))
      if(nrow(dF)==length(watermask)) {

        dF[waterind,'phv.predictions']=253
        writeRaster(num2ucoRaster(dF$phv.predictions),filename=file.path(bpth,cost,'geotif',style,paste0('preds-phv_',as.numeric(strftime(doyinfo$date,'%Y%m%d')),'_',spatialblend,'-',fscaMatch,'.tif')),overwrite=TRUE)
        # ncvar_put(phv.nc, var, vals=dF[,'phv.predictions'],start=c(1,1,tind),count=c(-1,-1,1))
        #
        # dF[waterind,'phv.fullpred']=253
        # ncvar_put(phvfull.nc, var, vals=dF[,'phv.fullpred'],start=c(1,1,tind),count=c(-1,-1,1))
        #      #
        dF[waterind,'phvrcn.predictions']=253
        # ncvar_put(phvrcn.nc,var,vals=dF[,'phvrcn.predictions'],start=c(1,1,tind),count=c(-1,-1,1))
        writeRaster(num2ucoRaster(dF$phvrcn.predictions),filename=file.path(bpth,cost,'geotif',style,paste0('preds-phvrcn_',as.numeric(strftime(doyinfo$date,'%Y%m%d')),'_',spatialblend,'-',fscaMatch,'.tif')),overwrite=TRUE)

        # file.path(bpth,cost,'netcdf',style,paste0('preds-phvrcn_',yr,'_',spatialblend,'-',fscaMatch,'.nc'))
        #
        # dF[waterind,'phvrcn.fullpred']=253
        # ncvar_put(phvrcnfull.nc,var,vals=dF[,'phvrcn.fullpred'],start=c(1,1,tind),count=c(-1,-1,1))
        #      #
        dF[waterind,'reconrt.predictions']=253
        writeRaster(num2ucoRaster(dF$reconrt.predictions),filename=file.path(bpth,cost,'geotif',style,paste0('preds-reconrt_',as.numeric(strftime(doyinfo$date,'%Y%m%d')),'_',spatialblend,'-',fscaMatch,'.tif')),overwrite=TRUE)
        # ncvar_put(reconrt.nc,var,vals=dF[,'reconrt.predictions'],start=c(1,1,tind),count=c(-1,-1,1))
        # #
        # dF[waterind,'reconrt.fullpred']=253
        # ncvar_put(reconrtfull.nc,var,vals=dF[,'reconrt.fullpred'],start=c(1,1,tind),count=c(-1,-1,1))

      } else {
        NULL
      }
    })
    # nc_close(phv.nc)
    # nc_close(phvrcn.nc)
# nc_close(phvfull.nc)
# nc_close(phvrcnfull.nc)
# nc_close(reconrt.nc)
# nc_close(reconrtfull.nc)

# } else {
# system(paste0('mkdir -p ',bpth,'/',cost,'/snotellocs'))
#      if(nrow(newdatapredsfull)%%237 != 0) stop()
#      # xy=grep('^x$|^y$',names(newdatapredsfull))
#      # newdatapredsfull=newdatapredsfull[,-xy]
#      fn2w=paste0(bpth,'/',cost,'/snotellocs/snotel_predictions_',unique(rdtsdF$yr),'.txt')
#      write.table(x=newdatapredsfull,file=fn2w,sep='\t',row.names=F,quote=F)
# }
}
