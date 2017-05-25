#' download and combine station data
#'
#' @param yr yr of the simulation date
#' @param station_locs tbl_df of site metadata
#' @param network default snotel
#' @export
#' @return tibble with station data from nrcs

get_stationswe_data <- function(yr=2017,station_locs,network='snotel'){
	if(network=='snotel'){
		nwcc_download_script=system.file('sh','gethistoric.sh',package='stationsweRegression')
		site_id=379
		for(site_id in station_locs$Site_ID){
			station_yr_file=file.path(getwd(),paste0(PATH_SNOTEL,'/',site_id,'-',yr,'CY.csv'))
			if(!file.exists(station_yr_file)){
				system2('source',paste(nwcc_download_script, site_id, 'STAND Daily', yr, 'CY'),stdout=file.path(getwd(),paste0(PATH_SNOTEL,'/',site_id,'-',yr,'CY.csv')))
			}
		}
		fns=list.files(path=PATH_SNOTEL,pattern=paste0(yr,'CY.csv'))

		allfiles=lapply(fns,
										function(fname) {
											# print(fname)
											tryCatch({
												read.csv(paste0(PATH_SNOTEL,'/',fname),skip=2,header=T,check.names=F,stringsAsFactors=F)
												},
												error=function(e){
													data.frame()
												}
											)
										})
		dat=bind_rows(allfiles)
		dat=dat[,-ncol(dat)]
		colnames(dat)=c('Site_ID','dte','time','swe','pcp','tobs','tmax','tmin','tavg','snwd')
		dat <- tbl_df(dat)

		dat2=full_join(station_locations,
									 dat,by=c('Site_ID'))
		# str(dat2)
		snoteldata=dat2 %>%
			# filter(time=='') %>%
			dplyr::select(Station_ID,Site_ID,Longitude, Latitude,dte,swe,snwd) %>%
			mutate(
				dte=as.Date(dte),
				snotel=replace(swe,swe<0,  NA),
				snotel=snotel*2.54/100,#convert inches to meters
				snwd=replace(snwd,snwd<0,  NA),
				snwd=snwd*2.54/100)
	}

	return(snoteldata)
}
