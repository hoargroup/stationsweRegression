#' setup and clean station inventory
#'
#' @param network which station network are you using. default 'snotel'. currently only option
#' @return tibble with (at least) column names Site_ID, site_name, Latitude, Longitude.
#' @export
#' @details There is a snotel inventory included with the package from Jan 2016. It can be updated from https://wcc.sc.egov.usda.gov/nwcc/inventory or theoretically replaced with that of a different network

get_inv <- function(network='snotel'){

	if(network=='snotel'){
		nwccinv=read_csv(system.file("extdata","nwcc_snotel_inventory.csv",package='stationsweRegression'))
		nwccinv <- nwccinv %>%
			rename(stationid=`station id`) %>%
			mutate(stationid=as.character(stationid))

		## ---- read more accurate locations, fix station names and create spatial points dF
		snotellocs=read_csv(system.file("extdata","SNOTEL_MASTER.csv",package='stationsweRegression')) %>%
			mutate(Site_ID=as.character(Site_ID))

		snotellocs=inner_join(snotellocs,nwccinv,by=c("Site_ID" = "stationid"))

		snotellocs.df=snotellocs %>%
			dplyr::select(Station_ID,Site_ID,site_name,State,Latitude,Longitude,Elevation_m,start_date,end_date) %>%
			mutate(
				site_name=as.character(site_name),
				site_name=gsub('#','no.',site_name,fixed=T),#ifelse(grepl('#',site_name),gsub('#','no.',site_name,fixed=T),site_name),
				site_name=gsub('-',' ',site_name,fixed=T), #site_name=ifelse(grepl('-',site_name),gsub('-',' ',site_name,fixed=T),site_name),
				site_name=gsub('\'','',site_name,fixed=T),# site_name=ifelse(grepl('\'',site_name),gsub('\'','',site_name,fixed=T),site_name),
				Station_ID=gsub(' ','',Station_ID))
	}

	return(snotellocs.df)
}

