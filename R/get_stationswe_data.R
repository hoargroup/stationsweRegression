#' download and combine station data
#'
#' @param station_locs tbl_df of site metadata
#' @param network default snotel
#' @export
#' @return tibble with station data from nrcs

get_stationswe_data <- function(station_locs,network){

	#define import functions for different networks
	if(network=='snotel'){
		station_locs <- station_locs %>%#some additional filtering possible with snotel network
			filter(start_date<=simdate) %>%
			filter(end_date>=simdate)

		import_data <- function(fn){
			tryCatch({
				# print(fn)
				read_csv(fn,
								 comment='#',
								 col_types = list(
								 	col_date('%Y-%m-%d'),
								 	col_number()
								 ),
								 skip=1,
								 col_names = c('dte','pillowswe')
					) %>%
					mutate(fn=basename(fn)) %>%
					separate(fn,into=c('network','Site_ID','dtestr'),sep='_',remove=TRUE)

				}, warning = function(w){
					print(paste0('warning reading ',fn))
				},
					error = function(e){
					print(paste0('error reading ',fn))
					data_frame()
				})
		}

	} else if(network=='cdec'){

		import_data <- function(fn){
			tryCatch({
				read_csv(fn,skip=2, quote="\'",
								 na='m',
								 col_types = list(
								 	col_date(format='%Y%m%d'),
								 	col_time(format='%H%M'),
								 	col_number()
								 ),
								 col_names = c('dte','time','pillowswe')
				) %>%
					mutate(fn=basename(fn)) %>%
					separate(fn,into=c('network','Site_ID','dtestr'),sep='_',remove=TRUE)

				}, error = function(e){
					data_frame()
				})
		}

	} else {

		stop('the network you specified for downloading pillow data is not supported.')

	}


	# cycle through stations to download if necessary. will not download the data again if a file already exists with an equal or later date than the simulation date.
	iloc=1
	for(iloc in 1:nrow(station_locs)){# use loop with index isntead of value so you can access State for the same row later
		site_id <- station_locs$Site_ID[iloc]

		# site_yr <- paste0(site_id,'_',yr,'CY')
		new_file=file.path(PATH_SNOTEL,paste0(network,'_',site_id,'_',datestr,'.csv'))
		old_files=dir(path=file.path(getwd(),PATH_SNOTEL),glob2rx(paste0(network,'_',site_id,'*.csv')),full.names=T)

		if(length(old_files)>0){
			existingDates <- as.Date(sapply(strsplit(x=basename(old_files),split='[._]',fixed=F),'[',3),'%Y%m%d')
			oldestDateind <- which.max(existingDates)
			oldestDate <- existingDates[oldestDateind]

		} else {
			oldestDate <- NA
		}
		# newDate <- as.Date(datestr,format='%Y%m%d')

		if(!isTRUE(oldestDate >= simdate )){

			if(network=='snotel'){
				state_id <- station_locs$State[iloc]
				downloadURL <- paste0('https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport/daily/start_of_period/',site_id,':',state_id,':SNTL|id=%22%22|name/POR_BEGIN,POR_END/WTEQ::value')

			} else if(network=='cdec'){
				downloadURL=paste0('http://cdec.water.ca.gov/cgi-progs/queryCSV?station_id=',site_id,'&dur_code=D&sensor_num=82&start_date=1/1/1900&end_date=',strftime(simdate,'%m/%d/%Y'))

			}

			file.remove(old_files)
			dstatus <- download.file(downloadURL,new_file,method="auto")

		}

	}

	fns = list.files(path = PATH_SNOTEL, pattern = glob2rx(paste0(network,'*.csv')),full.names=T)
	siteIDs <- sapply(strsplit(basename(fns),'[._]',fixed=F),'[',2)
	dat <- map_df(fns,import_data,.id=NULL)

	if(length(unique(dat$Site_ID))!=length(siteIDs)){
		print(paste0(' - Site data did not download correctly: ',siteIDs[!(siteIDs %in% dat$Site_ID)],collapse=', '))
	}

	print(' - done reading files')

	dat2=right_join(station_locs,
								 dat,by=c('Site_ID'))

	pillowdata=dat2 %>%
		dplyr::select(Site_ID, Longitude, Latitude, dte, pillowswe) %>%
		mutate(
			pillowswe=replace(pillowswe,pillowswe<0,  NA),
			pillowswe=pillowswe*2.54/100)#convert inches to meters

	return(pillowdata)

}

