#' download and combine station data
#'
#' @param station_locs tbl_df of site metadata
#' @param network default snotel
#' @export
#' @return tibble with station data from nrcs

get_stationswe_data <- function(station_locs,network){

	if(network=='snotel'){

		station_locs <- station_locs %>%
			filter(start_date<simdate) %>%
			filter(end_date>simdate)

		site_id=379
		for(site_id in station_locs$Site_ID){
			station_yr_file=file.path(getwd(),paste0(PATH_SNOTEL,'/',site_id,'-',yr,'CY.csv'))
			if(!file.exists(station_yr_file)){
				URL ="https://wcc.sc.egov.usda.gov/nwcc/view?intervalType=Historic+&report=STAND&timeseries=Daily&format=copy&sitenum="
				URL = paste0(URL,site_id,'&year=')
				URL = paste0(URL,yr,'&month=CY')
				download.file(URL,station_yr_file,method="auto")
			}
		}
		fns = list.files(path = PATH_SNOTEL, pattern = paste0(yr,"CY.csv"))
		fname=fns[1]
		allfiles = lapply(fns, function(fname) {
			tryCatch({
				# print(fname)
				suppressMessages({
					csvdata <- readr::read_csv(file.path(PATH_SNOTEL,fname),
																		 skip=3,
																		 col_names=T, #in case the order changes don't specify col names. replace by matching below and throw errror if col name is unexpected.
																		 # col_types=c(i,D,c,d,d,d,d,d,d,d), #don't specify this because there are different # of columns in files
																		 col_types = cols(  # use cols() to specify just some of the column types to save trouble later
																		 	"Site Id" = col_character(),
																		 	"Date" = col_date(format = '%Y-%m-%d'),
																		 	"Time" = col_character(),
																		 	"WTEQ.I-1 (in)" = col_double(),
																		 	"PREC.I-1 (in)" = col_double(),
																		 	"TOBS.I-1 (degC)" = col_double(),
																		 	"TMAX.D-1 (degC)" = col_double(),
																		 	"TMIN.D-1 (degC)" = col_double(),
																		 	"TAVG.D-1 (degC)" = col_double(),
																		 	"SNWD.I-1 (in)" = col_double()
																		 ),
																		 trim_ws=T)
					# csvdata <- read.csv(file.path(PATH_SNOTEL, fname), skip = 3, #read_csv is better because it automatically adds X10, X11, etc to unnamed columns. read.csv gives blank column names which I found hard to work with
					# header = T, check.names = F, stringsAsFactors = F)
				})
				# if(ncol(csvdata)!=11){
				# 	print(fname)
				# 	print(head(csvdata))
				# }

				return(csvdata)
			}, error = function(e) {
				data_frame()
			})
		})
		print('done reading files.')

		#the joys of automatically reading in lots of files, even from the same database! not all files came with the same num of cols, but the column names seem consistent. Replace existiing column names with easier column names. Remove extraneous columns such as "X10" etc. Check for unexpected column names.
		rawNames <- c("Site Id", "Date", "Time", "WTEQ.I-1 (in)","PREC.I-1 (in)","TOBS.I-1 (degC)","TMAX.D-1 (degC)", "TMIN.D-1 (degC)", "TAVG.D-1 (degC)","SNWD.I-1 (in)")
		newNames <- c("Site_ID", "dte", "tme", "swe", "pcp", "tobs", "tmax", "tmin", "tavg", "snwd")
		namesdF <- data_frame(rawNames,newNames)
		fixdF <- function(x) {
			extracols=grep(pattern = '^X\\d',x = names(x))
			x = x[, -extracols]
			names(x) <- namesdF$newNames[match(names(x), namesdF$rawNames)]
			if( any(is.na(names(x))) ) {
				snotelid = unique(x$Site_ID)
				fileyr = unique(strftime(x$dte,'%Y'))
				snotelfn = paste0(snotelid,'-',fileyr,'CY.csv')
				stop(paste0('You have an unexpected column name in SNOTEL file: ',snotelfn,'. You should investigate.'))
			}
			# print(head(x))
			# if(nrow(x)>0){
			# 	logi <- grepl('tme',names(x))
			# 	# print(logi)
			# 	# print(any(logi))
			# 	if(any(logi))		x <- x %>% dplyr::select(-tme) #since read_csv reads Time as a time column and we can't sepecify the column type as character, we should remove the column buecause dplyr can't convert a column of NA to proper times below.
			# }
			return(x)
		}
		allfiles <- lapply(allfiles,fixdF)
		dat = bind_rows(allfiles)

		dat2=full_join(station_locs,
									 dat,by=c('Site_ID'))
		# str(dat2)
		snoteldata=dat2 %>%
			# filter(time=='') %>%
			dplyr::select(Site_ID, Longitude, Latitude, dte, swe) %>%
			mutate(
				dte=as.Date(dte),
				snotel=replace(swe,swe<0,  NA),
				snotel=snotel*2.54/100)#convert inches to meters

		return(snoteldata)
	}


	if(network=='cdec'){
		# datestr='20180115'
		site_id='DAN'
		for(site_id in station_locs$Site_ID){
			new_file=file.path(getwd(),PATH_SNOTEL,paste0('cdec_',site_id,'_',datestr,'.csv'))
			# new_file=file.path('~/CUDrive/example_sweRegression',paste0(site_id,'_',datestr,'.csv'))
			old_files=dir(path=file.path(getwd(),PATH_SNOTEL),glob2rx(paste0('cdec_',site_id,'*.csv')),full.names=T)
			# print(basename(old_files))
			# old_files=dir(path='~/CUDrive/example_sweRegression',pattern=glob2rx(paste0('^',site_id,'*.csv$')),full.names=T)
			if(length(old_files)>0){
				existingDates <- as.Date(sapply(strsplit(x=basename(old_files),split='[._]',fixed=F),'[',3),'%Y%m%d')
				oldestDateind <- which.max(existingDates)
				oldestDate <- existingDates[oldestDateind]
			} else {
				oldestDate <- NA
			}
			newDate <- as.Date(datestr,format='%Y%m%d')

			if(!isTRUE(oldestDate >= newDate )){
				downloadURL=paste0('http://cdec.water.ca.gov/cgi-progs/queryCSV?station_id=',site_id,'&dur_code=D&sensor_num=82&start_date=1/1/1900&end_date=',strftime(newDate,'%m/%d/%Y'))
				dstatus <- download.file(downloadURL,new_file,method="auto")
			}
			if(length(old_files)>0){
				file.remove(old_files[-oldestDateind])
			}

		}
		fns = list.files(path = PATH_SNOTEL, pattern = glob2rx(paste0('cdec*.csv')),full.names=T)
		# fname=fns[1]
		# print(fname)

		importCDEC <- function(fn){
			tryCatch({
				read_csv(fn,skip=2, quote="\'",
								 na='m',
								 col_types = list(
								 	col_date(format='%Y%m%d'),
								 	col_time(format='%H%M'),
								 	col_number()
								 ),
								 col_names = c('dte','time','swe')
				)}, error = function(e){
					data_frame()
				})
		}

		siteIDs <- sapply(strsplit(basename(fns),'[._]',fixed=F),'[',2)
		dat <- map_df(fns,importCDEC,.id='Site_ID') %>%
			mutate(Site_ID=siteIDs[as.numeric(Site_ID)])

		print('done reading files')

		dat2=full_join(station_locs,
									 dat,by=c('Site_ID'))

		cdecdata=dat2 %>%
			# filter(time=='') %>%
			dplyr::select(Site_ID, Longitude, Latitude, dte, swe) %>%
			mutate(
				dte=as.Date(dte),
				snotel=replace(swe,swe<0,  NA),
				snotel=snotel*2.54/100)#convert inches to meters

		return(cdecdata)
	}

}
