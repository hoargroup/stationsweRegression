#' download and combine station data
#'
#' @param yr yr of the simulation date
#' @param station_locs tbl_df of site metadata
#' @param network default snotel
#' @export
#' @return tibble with station data from nrcs

get_stationswe_data <- function(yr=2017,station_locs,network='snotel'){
	if(network=='snotel'){
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

		dat2=full_join(station_locations,
									 dat,by=c('Site_ID'))
		# str(dat2)
		snoteldata=dat2 %>%
			# filter(time=='') %>%
			dplyr::select(Station_ID, Site_ID, Longitude, Latitude, dte, swe, snwd) %>%
			mutate(
				dte=as.Date(dte),
				snotel=replace(swe,swe<0,  NA),
				snotel=snotel*2.54/100,#convert inches to meters
				snwd=replace(snwd,snwd<0,  NA),
				snwd=snwd*2.54/100)
	}

	return(snoteldata)
}
