#' reads dates from a csv file
#'
#' @param fn filename of csv file with dates
#' @details the csv should have exactly 1 column with a column heading. the date format must be year-numeric_month-day
#' @export
#' @return a tibble of dates with new columns for yr, doy, day
#'

import_dates <- function(fn){
	read_csv(DATEFILE,col_names=TRUE) %>%
	setNames('dte') %>%
		mutate(dte=as.Date(dte,'%Y-%m-%d'),
					 yr=strftime(dte,'%Y'),
					 doy=strftime(dte,'%j')) %>%
		arrange(dte) %>%
		filter(complete.cases(.))
}
