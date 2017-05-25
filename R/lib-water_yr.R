#' wateryear
#'
#' function to calculate wateryear from date column
#' @param dates date to convert
#' @param start_month defaults to 10
#' @return wateryear of the date
#' @export


water_yr <- function(dates, start_month=10) {
	# Convert dates into POSIXlt
	dates.posix = as.POSIXlt(dates)
	# Year offset
	offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
	# Water year
	adj.year = dates.posix$year + 1900 + offset
	# Return the water year
	return(
		adj.year)
}
