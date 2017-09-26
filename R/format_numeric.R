#' helper function to format output to csvs
#' @param x dataframe to format
#' @export
#' @details base::format does not appear to produce a data.frame so it can't be used with write_tsv. I typically use this with sci=FALSE to turn off scientific notation but it can also control significant digits, decimal places etc. see `format` for options.

format_numeric <- function(x, ...) {
	numeric_cols <- vapply(x, is.numeric, logical(1))
	x[numeric_cols] <- lapply(x[numeric_cols], format, ...)
	x
}
