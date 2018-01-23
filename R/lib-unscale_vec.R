#' unscale vector
#'
#' function to unscale column of dataframe
#' @param vec numeric vector to unscale
#' @param scale_values vector with scale attributes to use
#' @export

unscale_vec<-function(vec, scale_values=NULL){

if(length(which(is.na(vec)))==length(vec)){
     return(NA)
}
	if(is.null(scale_values)){
     avg=attr(vec,'scaled:center')
     std=attr(vec,'scaled:scale')
	} else {
		avg=attr(scale_values,'scaled:center')
		std=attr(scale_values,'scaled:scale')
	}
	return(
		vec*std+avg
	)
}
