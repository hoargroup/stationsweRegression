#' unscale vector
#'
#' function to unscale column of dataframe
#' @param vec numeric vector to unscale
#' @export


unscale_vec<-function(vec){
if(length(which(is.na(vec)))==length(vec)){
     return(NA)
}
     avg=attr(vec,'scaled:center')
     std=attr(vec,'scaled:scale')
     return(
          vec*std+avg
          )
}
