#' error functions
#'
#' @param model model object
#' @param data dataframe to predict with modelobj
#' @export

myr2=function(model,data){
	yvar=all.vars(formula(model)[[2]])
	stats::cor(predict(model, data), as.data.frame(data)[[yvar]], use='complete.obs')^2
}

#' error functions
#'
#' @param model model object
#' @param data dataframe to predict with modelobj
#' @export
#'
mypctmae=function(model,data) {
	yvar=all.vars(formula(model)[[2]])
	mae(model,data)/mean(as.data.frame(data)[[yvar]],na.rm=T)*100
}

#' error functions
#'
#' @param yobs observed
#' @param yhat predicted
#' @export
r2=function(yobs,yhat){
	cor(yobs,yhat,use='complete.obs')^2
}

#' error functions
#'
#' @param yobs observed
#' @param yhat predicted
#' @export
rootmse = function(yobs,yhat){
	sqrt(mean((yobs-yhat)^2,na.rm=T))
}
