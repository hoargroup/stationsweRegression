#' r-square
#'
#' @param model model object
#' @param data dataframe to predict with modelobj
#' @details there are two rsquare functions because I clipped them from different pieces of code. They take different types of inputs but perform the same calculation.
#' @export

myr2=function(model,data){
	yvar=all.vars(formula(model)[[2]])
	yhat=predict(model, data)
	# print(yhat)
	yobs=as.data.frame(data)[[yvar]]
	# print(yobs)
	stats::cor(yhat, yobs, use='complete.obs')^2
}

#' mean absolute error as percent of mean obs
#'
#' @param model model object
#' @param data dataframe to predict with modelobj
#' @export
#'
mypctmae=function(model,data) {
	yvar=all.vars(formula(model)[[2]])
	mae(model,data)/mean(as.data.frame(data)[[yvar]],na.rm=T)*100
}

#' r-square
#'
#' @param yobs observed
#' @param yhat predicted
#' @details there are two rsquare functions because I clipped them from different pieces of code. They take different types of inputs but perform the same calculation.
#' @export
r2=function(yobs,yhat){
	cor(yobs,yhat,use='complete.obs')^2
}

#' root mean squared error
#'
#' @param yobs observed
#' @param yhat predicted
#' @export
rootmse = function(yobs,yhat){
	sqrt(mean((yobs-yhat)^2,na.rm=T))
}
