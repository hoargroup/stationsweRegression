#' Fit the GLM-Net model to the data
#' @param formula formula to model
#' @param dF dataframe that includes x and y variables for regression
#' @param cl a cluster object for parallel execution
#' @export
#'
#'
gnet_phvfsca=function(dF,formula,cl=NULL){
	if(is.null(cl)) yesParallel=FALSE else yesParallel=TRUE
	nfolds=floor(nrow(dF)/30)
	if(nfolds<4) nfolds=4
	if(nfolds>10) nfolds=10
	myformula=as.formula(formula)
	bestalpha <- pickAlpha(dF,myformula,nfolds,cl)
	cvfit=cv.glmnet(myformula,data=dF,nfolds=nfolds,type.measure='mse',alpha=bestalpha,parallel = yesParallel,use.model.frame=TRUE)
	cvfit$alpha=bestalpha
	return(cvfit)
}
