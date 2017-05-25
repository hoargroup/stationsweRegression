#' Fit the GLM-Net model to the data
#' @param dF dataframe that includes x and y variables for regression
#' @param cl a cluster object for parallel execution
#' @export
#'
#'
gnet_phvfsca=function(dF,cl=NULL){
	if(is.null(cl)) yesParallel=FALSE else yesParallel=TRUE
	nfolds=floor(nrow(dF)/30)
	if(nfolds<4) nfolds=4
	if(nfolds>10) nfolds=10
	myformula=as.formula(snotel~lon+lat+dem+eastness+northness+dist2coast+dist2contdiv+regionaleastness+regionalnorthness+regionalzness+zness+fsca)
	bestalpha <- pickAlpha(dF,myformula,nfolds,cl)
	cvfit=cv.glmnet(myformula,data=dF,nfolds=nfolds,type.measure='mse',alpha=bestalpha,parallel = yesParallel)
	cvfit$alpha=bestalpha
	return(cvfit)
}
