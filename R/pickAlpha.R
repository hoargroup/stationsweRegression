#' select alpha for glmnet by crossvalidation
#' @param dF dataframe with x and y variables for regression
#' @param myformula regression formula
#' @param nfolds number of folds for k-fold cross validation
#' @param cl cluster object for parallel processing
#' @details this is a helper function for gnet_phvfsca

pickAlpha=function(dF,myformula,nfolds,cl=cl){
	cvalpha=cvAlpha.glmnet(myformula,data=dF,nfolds=nfolds,type.measure='mse',outerParallel=cl)

	maxlambda=100
	alpha=cvalpha$alpha
	ialpha=1
	cvdf=data_frame(alpha) %>%
		bind_cols(data.frame(matrix(NA,ncol=maxlambda,nrow=length(alpha)))) %>%
		bind_cols(as.data.frame(matrix(NA,ncol=maxlambda,nrow=length(alpha))))#
	for(ialpha in seq_along(alpha)){
		cvlambdas=cvalpha$modlist[[ialpha]]$lambda
		numlambda=length(cvlambdas)
		cvdf[ialpha,2:(numlambda+1)] <- cvlambdas
		cvdf[ialpha,maxlambda+1+(1:numlambda)] <- cvalpha$modlist[[ialpha]]$cvm
	}
	cvresults <-
		cvdf %>%
		mutate(alpha=alpha) %>%
		gather(lambdaid,lambdaval,num_range('X',1:100)) %>%
		gather(mseid,mseval,num_range('V',1:100))

	med_mse <-
		cvresults %>%
		group_by(alpha) %>%
		summarise(
			medmse=median(mseval,na.rm=T)
		)

	med_se <- med_mse %>%
		summarise(
			se=sd(medmse,na.rm=T)/sqrt(n())
		) %>% as.numeric

	alphaind=which.min(abs(med_mse$medmse-(min(med_mse$medmse,na.rm=T)+med_se)))[1]
	bestalpha=alpha[alphaind]
	# bestalpha=1
	return(bestalpha)
}
