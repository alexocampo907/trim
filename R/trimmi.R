trimmi <- function(Y.obs,A,sMNAR,trim.side=NULL,alpha=NULL,num.perm=10000,quantile.CI=T,n.imp=10,t.dist=F,...){

  imp.data <- data.frame(Y.obs = Y.obs[sMNAR==0],A = A[sMNAR==0])
  imp.mix <- mice(imp.data,maxit = 2,m = n.imp,printFlag=F,...)

  tmp.data <- data.frame(Y.obs = Y.obs,A = A)
  which.imputed <- sMNAR == 0 & is.na(Y.obs)
  if(is.null(alpha)) alpha <- max(mean(is.na(tmp.data$Y.obs[tmp.data$A==1])),
                                  mean(is.na(tmp.data$Y.obs[tmp.data$A==0]))) # redudant?

  tmms.mix <- matrix(0,nrow = ncol(imp.mix$imp$Y.obs),ncol = 7)

  for(i in 1:ncol(imp.mix$imp$Y.obs)){
    tmp.data$Y.obs[which.imputed] <- imp.mix$imp$Y.obs[,i]
    tmms.mix[i,] <- c(trim(Y.obs = tmp.data$Y.obs,A = tmp.data$A,trim.side = "upper",
                           alpha = alpha,num.perm = num.perm,quantile.CI = quantile.CI))
  }

  rr <- simple.rubinsRule(estimates = tmms.mix[,3],variances = tmms.mix[,7]^2)

  res.trimmi <- c(colMeans(tmms.mix)[1:3],rr[2:5])
  names(res.trimmi) <- c("E[Y|A=1]","E[Y|A=0]","E[Y|A=1] - E[Y|A=0]","SE","2.5%","97.5%","p-value")

  return(res.trimmi)
}
