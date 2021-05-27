trim <- function(Y.obs,A,trim.side=NULL,alpha=NULL,num.perm = 10000,quantile.CI=T){

  if(is.null(trim.side)) stop('trim.side argument not specified')

  mus.hat <- simple.trim(Y.obs = Y.obs,A = A,trim.side = trim.side,alpha = alpha)

  # Confidence Interval
  mus <- numeric(num.perm)

  for(w in 1:num.perm){
    A.new <- A[sample(x = 1:length(A),replace = F)]
    mus[w] <- simple.trim(Y.obs = Y.obs,A = A.new,trim.side = trim.side,alpha = alpha)[3]
  }

  # P-value
  if(mus.hat[3] < 0){
    exact.p <- mean(mus<=mus.hat[3])
  }else{
    exact.p <- mean(mus>=mus.hat[3])
  }

  CI <- mus.hat[3] + c(-qnorm(0.975)*sd(mus),qnorm(0.975)*sd(mus))
  if(quantile.CI==T) CI <- mus.hat[3] + quantile(x = mus,probs = c(0.025,0.975))

  res.mat <- c(mus.hat,CI,exact.p*2,sd(mus))
  names(res.mat) <- c("E[Y|A=1]","E[Y|A=0]","E[Y|A=1] - E[Y|A=0]","2.5%","97.5%","p-value","SE")

  return(res.mat)
}
