simple.trim <- function(Y.obs,A,trim.side=NULL,alpha=NULL){

  if(is.null(alpha)) alpha <- max(mean(is.na(Y.obs[A==1])),mean(is.na(Y.obs[A==0])))

  Y.ranked <- Y.obs
  epsilon = 0.0001

  if(is.null(trim.side)) stop('trim.side argument not specified')
  if(trim.side == "upper"){

    Y.ranked[is.na(Y.obs) & A==1] = max(Y.obs[A==1],na.rm = T) + epsilon
    Y.ranked[is.na(Y.obs) & A==0] = max(Y.obs[A==0],na.rm = T) + epsilon

    Y.trim0 <- quantile(x = Y.ranked[A==0],probs = 1-alpha)
    Y.trim1 <- quantile(x = Y.ranked[A==1],probs = 1-alpha)

    mu0.hat <- mean(Y.ranked[Y.ranked<Y.trim0 & A==0])
    mu1.hat <- mean(Y.ranked[Y.ranked<Y.trim1 & A==1])
  }
  if(trim.side == "lower"){
    Y.ranked[is.na(Y.obs) & A==1] = min(Y.obs[A==1],na.rm = T) - epsilon
    Y.ranked[is.na(Y.obs) & A==0] = min(Y.obs[A==0],na.rm = T) - epsilon

    Y.trim0 <- quantile(x = Y.ranked[A==0],probs = alpha)
    Y.trim1 <- quantile(x = Y.ranked[A==1],probs = alpha)

    mu0.hat <- mean(Y.ranked[Y.ranked>Y.trim0 & A==0])
    mu1.hat <- mean(Y.ranked[Y.ranked>Y.trim1 & A==1])
  }

  mu.delta.hat <- mu1.hat-mu0.hat

  return(c(mu1.hat,mu0.hat,mu.delta.hat))
}
