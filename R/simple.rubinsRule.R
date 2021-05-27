simple.rubinsRule <- function(estimates,variances,t.dist=F){

  m <- length(estimates)

  Q <- mean(estimates)
  U <- mean(variances)
  B <- (1/(m-1)) * sum((estimates - Q)^2)
  T <- sqrt(U + (1 + (1/m))*B)

  if(t.dist==T){
    rD <- (1+(1/m))*B/U
    v <- (m-1)*(1+1/rD)^2
    RB.interval <- Q + c(0,T-Q,-qt(0.975)*T,qt(0.975)*T,2*pt(Q,df=v)-Q)
  }

  RB.interval <- Q + c(0,-qnorm(0.975)*T,qnorm(0.975)*T,2*pnorm(Q,mean = 0,sd = T)-Q,T-Q)
  names(RB.interval) <- c("Estimate","2.5%","97.5%","p-value","SE")

  return(RB.interval)
}
