GetCovarianze <- function(mx,Delta,Pi,B,k){
  #Calculate auxiliar params
  if(k == 2){
    mx = mx[-3,-3]
  }
  R.marg   	  = margin.table(mx,1)
  diag.matrix = diag(mx)
  if (is.null(Pi)){
    Pi		= rep(0,k)
  }
  if (is.null(Delta)){
    Delta		= rep(1,k)
  }
  if (is.null(B)){
    B		= 1
  }
  v = (1-Delta)/(1-Pi)
  E = Pi/(B - R.marg*v)
  Sum.E = sum(E)
  
  #Calculate covariances
  #Cov(Delta,Delta)
  Cov_Delta = -((v*E)%*%t(v*E))/Sum.E
  diag(Cov_Delta) = diag(Cov_Delta) + v*(diag.matrix/R.marg^2 + v*E)
  #Cov(Delta,Pi)
  Cov_mix = ((v*E)%*%t(E))/Sum.E
  diag(Cov_mix) = diag(Cov_mix) - v*E
  #Cov(Pi,Pi)
  Cov_Pi = -(E%*%t(E))/Sum.E
  diag(Cov_Pi) = diag(Cov_Pi) + E
  
  res = list("Cov_Delta" = Cov_Delta, "Cov_mix" = Cov_mix, "Cov_Pi" = Cov_Pi)
  return(res)
}
