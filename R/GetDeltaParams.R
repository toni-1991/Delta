#' Calculate Delta related parameters function
#'
#' This function perform all needed calculations to get all Delta related parameters. For do the exact calculations some variables previously calculated are needed.
#' 
#' @param mx Matrix. Agreement contingency table to perform calculations
#' @param fixedrows Boolean. Indicate if sample rows are fixed beforehand. 
#' @param Delta Vector. Each element indicate the probability of recognize an element i.
#' @param Pi Vector. Each element indicate the probability of classify at random an element in category i.
#' @param B Double. Numerical solution to the equation given by the model.
#' @param k Integer. Dimension of the problem.
#' @param Cov Matrix. Covariance matrix of Delta. 
#' @keywords Delta  mx fixedrows Pi B k Cov
#' @export
#' @examples
#' GetDeltaParams(...)

GetDeltaParams <- function(mx,fixedrows, Delta, Pi, B, k, Cov){
  #Calculate auxiliar params
  if(k == 2){
    mx = mx[-3,-3]
  }
  R.marg   	  = margin.table(mx,1)
  C.marg   	  = margin.table(mx,2)
  n			  = sum(R.marg)
  diag.matrix = diag(mx)
  diag.Cov 	  = diag(Cov)
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
  
  #Estimators calculations
  F = Delta
  P = R.marg*Delta/C.marg
  A = R.marg*Delta/n
  S = 2*R.marg*Delta/(R.marg + C.marg)
  Delta.total = sum(R.marg * Delta)/n
  
  #SE
  F.cov = diag.Cov
  #Sampling type 1
  if (fixedrows == FALSE){
    P.cov = (R.marg/C.marg)^2 * (diag.Cov + (C.marg - R.marg)/(R.marg*C.marg)*Delta ^2)
	A.cov = (R.marg/n)^2 * (diag.Cov + (n - R.marg)/(R.marg*n)*Delta ^2)
	S.cov = (2 * R.marg/(R.marg + C.marg))^2*(diag.Cov + Delta^2/(R.marg + C.marg)*(C.marg/R.marg - 2 + 2 * diag.matrix/(R.marg + C.marg)))
	if (k != 2){
	  Delta.total.cov = 1/n^2 * (n - 1/Sum.E - n * Delta^2)
	}
	else if (k == 2){
	  Delta.total.cov = 1/n^2 * (sum((R.marg %*%t(R.marg)) * Cov) + sum(R.marg * Delta^2) - n * Delta^2)
	}
  }#Sampling type 2
  else if (fixedrows == TRUE){
    P.cov = NULL
    A.cov = (R.marg/n)^2 * diag.Cov
	S.cov = NULL
	if (k != 2){
	  Delta.total.cov = 1/n^2 * (n - 1/Sum.E - sum(R.marg * Delta^2))
	}
	else if (k == 2){
	  Delta.total.cov = 1/n^2 * sum((R.marg %*%t(R.marg)) * Cov) 
	}
  }
  res = list("Delta.total" = Delta.total, "F" = F, "P" = P, "A" = A, "S" = S,
			 "Delta.total.cov" = Delta.total.cov, "F.cov" = F.cov, "P.cov" = P.cov, "A.cov" = A.cov, "S.cov" = S.cov)
			 
  return(res)
}
