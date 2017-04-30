#' Calculate Asintotic Delta related parameters function
#'
#' This function perform all needed calculations to get all Delta related parameters, for a 2x2 matrix. All calculations are asintotics.
#' 
#' @param mx Matrix. Agreement contingency table to perform calculations
#' @param fixedrows Boolean. Indicate if sample rows are fixed beforehand. 
#' @keywords Delta Asintotic mx fixedrows
#' @export
#' @examples
#' GetAsinDeltaParams(matrix(c(60,10,10,20),2,2),TRUE)

GetAsinDeltaParams <- function(mx,fixedrows = TRUE){
   #Calculate auxiliar params
  R.marg   	  = margin.table(mx,1)
  C.marg   	  = margin.table(mx,2)
  n			  = sum(R.marg)
  diag.matrix = diag(mx)
  
  Delta = (diag.matrix - sqrt(mx[3] * mx[2]))/R.marg
  Pi 	= c(sqrt(mx[2])/(sqrt(mx[3]) + sqrt(mx[2])),
			sqrt(mx[3])/(sqrt(mx[3]) + sqrt(mx[2])))
			
  diag.Cov 	  = 1/R.marg^2*(diag.matrix * (1-Delta) + 1/4*(mx[3] + mx[2] - n * mx[3] * mx[2]/(R.marg[1] * R.marg[2])))
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
    P.cov = 1/n^2 * (diag.matrix * (1-P) + 1/4*(mx[3] + mx[2]))
	A.cov = 1/n^2 * (diag.matrix + 1/4*(mx[3] + mx[2]) - n * A)
	S.cov = (n * (1 - Delta.total)/(R.marg + C.marg)^2)*(2 - (n * (1 - Delta.total) * (mx[3] + mx[2]))/(R.marg + C.marg)^2)
	Delta.total.cov = 1/n * (1 - Delta.total) * (1 + Delta.total)
  }#Sampling type 2
  else if (fixedrows == TRUE){
    P.cov = NULL
    A.cov = (R.marg/n)^2 * diag.Cov
	S.cov = NULL
	Delta.total.cov = (1 - Delta.total)/n * sum(diag.matrix/R.marg) 
  }
  
  res = list("Delta.total" = Delta.total, "F" = F, "P" = P, "A" = A, "S" = S,
			 "Delta.total.cov" = Delta.total.cov, "F.cov" = F.cov, "P.cov" = P.cov, "A.cov" = A.cov, "S.cov" = S.cov)
			 
  return(res)
}
