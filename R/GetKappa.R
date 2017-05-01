#' Calculate Cohen's Kappa coefficient function
#'
#' This function perform Cohen's Kappa coefficient calculations. The function provide the Kappa coefficient and SE.
#' 
#' @param mx Matrix. Agreement contingency table to perform calculations
#' @keywords Kappa Cohen mx
#' @export
#' @examples
#' GetKappa(matrix(c(50,10,10,20),2,2))

GetKappa <-function(mx){  
  #Calculate Observed
  Observed = sum(diag(mx))  

  #Calculate marginals for expected
  R.marg   = margin.table(mx,1)
  C.marg   = margin.table(mx,2)
  dim.matrix = dim(mx)
  n        = sum(R.marg)

  Expected = sum(R.marg * C.marg)/n

  #Calculate kappa
  if (Expected == n){
    kappa	= 1
  }
  else{
    kappa    = (Observed - Expected)/(n - Expected)
  }
  #Add for aux calculations some matrixs
  aux_calc	= matrix(rep(R.marg/n,dim.matrix[1]),dim.matrix[1],dim.matrix[1]) + t(matrix(rep(C.marg/n,dim.matrix[1]),dim.matrix[1],dim.matrix[1]))
  Copy_matrix = mx/n
  Observed	= Observed/n
  Expected	= Expected/n
  diag(Copy_matrix) = 0
  
  #Calculate SE
  a = sum(diag(mx)/n*(1 - aux_calc * (1-kappa))^2)
  b = (1-kappa)^2*sum(Copy_matrix * aux_calc^2)
  c = (kappa - Expected * (1-kappa))^2
  SE = 1/(1-Expected)^2*sqrt((a+b-c)/n)
  res = list("kappa"=kappa,"SE"=SE)
  class(res) <- "myGetKappa"
  return(res)
}

print.myGetKappa<- function(x){
   cat("Kappa \u00b1 S.E. = ",x$kappa, " \u00b1 ",x$SE,'\n')
}
