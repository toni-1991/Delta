GetGoodness <- function(mx,Pi,Delta){
  #Calculate aux variables
  dim.matrix  = dim(mx)[1]
  R.marg   	  = margin.table(mx,1)

  #In case of Null pi, set to 0
  if (is.null(Pi)){
    Pi		= rep(0,dim.matrix)
    Delta		= rep(1,dim.matrix)
  }

  #Define matrix
  E.matrix		= (R.marg * (1 - Delta)) %*% t(Pi)
  diag(E.matrix) 	= diag(mx)

  #Calculate numerator of chi-square
  chi.matrix	= (mx - E.matrix)^2
  diag(chi.matrix)= 0

  #To avoid undefinition in case of divide by 0, we will set E_{ij} = 0 to 1
  E.matrix[(E.matrix=0)] = 1

  # Calculate chi-squared
  chi.squared = sum(chi.matrix/E.matrix)

  #Degree of freedoms
  df = (dim.matrix-1) * (dim.matrix - 2) - 1

  pval = dchisq(chi.squared, df)

  # Standard output for chisq.test is X-squared = 3.2328, df = 3, p-value = 0.3571
  res = list("X.squared" = chi.squared, "df" = df, "p.value" = pval, "E.matrix" = E.matrix)
}