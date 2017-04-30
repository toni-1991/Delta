#B parameter calculation
GetB <-function(mx, tol, maxits){
  
  #Calculate needed parameters
  dim.matrix = dim(mx)
  R.marg   = margin.table(mx,1)
  C.marg   = margin.table(mx,2)
  n        = sum(R.marg)
  diag.matrix = diag(mx)
  Sum_xii  = sum(diag.matrix)
  
  #Initialize B0 and B0_i
  B0	     = 0
  B0_i       = 0

  #Calculate max position and value
  calc = (sqrt(C.marg - diag.matrix) + sqrt(R.marg - diag.matrix))^2
  B0_i = which.max(round(calc,10))
  B0   = calc[B0_i]

  #Create general function
  term = rep(-1, dim.matrix[1])

  radical = function(B){
    radicals = Re(sqrt((B + (C.marg - R.marg))^2 - 4 * B * (C.marg - diag.matrix)))
    return(radicals)
  }
  y = function(B){
    yB = (dim.matrix[1] - 2) * B + sum(term * radical(B))
    return(yB)
  }

  yB0=y(B0)
  #If its negative, should be updated to this function
  if (yB0 < 0) {
    term[B0_i]=1
  }

  #Since B = n(1-Delta)>=B0, B is between B0 and 2n, since the min value of delta is -1, 
  #be confused between levels
  root =  uniroot(y,c(B0,2*n), tol = tol, maxiter = maxits)$root

  res = list("B" = root, "term" = term)
  # "yB0" = yB0, "B0_i" = B0_i not needed
  return(res)
}