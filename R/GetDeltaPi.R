GetDeltaPi <-function(mx,tp,tol,maxits){
  
  if (tp != "3.1" & tp != "2.1"){
    #Checks are done on GetB, so no need to do it again
    res 	= GetB(mx,tol,maxits)

    #Keep all the needed info from the result
    B 		      = res$B
    term	      = res$term
  
    #Derivate other parameters
    R.marg   	  = margin.table(mx,1)
    C.marg   	  = margin.table(mx,2)
    diag.matrix   = diag(mx)
    n        	  = sum(R.marg)
  
    #Calculate parameters
    Pi 	    = (B + (C.marg - R.marg) + 
              term * sqrt(round((B + (C.marg - R.marg))^2 - 4 * B * (C.marg - diag.matrix),10)))/(2*B)
    Delta   = (diag.matrix - R.marg * Pi)/(R.marg * (1 - Pi))

    if (tp == "2.0" | tp == "2.2"){
      Pi = Pi[-3]
      Delta = Delta[-3]
    }
  } #WARNING: This case is not in specs, susceptible to change
  else if (tp == "2.1"){
    Pi 	    = rep(NULL,2)
    Delta 	= rep(1, 2)
    term 	= rep(-1, 2)
    B 	    = NULL
  }
  else {
    dim.matrix  = dim(mx)[1]
    Pi 	        = rep(NULL, dim.matrix)
    Delta 	    = rep(1, dim.matrix)
    term 	    = rep(-1, dim.matrix)
    B 	    	= NULL
  }
  
  res = list("B" = B, "term" = term,"Pi" = Pi, "Delta" = Delta)
  return(res)

}
