
#' @return \code{NULL}
#'
#' @rdname Delta
#' @export
#' @method print Delta 
print.Delta<- function(x){
   # Information about deleted rows;
   tp = x$tp
   samplingtype = x$samplingtype
   if (x$k != x$k0){
	if (x$k = x$k0-1){
	   cat("Category i = ",x$Del_rows, " had been deleted from the problem because r_i = c_i = 0",'\n')
	}
	else{
	   cat("Categories i = ",x$Del_rows, " had been deleted from the problem because r_i = c_i = 0",'\n')
	}
   }
   #Information about raw matrix and M1
   cat("Original data",'\n')
   print(x$M1)
   #No more results for cases 1.0 and 1.1
   if (x$tp != "1.0" & x$tp != "1.1"){
	#Special note and M2 shown in some cases;
    if (x$tp = "2.0" | tp == "2.1" | tp == "2.2") {
      cat("The problem has infinite solutions. The following solution has been obtained by creating an extra",'\n',
	  "(and fictitious) class where c3=r3=x33=1 and by adding 0.5 to all the observations. ",'\n',
	  "Data analyzed by Delta model",'\n')
  	  print(x$M2)
    }
    else if (tp == "3.0") {
      cat("The problem has undetermined solutions. The following solution has been obtained by adding 0.5 to",'\n',
	  "all the observations",'\n',"Data analyzed by Delta model",'\n')
      print(x$M2)

    }
    else if (tp == "3.2") {
      cat("The problem has infinite solutions. The following solution has been obtained by adding 0.5 to all ",'\n',
	  "the observations.",'\n',"Data analyzed by Delta model",'\n')
  	  print(x$M2)
    }
    else if (tp == "3.3") {
      cat("The problem has no solution. The following solution has been obtained by adding 0.5 to all the ",'\n',
	  "observations.",'\n',"Data analyzed by Delta model",'\n')
  	  print(x$M2)
    }

	#Expected quantities
    cat("Expected quantities",'\n')
    print(x$E_matrix)

    #Little summary
    cat("SUMMARY: Goodness of Fit + Kappa + Delta",'\n')
    print(x$Summary)
    #Note under some cases;
    if (tp == "2.0" | tp == "3.0") {
      cat("* A total of rows or column is equal to zero: Results obtained adding 0.5 to all cells. ",'\n')
    }
    else if (tp == "2.1" | tp == "3.1") {
      cat("*(Since R(i)=C(i)=x(i,i) for all i, S.E.(kappa) has been obtained adding 0.5 to the original data)")
    }
	
	#HIDDEN RESULTS: Cov matrix
	#Warning, is mixed covariance or is Delta Delta?
	cat("Covariance Matrix = Cov(Delta,Pi)",'\n')
	print(x$Cov)
	#HIDDEN RESULTS: All model parameters
	temp = paste("Model Parameters (Delta and Pi) and All Concordance Measures \u00b1 S.E.",'\n')
	Encoding(temp) = "UTF-8"
	print(temp)
	print(x$Fullparamstable)
	temp = paste(res$Deltaoverall," \u00b1 ",res$Deltaoverall_SE,'\n')
	Encoding(temp) = "UTF-8"
	print(temp)
	if (tp == "3.1"){
	  cat("The problem has infinite solutions for pi. For this reason the values of S.E. have been obtained by adding",'\n', 
	  "0.5 to all the observations. The new parameters are those marked with *.",'\n')
	}
	else if (tp == "3.4"){
	  cat("At least one estimation of 'delta i' or of 'pi i' falls on the boundary of the parametric space. For this ",'\n', 
	  "reason the values of S.E. have been obtained by adding 0.5 to all the observations. The new parameters are those",'\n', 
	  "marked with *.",'\n')
	}
	
	#Model parameters selected
	temp = paste("Model Parameters (Delta and Pi) and Concordance Measures \u00b1 S.E. with selected conditions",'\n')
	Encoding(temp) = "UTF-8"
	cat(temp)
	if (samplingtype<=3){
	  cat("Totals in rows are prefixed",'\n')
	}
	else {
	  cat("Totals in rows and columns are at random",'\n')
	}
  	if (samplingtype==1 | samplingtype==4){
	  cat("Row observer is standard",'\n')
	} 
  	else if (samplingtype==2){
	  cat("Row observer is in columns",'\n')
	} 
	else {
	  cat("There isn't a standard observer",'\n')
	}

	print(x$Paramstable)
	temp = paste(res$Deltaoverall," \u00b1 ",res$Deltaoverall_SE,'\n')
	Encoding(temp) = "UTF-8"
	print(temp)
	if (tp == "3.1"){
	  cat("The problem has infinite solutions for pi. For this reason the values of S.E. have been obtained by adding",'\n', 
	  "0.5 to all the observations. The new parameters are those marked with *.",'\n')
	}
	else if (tp == "3.4"){
	  cat("At least one estimation of 'delta i' or of 'pi i' falls on the boundary of the parametric space. For this ",'\n', 
	  "reason the values of S.E. have been obtained by adding 0.5 to all the observations. The new parameters are those",'\n', 
	  "marked with *.",'\n')
	}
	
	if (tp == "2.0" | tp == "2.1" | tp == "2.2"){
	  ### ASINTOTIC NORMAL SOLUTION ###;
	  cat("ASINTOTIC SOLUTION (based in raw data)",'\n')
      cat("(These solutions have been obtained like in previous cases, but adding c insteed of 0.5 and let c tend 0)",'\n')
      cat("Data analyzed by Delta model",'\n')
	  print(x$M_AN)
	  if (tp == "2.0"){
		cat("* A row or column total is zero: Results has been obtained adding 0.5 to all cells",'\n')
	  }
	  
    cat("SUMMARY: Kappa + Delta",'\n')
	  print(x$Summary_AN)
	  
	  if (tp == "2.0" ) {
		cat("* A total of rows or column is equal to zero: Results obtained adding 0.5 to all cells. ",'\n')
	  }
	  else if (tp == "2.1" ) {
		cat("*(Since R(i)=C(i)=x(i,i) for all i, S.E.(kappa) has been obtained adding 0.5 to the original data) ",'\n')
	  }
	  
	  #HIDDEN RESULTS: All model parameters
      temp = paste("Model Parameters (Delta and Pi) and All Concordance Measures \u00b1 S.E. (Asintotic Normal)",'\n')
	  Encoding(temp) = "UTF-8"
	  print(temp)
	  print(x$Fullparamstable_AN)
      temp = paste(res$Deltaoverall_AN," \u00b1 ",res$Deltaoverall_SE_AN,'\n')
	  Encoding(temp) = "UTF-8"
	  print(temp)
	  if (tp == "2.0"){
	  cat("The estimations of 'delta i' falls on the boundary of the parametric space. For this reason the values of S.E.",'\n',
			"have been obtained by adding 0.5 to all the observations. The new parameters are those marked with *." ,'\n')
	  }
	  
	  #Model parameters selected
	  temp = paste("Model Parameters (Delta and Pi) and Concordance Measures \u00b1 S.E. with selected conditions",'\n',
					"(Asintotic Normal)",'\n')
	  Encoding(temp) = "UTF-8"
	  cat(temp)
	  if (samplingtype<=3){
	    cat("Totals in rows are prefixed",'\n')
	  }
	  else {
	    cat("Totals in rows and columns are at random",'\n')
	  }
  	  if (samplingtype==1 | samplingtype==4){
	    cat("Row observer is standard",'\n')
	  } 
  	  else if (samplingtype==2){
	    cat("Row observer is in columns",'\n')
	  } 
	  else {
	    cat("There isn't a standard observer",'\n')
	  }
	  print(x$Paramstable_AN)
	  temp = paste(res$Deltaoverall_AN," \u00b1 ",res$Deltaoverall_SE_AN,'\n')
	  Encoding(temp) = "UTF-8"
	  print(temp)
	  if (tp == "2.0"){
	    cat("The estimations of 'delta i' falls on the boundary of the parametric space. For this reason the values of S.E.",'\n',
			"have been obtained by adding 0.5 to all the observations. The new parameters are those marked with *." ,'\n')
	  }
	  
	  
	  ### ASINTOTIC EXTRA SOLUTION ###;
	  cat("ASINTOTIC SOLUTION (based in raw data incremented by 1)",'\n')
      cat("(These solutions have been obtained like in previous cases, but for original data incremented in 1)",'\n')
      cat("Data analyzed by Delta model",'\n')
	  print(x$M_AE)

    cat("SUMMARY: Kappa + Delta",'\n')
	  print(x$Summary_AE)
	  
	  if (tp == "2.0" ) {
		cat("* A total of rows or column is equal to zero: Results obtained adding 0.5 to all cells. ",'\n')
	  }     
	  else if (tp == "2.1" ) {
		cat("*(Since R(i)=C(i)=x(i,i) for all i, S.E.(kappa) has been obtained adding 0.5 to the original data) ",'\n')
	  }
	  
	  #HIDDEN RESULTS: All model parameters
      temp = paste("Model Parameters (Delta and Pi) and All Concordance Measures \u00b1 S.E. (Asintotic Extra)",'\n')
	  Encoding(temp) = "UTF-8"
	  print(temp)
	  print(x$Fullparamstable_AE)
      temp = paste(res$Deltaoverall_AE," \u00b1 ",res$Deltaoverall_SE_AN,'\n')
	  Encoding(temp) = "UTF-8"
	  print(temp)
	  
	  #Model parameters selected
	  temp = paste("Model Parameters (Delta and Pi) and Concordance Measures \u00b1 S.E. with selected conditions",'\n',
					"(Asintotic Extra)",'\n')
	  Encoding(temp) = "UTF-8"
	  cat(temp)
	  if (samplingtype<=3){
	    cat("Totals in rows are prefixed",'\n')
	  }
	  else {
	    cat("Totals in rows and columns are at random",'\n')
	  }
  	  if (samplingtype==1 | samplingtype==4){
	    cat("Row observer is standard",'\n')
	  } 
  	  else if (samplingtype==2){
	    cat("Row observer is in columns",'\n')
	  } 
	  else {
	    cat("There isn't a standard observer",'\n')
	  }
	  print(x$Paramstable_AE)
	  temp = paste(res$Deltaoverall_AE," \u00b1 ",res$Deltaoverall_SE_AN,'\n')
	  Encoding(temp) = "UTF-8"
	  print(temp)
	}
  }
   cat("End of the problem: K = ", k,'\n')
}