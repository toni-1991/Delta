print.Delta<- function(x){
   # Information about deleted rows;
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
     print(x$E.matrix)

     #Little summary
     cat("SUMMARY: Goodness of Fit + Kappa + Delta",'\n')
      print(x$Summary)
     #Note under some cases;
     if (tp == "2.0" | tp == "3.0") {
        cat("* A total of rows or column is equal to zero: Results obteined adding 0.5 to all cells. ",'\n')
     }
     else if (tp == "2.1" | tp == "3.1") {
        cat("*(Since R(i)=C(i)=x(i,i) for all i, S.E.(kappa) has been obtained adding 0.5 to the original data)")
     }


   }

   cat("End of the problem: K = ", k,'\n')
}