#' Delta coefficient function
#'
#' This function provides an analysis of the matrix provided, returning all all the parameters estimations
#' and SE calculations that have sense with the fixedrows and gstandard provided.
#'
#' This function study the matrix provided by the user. This function modify the matrix deleting missing rows and columns and 
#' if it is needed for the estimation, adding 0.5 to each cell.  
#' 
#' Also calculate Cohen's Kappa coefficient and the goodness of fit for the Delta model.
#' @param datatable Matrix. Expected to be square matrix with at least 2 rows (columns), non negative values and at least an element different of zero.
#' @param fixedrows Boolean. Indicate if sample rows are fixed beforehand. Default is TRUE.
#' @param gstandard Text. Indicate if there are a Gold Standard by Rows or columns. Only first letter matter without Case sensitivity. Options are: "N" for None, "R" for in Rows and "C" for in Columns. Default is "N".
#' @param maxits Whole number. Indicate the maximum number of iterations of the numeric method to calculate B. Expected to be 100 <= maxits <= 5000. Default is 1000.
#' @param tol Double number. Indicate the precision of the numeric method to calculate B. Expected to be 1e-6 <= tol <= 1e-15.Default is 1e-12.
#' @param dplaces Whole number. Decimal placed to be shown in the result. Expected to be 1 <= dplaces <0 6. Default 4.
#' @keywords Delta datatable fixedrows gstandard maxits tol dplaces
#' @export
#' @examples
#' Delta(matrix(c(1,2,3,4),2,2))
#' Delta(matrix(c(65,5,10,20),2,2),fixedrows=TRUE,gstandard="Row")

Delta <- function(datatable,fixedrows = FALSE, gstandard = "No",
         maxits=1000,tol=1e-12,dplaces=4){

  # Check input variables
  delta.samplingtype = CheckInput(datatable,fixedrows ,gstandard ,maxits,tol,dplaces)

  #Keep original size
  k_0 = dim(datatable)[1]

  #Calculate matrix deleting Unnecessary rows
  res = GetM1(datatable)
  M1 = res$M1
  k  = res$k
  Del_rows = res$Deleted

  #Get type of problem
  tp = GetProblemType(M1,k)
  #For type 1, no extra matrix are needed
  if (tp != "1.0" & tp != "1.1"){
    Mxs = GetMx(tp, M1)
    M2  = Mxs$M2
    M3  = Mxs$M3
    M4  = Mxs$M4
    M5  = Mxs$M5
	#NO asintotics calculation 
	#Calculations: Part 2, derivate Kappa and SE
	kappa.res = GetKappa(M4)
	if (tp == "2.1" | tp == "3.1"){
		kappa.res$kappa = 1
	}
	
	kappa.val 	= kappa.res$kappa
	kappa.SE 	= kappa.res$SE
	
	#Calculations: Part 2, calculate pi_i, delta_i
	res.Pi 	= GetDeltaPi(M2,tp,tol,maxits)
	Pi 		= res.Pi$Pi
	Delta 	= res.Pi$Delta
	B 		= res.Pi$B
    #mat will be the matrix to be used for calcs
	mat = M2
  
	#Goodnes of fit to the model
    res = GetGoodness(M2,Pi,Delta)
	X.sq 	= res$X.squared
	df 		= res$df
	p.val 	= res$p.value
	E.matrix = res$E.matrix
	  
	#For 3.1 and 3.4, redo calcs with M3 for delta and pi
	#To avoid issues with tp = 3.1, tp will be fixed to 3.4
	if (tp == "3.1" | tp == "3.4"){
	  res.Pi 	= GetDeltaPi(M3,"3.4",tol,maxits)
	  Pi 		= res.Pi$Pi
	  Delta 	= res.Pi$Delta
	  B 		= res.Pi$B
	  mat 		= M3
	}
	
	Covar = GetCovarianze(mat,Delta,Pi,B,k)
	Cov_Delta 	= Covar$Cov_Delta
	Cov_mix 	= Covar$Cov_mix
	Cov_Pi 		= Covar$Cov_Pi
	
	res.Params = GetDeltaParams(mat,fixedrows, Delta, Pi, B, k, Cov_Delta)
	
	#Calculations Part 3: Asintotic calculations
	if (tp == "2.0" | tp == "2.1" | tp == "2.2"){
	  res.M4 = GetAsinDeltaParams(M4,fixedrows)
	  res.M5 = GetAsinDeltaParams(M5,fixedrows)
	}	
  }
 
  Cat.Delete = ""
  Note = paste("End of the problem: K = ", k)
  if(k == (k_0 - 1)){
    Cat.Delete  = paste("Category i = ",Del_rows, " had been deleted from the problem because r_i = c_i = 0")
  }
  else if(k < k_0){
    Cat.Delete  = paste("Categories i = ",Del_rows, " had been deleted from the problem because r_i = c_i = 0")
  }
  if (tp == "1.0" | tp == "1.1"){
    res = list("Cat.Delete" = Cat.Delete , "Raw.matrix" = datatable, "M1" = M1,"Note" = Note)
    return(res) 
  }
  else if (tp == "2.0" | tp == "2.1" | tp == "2.2"){
    Note = "The problem has infinite solutions. The following solution has been obtained by creating an extra 
	(and fictitious) class where c3=r3=x33=1 and by adding 0.5 to all the observations. "
    res = list("Cat.Delete" = Cat.Delete , "Raw.matrix" = datatable, "M1" = M1,"Note" = Note,"Delta.mat" = M2)
  }
  else if (tp == "3.0"){
    Note = "The problem has undetermined solutions. The following solution has been obtained by adding 0.5 to 
    all the observations"
    res = list("Cat.Delete" = Cat.Delete , "Raw.matrix" = datatable, "M1" = M1,"Note" = Note,"Delta.mat" = M2)
  }
  else if (tp == "3.2"){
    Note = "The problem has infinite solutions. The following solution has been obtained by adding 0.5 to all the observations."
    res = list("Cat.Delete " = Cat.Delete , "Raw.matrix" = datatable, "M1" = M1,"Note" = Note,"Delta.mat" = M2)
  }
  else if (tp == "3.3"){
    Note = "The problem has no solution. The following solution has been obtained by adding 0.5 to all the observations."
    res = list("Cat.Delete" = Cat.Delete , "Raw.matrix" = datatable, "M1" = M1,"Note" = Note,"Delta.mat" = M2)
  }
  else {
    res = list("Cat.Delete" = Cat.Delete , "Raw.matrix" = datatable, "M1" = M1)
  }
  
  res$E.matrix 	= E.matrix
  Chisq 		= paste("Chi-squared = ",X.sq, "( d.f. = ",df, ")")
  if (tp == "2.0" | tp == "3.0") {
    Kappa.results	= paste("Kappa \u00b1 S.E. = ",kappa.val, "* \u00b1 ",kappa.SE, "*")
	Summary1.Note = "* A total of rows or column is equal to zero: Results obteined adding 0.5 to all cells. "
  }
  else if (tp == "2.1" | tp == "3.1") {
    Kappa.results	= paste("Kappa \u00b1 S.E. = ",kappa.val, " \u00b1 ",kappa.SE, "*")
	Summary1.Note = "*(Since R(i)=C(i)=x(i,i) for all i, S.E.(kappa) has been obtained adding 0.5 to the original data) "
  } 
  else {
    Kappa.results	= paste("Kappa \u00b1 S.E. = ",kappa.val, " \u00b1 ",kappa.SE)
	Summary1.Note = ""
  }
  Delta.results	= paste("Delta \u00b1 S.E. = ",res.Params$Delta.total, " \u00b1 ",res.Params$Delta.total.cov)
  
  Encoding(Chisq) = "UTF-8"
  Encoding(Kappa.results) = "UTF-8"
  Encoding(Delta.results) = "UTF-8"

  Summary1 = data.frame(Chisq,Kappa.results,Delta.results)
  res$Summary1 = Summary1
  res$Summary1.Note = Summary1.Note
  res$Cov		= Cov_mix

  Agreement = paste(res.Params$A," \u00b1 ",res.Params$A.cov)
  Conformity = paste(res.Params$F," \u00b1 ",res.Params$F.cov)
  Predictivity = paste(res.Params$P," \u00b1 ",res.Params$P.cov)
  Consistency = paste(res.Params$S," \u00b1 ",res.Params$S.cov)
  Encoding(Agreement) = "UTF-8"
  Encoding(Conformity) = "UTF-8"
  Encoding(Predictivity) = "UTF-8"
  Encoding(Consistency) = "UTF-8"

  
  if (is.null(Pi)){
    Pi		= rep(0,k)
  }
  Table = data.frame(1:k,Delta,Pi,Agreement,Conformity,Predictivity,Consistency)
  if (tp == "3.1" ){
    colnames(Table) = c("Class","Delta (*)","Pi (*)","Agreement","Conformity","Predictivity","Consistency")
    res$Table1 = Table
    res$Overall = res.Params$Delta.total
    res$Overall.SE  = res.Params$Delta.total.cov
    Note = "The problem has infinite solutions for pi. For this reason the values of S.E. have been obtained by adding 0.5 to all the observations. The new parameters are those marked with *."
    res$Note2 = Note
  } 
  else if (tp == "3.4") {
    colnames(Table) = c("Class","Delta (*)","Pi (*)","Agreement","Conformity","Predictivity","Consistency")
    res$Table1 = Table
    res$Overall = res.Params$Delta.total
    res$Overall.SE  = res.Params$Delta.total.cov
    Note = "At least one estimation of 'delta i' or of 'pi i' falls on the boundary of the parametric space. For this reason the values of S.E. have been obtained by adding 0.5 to all the observations. The new parameters are those marked with *."
    res$Note2 = Note 
  }
  else {
    colnames(Table) = c("Class","Delta","Pi","Agreement","Conformity","Predictivity","Consistency")
    res$Table1 = Table
    res$Overall = res.Params$Delta.total
    res$Overall.SE  = res.Params$Delta.total.cov
  }

  if (delta.samplingtype == 1) {
    Table = Table[,c(-6,-7)]
  }
  else if (delta.samplingtype == 2) {
    Table = Table[,c(-5,-6,-7)]
  }
  else if (delta.samplingtype == 3) {
    Table = Table[,c(-5,-6,-7)]
  }
  else if (delta.samplingtype == 4) {
    Table = Table[,-7]
  }
  else if (delta.samplingtype == 5) {
    Table = Table[,c(-5,-6)]
  }

  if (tp == "3.1" ){
    res$Table2 = Table
    res$Overall2 = res.Params$Delta.total
    res$Overall2.SE  = res.Params$Delta.total.cov
    Note = "The problem has infinite solutions for pi. For this reason the values of S.E. have been obtained by adding 0.5 to all the observations. The new parameters are those marked with *."
    res$Note3 = Note
  } 
  else if (tp == "3.4") {
    res$Table2 = Table
    res$Overall2 = res.Params$Delta.total
    res$Overall2.SE  = res.Params$Delta.total.cov
    Note = "At least one estimation of 'delta i' or of 'pi i' falls on the boundary of the parametric space. For this reason the values of S.E. have been obtained by adding 0.5 to all the observations. The new parameters are those marked with *."
    res$Note3 = Note 
  }
  else {
    res$Table2 = Table
    res$Overall2 = res.Params$Delta.total
    res$Overall2.SE  = res.Params$Delta.total.cov
  }

  #Asintotic solutions
  # res.m4 and res.M5
  if (k==2){
    #Salida 8
    if (tp == 2.0){
	  Note = "* A total row or column is equal to zero: Results are obtained adding 0.5 to all cells. "
	  res$M.asintotic = M4
	  res$Note4 = Note
    }
    else {
	  res$M.asintotic = M1
    }
	
	  if (tp == "2.0" ) {
		Kappa.results	= paste("Kappa \u00b1 S.E. = ",kappa.val, "* \u00b1 ",kappa.SE, "*")
		Summary2.Note = "* A total of rows or column is equal to zero: Results obteined adding 0.5 to all cells. "
	  }
	  else if (tp == "2.1" ) {
		Kappa.results	= paste("Kappa \u00b1 S.E. = ",kappa.val, " \u00b1 ",kappa.SE, "*")
		Summary2.Note = "*(Since R(i)=C(i)=x(i,i) for all i, S.E.(kappa) has been obtained adding 0.5 to the original data) "
	  }
	  else {
		Kappa.results	= paste("Kappa \u00b1 S.E. = ",kappa.val, " \u00b1 ",kappa.SE)
		Summary2.Note = ""
	  }

	  Delta.results	= paste("Delta \u00b1 S.E. = ",res.M4$Delta.total, " \u00b1 ",res.M4$Delta.total.cov)
	  
	  Encoding(Kappa.results) = "UTF-8"
	  Encoding(Delta.results) = "UTF-8"

	  Summary2 = data.frame(Kappa.results,Delta.results)
	  res$Summary2 = Summary2
	  res$Summary2.Note = Summary2.Note
	  
	  Agreement = paste(res.M4$A," \u00b1 ",res.M4$A.cov)
	  Conformity = paste(res.M4$F," \u00b1 ",res.M4$F.cov)
	  Predictivity = paste(res.M4$P," \u00b1 ",res.M4$P.cov)
	  Consistency = paste(res.M4$S," \u00b1 ",res.M4$S.cov)
	  Encoding(Agreement) = "UTF-8"
	  Encoding(Conformity) = "UTF-8"
	  Encoding(Predictivity) = "UTF-8"
	  Encoding(Consistency) = "UTF-8"
	  
	  Table = data.frame(1:k,Delta,Pi,Agreement,Conformity,Predictivity,Consistency)
	  if (tp == "2.1" ){
		colnames(Table) = c("Class","Delta (*)","Pi (*)","Agreement (*)","Conformity (*)","Predictivity (*)","Consistency (*)")
		res$Table3 = Table
		res$Overall3 = res.M4$Delta.total
		res$Overall3.SE  = res.M4$Delta.total.cov
		Note = "The estimations of 'delta i' falls on the boundary of the parametric space. For this reason the values of S.E. 
		        have been obtained by adding 0.5 to all the observations. The new parameters are those marked with *. "
		res$Note5 = Note
	  } 
	  else {
		colnames(Table) = c("Class","Delta","Pi","Agreement","Conformity","Predictivity","Consistency")
		res$Table3 = Table
		res$Overall3 = res.M4$Delta.total
		res$Overall3.SE  = res.M4$Delta.total.cov
	  }
	  
	  
	  if (delta.samplingtype == 1) {
		Table = Table[,c(-6,-7)]
	  }
	  else if (delta.samplingtype == 2) {
		Table = Table[,c(-5,-6,-7)]
	  }
	  else if (delta.samplingtype == 3) {
		Table = Table[,c(-5,-6,-7)]
	  }
	  else if (delta.samplingtype == 4) {
		Table = Table[,-7]
	  }
	  else if (delta.samplingtype == 5) {
		Table = Table[,c(-5,-6)]
	  }

	  if (tp == "3.1" ){
		res$Table4 = Table
		res$Overall4 = res.M4$Delta.total
		res$Overall4.SE  = res.M4$Delta.total.cov
		Note = "The estimations of 'delta i' falls on the boundary of the parametric space. For this reason the values of S.E. 
		        have been obtained by adding 0.5 to all the observations. The new parameters are those marked with *. "
		res$Note6 = Note
	  } 
	  else {
		res$Table5 = Table
		res$Overall5 = res.M4$Delta.total
		res$Overall5.SE  = res.M4$Delta.total.cov
	  }
	  
	  #Extra 
	
	  res$M.asintotic2 = M5
	 
	
	  if (tp == "2.0" ) {
		Kappa.results	= paste("Kappa \u00b1 S.E. = ",kappa.val, "* \u00b1 ",kappa.SE, "*")
		Summary2.Note = "* A total of rows or column is equal to zero: Results obteined adding 0.5 to all cells. "
	  }
	  else if (tp == "2.1" ) {
		Kappa.results	= paste("Kappa \u00b1 S.E. = ",kappa.val, " \u00b1 ",kappa.SE, "*")
		Summary2.Note = "*(Since R(i)=C(i)=x(i,i) for all i, S.E.(kappa) has been obtained adding 0.5 to the original data) "
	  }
	  else {
		Kappa.results	= paste("Kappa \u00b1 S.E. = ",kappa.val, " \u00b1 ",kappa.SE)
		Summary2.Note = ""
	  }

	  Delta.results	= paste("Delta \u00b1 S.E. = ",res.M5$Delta.total, " \u00b1 ",res.M5$Delta.total.cov)
	  
	  Encoding(Kappa.results) = "UTF-8"
	  Encoding(Delta.results) = "UTF-8"

	  Summary2 = data.frame(Kappa.results,Delta.results)
	  res$Summary3 = Summary2
	  res$Summary3.Note = Summary2.Note
	  
	  Agreement = paste(res.M5$A," \u00b1 ",res.M5$A.cov)
	  Conformity = paste(res.M5$F," \u00b1 ",res.M5$F.cov)
	  Predictivity = paste(res.M5$P," \u00b1 ",res.M5$P.cov)
	  Consistency = paste(res.M5$S," \u00b1 ",res.M5$S.cov)
	  Encoding(Agreement) = "UTF-8"
	  Encoding(Conformity) = "UTF-8"
	  Encoding(Predictivity) = "UTF-8"
	  Encoding(Consistency) = "UTF-8"
	  
	  Table = data.frame(1:k,Delta,Pi,Agreement,Conformity,Predictivity,Consistency)
	  colnames(Table) = c("Class","Delta","Pi","Agreement","Conformity","Predictivity","Consistency")
	  res$Table6 = Table
	  res$Overall6 = res.M5$Delta.total
	  res$Overall6.SE  = res.M5$Delta.total.cov
	  
	  
	  if (delta.samplingtype == 1) {
		Table = Table[,c(-6,-7)]
	  }
	  else if (delta.samplingtype == 2) {
		Table = Table[,c(-5,-6,-7)]
	  }
	  else if (delta.samplingtype == 3) {
		Table = Table[,c(-5,-6,-7)]
	  }
	  else if (delta.samplingtype == 4) {
		Table = Table[,-7]
	  }
	  else if (delta.samplingtype == 5) {
		Table = Table[,c(-5,-6)]
	  }

	  res$Table7 = Table
	  res$Overall7 = res.M5$Delta.total
	  res$Overall7.SE  = res.M5$Delta.total.cov
  }

return(res)

}
#Delta(matrix(c(10,0,0,0,10,0,0,0,10),3,3),fixedrows=FALSE,gstandard="No",maxits=100,tol=1e-12,dplaces=4)
