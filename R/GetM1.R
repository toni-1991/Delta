#' Get reduced matrix (M1) function
#'
#' This function reduce matrix provided by the user deleting missing categories, those j where sum(datatable[j,]) = sum(datatable[,j]) = 0.
#' Also provide a list of the categories deleted and provides the new size of the problem
#'

#' @param datatable Matrix. Expected to be square matrix with at least 2 rows (columns), non negative values and at least an element different of zero.
#' @keywords M1 datatable
#' @export
#' @examples
#' GetM1(matrix(c(1,2,0,3,4,0,0,0,0),3,3))

GetM1 <-function(datatable){
#: (A) Definicion de la Matriz M0 por eliminacion de clases vacias de la matriz de entrada datatable: M0=datatable, salvo cuando 
#Exista i | ri=ci=0, en tal caso se elimina esa clase en M0. 
#(B) Se debe crear una lista con los indices de las filas/columnas que se han quitado para darla como informacion en la salida  
#(C) Se deben definir las dimensiones del problema: k?dim(M0)[1]
  #datatable initial matrix
  
  #Marginals of the table
  R.marg   = margin.table(datatable,1)
  C.marg   = margin.table(datatable,2)

  #Detect rows different 0
  Delete = ((R.marg+C.marg)!=0)
  #Rows with TRUE are saved in new.matrix
  M1 = datatable[Delete,Delete]
  k  = dim(M1)[1]
  if (is.null(k) & all(Delete == FALSE)){
    k = 0
  }
  else if (is.null(k) & class(M1) == "numeric"){
    k = 1
  }
  Deleted = which(!Delete, arr.ind = FALSE)
  if (all(Delete)){
    Deleted = NULL
  }
  
  #Create a list with the output
  res = list("M1" = M1, "k" = k, "Deleted" = Deleted)
  class(res) <- "myGetM1"
  return(res)
}

print.myGetM1<- function(x){
  cat("Simplified matrix is:\n")
  print(x$M1)
  cat("The problem has ", x$k, " categories.\n")
  if(!is.null( x$Deleted)){
	 cat("Category(ies) ", x$Deleted, " had been deleted.\n")
  }
}
