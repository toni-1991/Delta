#' Get matrix of the problem (Mx) function
#'
#' This function produce 4 new auxiliar matrix. The definition of the matrix depends on the type of problem (tp). For
#' example, if tp = 2.X, the program will create M2 with dimension 3x3 instead 2x2, and will add  to each cell, this way
#' we can use this auxiliars macros to solve the problem and avoid issues with solutions in the boundary or not completly defined. See complete
#' list in Detail section.
#'
#'  -------+------+------------+---------------+------------------+-----------------+
#'         | Case |      M2    |       M3      |        M4        |        M5       |
#'  -------+------+------------+---------------+------------------+-----------------+
#'  Purpose|      | Delta + SE | Delta + SE    | Delta asintotic  | Delta asintotic |
#'		   |      |            | M1 diagonal or|   Kappa + SE     |      extra      |
#'         |      |            | frontier sol. |                  |                 |
#'  -------+------+------------+---------------+------------------+-----------------+
#'         | 2.0  |  M1* +0.5  |       ---     |     M1 + 0.5     |       M1 + 1    |
#'  -------+------+------------+---------------+------------------+-----------------+
#'         | 2.1  |  M1* +0.5  |       ---     |     M1 + 0.5     |       M1 + 1    |
#'  -------+------+------------+---------------+------------------+-----------------+
#'         | 2.2  |  M1* +0.5  |       ---     |        M1        |       M1 + 1    |
#'  -------+------+------------+---------------+------------------+-----------------+
#'         | 3.0  |   M1 +0.5  |       ---     |     M1 + 0.5     |        ---      |
#'  -------+------+------------+---------------+------------------+-----------------+
#'         | 3.1  |     M1     |    M1 + 0.5   |     M1 + 0.5     |        ---      |
#'  -------+------+------------+---------------+------------------+-----------------+
#'         | 3.2  |  M1* +0.5  |       ---     |         M1       |        ---      |
#'  -------+------+------------+---------------+------------------+-----------------+
#'         | 3.3  |  M1* +0.5  |       ---     |         M1       |        ---      |
#'  -------+------+------------+---------------+------------------+-----------------+
#'         | 3.4  |     M1     |    M1 + 0.5   |         M1       |        ---      |
#'  -------+------+------------+---------------+------------------+-----------------+
#'         | 3.5  |     M1     |       ---     |         M1       |        ---      |
#'  -------+------+------------+---------------+------------------+-----------------+
#'
#' @param M1 Matrix. Initial matrix without missing categories.
#' @param tp String. Type of problem. Generated by GetProblemType function.
#' @keywords M1 tp Mx
#' @export
#' @examples
#' GetMx("3.2",matrix(c(1,2,0,3,4,0,0,0,1),3,3))


GetMx <- function(tp, M1){
#Lista de matrices M1,..., M5 definidas segun el tipo de problema. 
#Variables globales delta M[[x]] (Mathematica) o delta.M[[x]] (R) 
#con x el numero de matriz (x=1,..., 5) 
  #Create M2
  if (tp == "2.0" | tp == "2.1" | tp == "2.2") {
    M1_new = rbind(cbind(M1,c(0,0)),c(0,0,1))
    M2 = M1_new + 0.5
  }
  else if (tp == "3.0" | tp == "3.2" | tp == "3.3") {
    M2 = M1 + 0.5
  }  
  else if (tp == "3.1" | tp == "3.4" | tp == "3.5") {
    M2 = M1
  }
  else {
    stop("Unexpected type of problem")
  }

  #Create M3
  if (tp == "2.0" | tp == "2.1" | tp == "2.2" | tp == "3.0" | tp == "3.2" 
      | tp == "3.3" | tp == "3.5") {
    M3 = NULL
  }
  else if (tp == "3.1" | tp == "3.4") {
    M3 = M1 + 0.5
  }
  else {
    stop("Unexpected type of problem")
  }

  #Create M4
  if (tp == "2.2" | tp == "3.2" | tp == "3.3" | tp == "3.4" | tp == "3.5") {
    M4 = M1
  }
  else if (tp == "2.0" | tp == "2.1" | tp == "3.0" | tp == "3.1") {
    M4 = M1 + 0.5
  }
  else {
    stop("Unexpected type of problem")
  }

  #Create M5
  if (tp == "2.0" | tp == "2.1" | tp == "2.2") {
    M5 = M1 + 1
  }
  else if (tp == "3.0" | tp == "3.1" | tp == "3.2" | tp == "3.3" | tp == "3.4" | tp == "3.5") {
    M5 = NULL
  }
  else {
    stop("Unexpected type of problem")
  }

  res= list("M1" = M1, "M2" = M2, "M3" = M3, "M4" = M4, "M5" = M5)
  return(res)
}