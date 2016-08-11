#Transforming interactions as binary values (0-1)
binary_interaction <- function(diet_matrix) {
  # matrix of diet interaction can be asymmetric
  # matrix of diet needs to have only interaction values
    # if matrix includes names of species, use that as row and column names, not as part of matrix

  for(i in 1:nrow(diet_matrix)){
    for(j in 1:ncol(diet_matrix)){
      matrix.dims <- dim(diet_matrix)
      if(is.matrix(diet_matrix) == FALSE) {diet_matrix <- as.matrix(diet_matrix)}
      if(identical(matrix.dims,dim(diet_matrix)) == FALSE) {stop("VERIFY TABLE STRUCTURE.  Matrix of diet interaction can be asymmetric. Matrix of diet needs to have only interaction values. If matrix includes names of species, use that as row and column names, not as part of matrix")}
      if(diet_matrix[i,j] == "0") {NULL} else {diet_matrix[i,j] <- "1"}
    }
  }
  diet_matrix <- apply(diet_matrix,2,as.numeric)

  return(diet_matrix)
}
