# #Duplicating rows and columns
# #Identify food webs with rows/columns to duplicate - needs to be done if dealing with mutliple food webs at once
#   dup.fw <- NA
#   for(k in 1:length(GlobalWeb)){
#     x <- which(str_detect(rownames(x)," - ") == TRUE) #Identify rows that should be Duplicated
#     if(length(x) == 0) {NULL} else {dup.fw <- c(dup.fw,k)}
#   }
#   dup.fw <- dup.fw[-1]




#Duplication procedure
  #Duplicated rows
    #Matrix with duplicated rows
dupl_row <- function(x,y) { # x = matrix to duplicate taxon; y = taxon list from matrix x
      nb.name <- ncol(x)
      col.tx <- colnames(x)
      rownames(x) <- y
      mat.row <- matrix(ncol=nb.name,nrow=0) #Matrix to insert duplicated row values.
      colnames(mat.row) <- colnames(x) #Matrix to insert duplicated row values

      z <- which(str_detect(y," - ") == TRUE) #Identify rows that should be duplicated
      for(i in 1:length(z)){
        dup.row <- as.matrix(x[z[i],]) #Extract the values of each row to duplicate. as.matrix() important, otherwise when data.frame() imports columns as lists
        dup.name <- str_split(y[z[i]]," - ")[[1]] #Duplicated names to split
        nb.tx <-length(dup.name) #Number of taxa to split
        dup.row <- as.matrix(dup.row)
        dup.tx <- matrix(ncol=nb.name,nrow=nb.tx,data=rep(dup.row,nb.tx),byrow=TRUE,dimnames=list(dup.name,col.tx)) #duplicated values in a matrix
        mat.row <- rbind(mat.row,dup.tx) #rbind new rows
      }
    x <- rbind(x[-z,],mat.row) #Insert new rows to food web and remove duplicated strings
    return(x)
}


  #Duplicated columns
    #Matrix with duplicated rows
  dupl_col <- function(x,y) { # x = matrix to duplicate taxon; y = taxon list from matrix x
      nb.name <- nrow(x)
      col.tx <- rownames(x)
      colnames(x) <- y
      mat.col <- matrix(ncol=0,nrow=nb.name) #Matrix to insert duplicated column values
      rownames(mat.col) <- rownames(x) #Matrix to insert duplicated column values

      z <- which(str_detect(y," - ") == TRUE) #Identify columns that should be duplicated
      for(i in 1:length(z)){
        dup.col <- x[,z[i]] #Extract the values of each column to duplicate
        dup.name <- str_split(y[z[i]]," - ")[[1]] #Duplicated names to split
        nb.tx <-length(dup.name) #Number of taxa to split
        dup.tx <- matrix(ncol=nb.tx,nrow=nb.name,data=rep(dup.col,nb.tx),byrow=FALSE,dimnames=list(col.tx,dup.name)) #duplicated values in a matrix
        mat.col <- cbind(mat.col,dup.tx) #rbind new rows
      }
    x <- cbind(x[,-z],mat.col) #Insert new rows to food web and remove duplicated strings
    return(x)
}
