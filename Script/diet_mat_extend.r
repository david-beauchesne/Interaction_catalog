Diet_mat <- function(x){
  x <- as.data.frame(x)
  sp.names <- unique(c(rownames(x),colnames(x)))
  z <- matrix(nrow=length(sp.names),ncol=length(sp.names),dimnames=list(sp.names,sp.names))
  for(i in 1:nrow(z)){
    for(j in 1:nrow(z)){
      z[j,i] <- if(length(subset(rownames(x), rownames(x) == sp.names[j]))+length(subset(colnames(x), colnames(x) == sp.names[i])) == 2) {as.numeric(paste(x[sp.names[j],sp.names[i]]))} else {0}
    }
  }
  return(z)
}
