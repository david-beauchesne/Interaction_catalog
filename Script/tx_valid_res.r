# No results
no.res <- function(x) {
  y <- as.numeric(rownames(subset(x,x[,2] == "no result" | is.na(x[,3]))))
  y.ID <- x[y,]
  return(y.ID)
}

#Multiple taxon returned

multi_tx <- function(x){
  y <- which(str_detect(x," - ") == TRUE)
  return(x[y,])
}

# Invalid
invalid <- function(x,y) { # x = tx.valid, y = tx.res
  z <- as.numeric(rownames(subset(x,x[,2] == "invalid")))
  z.ID <- matrix(nrow=length(z),ncol=2)
  colnames(z.ID) <- c("submitted_name","canonical_form")
  rownames(z.ID) <- seq(1,length(z))
  submit <- NA
  canon <- NA
  for(i in z){submit <- c(submit,y[[i]][1,1])}
  for(i in z){canon <- c(canon,y[[i]][1,5])}
  z.ID[,1] <- submit[-1]
  z.ID[,2] <- canon[-1]
  return(z.ID)
}
