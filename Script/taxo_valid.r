taxo_valid <- function(x,y,res.check) { # x = tx.res results, y = taxon list, have to be same length
  #Taxon with and without results
  tx.lg <- length(x)
  tx.no <- NA
  tx.yes <- NA
  for(i in 1:tx.lg){
    if(length(x[[i]]) > 0) {tx.yes <- c(tx.yes,i)} else {tx.no <- c(tx.no,i)}
  }
  tx.no <- tx.no[-1] # - no gnr_resolve() results
  tx.yes <- tx.yes[-1] #Requests with gnr_resolve() results

  #Identify identical results
  tx.yes.ident.yes <- NA
  tx.yes.ident.no <- NA
  for(i in tx.yes){
    if(identical(x[[i]][1,5],x[[i]][1,1]) == TRUE) {tx.yes.ident.yes <- c(tx.yes.ident.yes,i)} else {tx.yes.ident.no <- c(tx.yes.ident.no,i)}
  }
  tx.yes.ident.yes <-  tx.yes.ident.yes[-1] # - identical results
  tx.yes.ident.no <- tx.yes.ident.no[-1] # - not identical results

  #Verify: tx.no, tx.yes.invalid, tx.yes.valid.no.name
  tx.valid <- matrix(nrow=length(y),ncol=3)
  colnames(tx.valid) <- c("tx.list","valid","rank")
  tx.valid[,1] <- y
  tx.valid[tx.no,2] <- "no result"
  tx.valid[tx.yes.ident.yes,2] <- "valid"
  tx.valid[tx.yes.ident.no,2] <- "invalid"
  rownames(tx.valid) <- seq(1,tx.lg)

  tail2 <- function(z){tail(z,n=1)}
  for(i in tx.yes){
    rank <- tolower(unlist(lapply(strsplit(x[[i]]$classification_path_ranks,"\\|"),tail2)))
    if(length(which(rank == "")) == 0) {rank <- rank} else {rank <- rank[-which(rank == "")]}
    rank <- paste(unique(rank),collapse=" - ")
    tx.valid[i,3] <- rank
  }
  no.char <- which(nchar(tx.valid[,3]) == 0)
  if(length(no.char) == 0) {NULL} else {tx.valid[no.char,3] <- NA}
  tx.valid[tx.no,3] <- NA

  #Results check up
  if(res.check == TRUE) {
    # No results
      no.res <- as.numeric(rownames(subset(tx.valid,tx.valid[,2] == "no result" | is.na(tx.valid[,3]))))
      no.res <- tx.valid[no.res,]
      if(is.vector(no.res) == FALSE) {
        no.res <- no.res[order(no.res[,1]),]
      }

    # Multiple taxon returned
      multi <- which(str_detect(tx.valid[,3]," - ") == TRUE)
      multi <- tx.valid[multi,]
      multi <- multi[order(multi[,1]),]

    # Invalid
      z <- as.numeric(rownames(subset(tx.valid,tx.valid[,2] == "invalid")))
      z.ID <- matrix(nrow=length(z),ncol=2)
      colnames(z.ID) <- c("submitted_name","canonical_form")
      submit <- NA
      canon <- NA
      for(i in z){submit <- c(submit,x[[i]][1,1])}
      for(i in z){canon <- c(canon,x[[i]][1,5])}
      z.ID[,1] <- submit[-1]
      z.ID[,2] <- canon[-1]
      z.ID <- z.ID[order(z.ID[,1]),]

      # # Unshade for a list of these results
      # print("!!!!!!!!! NO RESULTS OBTAINED FROM gnr_resolve() !!!!!!!!!")
      # print(no.res)
      # print("!!!!!!!!! INVALID RESULTS FROM gnr_resolve() !!!!!!!!!")
      # print(z.ID)
      # print("!!!!!!!!! MULTIPLE TAXON FROM gnr_resolve() !!!!!!!!!")
      # print(multi)

    } else {NULL}

    return(tx.valid)
  }
