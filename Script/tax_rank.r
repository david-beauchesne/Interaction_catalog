# determining rank of taxon
  taxonomic_rank <- function(x) { # x is the taxon list to evaluate
    
    library(taxize)
    tx.lg <- length(x)

    pb <- txtProgressBar(min = 0,max = tx.lg, style = 3)
    tx.rank <- NA
    for(i in 1:tx.lg){
      rank <- try(tax_rank(query = x[i], db = "both", pref = "itis", verbose = FALSE)[[1]])
      rank <- if(class(rank) == "character") {rank} else {NA}
      tx.rank <- c(tx.rank,rank)
      setTxtProgressBar(pb, i)
    }
    close(pb)
    tx.rank <- tx.rank[-1]

    return(tx.rank)

  }
