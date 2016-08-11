# Use of classification function for classification of missing taxonomies after taxo_resolve()

class_taxo <- function(taxon.list, database, taxon.rank) {
  # taxon.list: vector of taxon names
  # database: character name of database to use
  # taxon.rank: ranks to extract from classification
  library(taxize)

  nb.tx <- length(taxon.list)
  classification.taxon <- matrix(nrow = nb.tx, ncol = length(taxon.rank) + 1, data = NA)
  colnames(classification.taxon) <- c(taxon.rank, "source")
  rownames(classification.taxon) <- taxon.list
  class <- vector("list", nb.tx)
  pb <- txtProgressBar(min = 0,max = nb.tx, style = 3)

  for(i in 1:nb.tx) {
    class[[i]] <- classification(taxon.list[i], db = database, verbose = FALSE)
    setTxtProgressBar(pb, i)
  } # i
  close(pb)

  for(i in 1:nb.tx) {
    if(paste(class[[i]][[1]][1]) != "NA") { # problème ici
      for(j in 1:length(taxon.rank)) {
        rank <- which(tolower(class[[i]][[1]][, "rank"]) == taxon.rank[j])
        names <- as.character(class[[i]][[1]][, "name"])
        if(length(rank) == 1) {
          classification.taxon[i, taxon.rank[j]] <- names[rank]
          classification.taxon[i, ncol(classification.taxon)] <- database
        } # if
      } # j
    } # if
  } # i

    return(classification.taxon)
} # function
