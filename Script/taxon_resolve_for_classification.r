taxo_resolve_for_classification <- function(tx.res,tx.list) { # tx.list = list of taxon tx.list
  # Taxon with and without results
  # Need to deal with taxon without results...
  tx.lg <- length(tx.res)
  taxon.no.result <- numeric()
  taxon.result <- numeric()
  for(i in 1:tx.lg){
    if(length(tx.res[[i]]) > 0) {taxon.result <- c(taxon.result,i)} else {taxon.no.result <- c(taxon.no.result,i)}
  }

  # Types of classification ranks
  class_ranks <- character()
  for(i in taxon.result){
    for(j in which(nchar(tx.res[[i]]$classification_path) > 0)) {
      class_ranks <- c(class_ranks,strsplit(tolower(tx.res[[i]]$classification_path_ranks[j]),"\\|"))
    }
  }
  class_ranks <- unique(unlist(class_ranks))

  # Integrating classifications in list of matrices
  class <- vector("list",tx.lg)
  for(i in taxon.result) {

    classifications <- which(tx.res[[i]]$canonical_form == tx.res[[i]]$submitted_name & nchar(tx.res[[i]]$classification_path) > 0)

    if(length(classifications) > 0) {
      db.id <- tx.res[[i]]$data_source_title[classifications]
      class_path <- strsplit(tolower(tx.res[[i]]$classification_path[classifications]),"\\|")
      class_rank <- strsplit(tolower(tx.res[[i]]$classification_path_ranks[classifications]),"\\|")

      class[[i]] <- matrix(nrow = length(classifications), ncol = length(class_ranks), dimnames = list(db.id,class_ranks))

      for(j in 1:length(classifications)){
        for(k in which(nchar(class_rank[[j]]) > 0)) {
          class[[i]][j, class_rank[[j]][k]] <- class_path[[j]][k]
        }
      }

    class[[i]] <- class[[i]][which(class[[i]][, which(colnames(class[[i]]) == tx.list[i,2])] == tolower(tx.res[[i]]$submitted_name[1])), ]

    } #if

  } # i

  return(class)
} # function
