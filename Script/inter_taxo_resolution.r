# Evaluating the dual taxonomic resolution of observed interactions
inter_taxo_resolution <- function(tx.list, inter.list, taxo.resolution.accepted){
  # tx.list <- taxon list with taxonomic resolution, taxon should be rownames
  # inter.list <- list of binary interactions with three columns: 1 == predator, 2 == interaction type, 3 == prey
  # taxo.resolution.accepted <- list of taxonomic resolutions accepted for further analyses
  nb.inter <- nrow(inter.list)
  inter.resolution <- character(nb.inter)
  taxo.resol <- unique(tx.list[, 2])

  matrix.dims <- dim(inter.list)
  if(is.matrix(inter.list) == FALSE) {inter.list <- as.matrix(inter.list)}
  if(identical(matrix.dims,dim(inter.list)) == FALSE) {stop("Verify table structure")}

  # Evaluating taxonomic resolutions of predators and preys forming observed binary interactions
  for(i in 1:nb.inter) {
    pred.tx.resolution <- tx.list[inter.list[i, 1], 2]
    prey.tx.resolution <- tx.list[inter.list[i, 3], 2]
    inter.resolution[i] <- paste(pred.tx.resolution, prey.tx.resolution, sep = " - ")
  }

  inter.list <- cbind(inter.list, inter.resolution)

  taxo.combination <- as.vector(outer(taxo.resolution.accepted, taxo.resolution.accepted, paste, sep = " - "))  # Computes all combinations that could be of interest

  # Evaluating which interactions in the list meets our criteria for minimal dual taxonomic resolution
  row.inter.accepted <- numeric()
  for(i in 1:length(taxo.combination)) {
    row.inter.accepted <- c(row.inter.accepted,which(inter.resolution == taxo.combination[i]))
  }

  inter.list.tot <- inter.list[row.inter.accepted, ]

  return(inter.list.tot)
}
