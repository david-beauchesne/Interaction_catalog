GloBI_taxon_classification <- function(inter_GloBI) {

  # Extracting only trophic interactions from get_interactions()
  # Subset of function function to include only consumer interactions
   # eatenBy, eats, preyedUponBy, preysOn
   #Interaction types: eatenBy, eats, preyedUponBy, preysOn (GloBI definitions from: http://www.ontobee.org)
     #preysOn: An interaction relationship involving a predation process, where the subject consumes all or part of the target
     #preyedUponBy: is target of predation interaction with; has predator
     #eats: is subject of eating interaction with
     #eatenBy: eaten by; is target of eating interaction with
   #interactions de l'espèce [i] en tant que consommateur -> source[i] eats target | source[i] preysOn target | source eatenBy target[i] | source preyedUponBy target[i]
   #interactions en tant que ressource -> source eats target[i] | source preysOn target[i] | source[i] eatenBy target | source[i] preyedUponBy target
  source <- inter_GloBI[ ,c('source_taxon_name','source_taxon_path','source_taxon_path_ranks')]
  target <- inter_GloBI[ ,c('target_taxon_name','target_taxon_path','target_taxon_path_ranks')]
  colnames(source) <- c('taxon_name', 'taxon_path', 'taxon_path_ranks')
  colnames(target) <- c('taxon_name', 'taxon_path', 'taxon_path_ranks')
  taxon <- rbind(source, target)

  tx.lg <- nrow(taxon) # number of taxon in table

  # Types of classification ranks
  class_ranks <- character()
  pb <- txtProgressBar(min = 0,max = tx.lg, style = 3)
  init.time <- Sys.time()
  for(i in 1:tx.lg) {
    class_ranks <- c(class_ranks,strsplit(tolower(taxon$taxon_path_ranks[i]),"\\|")[[1]])
    class_ranks <- unique(unlist(str_trim(class_ranks, side = 'both')))
    setTxtProgressBar(pb, i)
  }
  print(Sys.time() - init.time)
  close(pb)

  # Integrating classifications in matrix
  class <- matrix(nrow = tx.lg, ncol = length(class_ranks), data = NA)
  colnames(class) <- class_ranks
  pb <- txtProgressBar(min = 0,max = tx.lg, style = 3)

  for(i in 1:tx.lg) {
    if(nchar(as.character(taxon$taxon_path_ranks[i])) > 0 & !is.na(taxon$taxon_path_ranks[i])) {
      class_path <- str_trim(strsplit(tolower(taxon$taxon_path[i]),"\\|")[[1]], side = "both")
      class_rank <- str_trim(strsplit(tolower(taxon$taxon_path_ranks[i]),"\\|")[[1]], side = "both")

      for(k in 1:length(class_rank)) {
        if(nchar(as.character(class_rank[k])) > 0) {
          class[i, class_rank[k]] <- class_path[k]
        } #if
      } #k
    } #if
    setTxtProgressBar(pb, i)
  } # i
  close(pb)

  # Uniform rank names
  subs <- c('cl.', 'fam.','gen.','ord.','phyl.','sp.','sub phylum','var.','regn.')
  make_subs <- c('class','family','genus','order','phylum','species','subphylum','variety','kingdom')

  for(i in 1:length(subs)) {
    mods <- which(!is.na(class[, subs[i]]))
    if(length(mods) > 0) {
      class[mods, make_subs[i]] <- class[mods, subs[i]]
    }
  }

  return(class)
} # function
