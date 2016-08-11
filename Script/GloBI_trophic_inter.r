GloBI_trophic_inter <- function(inter_GloBI) {

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

  trop.int <- subset(inter_GloBI, inter_GloBI$interaction_type == "eatenBy" | inter_GloBI$interaction_type == "eats" | inter_GloBI$interaction_type == "preyedUponBy" | inter_GloBI$interaction_type == "preysOn" & inter_GloBI$target_taxon_name != "no name")

  nb.GloBI.inter <- nrow(trop.int)
  binary_inter_GloBI <- trop.int[,c('interaction_type',
                                    'target_taxon_name',
                                    'source_taxon_name',
                                    'target_taxon_external_id',
                                    'source_taxon_external_id',
                                    'target_taxon_path',
                                    'source_taxon_path',
                                    'target_taxon_path_ranks',
                                    'source_taxon_path_ranks')
                                    ]

  pb <- txtProgressBar(min = 0,max = nb.GloBI.inter, style = 3)
  init.time <- Sys.time()
  for(i in 1:nb.GloBI.inter){ #changement des interactions pour avoir une matrice prédateur-proie

    if(binary_inter_GloBI$interaction_type[i] == "eatenBy" | binary_inter_GloBI$interaction_type[i] == "preyedUponBy") {
      src <- binary_inter_GloBI$source_taxon_name[i]
      tgt <- binary_inter_GloBI$target_taxon_name[i]
      src_id <- binary_inter_GloBI$source_taxon_external_id
      tgt_id <- binary_inter_GloBI$target_taxon_external_id
      src_path <- binary_inter_GloBI$source_taxon_path
      tgt_path <- binary_inter_GloBI$target_taxon_path
      src_rank <- binary_inter_GloBI$source_taxon_path_ranks
      tgt_rank <- binary_inter_GloBI$target_taxon_path_ranks

      binary_inter_GloBI$source_taxon_name[i] <- tgt
      binary_inter_GloBI$interaction_type[i] <- "eats"
      binary_inter_GloBI$target_taxon_name[i] <- src
      binary_inter_GloBI$source_taxon_external_id <- tgt_id
      binary_inter_GloBI$target_taxon_external_id <- src_id
      binary_inter_GloBI$source_taxon_path <- tgt_path
      binary_inter_GloBI$target_taxon_path <- src_path
      binary_inter_GloBI$source_taxon_path_ranks <- tgt_rank
      binary_inter_GloBI$target_taxon_path_ranks <- src_rank

    } #if

  setTxtProgressBar(pb, i)
  } # i
  print(Sys.time() - init.time)
  close(pb)

  binary_inter_GloBI[,1] <- rep(1, nrow(binary_inter_GloBI))
  binary_inter_GloBI <- unique(binary_inter_GloBI)

  return(binary_inter_GloBI)
}
