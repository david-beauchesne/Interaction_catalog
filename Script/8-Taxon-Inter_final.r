source("Script/remove_objects_keep_functions.R")
remove_objects_keep_functions()
# -----------------------------------------------------------------------------
# PROJECT:
#    Evaluating the structure of the communities of the estuary
#    and gulf of St.Lawrence
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# STEP:
#    8. Final list of taxon and interations from EmpWeb & GloBI
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# FILES:
#    Script <- file = "Script/8-Taxon-Inter_final.r"
#    RData1 <- file = "RData/Taxon.RData"
#    RData2 <- file = "RData/Biotic_inter.RData"
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# NOTES:
#   Combining EmpWeb & GloBI data for a comprehensive list of taxon and binary
#     interactions.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# LIBRARIES:
# -----------------------------------------------------------------------------
library(stringr)

# -----------------------------------------------------------------------------
# SCRIPT
# -----------------------------------------------------------------------------
load("RData/class_tx_tot.RData")
load("RData/GloBI_classification.RData")
load("RData/interactions.RData")
load("RData/GloBI_interactions.RData")
load("RData/sp_egsl.RData")

# ---------------------------------
# Unique binary interactions
# ---------------------------------

# Select binary interactions with species that have a fully resolved taxonomy
  Biotic_inter <- vector('list',4)
  names(Biotic_inter) <- c('Binary_interaction','Taxon_list','Inter_taxonomy','EGSL')

  # For Empirical Webs
    consumer <- numeric()
    resource <- numeric()
    pb <- txtProgressBar(min = 0,max = nrow(inter.tot), style = 3)
    for(i in 1:nrow(inter.tot)) {
      if(!is.na(class.tx.tot[inter.tot[i, 'Predator'], 1])) {
        consumer <- c(consumer, 1)
      } else {
        consumer <- c(consumer, 0)
      } #if

      if(!is.na(class.tx.tot[inter.tot[i, 'Prey'], 1])) {
        resource <- c(resource, 1)
      } else {
        resource <- c(resource, 0)
      } #if
      setTxtProgressBar(pb, i)
    } #i
    close(pb)

    # If all = 1, no need to adjust
    unique(consumer)
    unique(resource)

  # For GloBI interactions
    consumer <- numeric()
    resource <- numeric()
    pb <- txtProgressBar(min = 0,max = nrow(GloBI_interactions), style = 3)
    for(i in 1:nrow(GloBI_interactions)) {
      if(!is.na(GloBI_classification[GloBI_interactions[i, 'Predator'], 1])) {
        consumer <- c(consumer, 1)
      } else {
        consumer <- c(consumer, 0)
      } #if

      if(!is.na(GloBI_classification[GloBI_interactions[i, 'Prey'], 1])) {
        resource <- c(resource, 1)
      } else {
        resource <- c(resource, 0)
      } #if
      setTxtProgressBar(pb, i)
    } #i
    close(pb)

    # If all = 1, no need to adjust
    unique(consumer)
    unique(resource)

    cons_res <- cbind(resource,consumer)
    cons_res <- rowSums(cons_res)

    GloBI_interactions <- GloBI_interactions[-which(cons_res != 2), ]

    # Combining biotic interactions in complete dataset
    Biotic_inter[[1]] <- rbind(inter.tot[, 1:3], GloBI_interactions[, 1:3])
    colnames(Biotic_inter[[1]]) <- c('consumer','inter','resource')
    Biotic_inter[[1]] <- unique(Biotic_inter[[1]])

# ---------------------------------
# Unique taxon list
# ---------------------------------

  Biotic_inter[[2]] <- rbind(class.tx.tot, GloBI_classification)
  Biotic_inter[[2]] <- unique(Biotic_inter[[2]])

  # Adjust taxon names with taxonomy
  for(i in 1:nrow(Biotic_inter[[2]])) {
    Biotic_inter[[2]][i, 'taxon'] <- paste(Biotic_inter[[2]][i,paste(Biotic_inter[[2]][i, 'rank'])])
  }#i

  # First letter only as capital
  Names_change <- function(x){
    # Name resolve #1
    x <- str_trim(x, side="both") #remove spaces
    x <- tolower(x)
    x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
    return(x)
  }

  Biotic_inter[[2]][, -13] <- apply(Biotic_inter[[2]][, -13], 2, Names_change)
  Biotic_inter[[2]] <- apply(Biotic_inter[[2]], 2, gsub, pattern = 'NANA', replacement = 'NA')

  # Also adjust in interactions list
  for(i in 1:nrow(Biotic_inter[[1]])) {
    Biotic_inter[[1]][i, 'consumer'] <- Biotic_inter[[2]][paste(Biotic_inter[[1]][i, 'consumer']), 'taxon']
    Biotic_inter[[1]][i, 'resource'] <- Biotic_inter[[2]][paste(Biotic_inter[[1]][i, 'resource']), 'taxon']
  }

# Also adjust egsl species list
 Biotic_inter[[4]] <- matrix(nrow = nrow(sp.egsl), ncol = ncol(Biotic_inter[[2]]), data = NA, dimnames = list(c(), colnames(Biotic_inter[[2]])))
 rownames(Biotic_inter[[4]]) <- sp.egsl[,1]
 for(i in 1:nrow(sp.egsl)) {
    Biotic_inter[[4]][i, ] <- Biotic_inter[[2]][sp.egsl[i,1], ]
 }

  # Unique values and adjusting rownames
  Biotic_inter[[1]] <- unique(Biotic_inter[[1]])
  Biotic_inter[[2]] <- unique(Biotic_inter[[2]])
  rownames(Biotic_inter[[2]]) <- Biotic_inter[[2]][, 'taxon']
  rownames(Biotic_inter[[4]]) <- Biotic_inter[[4]][, 'taxon']
  Biotic_inter[[4]] <- Biotic_inter[[4]][,-c(7,9,10,13)]

# ---------------------------------
# Biotic interations with taxonomy
# ---------------------------------

Biotic_inter[[3]] <- Biotic_inter[[1]]

# Extracting taxonomy for each binary interaction
rank <- c("kingdom","phylum","class","order","family","genus","species")
consumer <- matrix(nrow = nrow(Biotic_inter[[3]]), ncol = length(rank), dimnames = list(c(), rank))
resource <- matrix(nrow = nrow(Biotic_inter[[3]]), ncol = length(rank), dimnames = list(c(), rank))
pb <- txtProgressBar(min = 0,max = nrow(GloBI_interactions), style = 3)

for(i in 1:nrow(Biotic_inter[[3]])) {
  consumer[i, ] <- Biotic_inter[[2]][Biotic_inter[[3]][i, 'consumer'], rank]
  resource[i, ] <- Biotic_inter[[2]][Biotic_inter[[3]][i, 'resource'], rank]
  setTxtProgressBar(pb, i)
} #i
close(pb)

# Combining taxonomy into single element
# Ranks are  "kingdom | phylum | class | order | family | genus | species"
consumer_taxo <- apply(consumer, 1, paste, collapse = ' | ')
resource_taxo <- apply(resource, 1, paste, collapse = ' | ')

# Changing column names
colnames(consumer) <- paste('cons_', rank, sep = '')
colnames(resource) <- paste('res_', rank, sep = '')

 Biotic_inter[[3]] <- cbind(Biotic_inter[[3]], consumer, resource, consumer_taxo, resource_taxo)

save(x = Biotic_inter, file = "RData/Interaction_catalog.RData")
