source("Script/remove_objects_keep_functions.R")
remove_objects_keep_functions()
# -----------------------------------------------------------------------------
# PROJECT:
#    Evaluating the structure of the communities of the estuary
#    and gulf of St.Lawrence
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# STEP:
#    6. List of binary interactions and taxon to include in the analysis
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# FILES:
#   RData1 <- file = "RData/GloBI_interactions.RData"
#   RData2 <- file = "RData/GloBI_taxon.RData"
#   Script <- file = "Script/6-Bin_inter_GloBI.r"
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# NOTES:
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# LIBRARIES:
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# SCRIPT
# -----------------------------------------------------------------------------
load("RData/GloBI.RData")

# Complete taxon list and interaction list from all webs
tx.list <- unique(GloBI[[1]][[5]])
rownames(tx.list) <- GloBI[[1]][[5]][, 1]
inter.list <- unique(GloBI[[1]][[4]])

# Taxonomic resolutions and those selected to move further in the analysis
# Decision is to use all taxonomic resolutions greater or equal to families
taxo.resol <- unique(inter.list[, 2])
taxo.resolution.accepted <- c("species", "genus", "family", "tribe", "subfamily", "superfamily")

# Extracting interactions for analysis
time_init <- Sys.time()
GloBI_interactions <- inter_taxo_resolution(tx.list, inter.list, taxo.resolution.accepted)
Sys.time() - time_init

# Extracting taxon list for analysis
row.tx.accepted <- numeric()
for(i in 1:length(taxo.resolution.accepted)) {
  row.tx.accepted <- c(row.tx.accepted,which(tx.list[,2] == taxo.resolution.accepted[i]))
}

GloBI_taxon <- tx.list[row.tx.accepted, ]


save(x = GloBI_taxon, file = "RData/GloBI_taxon.RData")
save(x = GloBI_interactions, file = "RData/GloBI_interactions.RData")
