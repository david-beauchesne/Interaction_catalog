source("Script/remove_objects_keep_functions.R")
remove_objects_keep_functions()
# -----------------------------------------------------------------------------
# PROJECT:
#    Evaluating the structure of the communities of the estuary
#    and gulf of St.Lawrence
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# STEP:
#    3. List of binary interations and taxon to include in the analysis
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# FILES:
#   RData1 <- file = "RData/interactions.RData"
#   RData2 <- file = "RData/taxon_list.RData"
#   Script <- file = "Script/3-Bin_inter_tx_tot.r"
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

#Loading all datasets with interactions
load("RData/barnes2008.RData")
load("RData/Kortsch2015.RData")
load("RData/GlobalWeb.RData")
load("RData/brose2005.RData")
# load("RData/EwE.RData") # Not yet finished
# load("RData/GloBI.RData") # Not yet finished

# List of webs
InterDataTot <- c(Barnes2008, Brose2005, GlobalWeb, Kortsch2015) # EwE to add

# Complete taxon list and interaction list from all webs
tx.list <- matrix(nrow=0, ncol=2, data=NA, dimnames = list(c(), c("taxon","rank")))
inter.list <- matrix(nrow=0, ncol=3, data=NA, dimnames = list(c(), c("Predator","FeedInter","Prey")))

for(i in 1:length(InterDataTot)) {
  tx.list <- rbind(tx.list, as.matrix(InterDataTot[[i]][[5]]))
  inter.list <- rbind(inter.list, as.matrix(InterDataTot[[i]][[4]]))
}

tx.list <- unique(tx.list)
inter.list <- unique(inter.list)

# Taxonomic resolutions and those selected to move further in the analysis
# Decision is to use all taxonomic resolutions greater or equal to families
taxo.resol <- unique(inter.list[, 2])
taxo.resolution.accepted <- c("species", "genus", "family", "tribe", "subfamily", "superfamily")

# Extracting interactions for analysis
time_init <- Sys.time()
inter.tot <- inter_taxo_resolution(tx.list, inter.list, taxo.resolution.accepted)
Sys.time() - time_init

# Changing to binary interactions
for(i in 1:nrow(inter.tot)) {
  if(inter.tot[i, 2] != "0" & inter.tot[i, 2] != "1") {inter.tot[i, 2] <- "1"}
}

# Extracting taxon list for analysis
row.tx.accepted <- numeric()
for(i in 1:length(taxo.resolution.accepted)) {
  row.tx.accepted <- c(row.tx.accepted,which(tx.list[,2] == taxo.resolution.accepted[i]))
}

tx.list.tot <- tx.list[row.tx.accepted, ]

save(x = tx.list.tot, file = "RData/taxon_list.RData")
save(x = inter.tot, file = "RData/interactions.RData")
