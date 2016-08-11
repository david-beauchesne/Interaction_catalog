# Compiling available data for EGSL species
load("RData/Biotic_inter.RData")

EGSL_inter <- matrix(nrow = nrow(Biotic_inter[[4]]), ncol = 6, dimnames = list(Biotic_inter[[4]][, 'taxon'], c('species','genus','family','order','class','phylum')))
pb <- txtProgressBar(min = 0,max = nrow(Biotic_inter[[4]]), style = 3)
for(i in 1:nrow(Biotic_inter[[4]])) {
  EGSL_inter[i, 'species'] <- length(which(Biotic_inter[[3]][, 'cons_species'] == Biotic_inter[[4]][i, 'species'] | Biotic_inter[[3]][, 'res_species'] == Biotic_inter[[4]][i, 'species']))
  EGSL_inter[i, 'genus'] <- length(which(Biotic_inter[[3]][, 'cons_genus'] == Biotic_inter[[4]][i, 'genus'] | Biotic_inter[[3]][, 'res_genus'] == Biotic_inter[[4]][i, 'genus']))
  EGSL_inter[i, 'family'] <- length(which(Biotic_inter[[3]][, 'cons_family'] == Biotic_inter[[4]][i, 'family'] | Biotic_inter[[3]][, 'res_family'] == Biotic_inter[[4]][i, 'family']))
  EGSL_inter[i, 'order'] <- length(which(Biotic_inter[[3]][, 'cons_order'] == Biotic_inter[[4]][i, 'order'] | Biotic_inter[[3]][, 'res_order'] == Biotic_inter[[4]][i, 'order']))
  EGSL_inter[i, 'class'] <- length(which(Biotic_inter[[3]][, 'cons_class'] == Biotic_inter[[4]][i, 'class'] | Biotic_inter[[3]][, 'res_class'] == Biotic_inter[[4]][i, 'class']))
  EGSL_inter[i, 'phylum'] <- length(which(Biotic_inter[[3]][, 'cons_phylum'] == Biotic_inter[[4]][i, 'phylum'] | Biotic_inter[[3]][, 'res_phylum'] == Biotic_inter[[4]][i, 'phylum']))
  setTxtProgressBar(pb, i)
} #i
close(pb)


EGSL_species_inter <- numeric(nrow(Biotic_inter[[4]]))
EGSL_genus <- unique(Biotic_inter[[4]][, 'genus'])
EGSL_family <- unique(Biotic_inter[[4]][, 'family'])
EGSL_genus_inter <- numeric(length(EGSL_genus))
EGSL_family_inter <- numeric(length(EGSL_family))

# Number of interactions for EGSL species
for(i in 1:nrow(Biotic_inter[[4]])) {
  EGSL_species_inter[i] <- length(which(Biotic_inter[[3]][, 'cons_species'] == Biotic_inter[[4]][i, 'species'] | Biotic_inter[[3]][, 'res_species'] == Biotic_inter[[4]][i, 'species']))
}

# Number of interactions for EGSL genus
for(i in 1:length(EGSL_genus)) {
  EGSL_genus_inter[i] <- length(which(Biotic_inter[[3]][, 'cons_genus'] == EGSL_genus[i] | Biotic_inter[[3]][, 'res_genus'] == EGSL_genus[i]))
}

# Number of interactions for EGSL families
for(i in 1:length(EGSL_family)) {
  EGSL_family_inter[i] <- length(which(Biotic_inter[[3]][, 'cons_family'] == EGSL_family[i] | Biotic_inter[[3]][, 'res_family'] == EGSL_family[i]))
}

Species_inter <- (sum(EGSL_species_inter > 0) / length(EGSL_species_inter)) * 100
Genus_inter <- (sum(EGSL_genus_inter > 0) / length(EGSL_genus_inter)) * 100
Family_inter <-(sum(EGSL_family_inter > 0) / length(EGSL_family_inter)) * 100





# Interactions by rank
Taxa_inter <- unique(c(Biotic_inter[[3]][, 'consumer'],Biotic_inter[[3]][, 'resource']))
Taxa_inter_rank <- character(length(Taxa_inter))

for(i in 1:length(Taxa_inter)){
  Taxa_inter_rank[i] <- Biotic_inter[[2]][Taxa_inter[i], 'rank']
}
