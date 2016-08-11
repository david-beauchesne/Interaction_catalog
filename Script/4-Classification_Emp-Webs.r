source("Script/remove_objects_keep_functions.R")
remove_objects_keep_functions()
# -----------------------------------------------------------------------------
# PROJECT:
#    Evaluating the structure of the communities of the estuary
#    and gulf of St.Lawrence
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# STEP:
#    4. Classification of taxon list from empirical webs
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# FILES:
#   RData <- file = "RData/class_tx_tot.RData"
#   Script <- file = "Script/4-Classification_Emp-Webs.r"
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# NOTES:
#   Dealing with classifications can be a bit confusing because of species
#   with multiple names. I will simply use the names I have in my list and
#   go from there. For the classification, I got in order with:
#     1. ITIS
#     2. EoL
#     3. GBIF (contains most species, if not all)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# LIBRARIES:
library(taxize)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# SCRIPT
# -----------------------------------------------------------------------------

load("RData/taxon_list.RData")
load("RData/sp_egsl.RData")

sp.egsl <- cbind(sp.egsl, rep("species",nrow(sp.egsl)))
colnames(sp.egsl) <- colnames(tx.list.tot)
rownames(sp.egsl) <- sp.egsl$taxon

tx.tot <- rbind(sp.egsl, tx.list.tot)
tx.tot <- unique(tx.tot)
nb.tx <- nrow(tx.tot)

# Extracting taxon itis, eol & gbif identifiers
# tx.id <- extract_id(tx.tot[,1]) # might not be necessary if I have everything I need through taxon_resolve

# Running taxo resolve again. This could be done in previous steps since I already run this, but I'd rather do it all at once here even if it involves more analysis time
time.init <- Sys.time()
tx.res <- taxo_resolve(tx.tot[,1])
Sys.time() - time.init

# Classification of taxon
classification <- taxo_resolve_for_classification(tx.res,tx.tot)

# Taxon with classifications taxon_resolve
empty.class <- numeric() # no result
class <- numeric() # result as matrix (multiple results)
vector.class <- numeric() # result as vector (single result)
for(i in 1:nb.tx) {
  if(is.matrix(classification[[i]]) == FALSE & is.null(classification[[i]]) == TRUE) {
  empty.class <- c(empty.class,i)
  } else if(is.matrix(classification[[i]]) == FALSE & is.null(classification[[i]]) == FALSE) {
    vector.class <- c(vector.class,i)
  } else {
    class <- c(class, i)
  }
}

# Table with classifications per taxon
class.tx.tot <- matrix(nrow = nb.tx, ncol = 11, data = NA)
tx.rank <- c("species", "genus", "tribe", "subfamily", "family", "superfamily", "order", "class", "phylum", "kingdom")
tx.rank <- tx.rank[seq(10,1)]
colnames(class.tx.tot) <- c(tx.rank,"source")

  # Taxon with multiple results - order of importance: WoRMS, ITIS, Catalogue of Life, NCBI
  for(i in class) {
    tx.check <- unlist(lapply(classification[[i]][,paste(tx.tot[i,2])], identical, y = paste(tolower(tx.tot[i,1]))))

    worms <- which(names(tx.check) == "WoRMS" & tx.check == TRUE)
    itis <- which(names(tx.check) == "ITIS" & tx.check == TRUE)
    col <- which(names(tx.check) == "Catalogue of Life" & tx.check == TRUE)
    ncbi <- which(names(tx.check) == "NCBI" & tx.check == TRUE)

    if(length(worms) > 0) {
      class.tx.tot[i, ] <- c(classification[[i]][worms[1], tx.rank], "worms")
    } else if(length(itis) > 0) {
      class.tx.tot[i, ] <- c(classification[[i]][itis[1], tx.rank], "itis")
    } else if(length(col) > 0) {
      class.tx.tot[i, ] <- c(classification[[i]][col[1], tx.rank], "col")
    } else if(length(ncbi) > 0) {
      class.tx.tot[i, ] <- c(classification[[i]][ncbi[1], tx.rank], "nbci")
    }
  }

  #Taxon with single result - missing db in my function. Eventually correct if deemed necessary
  for(i in vector.class) {
    class.tx.tot[i, ] <- c(classification[[i]][tx.rank], "unknown")
  }


  # No results from taxo_resolve() -> empty.class: no result; empty.taxo: empty results
  empty.taxo <- which(is.na(class.tx.tot[,11]) == TRUE)
  add.class.to.measure <- empty.taxo
  # add.class.to.measure <- sort(c(empty.taxo, empty.class))

  # Classification - will return prompt messages when mutiple taxon found
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # This needs to run independently from the rest to manage prompt messages
  classification.taxo.add <- class_taxo(taxon.list = tx.tot[add.class.to.measure, 1],
                                        database = "gbif",
                                        taxon.rank = tx.rank)
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  class.tx.tot[add.class.to.measure, ] <- classification.taxo.add

  empty.taxo2 <- which(is.na(class.tx.tot[,11]) == TRUE)

  # # Manually
  tx.empty <- tx.tot[empty.taxo2, ]
  manual.class <- matrix(nrow = length(empty.taxo2), ncol = 11, data = NA)
  colnames(manual.class) <- c(tx.rank,"source")
  rownames(manual.class) <- rownames(tx.empty)

  manual.class["Euchaetomera antarcticus", ] <- c("Animalia","Arthropoda","Malacostraca","Mysida",NA,"Mysidae","Erythropinae",NA,"Euchaetomera","Euchaetomera antarcticus", "worms") # I can't find the species, but I reconstructed the classification from the genus
  manual.class["Parschisturella ceruviata", ] <- c("Animalia","Arthropoda","Malacostraca","Amphipoda","Lysianassoidea","Uristidae",NA,NA,"Parschisturella","Parschisturella ceruviata", "worms") # I can't find the species, but I reconstructed the classification from the genus
  manual.class["Neripteron taitense", ] <- c("Animalia","Mollusca","Gastropoda","Cycloneritimorpha","Neritoidea","Neritidae",NA,NA,"Neripteron","Neripteron taitense", "worms")
  manual.class["Phoxocephalus regium", ] <- c("Animalia","Arthropoda","Malacostraca","Amphipoda",NA,"Phoxocephalidae","Phoxocephalinae",NA,"Phoxocephalus","Phoxocephalus regium", "worms") # I can't find the species, but I reconstructed the classification from the genus
  manual.class["Pontharpinia australis", ] <- c("Animalia","Arthropoda","Malacostraca","Amphipoda",NA,"Phoxocephalidae","Pontharpiniinae",NA,"Pontharpinia","Pontharpinia australis", "worms") # I can't find the species, but I reconstructed the classification from the genus
  manual.class["Palaemonetes floridanus", ] <- c("Animalia","Arthropoda","Malacostraca","Decapoda","Palaemonoidea","Palaemonidae","Palaemoninae",NA,"Palaemonetes","Palaemonetes floridanus", "worms") # I can't find the species, but I reconstructed the classification from the genus
  manual.class["Euclymeninae", ] <- c("Animalia","Annelida","Polychaeta",NA,NA,"Maldanidae","Euclymeninae",NA,NA,NA, "worms")

  class.tx.tot[empty.taxo2, ] <- manual.class

  empty.taxo3 <- which(is.na(class.tx.tot[,11]) == TRUE)
  empty.fields <- which(class.tx.tot == "", arr.ind = TRUE)

  for(i in 1:nrow(empty.fields)) {
    class.tx.tot[empty.fields[i,1], empty.fields[i,2]] <- NA
  }

  # Combining datasets
  class.tx.tot <- cbind(tx.tot, class.tx.tot)

  save(x=class.tx.tot,file="RData/class_tx_tot.RData")
