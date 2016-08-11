source("Script/remove_objects_keep_functions.R")
remove_objects_keep_functions()
# -----------------------------------------------------------------------------
# PROJECT:
#    Evaluating the structure of the communities of the estuary
#    and gulf of St.Lawrence
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# STEP:
#    7. Classification of taxon list from GloBI
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# FILES:
#   RData <- file = "RData/GloBI_classification.RData"
#   Script <- file = "Script/6-Classification_GloBI.r"
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# NOTES:
#   Part of the taxonomy is already included in the extractions from GloBI,
#   but not all. I need to extract those that are already available, verify
#   them (some different classifications for the same taxon) and extract
#   those missing.
#
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
library(stringr)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# SCRIPT
# -----------------------------------------------------------------------------
load("RData/GloBI_taxon.RData")
load("RData/GloBI.RData")

tx.tot <- GloBI_taxon
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

  classification.taxo.add <- classification.taxon

  class.tx.tot[add.class.to.measure, ] <- classification.taxo.add

  empty.taxo2 <- which(is.na(class.tx.tot[,11]) == TRUE)


  # # Manually

  no.result.to.delete <- c( "Baraeoptera roria",
                            "Cydorus latus",
                            "Hemiuris communis",
                            "Hyponigrus obsidianus",
                            "Sarortherdon macrochir",
                            "Scaphaloberis mucronata",
                            "Secernentia nematodes",
                            "Zealolessica cheira",
                            "Haploparaksis crassirostris",
                            'Spermophilus armatus',
                            "Spermophilus armatus",
                            "Spermophilus brunneus",
                            "Spermophilus franklinii",
                            "Spermophilus richardsonii",
                            "Spermophilus tridecemlineatus",
                            "Spermophilus washingtoni",
                            'Glossoma',
                            "Paracentropristes pomospilus",
                            "Delphacinae",
                            "Staphylininae",
                            "Ursinae",
                            "Zelandoperlinae",
                            "Euclymeninae",
                            "Pilumninae")

to.delete <- numeric() # À utiliser à la fin après avoir combiner les jeux de données
for(i in 1:length(no.result.to.delete)){
  to.delete <- c(to.delete,which(tx.tot == no.result.to.delete[i]))
}

  # Genus chosen when prompted, adjust species name
    class.tx.tot[which(tx.tot == 'Hyalites rahira'), 'species'] <- 'Acraea rahira'
    class.tx.tot[which(tx.tot == 'Hylina veliei'), 'species'] <- 'Hylina veliei'
    class.tx.tot[which(tx.tot == 'Limothrips schmutzi'), 'species'] <- 'Limothrips schmutzi'
    class.tx.tot[which(tx.tot == 'Microsorium punctatum'), 'species'] <- 'Microsorum punctatum'
    class.tx.tot[which(tx.tot == 'Paroxyna misella'), 'species'] <- 'Campiglossa misella'
    class.tx.tot[which(tx.tot == 'Paroxyna producta'), 'species'] <- 'Campiglossa producta'

  # Adjust empty fields
    empty.fields <- which(class.tx.tot == "", arr.ind = TRUE)
    for(i in 1:nrow(empty.fields)) {
      class.tx.tot[empty.fields[i,1], empty.fields[i,2]] <- NA
    }

  # Combining datasets
  class.tx.tot <- cbind(tx.tot, class.tx.tot)
  class.tx.tot <- class.tx.tot[-to.delete, ]

  # Verify if some are remaining not classified
  empty.taxo3 <- which(is.na(class.tx.tot[,13]) == TRUE)
  empty.taxo3

  GloBI_classification <- class.tx.tot
  GloBI_classification <- GloBI_classification[-which(GloBI_classification[,1] == 'Unidentified'), ]

  save(x = GloBI_classification, file="RData/GloBI_classification.RData")
