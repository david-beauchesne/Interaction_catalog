source("Script/remove_objects_keep_functions.R")
remove_objects_keep_functions()
# -----------------------------------------------------------------------------
# PROJECT:
#    Evaluating the structure of the communities of the estuary
#    and gulf of St.Lawrence
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# STEP:
#    2. Import empirical food web data and observed interactions data
#      2.2 Kortsch et al. 2015
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# FILES:
#   RawData1 <- file = "RawData/Web/Kortsch_et_al_2015/ARCTIC_WEB_BS_Kortsch_et_al_2015.txt"
#   RawData2 <- file = "RawData/Web/Kortsch_et_al_2015/BOREAL_WEB_BS_Kortsch_et_al_2015.txt"
#   RData <- file = "RData/Kortsch2015.RData"
#   Script  <- file = "Script/2-2_Emp_Web.r"
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# NOTES:
#   Reference:
#     Kortsch S, Primicerio R, Fossheim M, Dolgov AV, Aschan M (2015) Climate
#       change alters the structure of arctic marine food webs due to poleward
#       shifts of boreal generalists. Proceedings of the Royal Society B 282:
#       20151546. http://dx.doi.org/10.1098/rspb.2015.1546
#     Kortsch S, Primicerio R, Fossheim M, Dolgov AV, Aschan M (2015) Data
#       from: Climate change alters the structure of arctic marine food webs
#       due to poleward shifts of boreal generalists. Dryad Digital Repository.
#       http://dx.doi.org/10.5061/dryad.73r6j
#
#   Original file modified manually to import in r
#     file = "RawData/Web/Kortsch_et_al_2015/Boreal and Arctic food webs of the
#       Barents Sea_Kortsch.xlsx"
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# SCRIPT
# -----------------------------------------------------------------------------
library(stringr)

#Import raw data from source for ARCTIC food web
Kortsch2015_arctic <- vector("list", 5)
names(Kortsch2015_arctic) <- c("Kortsch_et_al_2015-ArcticWeb","Reference","Notes","Interactions","Species")
Kortsch2015_arctic[[1]] <-read.table(file="RawData/Web/Kortsch_et_al_2015/ARCTIC_WEB_BS_Kortsch_et_al_2015.txt",header = TRUE,sep="\t",quote = "")
Kortsch2015_arctic[[2]] <- c("Kortsch S, Primicerio R, Fossheim M, Dolgov AV, Aschan M (2015) Climate change alters the structure of arctic marine food webs due to poleward shifts of boreal generalists. Proceedings of the Royal Society B 282:20151546. http://dx.doi.org/10.1098/rspb.2015.1546", "Kortsch S, Primicerio R, Fossheim M, Dolgov AV, Aschan M (2015) Data from: Climate change alters the structure of arctic marine food webs due to poleward shifts of boreal generalists. Dryad Digital Repository. http://dx.doi.org/10.5061/dryad.73r6j")

#Import raw data from source for BOREAL food web
Kortsch2015_boreal <- vector("list", 5)
names(Kortsch2015_boreal) <- c("Kortsch_et_al_2015-BorealWeb","Reference","Notes","Interactions","Species")
Kortsch2015_boreal[[1]] <-read.table(file="RawData/Web/Kortsch_et_al_2015/BOREAL_WEB_BS_Kortsch_et_al_2015.txt",header = TRUE,sep="\t",quote = "")
Kortsch2015_boreal[[2]] <- c("Kortsch S, Primicerio R, Fossheim M, Dolgov AV, Aschan M (2015) Climate change alters the structure of boreal marine food webs due to poleward shifts of boreal generalists. Proceedings of the Royal Society B 282:20151546. http://dx.doi.org/10.1098/rspb.2015.1546","Kortsch S, Primicerio R, Fossheim M, Dolgov AV, Aschan M (2015) Data from: Climate change alters the structure of arctic marine food webs due to poleward shifts of boreal generalists. Dryad Digital Repository. http://dx.doi.org/10.5061/dryad.73r6j")

#----------------------------------------------------------------------------------------------------------
#Name resolver - evaluate taxon name validity
#----------------------------------------------------------------------------------------------------------

rownames(Kortsch2015_arctic[[1]]) <- Kortsch2015_arctic[[1]][,1]
rownames(Kortsch2015_boreal[[1]]) <- Kortsch2015_boreal[[1]][,1]
Kortsch2015_arctic[[1]] <- Kortsch2015_arctic[[1]][,3:ncol(Kortsch2015_arctic[[1]])]
Kortsch2015_boreal[[1]] <- Kortsch2015_boreal[[1]][,3:ncol(Kortsch2015_boreal[[1]])]
colnames(Kortsch2015_arctic[[1]]) <- rownames(Kortsch2015_arctic[[1]])

Names_change <- function(x){
  x <- paste(x," ")
  x <- gsub("_"," ",x)
  x <- gsub("g sp ","",x)
  x <- gsub("spp ","",x)
  x <- gsub("sp ","",x)
  x <- gsub("indet ","",x)
  x <- gsub("flagellat ","flagellate",x)
  x <- gsub("flagellates ","flagellate",x)
  x <- gsub("Oithona spinirostris/atlantica","Oithona spinirostris - Oithona atlantica",x)
  x <- gsub("larvae ","",x)
  x <- gsub("  "," ",x)
  x <- str_trim(x, side="both") #remove spaces
  x <- tolower(x)
  x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
  # Name resolve 1
  x <- paste(x," ")
  x <- gsub("Pareuchaeta glacialis","Paraeuchaeta glacialis",x)
  x <- gsub("Myriochele herri","Myriochele heeri",x)
  x <- gsub("Urasterias linckii","Urasterias lincki",x)
  x <- gsub("Bathyarca galacialis","Bathyarca glacialis",x)
  x <- gsub("Etimopterus spinax","Etmopterus spinax",x)
  x <- gsub("Phoca hispida","Pusa hispida",x)
  x <- str_trim(x, side="both") #remove spaces
  x <- tolower(x)
  x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
  return(x)
}

rownames(Kortsch2015_arctic[[1]]) <- Names_change(rownames(Kortsch2015_arctic[[1]]))
rownames(Kortsch2015_boreal[[1]]) <- Names_change(rownames(Kortsch2015_boreal[[1]]))
colnames(Kortsch2015_arctic[[1]]) <- rownames(Kortsch2015_arctic[[1]])
colnames(Kortsch2015_boreal[[1]]) <- rownames(Kortsch2015_boreal[[1]])

Kortsch2015_boreal[[1]] <- dupl_row(Kortsch2015_boreal[[1]],rownames(Kortsch2015_boreal[[1]]))
Kortsch2015_boreal[[1]] <- dupl_col(Kortsch2015_boreal[[1]],colnames(Kortsch2015_boreal[[1]]))

rownames(Kortsch2015_arctic[[1]]) <- Names_change(rownames(Kortsch2015_arctic[[1]]))
rownames(Kortsch2015_boreal[[1]]) <- Names_change(rownames(Kortsch2015_boreal[[1]]))
colnames(Kortsch2015_arctic[[1]]) <- rownames(Kortsch2015_arctic[[1]])
colnames(Kortsch2015_boreal[[1]]) <- rownames(Kortsch2015_boreal[[1]])

tx.list <- unique(c(rownames(Kortsch2015_arctic[[1]]),rownames(Kortsch2015_boreal[[1]])))
#Taxonomic resolver
tx.res <- taxo_resolve(tx.list)
tx.valid <- taxo_valid(tx.res,tx.list,res.check=TRUE)

#!!!!!!!!!!!!!!!!!!!!!!!!!!! MANUAL RESOLVE 1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# [1] "!!!!!!!!! NO RESULTS OBTAINED FROM gnr_resolve() !!!!!!!!!"
#     tx.list                  valid       rank
# 1   "Detritus"               "valid"     NA   <- OK
# 2   "Autothroph flagellate"  "no result" NA   <- OK
# 4   "Diatom"                 "valid"     NA   <- OK
# 5   "Heterotroph flagellate" "no result" NA   <- OK
# 6   "Ice algae"              "valid"     NA   <- OK
# 7   "Macroalgae"             "valid"     NA   <- OK
# 8   "Mixotroph flagellate"   "no result" NA   <- OK
# 9   "Phytoplankton"          "valid"     NA   <- OK
# 10  "Protozooplankton"       "no result" NA   <- OK
# 34  "Pareuchaeta glacialis"  "valid"     NA   <- modified
# 52  "Benthos"                "valid"     NA   <- OK
# 136 "Fish"                   "valid"     NA   <- OK
# 176 "Euclymeninae"           "valid"     NA   <- OK, subfamily
# [1] "!!!!!!!!! INVALID RESULTS FROM gnr_resolve() !!!!!!!!!"
#      submitted_name           canonical_form
# [1,] "Myriochele herri"       "Myriochele heeri"    <- modified
# [2,] "Urasterias linckii"     "Urasterias"          <- modified
# [3,] "Bathyarca galacialis"   "Bathyarca glacialis" <- modified
# [4,] "Etimopterus spinax"     "Etmopterus spinax"   <- modified
# [5,] "Physeter macrocephalus" "Physeter"            <- OK
# [1] "!!!!!!!!! MULTIPLE TAXON FROM gnr_resolve() !!!!!!!!!"
#     tx.list              valid   rank
# 3   "Bacteria"           "valid" "genus - kingdom"
# 29  "Oikopleura"         "valid" "genus - species"
# 88  "Polychaeta"         "valid" "genus - subphylum - class"
# 103 "Strongylocentrotus" "valid" "genus - species"
# 196 "Clupea harengus"    "valid" "species - infraspecies"
# 223 "Sebastes"           "valid" "genus - species"

subfamily <- c("Euclymeninae")
species <- c("Clupea harengus")
genus <- c("Oikopleura","Strongylocentrotus","Sebastes")
class <- c("Polychaeta")
kingdom <- c("Bacteria")
rownames(tx.valid) <- tx.list

tx.valid[paste(species),3] <- "species"
tx.valid[paste(subfamily),3] <- "subfamily"
tx.valid[paste(genus),3] <- "genus"
tx.valid[paste(class),3] <- "class"
tx.valid[paste(kingdom),3] <- "kingdom"

#----------------------------------------------------------------------------------------------------------
#   Extract binary interactions from diet matrix
#----------------------------------------------------------------------------------------------------------

#List of binary interactions - column species (consumer) eats row species (resource)
Kortsch2015_arctic[[4]] <- bin_inter(Kortsch2015_arctic[[1]])
Kortsch2015_boreal[[4]] <- bin_inter(Kortsch2015_boreal[[1]])

#----------------------------------------------------------------------------------------------------------
#   Extract taxon list for further analyses
#----------------------------------------------------------------------------------------------------------
tx.list <- unique(rownames(Kortsch2015_arctic[[1]]))
Kortsch2015_arctic[[5]] <- as.data.frame(matrix(nrow=length(tx.list),ncol=2,data=NA))
colnames(Kortsch2015_arctic[[5]]) <- c("taxon","rank")
rownames(Kortsch2015_arctic[[5]]) <- tx.list
Kortsch2015_arctic[[5]][,1] <- tx.list
Kortsch2015_arctic[[5]][,2] <- tx.valid[tx.list,3]


tx.list <- unique(rownames(Kortsch2015_boreal[[1]]))
Kortsch2015_boreal[[5]] <- as.data.frame(matrix(nrow=length(tx.list),ncol=2,data=NA))
colnames(Kortsch2015_boreal[[5]]) <- c("taxon","rank")
rownames(Kortsch2015_boreal[[5]]) <- tx.list
Kortsch2015_boreal[[5]][,1] <- tx.list
Kortsch2015_boreal[[5]][,2] <- tx.valid[tx.list,3]


#Combining both datasets as Kortsch2015
Kortsch2015 <- vector("list",2)
ModNames <- c("Kortsch2015_arctic","Kortsch2015_boreal")
names(Kortsch2015) <- ModNames
Kortsch2015[[1]] <- Kortsch2015_arctic
Kortsch2015[[2]] <- Kortsch2015_boreal

save(x=Kortsch2015,file="RData/Kortsch2015.RData")
