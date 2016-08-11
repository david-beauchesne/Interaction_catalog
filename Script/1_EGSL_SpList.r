source("Script/remove_objects_keep_functions.R")
remove_objects_keep_functions()
# -----------------------------------------------------------------------------
# PROJECT:
#    Evaluating the structure of the communities of the estuary
#    and gulf of St.Lawrence
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# STEP:
#   1. Species list for estuary and gulf of St. Lawrence
#     - Import species list from CaRMS
#     - Add marine birds
#     - Additional marine mammals
#     - Add invasive species
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# FILES:
#   RawData1 <- file = "RawData/EGSL/CaRMS_checklist_20160325.txt"
#   RawData2 <- file = "RawData/EGSL/MarBird_EGSL.txt"
#   RawData3 <- file = "RawData/EGSL/MarMam_EGSL.txt"
#   RawData4 <- file = "RawData/EGSL/InvSp_EGSL.txt"
#   RData <- file = "RData/sp_egsl.RData"
#   Script  <- file = "Script/1_EGSL_SpList.r"
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# NOTES:
#   Adjust the rawdata for species. For now, using an alternate
#   list generated manually with CaRMS data as the initial dataset
#   File = "RawData/EGSL/SP_SL.txt"
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# SCRIPT
# -----------------------------------------------------------------------------
library(stringr)
# Modified list
sp.egsl<-read.table(file="RawData/EGSL/SP_SL.txt", header=TRUE, sep="\t")

sp.egsl<-as.data.frame(sp.egsl)
colnames(sp.egsl) <- c("Species")
sp.egsl$Species <- as.character(sp.egsl$Species)

#Clean up of species names (to code better eventually)
sp.egsl$Species <- gsub("\\(.*?\\) ","",sp.egsl$Species) #remove species with additional names in parentheses
sp.egsl$Species <- gsub("f\\.[^\\.]*$","",sp.egsl$Species) #remove species forms
sp.egsl$Species <- gsub("var\\.[^\\.]*$","",sp.egsl$Species) #remove species varieties
sp.egsl$Species <- str_trim(sp.egsl$Species, side="both") #remove spaces

for(i in 1:nrow(sp.egsl)) {
  if(length(strsplit(sp.egsl[i,1]," ")[[1]]) > 2) {
    sp.egsl[i, 1] <- paste(strsplit(sp.egsl[i,1]," ")[[1]][1], strsplit(sp.egsl[i,1]," ")[[1]][2], sep = " ")
  }
}

sp.egsl <- unique(sp.egsl)
save(x=sp.egsl,file="RData/sp_egsl.RData")
