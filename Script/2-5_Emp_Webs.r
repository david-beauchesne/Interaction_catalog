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
#      2.5 Ecopath with Ecosim models
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# FILES:
#   RawData1 <- file = "RawData/Web/EwE/sites.RData"
#   RawData2 <- file = "RawData/Web/EwE/species.RData"
#   RawData3 <- file = "RawData/Web/EwE/DIET.RData"
#   RawData4 <- file = "RawData/Web/EwE/B_vec.RData"
#   RawData5 <- file = "RawData/Web/EwE/P_vec.RData"
#   RawData6 <- file = "RawData/Web/EwE/lQ_vec.RData"
#   RawData6 <- file = "RawData/Web/EwE/habitat.RData"


#   RData <- file = "RData/EwE.RData"
#   Script  <- file = "Script/2-5_Emp_Web.r"
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# NOTES:
#   Reference:
#
#   Notes:
#     sites.RData -> character vector with model names
#     species.RData -> list of 118 character vectors with species names
#     DIET.RData -> List of 118 diet matrices
#     B_vec.Rdata -> List of 118 biomass vectors (species biomass (t/km^2))
#     P_vec.Rdata -> List of 118 vectors with species biomass production (t/km^2)
#     lQ_vec.Rdata -> List of 118 vectors with species biomass consumption (t/km^2)
#     habitat.RData -> Vector of ecosystem types (1: marine; 2: freshwater;
#       3:terrestrial) for the 118 Ecopath models:
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# LIBRARIES:
library(stringr)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# SCRIPT
# -----------------------------------------------------------------------------
load(file="RawData/Web/EwE/DIET.RData")
load(file="RawData/Web/EwE/sites.RData")
load(file="RawData/Web/EwE/species.RData")
load(file="RawData/Web/EwE/habitat.RData")

# Selecting marine models
marine <- which(habitat == 1)
nb_mod <- length(marine)

EwE_diet <- vector("list",length(marine))
EwE_sites <- vector("list",length(marine))
EwE_species <- vector("list",length(marine))

for(i in 1:nb_mod){
  EwE_diet[[i]] <- DIET[[marine[i]]]
  EwE_sites[[i]] <- sites[[marine[i]]]
  EwE_species[[i]] <- species[[marine[i]]]
}
remove(DIET,sites,habitat,species)

# Transforming interactions in the diet matrix as binary values
for(i in 1:nb_mod) {
  EwE_diet[[i]] <- binary_interaction(EwE_diet[[i]])
}

# Adjusting species names... Fuck this shit...
tx.list <- unique(unlist(EwE_species))
tx.list <- iconv(tx.list[1:100],"WINDOWS-1252","UTF-8")

Names_change_initial <- function(x){
  x <- paste(x," ")
  x <- tolower(x)
  x <- gsub("in/epi","",x)

  x <- gsub(" \\(.*?\\)","",x) #removes parentheses
  # x <- gsub("^.*?: ","",x) #Prefix for taxon names followed by ":"
  # x <- gsub("sp\\.","",x) #Removing sp. from genus
  # x <- gsub("spp\\.","",x)  #Removing spp. from genus





  # x <- gsub("yoy "," ",x)


  # x <- gsub(" & "," - ",x)
  # x <- gsub(" and "," - ",x)
  # x <- gsub("\\/"," - ",x)
  # x <- gsub("  "," ",x)

}

tx.list <- Names_change(tx.list)

623

sm
lg
misc
md
Small molluscs/




locate(tx.list,"\"")


  x <- gsub("adult","",x)
  x <- gsub("juvenile","",x)
  x <- gsub("larvae","",x)
  x <- gsub("type","",x)
  x <- gsub("sepia officinalis.*$","sepia officinalis",x)
  x <- gsub(" 1","",x)
  x <- gsub(" 2","",x)
  x <- gsub("other","",x)
  x <- gsub("iv-vi copepodites of calanus and pseudocalanus","calanus - pseudocalanus",x)
  x <- gsub("iv-vi copepodites of paracalanus","paracalanus",x)
  x <- gsub("aggregate","",x)
  x <- gsub("ii-iii copepodites","copepod",x)
  x <- gsub("intertidal and marine invertebrates","intertidal invertebrates - marine invertebrates",x)
  x <- gsub("killer whale.*$","killer whale",x)
  x <- gsub("chinook.*$","chinook",x)
  x <- gsub("coho.*$","coho",x)
  x <- gsub("coho","coho salmon",x)
  x <- gsub("tuna1","tuna",x)
  x <- gsub("tuna2","tuna",x)
  x <- gsub("chinook","chinook salmon",x)
  x <- gsub("nauplii of copepoda","copepod",x)
  x <- gsub("small-size","",x)
  x <- gsub("smaller","",x)
  x <- gsub("small","",x)
  x <- gsub("medium-sized","",x)
  x <- gsub("medium-size","",x)
  x <- gsub("medium","",x)
  x <- gsub("large-sized","",x)
  x <- gsub("large-size","",x)
  x <- gsub("larger","",x)
  x <- gsub("large","",x)
  x <- gsub(" eating","-eating",x)
  x <- gsub(" feeding","-feeding",x)
  x <- gsub("marine pom.*$","pom",x)
  x <- gsub("from.*$","",x)
  x <- gsub("benthonic","benthic",x)
  x <- gsub("commercial sparids","sparids",x)
  x <- gsub("migrant","",x)
  x <- gsub("unid\\.","",x)
  x <- gsub("misc\\.","",x)
  x <- gsub("unidentified","",x)
  x <- gsub("meiofaunal-size","",x)
  x <- gsub("of ","",x)
  x <- gsub("copepod nauplii","copepod",x)
  x <- gsub("copepodites","copepod",x)
  x <- gsub("hyperiid amphipod","hyperiid",x)
  x <- gsub("hyperiids","hyperiid",x)
  x <- gsub("hyperids","hyperiid",x)
  x <- gsub("sized","",x)
  x <- gsub("terrestrial pom","pom",x)


}




x <- tolower(x)

  x <- str_trim(x, side="both") #remove spaces
  x <- tolower(x)
  x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
  x <- paste(x," ")
  x <- gsub("Periphyton ","Ceramium cimbricum - Chaetomorpha crassa - Enteromorpha intestinalis - Ulva fasciata - Ulva lactuca - Lyngbya majuscula ",x)
  x <- gsub("Detritivorous fish ","Liza macrolepis - Pomacentrus taeniometopon - Scarus ghobban - Scatophagus argus - Siganus guttatus - Valamugil cunnesius ",x)
  x <- gsub("","",x)
  x <- gsub("Shrimps ","Penaeus monodon - Penaeus penicillatus ",x)
  x <- gsub("Crabs ","Thalamita danae - Charybdis hellerii ",x)
  x <- gsub("Polychaetes ","Amphitrite ",x)
  x <- str_trim(x, side="both") #remove spaces
  x <- tolower(x)
  x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
return(x)






locate2(mat,"")
















# Adding species names to diet matrices
for(i in 1:93){
  if((ncol(EwE_diet[[i]]) - nrow(EwE_diet[[i]])) != 0) {
    print(paste("Model",i))
    stop("Verify matrix structure, for EwE models they need to have the same species list for rows and columns")
  }
  dimnames(EwE_diet[[i]]) <- list(EwE_species[[i]],EwE_species[[i]])
}
