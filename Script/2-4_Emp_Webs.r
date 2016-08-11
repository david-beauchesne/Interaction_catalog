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
#      2.4 Brose et al. 2005
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# FILES:
#   RawData <- file = "RawData/Web/Brose_et_al_2005/Brose_Webs.RData"
#   RData1 <- file = "RData/brose2005.RData"
#   Script  <- file = "Script/2-4_Emp_Web.r"
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# NOTES:
#   Reference:
#     Brose, U., Cushing, L., Berlow, E. L., Jonsson, T., Banasek-Richter, C.,
#     Bersier, L.-F., Blanchard, J. L., Brey, T., Carpenter, S. R., Blandenier,
#     M.-F. C., Cohen, J. E., Dawah, H. A., Dell, T., Edwards, F., Harper-Smith,
#     S., Jacob, U., Knapp, R. A., Ledger, M. E., Memmott, J., Mintenbeck, K.,
#     Pinnegar, J. K., Rall, B. C., Rayner, T., Ruess, L., Ulrich, W., Warren,
#     P., Williams, R. J., Woodward, G., Yodzis, P. and Martinez, N. D. (2005),
#     BODY SIZES OF CONSUMERS AND THEIR RESOURCES. Ecology, 86: 2545.
#     doi: 10.1890/05-0379
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# TASKS LEFT:
#
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# SCRIPT
# -----------------------------------------------------------------------------
library(stringr)
load(file="RawData/Web/Brose_et_al_2005/Brose_Webs.RData") #Webs

web.list <- NA
for(i in 1:length(Webs)){
  if(is.na(str_locate(Webs[[i]]$habitat,"marine")[1,1]) == FALSE) {web.list <- c(web.list,i)} else {NULL}
}
web.list <- web.list[-1]
#web.list[1] & web.list[4] not used for the analyses even if marine
#web.list[5] not used for the analyses since it is already in the GlobalWeb dataset (See 2.3_Emp_Webs.r)
web.list <- web.list[-c(1,4,5)]
web.num <- length(web.list)

Brose2005<- vector("list",web.num)
ModNames <- rep(NA,web.num)
for(i in 1:web.num){ModNames[i] <- gsub(" ","",paste("WEB",web.list[i]))}
names(Brose2005) <- ModNames

for(i in 1:web.num){
  Brose2005[[i]] <- vector("list", 5)
  names(Brose2005[[i]]) <- c(names(Brose2005)[i],"Reference","Notes","Interactions","Species")
  Brose2005[[i]][[1]] <- t(Webs[[web.list[i]]]$web) #put consumers as columns
  Brose2005[[i]][[2]] <- Webs[[web.list[i]]]$name
}

#----------------------------------------------------------------------------------------------------------
#                                             Jacob_Shelf
#----------------------------------------------------------------------------------------------------------
  #Functions to change names in webs
  Names_change <- function(x){
    x <- gsub("\\(.*?\\)","",x) #removing parentheses
    x <- str_trim(x, side="both") #remove spaces
    x <- gsub("  "," ",x)
    x <- gsub("sp\\.","",x) #Removing sp. from genus
    return(x)
  }
  colnames(Brose2005[[1]][[1]]) <- Names_change(colnames(Brose2005[[1]][[1]]))
  rownames(Brose2005[[1]][[1]]) <- Names_change(rownames(Brose2005[[1]][[1]]))

  #Substract
  Tx.change <- function(x){
    if(grepl("[A-Z]",strsplit(x," ")[[1]])[length(strsplit(x," ")[[1]])] == TRUE) {strsplit(x," ")[[1]][length(strsplit(x," ")[[1]])]} else if(grepl("[A-Z]",strsplit(x," ")[[1]])[length(strsplit(x," ")[[1]])-1] == TRUE) {paste(strsplit(x," ")[[1]][(length(strsplit(x," ")[[1]])-1):(length(strsplit(x," ")[[1]]))],collapse=" ")} else if(grepl("[A-Z]",strsplit(x," ")[[1]])[length(strsplit(x," ")[[1]])-2] == TRUE) {paste(strsplit(x," ")[[1]][(length(strsplit(x," ")[[1]])-2):(length(strsplit(x," ")[[1]]))],collapse=" ")} else if(grepl("[A-Z]",strsplit(x," ")[[1]])[length(strsplit(x," ")[[1]])-3] == TRUE) {paste(strsplit(x," ")[[1]][(length(strsplit(x," ")[[1]])-3):(length(strsplit(x," ")[[1]]))],collapse=" ")} else {NULL}
  }
  for(i in 1:ncol(Brose2005[[1]][[1]])){colnames(Brose2005[[1]][[1]])[i] <- Tx.change(colnames(Brose2005[[1]][[1]])[i])}
  for(i in 1:nrow(Brose2005[[1]][[1]])){rownames(Brose2005[[1]][[1]])[i] <- Tx.change(rownames(Brose2005[[1]][[1]])[i])}

  Brose_mod <- function(x){
    x <- gsub("Clavularia frankiliana","Clavularia frankliniana",x)
    x <- gsub("Clio pyramitata","Clio pyramidata ",x)
    x <- gsub("Diatoms","Diatom",x)
    x <- gsub("Dolloidraco longidorsalis","Dolloidraco longedorsalis",x)
    x <- gsub("Galitheutis glacialis","Galiteuthis glacialis",x)
    x <- gsub("Gymnoscoelus nicholsi","Gymnoscopelus nicholsi",x)
    x <- gsub("Moroteuthis ingens","Onykia ingens",x)
    x <- gsub("Morotheutis ingens","Onykia ingens",x)
    x <- gsub("Natatolana obusata","Natatolana obtusata",x)
    x <- gsub("Onacea","Oncaea",x)
    x <- gsub("Ostracods","Ostracoda",x)
    x <- gsub("Pelegobia longicirrata","Pelagobia longicirrata",x)
    x <- gsub("Scolymasta joubini","Anoxycalyx joubini",x)
    x <- gsub("Scolymastra joubini","Anoxycalyx joubini",x)
    x <- gsub("Trophon longistaffi","Trophonella longstaffi",x)
    x <- gsub("Trophon longstaffi","Trophonella longstaffi",x)
    x <- gsub("Yoldiella eightsi","Aequiyoldia eightsii",x)
    x <- gsub("Yolida eightsi","Aequiyoldia eightsii",x)
    x <- gsub("Ypsilocucumis turricata","Paracucumis turricata",x)
    x <- gsub("Clavularia frankiliana","Clavularia frankliniana",x)
    x <- gsub("Clio pyramitata","Clio pyramidata ",x)
    x <- gsub("Diatoms","Diatom",x)
    x <- gsub("Dolloidraco longidorsalis","Dolloidraco longedorsalis",x)
    x <- gsub("Galitheutis glacialis","Galiteuthis glacialis",x)
    x <- gsub("Gymnoscoelus nicholsi","Gymnoscopelus nicholsi",x)
    x <- gsub("Moroteuthis ingens","Onykia ingens",x)
    x <- gsub("Morotheutis ingens","Onykia ingens",x)
    x <- gsub("Natatolana obusata","Natatolana obtusata ",x)
    x <- gsub("Onacea","Oncaea",x)
    x <- gsub("Ostracods","Ostracoda",x)
    x <- gsub("Pelegobia longicirrata","Pelagobia longicirrata",x)
    x <- gsub("Scolymasta joubini","Anoxycalyx joubini",x)
    x <- gsub("Scolymastra joubini","Anoxycalyx joubini",x)
    x <- gsub("Trophon longistaffi","Trophonella longstaffi",x)
    x <- gsub("Trophon longstaffi","Trophonella longstaffi",x)
    x <- gsub("Yoldiella eightsi","Aequiyoldia eightsii",x)
    x <- gsub("Yolida eightsi","Aequiyoldia eightsii",x)
    x <- gsub("Ypsilocucumis turricata","Paracucumis turricata",x)
  # after first round of gnr_resolve
    x <- gsub("Isodyctia steifera","Isodictya setifera",x)
    x <- gsub("Ophiuroida","Ophiuroidea",x)
    x <- gsub("Vampryoteuthis","Vampyroteuthis",x)
    x <- gsub("Mertensiid ctenophore","Mertensiidae",x)
    x <- gsub("Macrourus holotrachis","Macrourus holotrachys",x)
    x <- gsub("Tubularia ralphii","Ectopleura crocea",x)
    x <- gsub("Abatus shackeltoni","Abatus shackletoni",x)
    x <- gsub("Amauropis rossiana","Amauropsis rossiana",x)
    x <- gsub("Austrosisignum grande","Austrosignum grande",x)
    x <- gsub("Camylaspis maculata","Campylaspis maculata",x)
    x <- gsub("Composothyris racovitzae","Compsothyris racovitzae",x)
    x <- gsub("Ctenocidaris gilberti","Ctenocidaris geliberti",x)
    x <- gsub("Dipulmaris antarctica","Diplulmaris antarctica",x)
    x <- gsub("Golfingia nordenskojoeldi","Golfingia nordenskjoldi",x)
    x <- gsub("Gorgonocephalus chiliensis","Gorgonocephalus chilensis",x)
    x <- gsub("Kondakovia longimama","Kondakovia longimana",x)
    x <- gsub("Momoculodes scabriculosus","Monoculodes scabriculosus",x)
    x <- gsub("Myodocopia","Myodocopida",x)
    x <- gsub("Nematroflustra flagellata","Nematoflustra flagellata",x)
    x <- gsub("Neogloboquadriana pachyderma","Neogloboquadrina pachyderma",x)
    x <- gsub("Ophiacantha pentacis","Ophiacantha pentactis",x)
    x <- gsub("Vaunthompsonia indermis","Vaunthompsonia inermis",x)
    x <- gsub("Crinoida","Crinoidea",x)
    x <- gsub("Nanoplankton","Nannoplankton",x)
    x <- gsub("Oncea curvata","Oncaea curvata",x)
    x <- gsub("Silicioflagellates","Silicoflagellata",x)
    x <- gsub("Vibillia antarctica","Vibilia antarctica",x)
    x <- gsub("Vibillia stebbingi","Vibilia stebbingi",x)
    x <- gsub("Actinaria","Actiniaria",x)
    x <- gsub("Asteroida","Asteroidea",x)
    x <- gsub("Nemertini","Nemertea",x)
    x <- gsub("Siphonophora","Siphonophorae",x)
    x <- gsub("Sipunculida","Sipuncula",x)
    x <- gsub("Tintinnid","Tintinnina",x)
    x <- gsub("Crania lecointei","Novocrania lecointei",x)
    x <- gsub("Ekmocucumis steineni","Heterocucumis steineni",x)
    x <- gsub("Ekmocucumis turqueti","Staurocucumis turqueti",x)
    x <- gsub("Monocaulus parvula","Zyzzyzus parvula",x)

  }
  colnames(Brose2005[[1]][[1]]) <- Brose_mod(colnames(Brose2005[[1]][[1]]))
  rownames(Brose2005[[1]][[1]]) <- Brose_mod(rownames(Brose2005[[1]][[1]]))

  colnames(Brose2005[[1]][[1]]) <- str_trim(colnames(Brose2005[[1]][[1]]),side="both")
  rownames(Brose2005[[1]][[1]]) <- str_trim(rownames(Brose2005[[1]][[1]]),side="both")
  #Substracting subspecies
  subsp_subs <- function(x){
    if(length(strsplit(x," ")[[1]]) == 3) {x <- paste(strsplit(x," ")[[1]][1:2],collapse=" ")} else {x}
  }
  for(i in 1:ncol(Brose2005[[1]][[1]])){colnames(Brose2005[[1]][[1]])[i] <- subsp_subs(colnames(Brose2005[[1]][[1]])[i])}
  for(i in 1:nrow(Brose2005[[1]][[1]])){rownames(Brose2005[[1]][[1]])[i] <- subsp_subs(rownames(Brose2005[[1]][[1]])[i])}

#Combining duplicated rows and columns in food web matrix
#see duplicate_row_col.r for function detail
  Brose2005[[1]][[1]] <- dupl_sp(Brose2005[[1]][[1]])


#----------------------------------------------------------------------------------------------------------
#Name resolver - evaluate name validity
#----------------------------------------------------------------------------------------------------------
tx.list <- unique(c(colnames(Brose2005[[1]][[1]]),rownames(Brose2005[[1]][[1]])))
tx.res <- taxo_resolve(tx.list) #voir script taxo_resolve.r
tx.valid <- taxo_valid(tx.res,tx.list,res.check=TRUE) #voir script taxo_valid.r -- no result; valid; invalid

#Manually verify results from taxo_valid()

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#      Assuming that the list changes, everything that follows would need to be done again manually.
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# FIRST ROUND OF gnr_resolve
# No results
# - "Aequiyoldia eightsii" <- OK
# - "Cetaceans" <- OK
# - "Isodyctia steifera" <- Brose_mod2
# - "Ophiuroida" <- Brose_mod2
# - "Phytodetritus" <- OK
# - "Squid" <- OK
# - "Vampryoteuthis" <- OK
# - "Mertensiid ctenophore"

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#     submitted_name                 canonical_form
#[1,] "Crinoida"                     "Crinoidea"
#[2,] "Kondakovia longimama"         "Kondakovia longimana"
#[3,] "Nanoplankton"                 "Nannoplankton"
#[4,] "Oncea curvata"                "Oncaea curvata"
#[5,] "Silicioflagellates"           "Silicoflagellata"
#[6,] "Vibillia antarctica"          "Vibilia antarctica"
#[7,] "Vibillia stebbingi"           "Vibilia stebbingi"
#[8,] "Abatus shackeltoni"           "Abatus shackletoni"
#[9,] "Amauropis rossiana"           "Amauropsis rossiana"
#[10,] "Austrosisignum grande"        "Austrosignum grande"
#[11,] "Camylaspis maculata"          "Campylaspis maculata"
#[12,] "Composothyris racovitzae"     "Compsothyris racovitzae"
#[13,] "Ctenocidaris gilberti"        "Ctenocidaris geliberti"
#[14,] "Dipulmaris antarctica"        "Diplulmaris antarctica"
#[15,] "Euchaetomera antarcticus"     "Euchaetomera"
#[16,] "Golfingia nordenskojoeldi"    "Golfingia nordenskjoldi"
#[17,] "Gorgonocephalus chiliensis"   "Gorgonocephalus chilensis"
#[18,] "Macrourus holotrachis"        "Macrourus"
#[19,] "Momoculodes scabriculosus"    "Monoculodes scabriculosus"
#[20,] "Myodocopia"                   "Myodocopida"
#[21,] "Nematroflustra flagellata"    "Nematoflustra flagellata"
#[22,] "Neogloboquadriana pachyderma" "Neogloboquadrina pachyderma"
#[23,] "Ophiacantha pentacis"         "Ophiacantha pentactis"
#[24,] "Parschisturella ceruviata"    "Parschisturella"
#[25,] "Physeter macrocephalus"       "Physeter"
#[26,] "Tubularia ralphii"            "Tubularia"
#[27,] "Vaunthompsonia indermis"      "Vaunthompsonia inermis"

#Reverify that there are not duplicated names
tx.list <- unique(c(colnames(Brose2005[[1]][[1]]),rownames(Brose2005[[1]][[1]])))
table(duplicated(colnames(Brose2005[[1]][[1]]))) #none 2016-04-23
table(duplicated(rownames(Brose2005[[1]][[1]]))) #none 2016-04-23

#----------------------------------------------------------------------------------------------------------
#   Diet matrix
#----------------------------------------------------------------------------------------------------------
#Extend diet matrices to include the full spectrum of interactions. At this point species that are only resource are found only as rows, same for consumers as columns
Brose2005[[1]][[1]] <- Diet_mat(Brose2005[[1]][[1]]) #See diet_mat_extend.r for full function

#----------------------------------------------------------------------------------------------------------
#   Extract binary interactions
#----------------------------------------------------------------------------------------------------------
Brose2005[[1]][[4]] <- bin_inter(Brose2005[[1]][[1]])

#----------------------------------------------------------------------------------------------------------
#   Extract taxon list for further analyses
#----------------------------------------------------------------------------------------------------------
Brose2005[[1]][[5]] <- as.data.frame(matrix(nrow=length(tx.list),ncol=2,data=NA))
colnames(Brose2005[[1]][[5]]) <- c("taxon","rank")
rownames(Brose2005[[1]][[5]]) <- tx.list
Brose2005[[1]][[5]][,1] <- tx.list
Brose2005[[1]][[5]][,2] <- tx.valid[,3]

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#                                           taxon                                rank
# Aequiyoldia eightsii       Aequiyoldia eightsii                                <NA>
# Ekmocucumis steineni       Ekmocucumis steineni                                <NA>
# Ekmocucumis turqueti       Ekmocucumis turqueti                                <NA>
# Golfingia mawsoni             Golfingia mawsoni infraspecies - subspecies - species
# Golfingia nordenskjoldi Golfingia nordenskjoldi infraspecies - subspecies - species
# Golfingia ohlini               Golfingia ohlini infraspecies - subspecies - species
# Monocaulus parvula           Monocaulus parvula                                <NA>
# Phascolion strombi           Phascolion strombi           infraspecies - subspecies
# Anthozoa                               Anthozoa                   class - subphylum
# Bacteria                               Bacteria                     genus - kingdom
# Cetaceans                             Cetaceans                                <NA>
# Chaetoceros                         Chaetoceros                    genus - subgenus
# Coelenterata                       Coelenterata                                <NA>
# Ctenophora                           Ctenophora                      genus - phylum
# Diatom                                   Diatom                                <NA>
# Echiurida                             Echiurida                    order - subclass
# Fish                                       Fish                                <NA>
# Flagellate                           Flagellate                                <NA>
# Holothuria                           Holothuria                     genus - species
# Mysidacea                             Mysidacea                  superorder - order
# Nannoplankton                     Nannoplankton                                <NA>
# Nemertea                               Nemertea                    phylum - species
# Nitzschia                             Nitzschia                     genus - species
# Phytodetritus                     Phytodetritus                                <NA>
# Phytoplankton                     Phytoplankton                                <NA>
# Polychaeta                           Polychaeta           genus - subphylum - class
# Sediment                               Sediment                                <NA>
# Silicoflagellata               Silicoflagellata                                <NA>
# Sipuncula                             Sipuncula                   phylum - subclass
# Squid                                     Squid                                <NA>
# Zooplankton                         Zooplankton                                <NA>

species <- c("Aequiyoldia eightsii","Golfingia mawsoni","Golfingia nordenskjoldi","Golfingia ohlini","Phascolion strombi","Euchaetomera antarcticus","Parschisturella ceruviata")
genus <- c("Chaetoceros","Holothuria","Nitzschia")
suborder <- c("Echiurida")
order <- c("Mysidacea")
phylum <- c("Coelenterata","Ctenophora","Nemertea","Sipuncula")
class <- c("Anthozoa","Polychaeta","Squid")
not.appl <- c("Bacteria","Cetaceans","Diatom","Fish","Flagellate","Nannoplankton","Phytodetritus","Phytoplankton","Sediment","Silicoflagellata","Zooplankton")

Brose2005[[1]][[5]][paste(species),2] <- "species"
Brose2005[[1]][[5]][paste(genus),2] <- "genus"
Brose2005[[1]][[5]][paste(suborder),2] <- "suborder"
Brose2005[[1]][[5]][paste(order),2] <- "order"
Brose2005[[1]][[5]][paste(phylum),2] <- "phylum"
Brose2005[[1]][[5]][paste(class),2] <- "class"
Brose2005[[1]][[5]][paste(not.appl),2] <- "NA"


#----------------------------------------------------------------------------------------------------------
#                                            Pinnegar et al. 2003
#----------------------------------------------------------------------------------------------------------
Names_change <- function(x){
  x <- gsub("\\(.*?\\)","",x) #removing parentheses
  x <- gsub("  "," ",x)
  x <- gsub("sp\\.","",x) #Removing sp. from genus
  x <- gsub("-sp","",x) #Removing sp. from genus
  x <- gsub("-fish","",x) #Removing sp. from genus
  x <- gsub("-Larvae","",x) #Removing sp. from genus
  x <- gsub("-unidentified","",x) #Removing sp. from genus
  x <- str_trim(x, side="both") #remove spaces
  return(x)
}
colnames(Brose2005[[2]][[1]]) <- Names_change(colnames(Brose2005[[2]][[1]]))
rownames(Brose2005[[2]][[1]]) <- Names_change(rownames(Brose2005[[2]][[1]]))

rownames(Brose2005[[2]][[1]]) <- gsub("Pearlside Trisopterus esmarkii","Pearlside Maurolicus muelleri",rownames(Brose2005[[2]][[1]]))
colnames(Brose2005[[2]][[1]]) <- gsub("Pearlside Trisopterus esmarkii","Pearlside Maurolicus muelleri",colnames(Brose2005[[2]][[1]]))

#Substract
Tx.change <- function(x){
  if(grepl("[A-Z]",strsplit(x," ")[[1]])[length(strsplit(x," ")[[1]])] == TRUE) {strsplit(x," ")[[1]][length(strsplit(x," ")[[1]])]} else if(grepl("[A-Z]",strsplit(x," ")[[1]])[length(strsplit(x," ")[[1]])-1] == TRUE) {paste(strsplit(x," ")[[1]][(length(strsplit(x," ")[[1]])-1):(length(strsplit(x," ")[[1]]))],collapse=" ")} else if(grepl("[A-Z]",strsplit(x," ")[[1]])[length(strsplit(x," ")[[1]])-2] == TRUE) {paste(strsplit(x," ")[[1]][(length(strsplit(x," ")[[1]])-2):(length(strsplit(x," ")[[1]]))],collapse=" ")} else if(grepl("[A-Z]",strsplit(x," ")[[1]])[length(strsplit(x," ")[[1]])-3] == TRUE) {paste(strsplit(x," ")[[1]][(length(strsplit(x," ")[[1]])-3):(length(strsplit(x," ")[[1]]))],collapse=" ")} else {NULL}
}
for(i in 1:ncol(Brose2005[[2]][[1]])){colnames(Brose2005[[2]][[1]])[i] <- Tx.change(colnames(Brose2005[[2]][[1]])[i])}
for(i in 1:nrow(Brose2005[[2]][[1]])){rownames(Brose2005[[2]][[1]])[i] <- Tx.change(rownames(Brose2005[[2]][[1]])[i])}

# Hatchet-fish looked up in FishBase:
#   Diaphanous hatchet fish	English	UK	Sternoptyx diaphana
#   Half-naked hatchetfish	English	UK	Argyropelecus hemigymnus
#   Hatchetfish	            English	UK	Argyropelecus gigas
#   Highlight hatchetfish	  English	UK	Sternoptyx pseudobscura
#   Pacific hatchet fish	  English	UK	Argyropelecus affinis
# ‎Higher classification: Family <- Sternoptychidae
#
# Rockling looked up in FishBase
#   Arctic rockling	              English	UK	Gaidropsarus argentatus
#   Bigeye rockling	              English	UK	Gaidropsarus macrophthalmus
#   Fivebeard rockling	          English	UK	Ciliata mustela
#   Fourbeard rockling	          English	UK	Enchelyopus cimbrius
#   Mediterranean bigeye rockling	English	UK	Gaidropsarus biscayensis
#   Northern rockling	            English	UK	Ciliata septentrionalis
#   Shore rockling	              English	UK	Gaidropsarus mediterraneus
#   Threadfin rockling	          English	UK	Gaidropsarus ensis
#   Three-bearded rockling	      English	UK	Gaidropsarus vulgaris
# ‎Higher classification: Family <- Lotidae
#
# Sandeel in FishBase
#   Great sandeel 	  English	UK	Hyperoplus lanceolatus	FAO
#   Small sandeel   	English	UK	Ammodytes tobianus	FAO
#   Smooth sandeel  	English	UK	Gymnammodytes semisquamatus	FAO
#   Greater sandeel 	English	UK	Hyperoplus lanceolatus	FAO old
#   Smooth sandeel  	English	UK	Ammodytes tobianus	FAO old
#   Corbin's sandeel	English	UK	Hyperoplus immaculatus	Vernacular
#   Great sandeel	    English	UK	Hyperoplus lanceolatus	Vernacular
#   Greater sandeel	  English	UK	Hyperoplus lanceolatus	Vernacular
#   Lesser sandeel	  English	UK	Ammodytes marinus	Vernacular
#   Lesser sandeel	  English	UK	Ammodytes tobianus	Vernacular
#   Pacific sandeel 	English	UK	Ammodytes personatus	Vernacular
#   Raitt's sandeel 	English	UK	Ammodytes marinus	Vernacular
#   Sandeel         	English	UK	Ammodytes tobianus	Vernacular
#   Small sandeel   	English	UK	Ammodytes tobianus	Vernacular
#   Smooth sandeel  	English	UK	Gymnammodytes semisquamatus
# ‎Higher classification: Family <- Ammodytidae
#
# Scalsfish in FishBase
#   Imperial scaldfish    	English	UK	Arnoglossus imperialis	FAO
#   Mediterranean scaldfish	English	UK	Arnoglossus laterna	FAO
#   Thor's scaldfish      	English	UK	Arnoglossus thori	FAO
#   Grohmann's scaldfish  	English	UK	Arnoglossus thori	Vernacular
#   Imperial scaldfish    	English	UK	Arnoglossus imperialis	Vernacular
#   Scaldfish	              English	UK  Arnoglossus laterna	Vernacular
#   Scaldfish             	English	UK  Arnoglossus imperialis	Vernacular
# ‎Higher classification: Family <- Bothidae (expect one species. Next higher is Pleuronectiformes, aka flatfish, which is already included)

Brose_mod2 <- function(x) {
  x <- gsub("Flatfish","Pleuronectiformes",x)
  x <- gsub("Goby","Gobiidae",x)
  x <- gsub("Hatchet","Sternoptychidae",x)
  x <- gsub("Rockling","Lotidae",x)
  x <- gsub("Sandeel","Ammodytidae",x)
  x <- gsub("Scaldfish","Bothidae",x)
  x <- gsub("Micromesistius potassou","Micromesistius poutassou",x)
  x <- gsub("Clupeoid","Clupeidae",x)
  x <- gsub("Gadoid","Gadidae",x)
  x <- gsub("Myctophid","Myctophidae",x)
}

colnames(Brose2005[[2]][[1]]) <- Brose_mod2(colnames(Brose2005[[2]][[1]]))
rownames(Brose2005[[2]][[1]]) <- Brose_mod2(rownames(Brose2005[[2]][[1]]))

#Combining duplicated rows and columns in food web matrix
#see duplicate_row_col.r for function detail
  Brose2005[[2]][[1]] <- dupl_sp(Brose2005[[2]][[1]])

tx.list <- unique(c(colnames(Brose2005[[2]][[1]]),rownames(Brose2005[[2]][[1]])))

#----------------------------------------------------------------------------------------------------------
#Name resolver - evaluate name validity
#----------------------------------------------------------------------------------------------------------
tx.res <- taxo_resolve(tx.list) #voir script taxo_resolve.r
tx.valid <- taxo_valid(tx.res,tx.list,res.check=TRUE) #voir script taxo_valid.r -- no result; valid; invalid
#Manually verify results from taxo_valid()

#----------------------------------------------------------------------------------------------------------
#   Diet matrix
#----------------------------------------------------------------------------------------------------------
#Extend diet matrices to include the full spectrum of interactions. At this point species that are only resource are found only as rows, same for consumers as columns
Brose2005[[2]][[1]] <- Diet_mat(Brose2005[[2]][[1]]) #See diet_mat_extend.r for full function

#----------------------------------------------------------------------------------------------------------
#   Extract binary interactions from diet matrix
#----------------------------------------------------------------------------------------------------------
Brose2005[[2]][[4]] <- bin_inter(Brose2005[[2]][[1]])

#----------------------------------------------------------------------------------------------------------
#   Extract taxon list for further analyses
#----------------------------------------------------------------------------------------------------------
Brose2005[[2]][[5]] <- as.data.frame(matrix(nrow=length(tx.list),ncol=2,data=NA))
colnames(Brose2005[[2]][[5]]) <- c("taxon","rank")
rownames(Brose2005[[2]][[5]]) <- tx.list
Brose2005[[2]][[5]][,1] <- tx.list
Brose2005[[2]][[5]][,2] <- tx.valid[,3]

no.result <- which(is.na(Brose2005[[2]][[5]][,2]) == TRUE)
multi.tx <- which(str_detect(Brose2005[[2]][[5]][,2]," - ") == TRUE)

verif <- c(no.result,multi.tx)
verif <- verif[order(verif)]

# Manual verification of missing ranks
Brose2005[[2]][[5]][verif,]

species <- c("Clupea harengus","Lepidorhombus whiffiagonis")
family <- c("Lotidae")
not.appl <- c("Fish")

Brose2005[[2]][[5]][paste(species),2] <- "species"
Brose2005[[2]][[5]][paste(family),2] <- "family"
Brose2005[[2]][[5]][paste(not.appl),2] <- "NA"

names(Brose2005) <- paste("Brose2005 - ",names(Brose2005),sep = "")

save(x=Brose2005,file="RData/brose2005.RData")
