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
#      2.3 GlobalWeb data http://globalwebdb.com
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# FILES:
#   RawData[1:93] <- file = "RawData/Web/WEB[1:93].csv"
#   RawData94 <- file = "RawData/Web/GlobalWeb/Web_list.txt"
#   RData1 <- file = "RData/GlobalWeb.RData"
#   Script  <- file = "Script/2-3_Emp_Web.r"
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# NOTES:
#   Reference:
#     Cite individual references for each food web
#
#   ECOWeB database:
#     Webs 1 - 213 were compiled by Cohen J.E. (2010) and are freely available
#     on http://digitalcommons.rockefeller.edu/cohen_joel_laboratory/1/ or on
#     http://hdl.handle.net/10209/306
#
#     Data:
#       file = "RawData/Web/GlobalWeb/ECOWeB/"
#
#     Reference:
#       "Cohen, J. E. (compiler) 2010 [or year in which your version of the data
#       base was prepared] Ecologists' Co-Operative Web Bank. Version 1.1
#       [or version number specified in README.txt].  Machine-readable data
#       base of food webs.  New York: The Rockefeller University."
#
#     Terms and conditions:
#       If used, this compilation of dataset must be cited and an agreement of
#       the terms and conditions must be signed and send to J.E. Cohen. See
#       "RawData/Web/ECOWeB/README.txt" for more information.
#
#     Raw data description:
#       See "RawData/Web/ECOWeB/README.txt" for destails on the structure of the
#       data compiled by Cohen J.E. (2010)
#
#     Diet matrix:
#       In some webs, an entry of -1 or -2 results from a coding scheme of
#       Cohen (1978).  All -1 entries should be interpreted as 1 (these
#       represent links inferred from the descriptive text) and all -2 entries
#       should be interpreted as 0.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# LIBRARIES:
library(stringr)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# SCRIPT
# -----------------------------------------------------------------------------
web.list <- read.table(file="RawData/Web/GlobalWeb/Web_list.txt",header = FALSE,sep="\t",quote = "")
wls <- matrix(ncol=5,nrow=nrow(web.list)/5,data=NA)
  for(j in seq(1,nrow(web.list),by=5)){
    wls[((j+4)/5),1] <- paste(web.list[j,1])
    wls[((j+4)/5),2] <- paste(web.list[(j+1),1])
    wls[((j+4)/5),3] <- paste(web.list[(j+2),1])
    wls[((j+4)/5),4] <- paste(web.list[(j+3),1])
    wls[((j+4)/5),5] <- paste(web.list[(j+4),1])
  }
web.list <- wls
remove(wls)
web.list[,4] <- gsub("\"","",web.list[,4])

#Selecting appropriate food webs
web.list <- subset(web.list,
  web.list[,3] == "Estuary" |
  web.list[,3] == "Estuary - salt marsh" |
  web.list[,3] == "Salt marsh" |
  web.list[,3] == "Estuary - marine" |
  web.list[,3] == "Marine" |
  web.list[,3] == "Marine sublittoral" |
  web.list[,3] == "Upwelling areas" |
  web.list[,3] == "Intertidal zone" |
  web.list[,3] == "Estuary-marine" |
  web.list[,3] == "Deep sea" |
  web.list[,3] == "Rocky shore"  |
  web.list[,3] == "Mangrove swamp" |
  web.list[,3] == "Mudflats" |
  web.list[,3] == "Rockpool" |
  web.list[,3] == "Coast")

#Importing food web raw data
GlobalWeb <- vector("list",nrow(web.list))
ModNames <- rep(NA,nrow(web.list))
for(i in 1:nrow(web.list)){ModNames[i] <- gsub(" ","",paste("WEB",web.list[i,1]))}
names(GlobalWeb) <- ModNames
for(i in 1:nrow(web.list)){
  GlobalWeb[[i]] <- vector("list", 5)
  names(GlobalWeb[[i]]) <- c(gsub(" ","",paste("WEB",web.list[i,1])),"Reference","Notes","Interactions","Species")
  GlobalWeb[[i]][[1]] <- read.csv(file=gsub(" ","",paste("RawData/Web/GlobalWeb/WEB",web.list[i,1],".csv")),header = FALSE,sep=",",quote = "",skip=1)
  GlobalWeb[[i]][[2]] <- c("GlobalWeb",web.list[i,4])
}

for(k in 1:length(GlobalWeb)){
  GlobalWeb[[k]][[1]] <- as.matrix(GlobalWeb[[k]][[1]])
}

#MANUAL clean up of raw data
  #GlobalWeb c(WEB320;WEB321;WEB322;WEB323;WEB324;WEB333) - Original files were modified to remove "," in species names. Changes taken into account when tables are imported
  #GlobalWeb c(WEB257;WEB258;WEB259;WEB260) - Amphitoe changed for Ampithoe
  #GlobalWeb$WEB41 - Duplicate row and columns names for tuna, modified in original file
  #GlobalWeb$WEB253 - Removed Miscelaneous
  #Matrix structure (consumer as columns & prey as rows)
  GlobalWeb$WEB7$WEB7 <- GlobalWeb$WEB7$WEB7[1:17,]#substracting empty rows
  GlobalWeb$WEB320$WEB320 <- GlobalWeb$WEB320$WEB320[1:78,]#substracting rows 79:81 because they are not species
  GlobalWeb$WEB321$WEB321 <- GlobalWeb$WEB321$WEB321[1:78,]#substracting rows 79:81 because they are not species
  GlobalWeb$WEB322$WEB322 <- GlobalWeb$WEB322$WEB322[1:78,]#substracting rows 79:81 because they are not species
  GlobalWeb$WEB323$WEB323 <- GlobalWeb$WEB323$WEB323[1:78,]#substracting row 79 because not species
  GlobalWeb$WEB324$WEB324 <- GlobalWeb$WEB324$WEB324[1:78,]#substracting rows 79:81 because they are not species
  GlobalWeb$WEB324$WEB324 <- GlobalWeb$WEB324$WEB324[1:78,]#substracting row 79:81 because they are not species
  GlobalWeb$WEB341$WEB341 <- GlobalWeb$WEB341$WEB341[1:30,]#substracting row 30 because not species
  GlobalWeb$WEB290$WEB290 <- GlobalWeb$WEB290$WEB290[,c(-6)]#substracting column 6 (CIRR) because not in original article
  GlobalWeb$WEB290$WEB290 <- t(GlobalWeb$WEB290$WEB290)#transpose predators as columns
  GlobalWeb$WEB291$WEB291 <- t(GlobalWeb$WEB291$WEB291)#transpose predators as columns
  GlobalWeb$WEB292$WEB292 <- t(GlobalWeb$WEB292$WEB292)#transpose predators as columns
  GlobalWeb$WEB293$WEB293 <- t(GlobalWeb$WEB293$WEB293)#transpose predators as columns
  GlobalWeb$WEB37$WEB37 <- gsub("-1","1",as.matrix(GlobalWeb$WEB37$WEB37))#See Cohen (2010) for explanation on - values


  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        #GlobalWeb$WEB342$WEB342 - À vérifier et uniformiser!
        #Trouver la référence et vérifier la matrice d'alimentation
        GlobalWeb$WEB342$WEB342 <- t(GlobalWeb$WEB342$WEB342)#transpose predators as columns
        #----------------------------------------------------------------------------------------------------------
        #GlobalWeb$WEB333$WEB333 has  "-", "+" & "+/-".
        #For now I will consider "-", "+" & "+/-" as simple predatory interactions, but this needs to be validated
        #"+" means that the taxon row causes an increase in the taxon column, meaning it acts as a resource
        #"-" means that the taxon row causes a decrease in the taxon column, meaning it acts as a consumer
        #"+/-" means that the taxon row both causes an increase and decrease in the taxon column, meaning it acts as both consumer and resource
        #The diet matrix is then transposed (pred as column, prey as row); "+" = 0; "-" = 1; "+/-" = 1. Detritus row has to be at 0 as well (all "+" in matrix)
        GlobalWeb$WEB333$WEB333 <- t(GlobalWeb$WEB333$WEB333)#transpose predators as columns
        GlobalWeb$WEB333$WEB333 <- gsub("\\+/-","1",as.matrix(GlobalWeb$WEB333$WEB333))
        GlobalWeb$WEB333$WEB333 <- gsub("\\+","0",as.matrix(GlobalWeb$WEB333$WEB333))
        GlobalWeb$WEB333$WEB333[2:82,2:82] <- gsub("\\-","1",as.matrix(GlobalWeb$WEB333$WEB333[2:82,2:82]))
        GlobalWeb$WEB333$WEB333[82,2] <- "0"#Detritus does not consume man
        #----------------------------------------------------------------------------------------------------------
        #GlobalWeb$WEB266$WEB266 has "<" in the matrix
        GlobalWeb$WEB266$WEB266 <-gsub("<","",as.matrix(GlobalWeb$WEB266$WEB266))
        #----------------------------------------------------------------------------------------------------------
        #GlobalWeb$WEB350$WEB350 has duplicated rows (ostracods and polychaetes). I want to merge them.
        #Plus, the original article does not seem to reflect the food web from the GlobalWeb database
        GlobalWeb$WEB350$WEB350[9,12] <- 1 #Add interaction for ostracod to remove duplicated row
        GlobalWeb$WEB350$WEB350[7,11] <- 1 #Add interaction for polychaetes to remove duplicated row
        GlobalWeb$WEB350$WEB350 <- GlobalWeb$WEB350$WEB350[c(-14,-15),] #remove duplicated row
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  #----------------------------------------------------------------------------------------------------------

    # Adjusting species names from web 267
    Names_change <- function(x){
      # Name resolve #2
      x <- str_trim(x, side="both") #remove spaces
      x <- tolower(x)
      x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
      x <- paste(x," ")
      x <- gsub("Scianids ","Cynoscion xanthulum ",x)
      x <- gsub("Elopids ","Elops affinis ",x)
      x <- gsub("Lutjanids ","Lutjanus novemfasciatus - Lutjanus argentiventris ",x)
      x <- gsub("Carangids ","Caranx hippos ",x)
      x <- gsub("Centropomids ","Centropomus robalito - Centropomus nigrescens ",x)
      x <- gsub("Ariids ","Arius guatemalensis ",x)
      x <- gsub("Haemulids ","Anisotremus interreptus - Haemulon sexfasciatum ",x)
      x <- gsub("Pleuronectoids ","Cynoglossus zanzibarensis ",x)
      x <- gsub("Callinectes ","Callinectes arcuatus ",x)
      x <- gsub("Belonoids ","Hemiramphus balao - Hemiramphus brasilienis ",x)
      x <- gsub("Clupeoids ","Atherinomuros stipes - Engraulis mordax - Dorosoma petense ",x)
      x <- gsub("Gerreids ","Diapterus peruvians ",x)
      x <- gsub("Gobioids ","Dormitator latrifons ",x)
      x <- gsub("Mugilids ","Mugil cephalus ",x)
      x <- gsub("Palaemonids ","Macrobranchium ",x)
      x <- gsub("Litopenaeus ","Penaeus ",x)
      x <- gsub("Chanids ","Chanos chanos ",x)
      x <- str_trim(x, side="both") #remove spaces
      x <- tolower(x)
      x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
      return(x)
    }
    GlobalWeb$WEB267[[1]][,1] <- Names_change(GlobalWeb$WEB267[[1]][,1])
    GlobalWeb$WEB267[[1]][1,] <- Names_change(GlobalWeb$WEB267[[1]][1,])
  #----------------------------------------------------------------------------------------------------------

  # --------------------------- CHANGING WEB333 --------------------------------

  Names_change <- function(x){ # WEB333
    # Name resolve #2
    x <- str_trim(x, side="both") #remove spaces
    x <- tolower(x)
    x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
    x <- paste(x," ")
    x <- gsub("Redfish ","Sebastes fasciatus ",x)
    x <- gsub("Sea robins ","Triglidae ",x)
    x <- str_trim(x, side="both") #remove spaces
    x <- tolower(x)
    x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
  return(x)
  }
  GlobalWeb$WEB333[[1]][,1] <- Names_change(GlobalWeb$WEB333[[1]][,1])
  GlobalWeb$WEB333[[1]][1,] <- Names_change(GlobalWeb$WEB333[[1]][1,])
  #----------------------------------------------------------------------------------------------------------

  # --------------------------- CHANGING WEB339 --------------------------------

  Names_change <- function(x){ # WEB339
    # Name resolve #2
    x <- str_trim(x, side="both") #remove spaces
    x <- tolower(x)
    x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
    x <- paste(x," ")
    x <- gsub("Redfish ","Centroberyx affinis ",x)
    x <- str_trim(x, side="both") #remove spaces
    x <- tolower(x)
    x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
  return(x)
  }
  GlobalWeb$WEB339[[1]][,1] <- Names_change(GlobalWeb$WEB339[[1]][,1])
  GlobalWeb$WEB339[[1]][1,] <- Names_change(GlobalWeb$WEB339[[1]][1,])

  #----------------------------------------------------------------------------------------------------------

  # --------------------------- CHANGING WEB288 --------------------------------
  Names_change <- function(x){ # WEB288
  x <- tolower(x)
  x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")

  x <- paste(x," ")
  x <- gsub("Epiphyte grazing amphipods ","Acunmindeutopus naglei - Ampithoe longimana - Caprella penantis - Cymadusa compta - Lembos rectangularis - Batea catharinensis - Elasmopus levis - Melita - Synchelidium - Listriella barnardi - Lyssianopis alba ",x)
  x <- gsub("Suspension-feeding molluscs ","Brachiodontes exustus - Chione cancellata - Argopecten irradians - Bivalvia - Crepidula fornicata - Crepidula convexa ",x)
  x <- gsub("Hermit crabs ","Pagurus - Pagurus mcglaughlini ",x)
  x <- gsub("Spider crabs (herbivores) ","Libinia dubia ",x)
  x <- gsub("Omnivorous crabs ","Neopanope texana - Pinixia floridana ",x)
  x <- gsub("Blue crabs ","Callinectes sapidus ",x)
  x <- gsub("Isopods ","Erichsionella - Paracerces caudata - Edotea triloba ",x)
  x <- gsub("Brittle stars ","Ophioderma brevispinum ",x)
  x <- gsub("Deposit feeding peracaridan crustraceans ","Ampelisca - Gammarus mucronatus - Cerapus tubularis - Corophium - Crustacea - Cumacea - Tanaeid - Ostracoda	- Mysidopsis ",x)
  x <- gsub("Herbivorous shrimp ","Hippolyte zostericola - Alpheus normani ",x)
  x <- gsub("Predatory shrimps ","Palaemonetes floridanus - Penaeus duorarum - Processa bermudensis ",x)
  x <- gsub("Catfish and stingrays ","Dasyatis sabina - Arius felis ",x)
  x <- gsub("Tonguefish ","Symphurus plagisua ",x)
  x <- gsub("Flounder and needlefish ","Paralichthys albigutta - Strongylura marina ",x)
  x <- gsub("Southern hake and sea robins ","Urophycis floridana - Prionotus scitulus - Prionotus tribulus ",x)
  x <- gsub("Southern hake and searobins ","Urophycis floridana - Prionotus scitulus - Prionotus tribulus ",x)
  x <- gsub("Atlantic silversides and bay anchovy ","Menidia beryllina - Anchoa mitchelli ",x)
  x <- gsub("Killifishes ","Fundulus similis - Fundulus confluentus - Adinia xenica ",x)
  x <- gsub("Gobies and blennies ","Microgobius gulosus - Gobiosoma robustum ",x)
  x <- gsub("Pinfish ","Lagodon rhomboides ",x)
  x <- gsub("Spot ","Leiostomus xanthurus ",x)
  x <- gsub("Pipefish and seahorses ","Hippocampus zosterae - Syngnathus scovelli ",x)
  x <- gsub("Red drum ","Sciaenops ocellatus ",x)
  x <- gsub("Deposit-feeding gastropods ","Acetocina candei - Swartziella catesbyana - Cadulus carolinesis - Haminoea succinea - Acteon punctostriatus - Olivella mutica - Truncatella pulchella - Nassarius vibex ",x)
  x <- gsub("Predatory gastropods ","Urosalpinx perrugata - Nudibranchia - Opalia hotessieriana - Epitonium albidum - Terebra - Polinices - Busycon spiratum - Turbonilla dalli - Turbonilla hemphilli - Prunum apicinum - Prunum bellulum - Prunum aureocincta - Natica pusilla - Hylina veliei - Acanthocitona pygmaea - Odostomia seminuda - Seila adamsi ",x)
  x <- gsub("Epiphyte-grazing gastropods ","Cerithium lutosum - Mitrella lunata - Solariella lamellosa - Anachis avara ",x)
  x <- gsub("Other gastropods ","Mangelia plicosa - Hylina veliei - Jaspidella jaspidea ",x)
  x <- gsub("Deposit-feeding polychaetes ","Aricidea - Capitellidae - Cirratulidae - Maldanidae - Orbiniidae - Paraonidae - Pectanaridae - Syllidae - Amphitritidae - Spionidae ",x)
  x <- gsub("Predatory polychaetes and nemertines ","Glyceridae - Nereidae - Onuphidae - Hesionidae - Nemertines ",x)
  x <- gsub("Suspension-feeding polychaetes ","Serpulidae - Sabellidae ",x)
  x <- gsub("Zooplankton ","Acartia tonsa - Foraminifera - Harpacticoida - Nematoda - Polychaeta - Pycnogonidae ",x)
  x <- gsub("Benthos-eating birds ","Rallus longirostris - Bucephala albeaola - Charadrius semipalmatus ",x)
  x <- gsub("Fish-eating birds ","Casmerodius albus - Gavia immer - Ardea herodias - Hydranassa tricolor - Mergus serrator - Phalacrocorax carbo - Megaceryle alcyon ",x)
  x <- gsub("Fish and crustacean eating birds ","Lophodytes cucullatus - Catoptrophorus semipalmatus - Tringa melanoleuca ",x)
  x <- gsub("Gulls and Terns ","Sterna forsteri - Larus atricilla - Larus argentatus - Larus delawarensis ",x)
  x <- gsub("Raptors ","Haliaeetus leucocephalus - Circus cyaneus ",x)
  x <- gsub("Herbivorous ducks ","Anas discors ",x)
  x <- gsub("Seagrass ","Halodule wrightii ",x)
  x <- str_trim(x, side="both") #remove spaces
  x <- tolower(x)
  x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
  return(x)
  }
  GlobalWeb$WEB288[[1]][,1] <- Names_change(GlobalWeb$WEB288[[1]][,1])
  GlobalWeb$WEB288[[1]][1,] <- Names_change(GlobalWeb$WEB288[[1]][1,])
  # ------------------------------------------------------------------------------------------
  # --------------------------- CHANGING WEB265 --------------------------------

  Names_change <- function(x){ # WEB265
    # Name resolve #2
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
  }
  GlobalWeb$WEB265[[1]][,1] <- Names_change(GlobalWeb$WEB265[[1]][,1])
  GlobalWeb$WEB265[[1]][1,] <- Names_change(GlobalWeb$WEB265[[1]][1,])
  #--------------------------------------------------------------------------------------------------------
  # --------------------------- CHANGING WEB8 & 122 --------------------------------

  Names_change <- function(x){ # WEB8 & 122
    # Name resolve #2
    x <- str_trim(x, side="both") #remove spaces
    x <- tolower(x)
    x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
    x <- paste(x," ")
    x <- gsub("Shrimps ","Paleomonetes pugio ",x)
    x <- gsub("Shrimp ","Paleomonetes pugio ",x)
    x <- str_trim(x, side="both") #remove spaces
    x <- tolower(x)
    x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
  return(x)
  }
  GlobalWeb$WEB8[[1]][,1] <- Names_change(GlobalWeb$WEB8[[1]][,1])
  GlobalWeb$WEB8[[1]][1,] <- Names_change(GlobalWeb$WEB8[[1]][1,])
  GlobalWeb$WEB122[[1]][,1] <- Names_change(GlobalWeb$WEB122[[1]][,1])
  GlobalWeb$WEB122[[1]][1,] <- Names_change(GlobalWeb$WEB122[[1]][1,])
  #--------------------------------------------------------------------------------------------------------


  #Transforming interactions as binary values (0-1)
  for(k in 1:length(GlobalWeb)){
    for(i in 2:nrow(GlobalWeb[[k]][[1]])){
      for(j in 2:ncol(GlobalWeb[[k]][[1]])){
        GlobalWeb[[k]][[1]] <- as.matrix(GlobalWeb[[k]][[1]])
        if(GlobalWeb[[k]][[1]][i,j] == "0") {NULL} else {GlobalWeb[[k]][[1]][i,j] <- "1"}
      }
    }
  }

  # Colnames and rownames with species names
  for(k in 1:length(GlobalWeb)){
    rownames(GlobalWeb[[k]][[1]]) <- GlobalWeb[[k]][[1]][, 1]
    colnames(GlobalWeb[[k]][[1]]) <- GlobalWeb[[k]][[1]][1, ]
    GlobalWeb[[k]][[1]] <- GlobalWeb[[k]][[1]][2:nrow(GlobalWeb[[k]][[1]]), 2:ncol(GlobalWeb[[k]][[1]])]
  }

  # Extend diet matrices to include the full spectrum of interactions. At this point species that are only resource are found only as rows, same for consumers as columns
  for(k in 1:length(GlobalWeb)){
    GlobalWeb[[k]][[1]] <- Diet_mat(GlobalWeb[[k]][[1]])
  }

  #Taxon names
    #Adjusting species names from GlobalWeb$WEB248 - Species names are not correctly inputed. There might be other lists like this that hold more species identification, but as I noticed this one I decided to clean it up with species described in the original article.
      rownames(GlobalWeb$WEB248[[1]]) <- tolower(rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("anthopleura","anthopleura aureoradiata",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("abarenicola","abarenicola affinis",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("capitellid","capitellidae",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("heteromastus","heteromastus filiformis",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("paraonid","paraonis",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("scoloplos-john","scoloplos johnstonei",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("macroclymenella","macroclymenella stewartensis",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("hesionid","hesionidae",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("exogone-hetero","exogone heterosetosa",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("perinereis","perinereis nuntia",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("boccardia-syrtis","boccardia syrtis",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("boccardia-acus","boccardia acus",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("colurostylis-lemu","colurostylis lemurum",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("diastylopsis-thil","diastylopsis thileniusi",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("tanaidacid","tanaidacidae",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("heterophoxus-steph","heterophoxus stephenseni",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("phoxocephalus-reg","phoxocephalus regium",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("pontharpinia-aust","pontharpinia australis",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("cymodopsis","cymodopsis montis",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("callianassa","callianassa filholi",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("heterosquilla","heterosquilla tricarinata",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("macrophthalmus","macrophthalmus hirtipes",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("hemigrapsus cre","hemigrapsus crenulatus",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("hemigrapsus edw","hemigrapsus edwardsii",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("helmsi","notoacmea helmsi",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("diloma","diloma subrostrata",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("zeacumantus","zeacumantus subcarinatus",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("pyramidellid","pyramidellidae",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("cominella","cominella glandiformis",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("nucula","nucula dunedinensis",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("perrierina","perrierina turneri",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("macomona","macomona liliana",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("austrovenus","austrovenus stutchburyi",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("spotty","notolabrus celidotus",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("barracuta","thyrsites atun",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("oystercatcher 1","haematopus ostralegus",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("oystercatcher 2","haematopus unicolor",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("vanellus","vanellus miles",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("bbgull","larus dominicanus",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("rbgull","larus novaehollandiae",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("wfheron","ardea novaehollandiae",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("wftern","sterna striata",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("mallard","anas platyrhynchos",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("maritrema novae","maritrema novaezealandensis",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("curtuteria 1","curtuteria australis",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("curtuteria 2","curtuteria australis",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("profilicollis anta","profilicollis antarcticus",rownames(GlobalWeb$WEB248[[1]]))
      rownames(GlobalWeb$WEB248[[1]]) <- gsub("profilicollis novae","profilicollis novaezelandensis",rownames(GlobalWeb$WEB248[[1]]))
      colnames(GlobalWeb$WEB248[[1]]) <- rownames(GlobalWeb$WEB248[[1]])


  for(k in 1:length(GlobalWeb)){
    rownames(GlobalWeb[[k]][[1]]) <- tolower(rownames(GlobalWeb[[k]][[1]]))
    rownames(GlobalWeb[[k]][[1]]) <- gsub("^.*?: ","",rownames(GlobalWeb[[k]][[1]])) #Prefix for taxon names followed by ":"
    rownames(GlobalWeb[[k]][[1]]) <- gsub("sp\\.","",rownames(GlobalWeb[[k]][[1]])) #Removing sp. from genus
    rownames(GlobalWeb[[k]][[1]]) <- gsub("spp\\.","",rownames(GlobalWeb[[k]][[1]]))  #Removing spp. from genus
    rownames(GlobalWeb[[k]][[1]]) <- gsub("\\'","",rownames(GlobalWeb[[k]][[1]]))

    #Extend species name in full
      rownames(GlobalWeb[[k]][[1]]) <- gsub("l\\.\\ aurata","liza aurata",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("l\\.\\ yokohamae","limanda yokohamae",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("e\\.\\ pacifica","euphausia pacifica",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("d\\.\\ elucens","diaphus elucens",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("c\\.\\ microtretus","chthamalus microtretus",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("b\\.\\ eburneus","balanus eburneus",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("l\\.\\ obtusata","littorina obtusata",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("l\\.\\ saxatilis","littorina saxatilis",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("n\\.\\ plumchrus","neocalanus plumchrus",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("z\\.\\ marina","zostera marina",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("o\\.\\ orca","orcinus orca",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("p\\.\\ macrocephalus","physeter macrocephalus",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("b\\.\\ acutorostrata","balaenoptera acutorostrata",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("l\\.\\ weddellii","leptonychotes weddellii",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("o\\.\\ rossii","ommatophoca rossii",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("p\\.\\ longirostris","	parapenaeus longirostris",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("a\\.\\ gazella","archtocephalus gazella",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("l\\.\\ carcinophagus","lobodon carcinophagus",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("t\\.\\ spinifera","thysanoessa spinifera",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("e\\.\\ superba","euphausia superba",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("c\\.\\lucasii","cyllopus lucasii",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("s\\.\\ gazellae","sagitta gazellae",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("s\\.\\ thompsoni","salpa thompsoni",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("b\\.\\ picta","brachioteuthis picta",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("b\\.\\ antarcticus","bathylagus antarcticus",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("e\\.\\ antarctica","electrona antarctica",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("g\\.\\ braueri","gymnoscopelus braueri",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("p\\.\\ bolini","protomyctophum bolini",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("p\\.\\ tenisoni","protomyctophum tenisoni",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("a\\.\\ longimana","ampithoe longimana",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("a\\.\\ valida","ampithoe valida",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("c\\.\\ penantis","caprella penantis",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("p\\.\\ vulgaris","palaemonetes vulgaris",rownames(GlobalWeb[[k]][[1]]))

    #Removing parentheses
      rownames(GlobalWeb[[k]][[1]]) <- gsub("juv\\.","juvenile",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("gadidae \\(cod haddock pollock)","cod - haddock - pollock",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("hexagrammidae \\(lingcod greenling)","lingcod - greenling",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("pelagic shrimp \\(sergestidae panaeidae)","sergestidae - penaeidae",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("polychaetes \\(tomopteris  - pelagagobia )","tomopteris - pelagagobia",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("copepods \\(neocalanus cristatus - neocalanus plumchrus)","neocalanus cristatus - neocalanus plumchrus",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("cnidarians \\(hydromedusae - scyphomedusae)","hydromedusae - scyphomedusae",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("pteropods \\(limacina helicina - clionidae)","limacina helicina - clionidae",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("cephalopods \\(loligo opalescens)","loligo opalescens",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("ducks \\(lesser scaup)","lesser scaup",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("ctenophores \\(beroe )","beroe",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("flatfish \\(water-column feeders)","flatfish water-column feeder",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("flatfish \\(benthic feeders)","flatfish benthic feeder",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("flatfish \\(small)","small flatfish",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("other epibenthic shrimp \\(caridea)","caridea",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("epifauna \\(suspension feeders)","suspension feeding epifauna",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("epifauna \\(carnivorous)","carnivorous epifauna",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub(" \\(.*?\\)","",rownames(GlobalWeb[[k]][[1]]))

    #Removing/modifying life stages, pre- sufixes, size classes, etc
      rownames(GlobalWeb[[k]][[1]]) <- gsub("adult","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("juvenile","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("larvae","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("type","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("sepia officinalis.*$","sepia officinalis",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub(" 1","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub(" 2","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("other","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("iv-vi copepodites of calanus and pseudocalanus","calanus - pseudocalanus",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("iv-vi copepodites of paracalanus","paracalanus",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("aggregate","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("ii-iii copepodites","copepod",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("intertidal and marine invertebrates","intertidal invertebrates - marine invertebrates",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("killer whale.*$","killer whale",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("chinook.*$","chinook",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("coho.*$","coho",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("coho","coho salmon",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("tuna1","tuna",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("tuna2","tuna",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("chinook","chinook salmon",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("nauplii of copepoda","copepod",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("small-size","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("smaller","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("small","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("medium-sized","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("medium-size","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("medium","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("large-sized","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("large-size","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("larger","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("large","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub(" eating","-eating",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub(" feeding","-feeding",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("marine pom.*$","pom",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("from.*$","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("benthonic","benthic",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("commercial sparids","sparids",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("migrant","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("unid\\.","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("misc\\.","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("unidentified","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("meiofaunal-size","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("of ","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("copepod nauplii","copepod",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("copepodites","copepod",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("hyperiid amphipod","hyperiid",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("hyperiids","hyperiid",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("hyperids","hyperiid",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("sized","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("terrestrial pom","pom",rownames(GlobalWeb[[k]][[1]]))

    #Modify species names manually for mistakes or simple changes
      rownames(GlobalWeb[[k]][[1]]) <- gsub("copepods","copepod",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("copepoda","copepod",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("copepod","copepoda",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("thais sp","thais",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("wolfish","wolffish",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("bacteria and fungi associated with detritus","bacteria - fungi",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("benthic amphipods isopods and cumaceans","benthic amphipod - benthic isopod - benthic cumacean",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("benthic-diatoms","benthic diatom",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("dinoflaggelates","dinoflagellates",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("encrusting and mat-forming algae","encrusting algae - mat-forming algae",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("euphausid","euphausiid",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("molt debris","",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("fish and crustacean-eating birds","fish- crustacean-eating birds",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("harbour seal","harbor seal",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("polychates","polychaete",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("meso and microzooplankton","mesozooplankton - microzooplankton",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("seariesia","searlesia",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("hms billfish","billfish",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("hms scombrids","scombrid",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("hms sharks","shark",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("micro- and meio-benthos","microbenthos - meiobenthos",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("micro-","micro",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("macro-","macro",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("microflagellates and perhaps bacteria","microflafellates",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("oyster catcher","oystercatcher",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("trachiurus japonica","trachurus japonicus",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("lh sculpin","longhorn sculpin",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("clupeid fishes","clupeidae",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("clupeid fish","clupeidae",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("crago","crago septemspinosus",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("gammaridean amphipod","gammaridea",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("brittle stars","brittlestars",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("mollusks","molluscs",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("macrofaunal-size nematodes","nematodes",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("lobsters","lobster",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("cephalopodes","cephalopod",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("cephalopods","cephalopod",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("caprellid amphipod","caprellids",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("burrowing ephemerids" ,"ephemerids",rownames(GlobalWeb[[k]][[1]]))

    #Removing plural to remove duplicate names
      rownames(GlobalWeb[[k]][[1]]) <- gsub("gulls","gull",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("terns","tern",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("fishes","fish",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("clams","clam",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("mussels","mussel",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("clupeids","clupeid",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("hake","hakes",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("harbor seals","harbor seal",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("herons","heron",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("herrings","herring",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("horse mackerels","horse mackerel",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("imports","import",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("tunas","tuna",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("shrimps","shrimp",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("octopuses","octopus",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("polychaetes","polychaete",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("mullets","mullet",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("mackerels","mackerel",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("leopard seals","leopard seal",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("amphipods","amphipod",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("mesopelagics","mesopelagic",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("sharks","shark",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("crabeater seals","crabeater seal",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("anchovies","anchovy",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("blue crabs","blue crab",rownames(GlobalWeb[[k]][[1]]))
      rownames(GlobalWeb[[k]][[1]]) <- gsub("stingrays","stingray",rownames(GlobalWeb[[k]][[1]]))

    rownames(GlobalWeb[[k]][[1]]) <- str_trim(rownames(GlobalWeb[[k]][[1]]), side="both") #remove spaces
    colnames(GlobalWeb[[k]][[1]]) <- rownames(GlobalWeb[[k]][[1]])
  }

#Dividing multiple taxon name strings in single row
  #Multiple names in a single cell for a diet matrix, divided by " - ", " and "
  for(k in 1:length(GlobalWeb)){
    rownames(GlobalWeb[[k]][[1]]) <- gsub(" & "," - ",rownames(GlobalWeb[[k]][[1]]))
    rownames(GlobalWeb[[k]][[1]]) <- gsub(" and "," - ",rownames(GlobalWeb[[k]][[1]]))
    rownames(GlobalWeb[[k]][[1]]) <- gsub("\\/"," - ",rownames(GlobalWeb[[k]][[1]]))
    rownames(GlobalWeb[[k]][[1]]) <- gsub("  "," ",rownames(GlobalWeb[[k]][[1]]))
    colnames(GlobalWeb[[k]][[1]]) <- rownames(GlobalWeb[[k]][[1]])
  }

#Combining duplicated rows and columns
  for(k in 1:length(GlobalWeb)){
    GlobalWeb[[k]][[1]] <- dupl_sp(GlobalWeb[[k]][[1]])
  }

#Dividing rows or columns with multiple taxon
  dup.fw <- NA
  for(k in 1:length(GlobalWeb)){
    x <- which(str_detect(rownames(GlobalWeb[[k]][[1]])," - ") == TRUE) #Identify rows that should be Duplicated
    if(length(x) == 0) {NULL} else {dup.fw <- c(dup.fw,k)}
  }
  dup.fw <- dup.fw[-1]

  for(k in dup.fw) {
    GlobalWeb[[k]][[1]] <- dupl_row(GlobalWeb[[k]][[1]],rownames(GlobalWeb[[k]][[1]]))
    GlobalWeb[[k]][[1]] <- dupl_col(GlobalWeb[[k]][[1]],colnames(GlobalWeb[[k]][[1]]))
}

for(i in 1: length(GlobalWeb)){
  if(identical(colnames(GlobalWeb[[i]][[1]]),rownames(GlobalWeb[[i]][[1]])) == FALSE) {stop("les matrices n'ont pas les mêmes noms de colonnes et de lignes")
  } # else {
  #   print("tout est ok")
  # }
}

#----------------------------------------------------------------------------------------------------------
#Name resolver - evaluate taxon name validity - 2nd round of revisions
#----------------------------------------------------------------------------------------------------------
Names_change <- function(x){
  # Name resolve #1
  x <- str_trim(x, side="both") #remove spaces
  x <- tolower(x)
  x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
  x <- paste(x," ")
  x <- gsub("Man ","Homo sapiens",x)
  x <- gsub("Hyporhampus ","Hyporhamphus ",x)
  x <- gsub("Lamya ","Arcuatula ",x)
  x <- gsub("Primary producers ","Phytoplankton ",x)
  x <- gsub("Marsh spiders ","Araneae ",x)
  x <- gsub("Planktonic bacteria ","Bacteria",x)
  x <- gsub("Primary producers ","Phytoplankton ",x)
  x <- gsub("Sediment bacteria ","Bacteria ",x)
  x <- gsub("Acorn barnacles ","Sessilia ",x) #order
  x <- gsub("Adelie penguins ","Pygoscelis adeliae ",x)
  x <- gsub("Benthic bacteria ","Bacteria ",x)
  x <- gsub("Allochertes ptilocerus ","Parhyale ptilocerus ",x)
  x <- gsub("Amphipod ","Amphipoda ",x)
  x <- gsub("Benthic amphipod ","Amphipoda",x)
  x <- gsub("Epiphyte-grazing amphipod ","Amphipoda ",x)
  x <- gsub("Pelagic amphipod ","Amphipoda ",x)
  x <- gsub("Alcids ","Alcidae ",x)
  x <- gsub("Alewife ","Alosa pseudoharengus ",x)
  x <- gsub("American alligator ","Alligator mississippiensis ",x)
  x <- gsub("American plaice ","Hippoglossoides platessoides ",x)
  x <- gsub("American shad ","Alosa sapidissima ",x)
  x <- gsub("Anchovy ","Engraulidae ",x)
  x <- gsub("Anemones ","Actiniaria ",x)
  x <- gsub("Anglerfish ","Lophiiformes ",x)
  x <- gsub("Annelids ","Annelida ",x)
  x <- gsub("Appendicularians ","Appendicularia ",x)
  x <- gsub("Arctic char ","Salvelinus alpinus ",x)
  x <- gsub("Arge phytoplankton ","Phytoplankton ",x)
  x <- gsub("Ariids ","Ariidae ",x)
  x <- gsub("Ascidians ","Ascidiacea ",x)
  x <- gsub("Asteroids ","Asteroideae ",x)
  x <- gsub("Atlantic silverside ","Menidia menidia ",x)
  x <- gsub("Bald eagle ","Haliaeetus leucocephalus ",x)
  x <- gsub("Baleen whales ","Mysticeti ",x)
  x <- gsub("Barnacles ","Cirripedia ",x)
  x <- gsub("Barracuda ","Sphyraena ",x)
  x <- gsub("Bay anchovy ","Anchoa mitchilli ",x)
  x <- gsub("Bay shrimp ","Crangon crangon ",x)
  x <- gsub("Bearded seals ","Erignathus barbatus ",x)
  x <- gsub("Beluga ","Delphinapterus leucas ",x)
  x <- gsub("Benthic cephalopod ","Cephalopoda ",x)
  x <- gsub("Benthic crustacea ","Crustacea ",x)
  x <- gsub("Benthic cumacean ","Cumacea ",x)
  x <- gsub("Benthic diatom ","Bacillariophyceae ",x)
  x <- gsub("Benthic diatoms ","Bacillariophyceae ",x)
  x <- gsub("Benthic isopod ","Isopoda ",x)
  x <- gsub("Benthopelagic cephalopod ","Cephalopoda ",x)
  x <- gsub("Cephalopod ","Cephalopoda ",x)
  x <- gsub("Benthos-eating birds ","Aves ",x)
  x <- gsub("Birds ","Aves ",x)
  x <- gsub("Coastal seabirds ","Aves ",x)
  x <- gsub("Fish- crustacean-eating birds ","Aves ",x)
  x <- gsub("Fish-eating birds ","Aves ",x)
  x <- gsub("Marine birds ","Aves ",x)
  x <- gsub("Pelagic seabirds ","Aves ",x)
  x <- gsub("Seabirds ","Aves ",x)
  x <- gsub("Shorebirds ","Charadriiformes ",x)
  x <- gsub("Song birds ","Passeri ",x)
  x <- gsub("Wading birds ","Charadriiformes ",x)
  x <- gsub("Billfish ","Xiphioidei ",x)
  x <- gsub("Bivalves ","Bivalvia ",x)
  x <- gsub("Black drum ","Pogonias cromis ",x)
  x <- gsub("Black perch ","Embiotoca jacksoni ",x)
  x <- gsub("Black rockfish ","Sebastes melanops ",x)
  x <- gsub("Black turnstone ","Arenaria melanocephala ",x)
  x <- gsub("Blennies ","Blennioidei ",x)
  x <- gsub("Blenny","Blennioidei ",x)
  x <- gsub("Blowfish ","Tetraodontidae ",x)
  x <- gsub("Blue whiting ","Micromesistius poutassou ",x)
  x <- gsub("Bluefish ","Pomatomus saltatrix ",x)
  x <- gsub("Brittlestars ","Ophiuroidea ",x)
  x <- gsub("Bryozoans ","Bryozoa ",x)
  x <- gsub("Bull shark ","Carcharhinus leucas ",x)
  x <- gsub("Butterfish ","Stromateidae ",x)
  x <- gsub("Cabezon ","Scorpaenichthys marmoratus ",x)
  x <- gsub("Calanoids ","Calanoida ",x)
  x <- gsub("California halibut","Paralichthys californicus ",x)
  x <- gsub("California scorpion fish ","Scorpaena guttata ",x)
  x <- gsub("California sea lion ","Zalophus californianus ",x)
  x <- gsub("Cancer crabs ","Cancer ",x)
  x <- gsub("Caplin ","Mallotus villosus ",x)
  x <- gsub("Caprellids ","Caprellidae ",x)
  x <- gsub("Caprellidss ","Caprellidae ",x)
  x <- gsub("Carangids ","Carangidae ",x)
  x <- gsub("Cardinal fish ","Apogonops anomalus ",x)
  x <- gsub("Carnivore polychaete ","Polychaeta ",x)
  x <- gsub("Deposit-feeding polychaete ","Polychaeta ",x)
  x <- gsub("Errant polychaete ","Polychaeta ",x)
  x <- gsub("Filter-feeding polychaete ","Polychaeta ",x)
  x <- gsub("Omnivore polychaete ","Polychaeta ",x)
  x <- gsub("Polychaete ","Polychaeta ",x)
  x <- gsub("Predatory polychaete ","Polychaeta ",x)
  x <- gsub("Sedentary polychaete ","Polychaeta ",x)
  x <- gsub("Serpulid polychaete ","Serpulidae ",x)
  x <- gsub("Sub-surface deposit-feeding polychaete ","Polychaeta ",x)
  x <- gsub("Surface deposit-feeding polychaete ","Polychaeta ",x)
  x <- gsub("Suspension-feeding polychaete ","Polychaeta ",x)
  x <- gsub("Carnivorous calanoida ","Calanoida ",x)
  x <- gsub("Carnivorous diptera ","Diptera ",x)
  x <- gsub("Carnivorous terrestrial insects ","Insecta ",x)
  x <- gsub("Herbivorous insects ","Insecta ",x)
  x <- gsub("Herbivorous terrestrial insects ","Insecta ",x)
  x <- gsub("Insects ","Insecta ",x)
  x <- gsub("Marsh insects ","Insecta ",x)
  x <- gsub("Catfish ","Siluriformes ",x)
  x <- gsub("Centropomids ","Centropomidae ",x)
  x <- gsub("Chaetognaths ","Chaetognatha ",x)
  x <- gsub("Chanids ","Chanidae ",x)
  x <- gsub("Chinook salmon ","Oncorhynchus tshawytscha ",x)
  x <- gsub("Chitons ","Polyplacophora ",x)
  x <- gsub("Chub mackerel ","Scomber japonicus ",x)
  x <- gsub("Chum salmon ","Oncorhynchus keta ",x)
  x <- gsub("Ciliates ","Ciliophora ",x)
  x <- gsub("Clam ","Bivalvia ",x)
  x <- gsub("Clapper rail ","Rallus crepitans ",x)
  x <- gsub("Clupeid ","Clupeidae ",x)
  x <- gsub("Coccolithophores ","Coccosphaerales ",x)
  x <- gsub("Cod ","Gadus ",x)
  x <- gsub("Coelenterates ","Coelenterata ",x)
  x <- gsub("Coho salmon ","Oncorhynchus kisutch ",x)
  x <- gsub("Common mummichog ","Fundulus heteroclitus ",x)
  x <- gsub("Common murre ","Uria aalge ",x)
  x <- gsub("Common octopus ","Octopus vulgaris ",x)
  x <- gsub("Corals ","Anthozoa ",x)
  x <- gsub("Cormorant ","Phalacrocoracidae ",x)
  x <- gsub("Crabeater seal ","Lobodon carcinophagus ",x)
  x <- gsub("Crabs ","Brachyura ",x)
  x <- gsub("Crayfish ","Astacoidea ",x)
  x <- gsub("Crepidulids ","Crepidula ",x)
  x <- gsub("Crested goby ","Rhinogobiops nicholsii ",x)
  x <- gsub("Cribina ","Cribrina ",x)
  x <- gsub("Cricket ","Gryllidae ",x)
  x <- gsub("Croakers ","Sciaenidae ",x)
  x <- gsub("Crustacean parts ","Crustacea ",x)
  x <- gsub("Crustaceans"," Crustacea ",x)
  x <- gsub("Deposit-feeding peracaridan crustaceans ","Peracarida ",x)
  x <- gsub("Epibenthic crustacea ","Crustacea ",x)
  x <- gsub("Hypoplanktonic crustacea ","Crustacea ",x)
  x <- gsub("Macrocrustacea ","Crustacea ",x)
  x <- gsub("Microcrustaceans ","Crustacea ",x)
  x <- gsub("Pelagic crustacean ","Crustacea ",x)
  x <- gsub("Cryptophytes ","Cryptophyta ",x)
  x <- gsub("Crystal krill ","Euphausia crystallorophias ",x)
  x <- gsub("Ctenophores ","Ctenophora ",x)
  x <- gsub("Cumaceans ","Cumacea ",x)
  x <- gsub("Cunner ","Tautogolabrus adspersus ",x)
  x <- gsub("Cusk ","Brosme brosme ",x)
  x <- gsub("Cutthroat trout ","Oncorhynchus clarkii ",x)
  x <- gsub("Cyclopoids ","Cyclopoida ",x)
  x <- gsub("Decapods ","Decapoda ",x)
  x <- gsub("Deposit-feeding gastropods ","Gastropoda ",x)
  x <- gsub("Epiphyte-grazing gastropods ","Gastropoda ",x)
  x <- gsub("Gastropods ","Gastropoda ",x)
  x <- gsub("Herbivorous gastropods ","Gastropoda ",x)
  x <- gsub("Predatory gastropods ","Gastropoda ",x)
  x <- gsub("Diatoms ","Bacillariophyceae ",x)
  x <- gsub("Dinoflagellates ","Dinoflagellata ",x)
  x <- gsub("Doliolids ","Doliolida ",x)
  x <- gsub("Dolphin coryphaena ","Coryphaena ",x)
  x <- gsub("Dolphins ","Cetacea ",x)
  x <- gsub("Dragonflies ","Anisoptera ",x)
  x <- gsub("Ducks ","Anatidae ",x)
  x <- gsub("Dungeness crab ","Metacarcinus magister ",x)
  x <- gsub("Dunlin ","Calidris alpina ",x)
  x <- gsub("Echinoderms ","Echinodermata ",x)
  x <- gsub("Eel ","Anguilliformes ",x)
  x <- gsub("Eelgrass ","Zostera ",x)
  x <- gsub("Eider ","Somateria ",x)
  x <- gsub("Elephant seal ","Mirounga ",x)
  x <- gsub("Elopids ","Elopidae ",x)
  x <- gsub("Emperor penguins ","Aptenodytes forsteri ",x)
  x <- gsub("Ephemerids ","Ephemeroptera ",x)
  x <- gsub("Ephausiids ","Euphausiacea ",x)
  x <- gsub("Euphasiids ","Euphausiacea ",x)
  x <- gsub("Euphausiid ","Euphausiacea ",x)
  x <- gsub("Euphausiids ","Euphausiacea ",x)
  x <- gsub("Fat sleepers ","Dormitator ",x)
  x <- gsub("Flatfish ","Pleuronectiformes ",x)
  x <- gsub("Flatfish benthic feeder ","Pleuronectiformes ",x)
  x <- gsub("Flatfish water-column feeder ","Pleuronectiformes ",x)
  x <- gsub("Flathead ","Platycephalidae ",x)
  x <- gsub("Flounder ","Pleuronectoidei ",x)
  x <- gsub("Fluke ","Paralichthys dentatus ",x)
  x <- gsub("Flying fish ","Exocoetidae ",x)
  x <- gsub("Foraminiferans ","Foraminifera ",x)
  x <- gsub("Four-spot flounder ","Pseudorhombus diplospilus ",x)
  x <- gsub("Gammarideas ","Gammaridea ",x)
  x <- gsub("Gammarids ","Gammaridea ",x)
  x <- gsub("Geelbek ","Atractoscion aequidens ",x)
  x <- gsub("Gelatinous tunicates","Tunicata ",x)
  x <- gsub("Gerreids ","Gerreidae ",x)
  x <- gsub("Gobies ","Gobiidae ",x)
  x <- gsub("Gobioids ","Gobioides ",x)
  x <- gsub("Goby ","Gobiidae ",x)
  x <- gsub("Gold-spotted killifish ","Floridichthys carpio ",x)
  x <- gsub("Goosefish ","Lophiidae ",x)
  x <- gsub("Gray whales ","Eschrichtius robustus ",x)
  x <- gsub("Green heron ","Butorides virescens ",x)
  x <- gsub("Greenland shark ","Somniosus microcephalus ",x)
  x <- gsub("Greenling ","Hexagrammidae ",x)
  x <- gsub("Grenadier ","Macrouridae ",x)
  x <- gsub("Gull ","Laridae ",x)
  x <- gsub("Haddock ","Melanogrammus aeglefinus ",x)
  x <- gsub("Haemulids ","Haemulidae ",x)
  x <- gsub("Halfbeaks ","Hemiramphidae ",x)
  x <- gsub("Halibut ","Hippoglossus ",x)
  x <- gsub("Harbor seal ","Phoca vitulina ",x)
  x <- gsub("Harp seal ","Pagophilus groenlandicus ",x)
  x <- gsub("Harpacticoids ","Harpacticoida ",x)
  x <- gsub("Heliozoans ","Heliozoa ",x)
  x <- gsub("Herbivorous ducks ","Anatidae ",x)
  x <- gsub("Herbivorous snails ","Gastropoda ",x)
  x <- gsub("Hermit crabs ","Paguroidea ",x)
  x <- gsub("Heron ","Ardeidae ",x)
  x <- gsub("Herring ","Clupeidae ",x)
  x <- gsub("Hornyhead turbot ","Pleuronichthys verticalis ",x)
  x <- gsub("Hyperiid ","Hyperiidea ",x)
  x <- gsub("Isopods ","Isopoda ",x)
  x <- gsub("Jack mackerel ","Trachurus ",x)
  x <- gsub("Jack ","Carangidae ",x)
  x <- gsub("Jacks ","Carangidae ",x)
  x <- gsub("Jackass morwong ","Nemadactylus macropterus ",x)
  x <- gsub("Jellyfish ","Cnidaria ",x)
  x <- gsub("Keyhole limpets ","Fissurellidae ",x)
  x <- gsub("Killer whale ","Orcinus orca ",x)
  x <- gsub("Killifish ","Cyprinodontiformes ",x)
  x <- gsub("Kingfisher ","Alcedines ",x)
  x <- gsub("Knot ","Calidris ",x)
  x <- gsub("Krill ","Euphausiacea ",x)
  x <- gsub("Lamya ","Arcuatula ",x)
  x <- gsub("Lanternfish ","Myctophidae ",x)
  x <- gsub("Larvaceans ","Appendicularia ",x)
  x <- gsub("Least killifish ","Heterandria formosa ",x)
  x <- gsub("Leopard seal ","Hydrurga leptonyx ",x)
  x <- gsub("Lesser scaup ","Aythya affinis ",x)
  x <- gsub("Limpets ","Gastropoda ",x)
  x <- gsub("Lingcod ","Ophiodon elongatus ",x)
  x <- gsub("Little skate ","Leucoraja erinacea ",x)
  x <- gsub("Littorines ","Littorina ",x)
  x <- gsub("Lobster ","Nephropidae ",x)
  x <- gsub("Loggerhead turtles ","Caretta caretta ",x)
  x <- gsub("Longfin sand dab ","Citharichthys xanthostigma ",x)
  x <- gsub("Longhorn sculpin ","Myoxocephalus octodecemspinosus ",x)
  x <- gsub("Loon ","Gavia ",x)
  x <- gsub("Mackerel ","Scombridae ",x)
  x <- gsub("Macrobranchium ","Macrobrachium ",x)
  x <- gsub("Mammals ","Mammalia ",x)
  x <- gsub("Mantis shrimp ","Squilla mantis ",x)
  x <- gsub("Marine mammals ","Mammalia ",x)
  x <- gsub("Market squid ","Doryteuthis opalescens ",x)
  x <- gsub("Marlin ","Istiophoridae ",x)
  x <- gsub("Merganser ","Mergus ",x)
  x <- gsub("Micropogon ","Micropogonias ",x)
  x <- gsub("Microprotozoa ","Protozoa ",x)
  x <- gsub("Minnow ","Cyprinidae ",x)
  x <- gsub("Mollies ","Poecilia ",x)
  x <- gsub("Molluscs ","Mollusca ",x)
  x <- gsub("Mosquito fish ","Gambusia affinis ",x)
  x <- gsub("Mosquito ","Culicidae ",x)
  x <- gsub("Mugilids ","Mugilidae ",x)
  x <- gsub("Mullet ","Mugilidae ",x)
  x <- gsub("Muricanthus ","Hexaplex ",x)
  x <- gsub("Mussel ","Mytilidae ",x)
  x <- gsub("Myctophids ","Myctophidae ",x)
  x <- gsub("Mysids ","Mysida ",x)
  x <- gsub("Narwhal ","Monodon monoceros ",x)
  x <- gsub("Nauplii ","Nauplius ",x)
  x <- gsub("Naupliuses ","Nauplius ",x)
  x <- gsub("Ocean perch ","Sebastes norvegicus ",x)
  x <- gsub("Ocean pout ","Zoarces americanus ",x)
  x <- gsub("Ocean sunfish ","Mola mola ",x)
  x <- gsub("Needlefish ","Belonidae ",x)
  x <- gsub("Nematodes ","Nematoda ",x)
  x <- gsub("Nemerteans ","Nemertea ",x)
  x <- gsub("Nemertini ","Nemertea ",x)
  x <- gsub("Nephthys ","Nephtys ",x)
  x <- gsub("Nereids ","Nereididae ",x)
  x <- gsub("Nerites ","Neritidae ",x)
  x <- gsub("Northern anchovy ","Engraulis mordax ",x)
  x <- gsub("Norway lobster ","Nephrops norvegicus ",x)
  x <- gsub("Oithona-oncaea ","Oithona - oncaea",x)
  x <- gsub("Oligochaete ","Oligochaeta ",x)
  x <- gsub("Oligochaets ","Oligochaeta ",x)
  x <- gsub("Omnivorous crabs ","Brachyura ",x)
  x <- gsub("Ophiuroids ","Ophiuroidea ",x)
  x <- gsub("Osprey ","Pandion haliaetus ",x)
  x <- gsub("Ostracods ","Ostracoda ",x)
  x <- gsub("Oystercatcher ","Haematopus ",x)
  x <- gsub("Oysters ","Ostreoidea ",x)
  x <- gsub("Pacific angel shark ","Squatina californica ",x)
  x <- gsub("Pacific bonito ","Sarda chiliensis lineolata ",x)
  x <- gsub("Pacific hakes ","Merluccius productus ",x)
  x <- gsub("Pacific herring ","Clupea pallasii ",x)
  x <- gsub("Pacific mackerel ","Scomber australasicus ",x)
  x <- gsub("Pacific menhaden ","Ethmidium maculatum ",x)
  x <- gsub("Pacific sand dab ","Citharichthys sordidus ",x)
  x <- gsub("Palaemonids ","Palaemonidae ",x)
  x <- gsub("Paleomonetes ","Palaemonetes ",x)
  x <- gsub("Pandalids ","Pandalidae ",x)
  x <- gsub("Passerines ","Passeriformes ",x)
  x <- gsub("Pelagic amphipod ","Amphipoda ",x)
  x <- gsub("Pelagobla ","Pelagobia ",x)
  x <- gsub("Pelecypod ","Bivalvia ",x)
  x <- gsub("Pelecypoda ","Bivalvia ",x)
  x <- gsub("Penaeid shrimp ","Penaeidae ",x)
  x <- gsub("Penguins ","Spheniscidae ",x)
  x <- gsub("Petrels ","Procellariiformes ",x)
  x <- gsub("Pilchard ","Clupeidae ",x)
  x <- gsub("Pinfish ","Lagodon rhomboides ",x)
  x <- gsub("Pink sea perch ","Zalembius rosaceus ",x)
  x <- gsub("Pinnipeds ","Pinnipedia ",x)
  x <- gsub("Pipefish ","Syngnathinae ",x)
  x <- gsub("Planktonic bacteria ","Bacteria ",x)
  x <- gsub("Pleuronectids ","Pleuronectidae ",x)
  x <- gsub("Pleuronectoids ","Pleuronectidae ",x)
  x <- gsub("Poeciliids ","Poeciliidae ",x)
  x <- gsub("Pollock ","Pollachius ",x)
  x <- gsub("Predaceous crabs ","Brachyura ",x)
  x <- gsub("Protozoans ","Protozoa ",x)
  x <- gsub("Pteropods ","Pteropoda ",x)
  x <- gsub("Racoon ","Procyon lotor ",x)
  x <- gsub("Radiolarians ","Radiolaria ",x)
  x <- gsub("Rays ","Batoidea ",x)
  x <- gsub("Red drum ","Sciaenops ocellatus ",x)
  x <- gsub("Red hakes ","Urophycis chuss ",x)
  x <- gsub("Red-winged blackbird ","Agelaius phoeniceus ",x)
  x <- gsub("Redbait ","Emmelichthys nitidus ",x)
  x <- gsub("Right whales ","Eubalaena ",x)
  x <- gsub("Ringed seal ","Pusa hispida ",x)
  x <- gsub("Rorquals ","Balaenopteridae ",x)
  x <- gsub("Round herring ","Dussumieriidae ",x)
  x <- gsub("Rose shrimp ","Parapenaeus longirostris ",x)
  x <- gsub("Rubber lip sea perch ","Rhacochilus toxotes ",x)
  x <- gsub("Sablefish ","Anoplopoma fimbria ",x)
  x <- gsub("Salmon ","Salmonidae ",x)
  x <- gsub("Salps ","Salpidae ",x)
  x <- gsub("Sand bass ","Morone chrysops ",x)
  x <- gsub("Sand lance ","Ammodytidae ",x)
  x <- gsub("Sardine ","Clupeidae ",x)
  x <- gsub("Saury ","Cololabis adocetus ",x)
  x <- gsub("Scallops ","Pectinidae ",x)
  x <- gsub("Scianids ","Sciaenidae ",x)
  x <- gsub("Scombrid ","Scombridae ",x)
  x <- gsub("Scorpaenids ","Scorpaenidae ",x)
  x <- gsub("Scoter ","Melanitta ",x)
  x <- gsub("Sea cucumbers ","Holothuroidea ",x)
  x <- gsub("Sea mammals ","Mammalia ",x)
  x <- gsub("Sea otters ","Enhydra lutris ",x)
  x <- gsub("Sea raven ","Hemitripteridae ",x)
  x <- gsub("Sea urchins ","Echinoidea ",x)
  x <- gsub("Seahorses ","Hippocampus ",x)
  x <- gsub("Seals ","Pinnipedia ",x)
  x <- gsub("Sediment bacteria ","Bacteria ",x)
  x <- gsub("Shad ","Alosinae ",x)
  x <- gsub("Shark ","Selachimorpha ",x)
  x <- gsub("Sharpnose sea perch ","Phanerodon atripes ",x)
  x <- gsub("Sheepshead minnow ","Cyprinodon variegatus ",x)
  x <- gsub("Shelduck ","Tadorna ",x)
  x <- gsub("Shiner perch ","Cymatogaster aggregata ",x)
  x <- gsub("Silver hakes ","Merluccius bilinearis ",x)
  x <- gsub("Siphonophores ","Siphonophorae ",x)
  x <- gsub("Sipunculids ","Sipuncula ",x)
  x <- gsub("Skates ","Rajidae ",x)
  x <- gsub("Smooth dogfish ","Mustelus canis ",x)
  x <- gsub("Snails ","Gastropoda ",x)
  x <- gsub("Snake mackerel ","Gempylus serpens ",x)
  x <- gsub("Sockeye salmon ","Oncorhynchus nerka ",x)
  x <- gsub("Solitary tunicates ","Tunicata ",x)
  x <- gsub("Sooty shearwaters ","Ardenna griseus ",x)
  x <- gsub("Sparids ","Sparidae ",x)
  x <- gsub("Sperm whale ","Physeter macrocephalus ",x)
  x <- gsub("Spider crabs ","Majoidea ",x)
  x <- gsub("Spiders ","Araneae ",x)
  x <- gsub("Spiny dogfish ","Squalus acanthias ",x)
  x <- gsub("Sponges ","Porifera ",x)
  x <- gsub("Spot ","Leiostomus xanthurus ",x)
  x <- gsub("Spotted hakes ","Urophycis regia ",x)
  x <- gsub("Squid ","Teuthida ",x)
  x <- gsub("Starfish ","Asteroidea ",x)
  x <- gsub("Steelhead trout ","Oncorhynchus mykiss ",x)
  x <- gsub("Stingray ","Myliobatoidei ",x)
  x <- gsub("Stomatopods ","Stomatopoda ",x)
  x <- gsub("Storm-petrels ","Hydrobatidae ",x)
  x <- gsub("Striped bass ","Morone saxatilis ",x)
  x <- gsub("Striped mummichog ","Fundulus majalis ",x)
  x <- gsub("Summer flounder ","Paralichthys dentatus ",x)
  x <- gsub("Suspension-feeding molluscs ","Mollusca ",x)
  x <- gsub("Swimming crabs ","Portunidae ",x)
  x <- gsub("Tanner crab ","Chionoecetes ",x)
  x <- gsub("Tern ","Sternidae ",x)
  x <- gsub("Terrestrial plants ","Plantae ",x)
  x <- gsub("Thaliacians ","Thaliacea ",x)
  x <- gsub("Thorny skate ","Amblyraja radiata ",x)
  x <- gsub("Tintinnids ","Tintinnida ",x)
  x <- gsub("Tomopterids ","Tomopteridae ",x)
  x <- gsub("Tonguefish ","Cynoglossidae ",x)
  x <- gsub("Toothed whales ","Odontoceti ",x)
  x <- gsub("Trout ","Salmoninae ",x)
  x <- gsub("Tuna ","Thunnini ",x)
  x <- gsub("Tunicates ","Tunicata ",x)
  x <- gsub("Turnstone ","Arenaria ",x)
  x <- gsub("Urchins ","Echinoidea ",x)
  x <- gsub("Vermetids ","Vermetidae ",x)
  x <- gsub("Vermillion rockfish ","Sebastes miniatus ",x)
  x <- gsub("Walrus ","Odobenus rosmarus ",x)
  x <- gsub("Water plant ","Plantae ",x)
  x <- gsub("Waterfowl ","Anseriformes ",x)
  x <- gsub("Weakfish ","Cynoscion regalis ",x)
  x <- gsub("Weddell seals ","Leptonychotes weddellii ",x)
  x <- gsub("Whales ","Cetacea ",x)
  x <- gsub("White croaker ","Genyonemus lineatus ",x)
  x <- gsub("White hakes ","Urophycis tenuis ",x)
  x <- gsub("Wigeongrass ","Ruppia maritima ",x)
  x <- gsub("Windowpane flounder ","Scophthalmus aquosus ",x)
  x <- gsub("Winter flounder ","Pseudopleuronectes americanus ",x)
  x <- gsub("Winter skate ","Leucoraja ocellata ",x)
  x <- gsub("Witch flounder ","Glyptocephalus cynoglossus ",x)
  x <- gsub("Wolffish ","Anarhichadidae ",x)
  x <- gsub("Worms ","Bilateria ",x)
  x <- gsub("Yellowtail flounder ","Limanda ferruginea ",x)
  x <- gsub("Blacksmith ","Chromis punctipinnis ",x)
  x <- gsub("Carcinides ","Carcinus ",x)
  x <- gsub("Clupeoids ","Clupeidae ",x)
  x <- gsub("Condrichtis ","Chondrichthyes ",x)
  x <- gsub("Cucumberfish ","Prototroctes maraena ",x)
  x <- gsub("Dogfish ","Squalidae ",x)
  x <- gsub("Yellowtail ","Seriola lalandi ",x)
  x <- gsub("Hakess ","Seriola lalandi ",x)
  x <- gsub("Ling ","Genypterus blacodes ",x)
  x <- gsub("Lightfish ","Phosichthyidae ",x)
  x <- gsub("Warehous ","Seriolella brama - Seriolella punctata ",x)
  x <- gsub("Tooth-fish ","Dissostichus mawsoni ",x)
  x <- gsub("Snooks ","Centropomidae ",x)
  x <- gsub("Snappers ","Lutjanidae ",x)
  x <- gsub("Snoek ","Thyrsites atun ",x)
  x <- gsub("Smelt ","Thaleichthys pacificus ",x)
  x <- gsub("Silversides ","Atheriniformes ",x)
  x <- gsub("Shore crabs ","Pachygrapsus crassipes - Hemigrapsus oregonensis - Hemigrapsus nudus",x)
  x <- gsub("Sheepshead ","Archosargus probatocephalus ",x)
  x <- gsub("School whiting ","Sillago flindersi ",x)
  x <- gsub("Redshank ","Tringa ",x)
  x <- gsub("Porpoise shark ","Isurus oxyrinchus ",x)
  x <- gsub("Penaid ","Penaeus aztecus ",x)
  x <- gsub("Mud snail ","Hydrobia ",x)
  x <- gsub("Menhaden ","Brevoortia tyrannus ",x)
  x <- gsub("Kob ","Argyrosomus japonicus ",x)
  x <- gsub("Blue crab ","Callinectes sapidus ",x)
  x <- gsub("Hakes ","Merluccius productus ",x)
  x <- gsub("Horse mackerel ","Trachurus ",x)
  x <- gsub("Hydroids ","Hydrozoa ",x)
  x <- gsub("Prawns ","Dendrobranchiata ",x)
  x <- gsub("Shrimp ","Penaeidae ",x)

  # Name resolve #2
  x <- gsub("Anisotremus interreptus ","Anisotremus interruptus ",x)
  x <- gsub("Archtocephalus gazella ","Arctocephalus gazella ",x)
  x <- gsub("Aristeu varidens ","Aristeus varidens ",x)
  x <- gsub("Atherinomuros stipes ","Atherinomorus stipes ",x)
  x <- gsub("Cailianassa ","Callianassa ",x)
  x <- gsub("Chthamalus microtretus ","Chthamalus fissus ",x)
  x <- gsub("Diapterus peruvians ","Diapterus peruvianus ",x)
  x <- gsub("Dormitator latrifons ","Dormitator latifrons ",x)
  x <- gsub("Dorosoma petense ","Dorosoma petenense ",x)
  x <- gsub("Gaetanus tenuisplnus ","Gaetanus tenuispinus ",x)
  x <- gsub("Glicera tridactyla ","Glycera tridactyla ",x)
  x <- gsub("Gobiesoma bosci ","Gobiosoma bosci ",x)
  x <- gsub("Hemigrapsus edwardsii ","Hemigrapsus sexdentatus ",x)
  x <- gsub("Hemiramphus brasilienis ","Hemiramphus brasiliensis ",x)
  x <- gsub("Mexican mojarras ","Cichlidae ",x)
  x <- gsub("Mista picta ","Mysta picta ",x)
  x <- gsub("Oncacea ","Oncaea ",x)
  x <- gsub("Pagurus mcglaughlini ","Pagurus maclaughlinae ",x)
  x <- gsub("Pleuronectiformes benthic feeder ","Pleuronectiformes ",x)
  x <- gsub("Pleuronectiformes water-column feeder ","Pleuronectiformes ",x)
  x <- gsub("Poronatus ","Poronotus triacanthus",x)
  x <- gsub("Sygnathus floridae ","Syngnathus floridae ",x)
  x <- gsub("Sygnathus fuscus ","Syngnathus fuscus ",x)
  x <- gsub("Tanaidacidae ","Tanaididae ",x)
  x <- gsub("Xiphophorous helleri ","Xiphophorus helleri ",x)
  x <- gsub("Prunum aureocincta ","Dentimargo aureocinctus ",x)
  x <- gsub("Fish carnivores ","Pisces ",x)
  x <- gsub("Fish eggs ","Pisces ",x)
  x <- gsub("Fish fry ","Pisces ",x)
  x <- gsub("Fish herbivores ","Pisces ",x)
  x <- gsub("Fish ","Pisces ",x)
  x <- gsub("Herbivorous fish ","Pisces ",x)
  x <- gsub("Benthic fish ","Pisces ",x)
  x <- gsub("Benthic-feeding fish ","Pisces ",x)
  x <- gsub("Benthivorous rockfish ","Pisces ",x)
  x <- gsub("Demersal fish ","Pisces ",x)
  x <- gsub("Benthopelagic fish ","Pisces ",x)
  x <- gsub("Benthos-eating fish ","Pisces ",x)
  x <- gsub("Deep-sea fish ","Pisces ",x)
  x <- gsub("Herbivorous fish ","Pisces ",x)
  x <- gsub("Mesopelagic fish ","Pisces ",x)
  x <- gsub("Zooplanktivorous fish ","Pisces ",x)
  x <- gsub("Rockfish ","Pisces ",x)
  x <- gsub("Reef fish ","Pisces ",x)
  x <- gsub("Predaceous fish ","Pisces ",x)
  x <- gsub("Planktivorous rockfish ","Pisces ",x)
  x <- gsub("Piscivorous rockfish ","Pisces ",x)
  x <- gsub("Piscivorous fish ","Pisces ",x)
  x <- gsub("Pelagic fish ","Pisces ",x)
  x <- gsub("Nearshore fish ","Pisces ",x)
  x <- gsub("Groundfish ","Pisces ",x)
  x <- gsub("Acanthocitona pygmaea ","Acanthochitona pygmaea ",x)
  x <- gsub("Acetocina candei ","Acteocina candei ",x)
  x <- gsub("Amphitritidae ","Amphitritides ",x)
  x <- gsub("Ardenna griseus ","Puffinus griseus ",x)
  x <- gsub("Brachiodontes exustus ","Brachidontes exustus ",x)
  x <- gsub("Cadulus carolinesis ","Polyschides carolinensis ",x)
  x <- gsub("Chalina ","Haliclona ",x)
  x <- gsub("Chonophorous genivittatus ","Stenogobius genivittatus ",x)
  x <- gsub("Cyanoplax dientens ","Cyanoplax dentiens ",x)
  x <- gsub("Emplectonema gracilis ","Emplectonema gracile ",x)
  x <- gsub("Enchinometra lucunter ","Echinometra lucunter ",x)
  x <- gsub("Erichsionella ","Erichsonella ",x)
  x <- gsub("Erynnis japonica ","Evynnis japonica ",x)
  x <- gsub("Gibulla umbilicalis ","Gibbula umbilicalis ",x)
  x <- gsub("Scyphomedusae ","Scyphozoa ",x)
  x <- gsub("Hydromedusae ","Hydrozoa ",x)
  x <- gsub("Medusae ","Cnidaria ",x)
  x <- gsub("Hylina veliei ","Prunum succineum ",x)
  x <- gsub("Hypacanthus ","Lichia amia ",x)
  x <- gsub("Lichenophora ","Disporella hispida ",x)
  x <- gsub("Metopograpsis messor ","Metopograpsus messor ",x)
  x <- gsub("Mojarras ","Gerreidae",x)
  x <- gsub("Neritina tahitiensis ","Neripteron taitense ",x)
  x <- gsub("Nodolittorina tuberculata ","Echinolittorina tuberculata ",x)
  x <- gsub("Oxyurichthyes lonchotus ","Oxyurichthys lonchotus ",x)
  x <- gsub("Paracerces caudata ","Paracerceis caudata ",x)
  x <- gsub("Parathemisto gracilis ","Themisto gaudichaudii ",x)
  x <- gsub("Passeri ","Passeriformes ",x)
  x <- gsub("Pelagagobia ","Pelagobia ",x)
  x <- gsub("Pinixia floridana ","Pinnixa floridana ",x)
  x <- gsub("Prinospio ","Prionospio ",x)
  x <- gsub("Raccoon ","Procyon lotor ",x)
  x <- gsub("Spartina patena ","Spartina patens ",x)
  x <- gsub("Swartziella catesbyana ","Schwartziella catesbyana ",x)
  x <- gsub("Symphurus plagisua ","Symphurus plagiusa ",x)
  x <- gsub("Thais biserialis ","Stramonita biserialis ",x)
  x <- gsub("Thais lima ","Nucella lima ",x)
  x <- gsub("Toadfish ","Batrachoididae ",x)
  x <- gsub("Vermilion rockfish ","Sebastes miniatus ",x)
  x <- gsub("Melania indefinita ","Thiara ",x)
  x <- gsub("Pectanaridae ","Pectinariidae ",x)
  x <- gsub("Pisobia ","Calidris minutilla ",x)
  x <- gsub("Plactynemis ","Platycnemis ",x)
  x <- gsub("Potamya ","Potamyia ",x)
  x <- gsub("Theobaldia ","Culicidae ",x)
  x <- gsub("Conger marginatus ","Conger cinereus ",x)
  x <- gsub("Mysidacea ","Mysida ",x)
  x <- gsub("Radiolaria ","Radiozoa ",x)
  x <- gsub("Sarda chiliensis lineolata ","Sarda chiliensis ",x)
  x <- gsub("Neopanope texana sayi ","Neopanope texana ",x)
  x <- str_trim(x, side="both") #remove spaces
  x <- tolower(x)
  x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
  return(x)
}


for(k in 1:length(GlobalWeb)) {
  rownames(GlobalWeb[[k]][[1]]) <- Names_change(rownames(GlobalWeb[[k]][[1]]))
  colnames(GlobalWeb[[k]][[1]]) <- rownames(GlobalWeb[[k]][[1]])
}

# ------------------------------------------------------------------------------------
# AGAIN THE SAME PROCEDURE
#Combining duplicated rows and columns
  for(k in 1:length(GlobalWeb)){
    GlobalWeb[[k]][[1]] <- dupl_sp(GlobalWeb[[k]][[1]])
  }

#Dividing rows or columns with multiple taxon
  dup.fw <- NA
  for(k in 1:length(GlobalWeb)){
    x <- which(str_detect(rownames(GlobalWeb[[k]][[1]])," - ") == TRUE) #Identify rows that should be Duplicated
    if(length(x) == 0) {NULL} else {dup.fw <- c(dup.fw,k)}
  }
  dup.fw <- dup.fw[-1]

  for(k in dup.fw) {
    GlobalWeb[[k]][[1]] <- dupl_row(GlobalWeb[[k]][[1]],rownames(GlobalWeb[[k]][[1]]))
    GlobalWeb[[k]][[1]] <- dupl_col(GlobalWeb[[k]][[1]],colnames(GlobalWeb[[k]][[1]]))
}

for(i in 1: length(GlobalWeb)){
  if(identical(colnames(GlobalWeb[[i]][[1]]),rownames(GlobalWeb[[i]][[1]])) == FALSE) {stop("les matrices n'ont pas les mêmes noms de colonnes et de lignes")
  } # else {
  #   print("tout est ok")
  # }
}


Names_change <- function(x){
  # Name resolve #1
  x <- str_trim(x, side="both") #remove spaces
  x <- tolower(x)
  x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
  return(x)
}

for(k in 1:length(GlobalWeb)) {
  rownames(GlobalWeb[[k]][[1]]) <- Names_change(rownames(GlobalWeb[[k]][[1]]))
  colnames(GlobalWeb[[k]][[1]]) <- rownames(GlobalWeb[[k]][[1]])
}

# ------------------------------------------------------------------------------------

n <-character()
m <- numeric()
for(k in 1:length(GlobalWeb)){
  n <- c(n,unique(colnames(GlobalWeb[[k]][[1]])))
  m <- c(m,rep(names(GlobalWeb[[k]])[1],length(unique(colnames(GlobalWeb[[k]][[1]])))))
}
mat <- cbind(n,m)

#Taxon list
  tx.list <- NA
  for(i in 1:length(GlobalWeb)){
    x <- rownames(GlobalWeb[[i]][[1]])
    tx.list <- c(tx.list,x)
  }
  tx.list <- unique(tx.list)
  tx.list <- tx.list[-1]
  # tx.list

#Taxonomic resolver
  tx.res <- taxo_resolve(tx.list)
  tx.valid <- taxo_valid(tx.res,tx.list,res.check=TRUE)
  # com_sci <- comm2sci(tx.list[order(tx.list)])

#!!!!!!!!!!!!!!!!!!!!!!!!!!! MANUAL RESOLVE 1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# [1] "Abarenicola affinis"                 <- OK
# [2] "Abietinaria"                         <- OK
# [3] "Acanthina"
# [4] "Acanthina spirata"
# [5] "Acanthocyclus"
# [6] "Acanthopleura granulata"
# [7] "Acartia"
# [8] "Acartia tonsa"
# [9] "Acmaea"
# [10] "Acmaea digitalis"
# [11] "Acmaea jamaicensis"
# [12] "Acmaea mitra"
# [13] "Acmaea pelta"
# [14] "Acmaea scabra"
# [15] "Acmaea testudinalis"
# [16] "Acorn barnacles"                    <- modif
# [17] "Acuariidae"
# [18] "Adelie penguins"                    <- modif
# [19] "Aetideidae"
# [20] "Agauopsis"
# [21] "Alcids"                             <- modif
# [22] "Alewife"                            <- modif
# [23] "Algae"                              <- OK
# [24] "Algal feeders"                      <- OK
# [25] "Alkmaria romijni"
# [26] "Allochertes ptilocerus"
# [27] "American alligator"                 <- modif
# [28] "American plaice"                    <- modif
# [29] "American shad"                      <- modif
# [30] "Amnicolidae"
# [31] "Amphipod"                           <- modif
# [32] "Amphipoda"
# [33] "Amphissa"
# [34] "Ampithoe longimana"
# [35] "Ampithoe valida"
# [36] "Anas"
# [37] "Anas platyrhynchos"
# [38] "Anchovy"                            <- modif
# [39] "Anemones"                           <- modif
# [40] "Anglerfish"                         <- modif
# [41] "Annelids"                           <- modif
# [42] "Anopheles"
# [43] "Anthopleura aureoradiata"
# [44] "Anurida"
# [45] "Aonides oxycephala"
# [46] "Appendicularia"
# [47] "Appendicularians"                   <- modif
# [48] "Archidoris"
# [49] "Archtocephalus gazella"
# [50] "Arctic char"                        <- modif
# [51] "Ardea novaehollandiae"
# [52] "Arenicola"
# [53] "Arenicola marina"
# [54] "Arge phytoplankton"                 <- modif
# [55] "Ariidae"
# [56] "Ariids"                             <- modif
# [57] "Aristeu varidens"
# [58] "Ascarophis"
# [59] "Ascidians"                          <- modif
# [60] "Ascophyllum"
# [61] "Ascophyllum nodosum"
# [62] "Asio"
# [63] "Asterias"
# [64] "Asteroids"                          <- modif
# [65] "Astraea undosa"
# [66] "Astrometis sertulifera"
# [67] "Atlantic silverside"                <- modif
# [68] "Attached plants"                        <- OK
# [69] "Austrovenus stutchburyi"
# [70] "Bacteria"                               <- OK
# [71] "Bacterioplankton"                       <- OK
# [72] "Balaenoptera acutorostrata"
# [73] "Balanus"
# [74] "Balanus balanoides"
# [75] "Balanus eburneus"
# [76] "Balanus glandula"
# [77] "Bald eagle"                           <- modif
# [78] "Baleen whales"                        <- modif
# [79] "Barnacles"                            <- modif
# [80] "Barracuda"                            <- modif
# [81] "Basic food"                             <- OK
# [82] "Bathylagus antarcticus"
# [83] "Bay anchovy"                          <- modif
# [84] "Bay shrimp"                           <- modif
# [85] "Bearded seals"                        <- modif
# [86] "Belonoids"                        <- modif
# [87] "Beluga"                             <- modif
# [88] "Benthic algae"                          <- OK
# [89] "Benthic amphipod"                       <- modif
# [90] "Benthic bacteria"                       <- modif
# [91] "Benthic carnivores"                     <- OK
# [92] "Benthic cephalopod"                     <- modif
# [93] "Benthic crustacea"                      <- modif
# [94] "Benthic cumacean"                       <- modif
# [95] "Benthic detritus"                       <- OK
# [96] "Benthic diatom"                         <- modif
# [97] "Benthic diatoms"                        <- modif
# [98] "Benthic epifauna"                       <- OK
# [99] "Benthic filter-feeders"                 <- OK
# [100] "Benthic fish"                           <- OK
# [101] "Benthic infauna"                        <- OK
# [102] "Benthic invertebrates"                  <- OK
# [103] "Benthic isopod"                        <- modif
# [104] "Benthic macrofauna"                     <- OK
# [105] "Benthic vertebrates"                    <- OK
# [106] "Benthic-feeding fish"                   <- OK
# [107] "Benthivorous rockfish"                 <- OK
# [108] "Benthopelagic cephalopod"              <- modif
# [109] "Benthopelagic fish"                     <- OK
# [110] "Benthos"                                <- OK
# [111] "Benthos-eating birds"                  <- modif
# [112] "Benthos-eating fish"                    <- OK
# [113] "Beroe"
# [114] "Bidessus"
# [115] "Billfish"                              <- modif
# [116] "Birds"                                 <- modif
# [117] "Bivalves"                              <- modif
# [118] "Black drum"                            <- modif
# [119] "Black perch"                           <- modif
# [120] "Black rockfish"                        <- modif
# [121] "Black turnstone"                       <- modif
# [122] "Blacksmith"                            <- modif
# [123] "Blennies"                              <- modif
# [124] "Blenny"                                <- modif
# [125] "Blowfish"                              <- modif
# [126] "Blue crab"                             <- modif
# [127] "Blue whiting"                          <- modif
# [128] "Blue-green algae"                      <- OK
# [129] "Bluefish"                              <- OK
# [130] "Boccardia acus"
# [131] "Boccardia syrtis"
# [132] "Brachidontes"
# [133] "Brachioteuthis picta"
# [134] "Brevoortia"
# [135] "Brittlestars"                          <- modif
# [136] "Bryozoa"
# [137] "Bryozoans"                             <- modif
# [138] "Bugula"
# [139] "Bull shark"                            <- modif
# [140] "Butorides"
# [141] "Butterfish"                            <- modif
# [142] "Cabezon"                               <- modif
# [143] "Cailianassa"
# [144] "Calanidae"
# [145] "Calanoides"
# [146] "Calanoides acutus"
# [147] "Calanoids"                             <- modif
# [148] "Calanus"
# [149] "Calanus pacificus"
# [150] "Calanus propinquus"
# [151] "California halibut"                    <- modif
# [152] "California scorpion fish"              <- modif
# [153] "California sea lion"                   <- modif
# [154] "Callianassa filholi"
# [155] "Callinectes"
# [156] "Callinectes sapidus"
# [157] "Cancer"
# [158] "Cancer crabs"                          <- modif
# [159] "Capitella"
# [160] "Capitella capitata"
# [161] "Capitellidae"
# [162] "Caplin"                                <- modif
# [163] "Caprella"
# [164] "Caprella penantis"
# [165] "Caprellids"                            <- modif
# [166] "Caprellidss"                           <- modif
# [167] "Carangids"                             <- modif
# [168] "Carcinides"                            <- modif
# [169] "Carcinus"
# [170] "Carcinus maenas"
# [171] "Cardinal fish"                         <- modif
# [172] "Cardium"
# [173] "Caridea"
# [174] "Carnivore polychaete"                <- modif
# [175] "Carnivores"                      <- OK
# [176] "Carnivorous calanoida"               <- modif
# [177] "Carnivorous diptera"                 <- modif
# [178] "Carnivorous epifauna"              <- OK
# [179] "Carnivorous plankton"                <- OK
# [180] "Carnivorous terrestrial insects"     <- modif
# [181] "Carnivorous zooplankton"           <- OK
# [182] "Catfish"                             <- modif
# [183] "Centropages"
# [184] "Centropomids"                        <- modif
# [185] "Cephalopod"                        <- modif
# [186] "Cerastoderma"
# [187] "Cerastoderma edule"
# [188] "Ceratostoma"
# [189] "Cerebratulus"
# [190] "Chaetognatha"
# [191] "Chaetognaths"                        <- modif
# [192] "Chaetomorpha"
# [193] "Chaetozone setosa"
# [194] "Chaeturichthys hexanema"
# [195] "Chalina"
# [196] "Chanids"                             <- modif
# [197] "Charybdis orientalis"
# [198] "Chelon labrosus"
# [199] "Chinook salmon"                      <- modif
# [200] "Chironomidae"
# [201] "Chironomus"
# [202] "Chitons"                             <- modif
# [203] "Chlamydomonas"
# [204] "Chonophorous genivittatus"
# [205] "Chthamalus dalli"
# [206] "Chthamalus microtretus"
# [207] "Chub mackerel"                       <- modif
# [208] "Chum salmon"                         <- modif
# [209] "Ciliata mustela"
# [210] "Ciliates"                            <- modif
# [211] "Circus"
# [212] "Cirolana"
# [213] "Cirratulidae"
# [214] "Cladophora"
# [215] "Clam"                                <- modif
# [216] "Clapper rail"                        <- modif
# [217] "Clava"
# [218] "Clevelandia"
# [219] "Clio"
# [220] "Clio pyramidata"
# [221] "Clionidae"
# [222] "Cloeon"
# [223] "Clupea"
# [224] "Clupeid"                         <- modif
# [225] "Clupeidae"
# [226] "Clupeoids"                         <- modif
# [227] "Clymenella"
# [228] "Coastal seabirds"                      <- modif
# [229] "Coccolithophores"                      <- modif
# [230] "Cod"                                 <- modif
# [231] "Coelenterates"                       <- modif
# [232] "Coho salmon"                         <- modif
# [233] "Colonial sessile invertebrates"      <- OK
# [234] "Columbella"
# [235] "Colurostylis lemurum"
# [236] "Cominella glandiformis"
# [237] "Common mummichog"                      <- modif
# [238] "Common murre"                        <- modif
# [239] "Common octopus"                      <- modif
# [240] "Conchoecia"
# [241] "Concholepas concholepas"
# [242] "Condrichtis"                       <- modif
# [243] "Conger conger"
# [244] "Conger marginatus"
# [245] "Copepoda"
# [246] "Coral feeders"                 <- OK
# [247] "Corals"                            <- modif
# [248] "Cormorant"                         <- modif
# [249] "Corophium"
# [250] "Corophium multisetosum"
# [251] "Corvus"
# [252] "Crabeater seal"                    <- modif
# [253] "Crabs"                             <- modif
# [254] "Crago septemspinosus"
# [255] "Crangon"
# [256] "Crangon affinis"
# [257] "Crangon crangon"
# [258] "Crassilabrum crassilabrum"
# [259] "Crassostrea angulata"
# [260] "Crayfish"                          <- modif
# [261] "Crepidula"
# [262] "Crepidulids"                       <- modif
# [263] "Crested goby"                      <- modif
# [264] "Cribina"                           <- modif
# [265] "Cricket"                           <- modif
# [266] "Croakers"                          <- modif
# [267] "Crustacea"
# [268] "Crustacean parts"                  <- modif
# [269] "Crustaceans"                       <- modif
# [270] "Cryptophytes"                      <- modif
# [271] "Crystal krill"                     <- modif
# [272] "Ctenophora"
# [273] "Ctenophores"                       <- modif
# [274] "Cucumberfish"                      <- modif
# [275] "Culex"
# [276] "Cumaceans"                         <- modif
# [277] "Cunner"                            <- modif
# [278] "Curtuteria australis"
# [279] "Cusk"                              <- modif
# [280] "Cutthroat trout"                   <- modif
# [281] "Cyanoplax dientens"
# [282] "Cyathura carinata"
# [283] "Cyclopoida"
# [284] "Cyclopoids"                        <- modif
# [285] "Cylisticus"
# [286] "Cyllopus lucasii"
# [287] "Cymadusa"
# [288] "Cymadusa compta"
# [289] "Cymodopsis montis"
# [290] "Cynoscion"
# [291] "Decamastus"
# [292] "Decapods"                          <- modif
# [293] "Deep-sea fish"         <- OK
# [294] "Demersal fish"         <- OK
# [295] "Demersal piscivores"   <- OK
# [296] "Demersal species"      <- OK
# [297] "Demersals"             <- OK
# [298] "Dentex"
# [299] "Deposit feeders"       <- OK
# [300] "Deposit-feeding gastropods"                  <- modif
# [301] "Deposit-feeding peracaridan crustaceans"     <- modif
# [302] "Deposit-feeding polychaete"                  <- modif
# [303] "Dermasterias"
# [304] "Dermasterias imbricata"
# [305] "Detritivorous fish"            <- OK
# [306] "Detritus"                      <- OK
# [307] "Detritus feeders"              <- OK
# [308] "Diaphus coeruleus"
# [309] "Diaphus elucens"
# [310] "Diastylopsis thileniusi"
# [311] "Diatoms"                         <- modif
# [312] "Diaulota densissima"
# [313] "Dicentrarchus labrax"
# [314] "Diloma subrostrata"
# [315] "Dinoflagellates"                 <- modif
# [316] "Diopatra neapolitana"
# [317] "Diopatra ornata"
# [318] "Discard"                       <- OK
# [319] "Discoglossus"
# [320] "Dissolved inorganic carbon"      <- OK
# [321] "Dissolved organic matter"        <- OK
# [322] "Dogfish"                         <- modif
# [323] "Doliolids"                       <- modif
# [324] "Dolphin coryphaena"              <- modif
# [325] "Dolphins"                        <- modif
# [326] "Dom"                             <- OK
# [327] "Dragonflies"                     <- modif
# [328] "Ducks"                           <- modif
# [329] "Dungeness crab"                  <- modif
# [330] "Dunlin"                          <- modif
# [331] "Dynamenella glabra"
# [332] "Echinoderms"                     <- modif
# [333] "Edwardsia"
# [334] "Eel"                             <- modif
# [335] "Eelgrass"                        <- modif
# [336] "Eider"                           <- modif
# [337] "Elasmopus"
# [338] "Electrona antarctica"
# [339] "Eleotris sandwicensis"
# [340] "Elephant seal"                   <- modif
# [341] "Elopids"                         <- modif
# [342] "Emperor penguins"                <- modif
# [343] "Emplectonema"
# [344] "Emplectonema gracilis"
# [345] "Enchinometra lucunter"
# [346] "Encrusting algae"                  <- OK
# [347] "Endocladia muricata"
# [348] "Engraulis encrasicolus"
# [349] "Engraulis japonica"
# [350] "Enopla"
# [351] "Ensis"
# [352] "Enteromorpha"
# [353] "Enteromorpha intestinalis"
# [354] "Ephausiids"                          <- modif
# [355] "Ephemerids"                          <- modif
# [356] "Epibenthic crustacea"                <- modif
# [357] "Epiphyte-grazing amphipod"           <- modif
# [358] "Epiphyte-grazing gastropods"         <- modif
# [359] "Erichsonella"
# [360] "Erichsonella attenuata"
# [361] "Errant polychaete"                   <- modif
# [362] "Erynnis japonica"
# [363] "Eteone flava"
# [364] "Euchaeta"
# [365] "Euchaeta antarctica"
# [366] "Euchirella rostromagna"
# [367] "Eukrohnia"
# [368] "Eukrohnia hamata"
# [369] "Euphasiids"                          <- modif
# [370] "Euphausia"
# [371] "Euphausia frigida"
# [372] "Euphausia pacifica"
# [373] "Euphausia similis"
# [374] "Euphausia superba"
# [375] "Euphausiacea"
# [376] "Euphausiid"                          <- modif
# [377] "Euphausiids"                         <- modif
# [378] "Eurytium"
# [379] "Exogone"
# [380] "Exogone heterosetosa"
# [381] "Fat sleepers"                        <- modif
# [382] "Fecal material"                      <- OK
# [383] "Filicrisia franciscana"              <- OK
# [384] "Filter feeders"                      <- OK
# [385] "Filter-feeding polychaete"               <- modif
# [386] "Fish"                                  <- OK
# [387] "Fish carnivores"                       <- OK
# [388] "Fish eggs"                           <- OK
# [389] "Fish fry"                              <- OK
# [390] "Fish herbivores"                       <- OK
# [391] "Fish- crustacean-eating birds"           <- modif
# [392] "Fish-eating birds"                       <- modif
# [393] "Fishery offal"                         <- OK
# [394] "Fissurella barbadensis"
# [395] "Flagellates"
# [396] "Flatfish"                              <- modif
# [397] "Flatfish benthic feeder"               <- modif
# [398] "Flatfish water-column feeder"          <- modif
# [399] "Flathead"                              <- modif
# [400] "Flounder"                              <- modif
# [401] "Fluke"                                 <- modif
# [402] "Flying fish"                           <- modif
# [403] "Foraminifera"
# [404] "Foraminiferans"                        <- modif
# [405] "Four-spot flounder"                    <- modif
# [406] "Fucus"
# [407] "Fucus vesiculosus"
# [408] "Fundulus"
# [409] "Fungi"
# [410] "Gaetanus tenuisplnus"
# [411] "Gammaridae"
# [412] "Gammarideas"                       <- modif
# [413] "Gammarids"                         <- modif
# [414] "Gammarus"
# [415] "Gammarus mucronatus"
# [416] "Gastropoda"
# [417] "Gastropods"                        <- modif
# [418] "Geelbek"                           <- modif
# [419] "Gelatinous tunicates"              <- modif
# [420] "Gelatinous zooplankton"
# [421] "Gemma"
# [422] "Gerreids"                          <- modif
# [423] "Gibulla umbilicalis"
# [424] "Gigartina agardhii"
# [425] "Glicera tridactyla"
# [426] "Glycera"
# [427] "Glycera tridactyla"
# [428] "Gobies"                              <- modif
# [429] "Gobiesoma bosci"
# [430] "Gobiesox strumosus"
# [431] "Gobioids"                            <- modif
# [432] "Gobius niger"
# [433] "Goby"                                <- modif
# [434] "Gold-spotted killifish"              <- modif
# [435] "Gomphus"
# [436] "Goosefish"                           <- modif
# [437] "Grapsus grapsus"
# [438] "Graptodytes"
# [439] "Gray whales"                         <- modif
# [440] "Green heron"                         <- modif
# [441] "Green macroalgae"                      <- OK
# [442] "Greenland shark"                     <- modif
# [443] "Greenling"                           <- modif
# [444] "Grenadier"                           <- modif
# [445] "Groundfish"
# [446] "Gull"                                <- modif
# [447] "Gymnophallus"
# [448] "Gymnoscopelus braueri"
# [449] "Haddock"                             <- modif
# [450] "Haematopus ater"
# [451] "Haematopus ostralegus"
# [452] "Haematopus unicolor"
# [453] "Haemulidae"
# [454] "Haemulids"                           <- modif
# [455] "Hakes"                               <- modif
# [456] "Hakess"                              <- modif in WEB 357, Yodsiz
# [457] "Halfbeaks"                           <- modif
# [458] "Halibut"                             <- modif
# [459] "Halichondria"
# [460] "Haliplanella"
# [461] "Haliplus"
# [462] "Halodule"
# [463] "Haloptilus ocellatus"
# [464] "Halosydna"
# [465] "Haminoea hydatis"
# [466] "Harbor seal"                           <- modif
# [467] "Harp seal"                             <- modif
# [468] "Harpacticoids"                         <- modif
# [469] "Hediste diversicolor"
# [470] "Heliaster"
# [471] "Heliaster helianthus"
# [472] "Heliozoans"                            <- modif
# [473] "Hemigrapsus"
# [474] "Hemigrapsus crenulatus"
# [475] "Hemigrapsus edwardsii"
# [476] "Henricia"
# [477] "Herbivorous ducks"                     <- modif
# [478] "Herbivorous fish"
# [479] "Herbivorous gastropods"                <- modif
# [480] "Herbivorous insects"                   <- modif
# [481] "Herbivorous plankton"
# [482] "Herbivorous shrimp"                    <- modif
# [483] "Herbivorous snails"                    <- modif
# [484] "Herbivorous terrestrial insects"       <- modif
# [485] "Herbivorous zooplankton"
# [486] "Hermit crabs"                          <- modif
# [487] "Heron"                                 <- modif
# [488] "Herpetocypris"
# [489] "Herring"                               <- modif
# [490] "Hesionidae"
# [491] "Heterocypris"
# [492] "Heteromastus filiformis"
# [493] "Heterophoxus stephenseni"
# [494] "Heterorhabdus"
# [495] "Heterorhabdus austrinus"
# [496] "Heterosquilla tricarinata"
# [497] "High carnivores"
# [498] "Hippolyte"
# [499] "Hirudinea"
# [500] "Hornyhead turbot"                      <- modif
# [501] "Horse mackerel"                        <- modif
# [502] "Hyale"
# [503] "Hydrobia"
# [504] "Hydrobia ulvae"
# [505] "Hydroides"
# [506] "Hydroids"                              <- modif
# [507] "Hydromedusae"
# [508] "Hydrometra"
# [509] "Hydrurga leptonyx"
# [510] "Hymenosoma"
# [511] "Hypacanthus"
# [512] "Hyperiid"                              <- modif
# [513] "Hyperiidea"
# [514] "Hypoplanktonic crustacea"              <- modif
# [515] "Hyporhampus"
# [516] "Ice algae"                             <- OK
# [517] "Ice invertebrates"
# [518] "Idotea"
# [519] "Idotea balthica"
# [520] "Idotea chelipes"
# [521] "Illex"
# [522] "Import"
# [523] "Infauna"
# [524] "Infusoria"
# [525] "Insects"                               <- modif
# [526] "Intertidal invertebrates"
# [527] "Invertebrate"
# [528] "Invertebrate eggs"
# [529] "Invertebrate predators"
# [530] "Invertebrates"
# [531] "Isopods"                               <- modif
# [532] "Jack"                                  <- modif
# [533] "Jack mackerel"                         <- modif
# [534] "Jackass morwong"                       <- modif
# [535] "Jacks"                                 <- modif
# [536] "Jellyfish"                             <- modif
# [537] "Johnius"
# [538] "Katharina"
# [539] "Kelletia kelletii"
# [540] "Keyhole limpets"                       <- modif
# [541] "Killer whale"                          <- modif
# [542] "Killifish"                             <- modif
# [543] "Kingfisher"                            <- modif
# [544] "Knot"                                  <- modif
# [545] "Kob"                                 <- modif
# [546] "Krill"                                 <- modif
# [547] "Kuhlia sandvicensis"
# [548] "Lagis koreni"
# [549] "Lamya"                                 <- modif
# [550] "Lanternfish"                           <- modif
# [551] "Larus dominicanus"
# [552] "Larus novaehollandiae"
# [553] "Larvaceans"                            <- modif
# [554] "Lasaea cistula"
# [555] "Least killifish"                       <- modif
# [556] "Lebistes reticulatus"
# [557] "Leiostomus"
# [558] "Lekanesphaera levii"
# [559] "Leopard seal"                          <- modif
# [560] "Leptasterias"
# [561] "Leptonychotes weddellii"
# [562] "Lesser scaup"                          <- mocif
# [563] "Leucozonia"
# [564] "Levinseniella"
# [565] "Lichenophora"
# [566] "Lightfish"                       <- modif
# [567] "Limacina"
# [568] "Limacina helicina"
# [569] "Limanda herzensteini"
# [570] "Limanda yokohamae"
# [571] "Limpets"                               <- modif
# [572] "Limulus"
# [573] "Ling"                                  <- modif
# [574] "Lingcod"                               <- modif
# [575] "Liparis tanakai"
# [576] "Lithognathus"
# [577] "Lithophyllum"
# [578] "Litopenaeus"
# [579] "Little skate"                          <- modif
# [580] "Littorina"
# [581] "Littorina littorea"
# [582] "Littorina obtusata"
# [583] "Littorina planaxis"
# [584] "Littorina saxatilis"
# [585] "Littorina scabra"
# [586] "Littorina scutulata"
# [587] "Littorines"                            <- modif
# [588] "Liza aurata"
# [589] "Liza ramada"
# [590] "Lobodon carcinophagus"
# [591] "Lobster"                               <- modif
# [592] "Loggerhead turtles"                    <- modif
# [593] "Loligo"
# [594] "Loligo opalescens"
# [595] "Longfin sand dab"                      <- modif
# [596] "Longhorn sculpin"                      <- modif
# [597] "Loon"                                  <- modif
# [598] "Lophius litulon"
# [599] "Lower carnivores-various fish"
# [600] "Lumbrinereis"
# [601] "Lumbrineris impatiens"
# [602] "Lutjanids"
# [603] "Lutra felina"
# [604] "Mackerel"                              <- modif
# [605] "Macoma"
# [606] "Macomona liliana"
# [607] "Macroalgae"                              <- OK
# [608] "Macrobenthos"
# [609] "Macrobrachium"
# [610] "Macrobranchium"                        <- modif
# [611] "Macroclymenella stewartensis"
# [612] "Macrocrustacea"                          <- modif
# [613] "Macrocystis pyrifera"
# [614] "Macroepiphytes"
# [615] "Macrofauna"
# [616] "Macrophthalmus hirtipes"
# [617] "Macrophytes"
# [618] "Macrozooplankton"
# [619] "Malacoceros"
# [620] "Maldanidae"
# [621] "Mammals"                             <- modif
# [622] "Man"                                 <- modif
# [623] "Manayunkia"
# [624] "Mangrove leaf detritus"
# [625] "Mangrove leaves"
# [626] "Mantis shrimp"                       <- modif
# [627] "Marine birds"                        <- modif
# [628] "Marine invertebrates"
# [629] "Marine mammals"                      <- modif
# [630] "Marine plants"
# [631] "Maritrema novaezealandensis"
# [632] "Market squid"                        <- modif
# [633] "Marlin"                              <- modif
# [634] "Marsh insects"                       <- modif
# [635] "Marsh plants"
# [636] "Marsh spiders"                       <- modif
# [637] "Mat-forming algae"                     <- OK
# [638] "Mediomastus fragilis"
# [639] "Medusae"
# [640] "Megabenthos"
# [641] "Megaceryle"
# [642] "Meibenthos"
# [643] "Meiobenthos"
# [644] "Meiofauna"
# [645] "Meladema"
# [646] "Melampus"
# [647] "Melampus parvulus"
# [648] "Melania indefinita"
# [649] "Melita palmata"
# [650] "Menhaden"                            <- modif
# [651] "Merganser"                         <- modif
# [652] "Merluccius"
# [653] "Meroplankton"
# [654] "Mesopelagic"
# [655] "Mesopelagic fish"
# [656] "Mesozooplankton"
# [657] "Metapenaeopsis dalei"
# [658] "Metopograpsis messor"
# [659] "Metridia"
# [660] "Metridia curticauda"
# [661] "Metridia gerlachei"
# [662] "Metridium"
# [663] "Mexican mojarras"
# [664] "Microbenthos"
# [665] "Microcrustaceans"                    <- modif
# [666] "Microepiphytes"
# [667] "Microfauna"
# [668] "Microflafellates"
# [669] "Microphytobenthos"
# [670] "Micropogon"                          <- modif
# [671] "Microprotozoa"                       <- modif
# [672] "Microtus"
# [673] "Microzooplankton"
# [674] "Middle carnivores-a variety fish"
# [675] "Middlebenthos"
# [676] "Midwater carnivores"
# [677] "Midwater plankton feeders"
# [678] "Minnow"                              <- modif
# [679] "Mirounga leonina"
# [680] "Mista picta"
# [681] "Mixed-food consumers"
# [682] "Modiolus"
# [683] "Modiolus demissus"
# [684] "Mojarras"
# [685] "Molgula manhattensis"
# [686] "Mollies"                             <- modif
# [687] "Molluscs"                            <- modif
# [688] "Mosquito"                            <- modif
# [689] "Mosquito fish"                       <- modif
# [690] "Mud snail"                           <- modif
# [691] "Mugil"
# [692] "Mugil cephalus"
# [693] "Mugilids"                            <- modif
# [694] "Mullet"                              <- modif
# [695] "Muricanthus"                         <- modif
# [696] "Mus"
# [697] "Musculus"
# [698] "Mussel"                              <- modif
# [699] "Mya"
# [700] "Myctophids"                          <- modif
# [701] "Myoxocephalus"
# [702] "Mysidacea"
# [703] "Mysids"                              <- modif
# [704] "Mytilus"
# [705] "Mytilus californianus"
# [706] "Mytilus edulis"
# [707] "Mytilus galloprovincialis"
# [708] "Narwhal"                             <- modif
# [709] "Nassarius"
# [710] "Nauplii"                             <- modif
# [711] "Naupliuses"                          <- modif
# [712] "Nearshore fish"
# [713] "Nearshore phytoplankton"
# [714] "Needlefish"                          <- modif
# [715] "Nematodes"                           <- modif
# [716] "Nemerteans"                          <- modif
# [717] "Nemertini"                           <- modif
# [718] "Nemertopsis gracilis"
# [719] "Neocalanus cristatus"
# [720] "Neocalanus plumchrus"
# [721] "Neopanope texana sayi"
# [722] "Nephthys"                            <- modif
# [723] "Nereids"                             <- modif
# [724] "Nereis"
# [725] "Nereis diversicolor"
# [726] "Nereis grubei"
# [727] "Nereis succinea"
# [728] "Nerites"                             <- modif
# [729] "Neritina tahitiensis"
# [730] "Nodolittorina tuberculata"
# [731] "Northern anchovy"                    <- modif
# [732] "Norway lobster"                      <- modif
# [733] "Notoacmea helmsi"
# [734] "Notolabrus celidotus"
# [735] "Notolepis coatsi"
# [736] "Notomastus"
# [737] "Notonecta"
# [738] "Notoplana acticola"
# [739] "Notothenia rossii"
# [740] "Nucella lapillus"
# [741] "Nucula dunedinensis"
# [742] "Obelia"
# [743] "Ocean perch"                       <- modif
# [744] "Ocean pout"                        <- modif
# [745] "Ocean sunfish"                     <- modif
# [746] "Octopus"
# [747] "Octopus bimaculatus"
# [748] "Oecetis"
# [749] "Oikopleura"
# [750] "Oithona"
# [751] "Oithona-oncaea"                    <- modif
# [752] "Oligochaeta"
# [753] "Oligochaete"                       <- modif
# [754] "Oligochaets"                       <- modif
# [755] "Ommatophoca rossii"
# [756] "Omnivore polychaete"               <- modif
# [757] "Omnivores"
# [758] "Omnivorous crabs"                  <- modif
# [759] "Oncaea"
# [760] "Oncaea antarctica"
# [761] "Onoba"
# [762] "Opeatostoma"
# [763] "Ophiuroids"                        <- modif
# [764] "Orchelimum"
# [765] "Orchestia"
# [766] "Orcinus orca"
# [767] "Organic debris"
# [768] "Organic material"
# [769] "Organic matter"
# [770] "Organic matter in mud"
# [771] "Osprey"                            <- modif
# [772] "Ostracoda"
# [773] "Ostracods"                         <- modif
# [774] "Oxyurichthyes lonchotus"
# [775] "Oystercatcher"                     <- modif
# [776] "Oysters"                           <- modif
# [777] "Pachycheles"
# [778] "Pachygrapsus"
# [779] "Pachygrapsus crassipes"
# [780] "Pacific angel shark"               <- modif
# [781] "Pacific bonito"                    <- modif
# [782] "Pacific hakes"                     <- modif
# [783] "Pacific herring"                   <- modif
# [784] "Pacific mackerel"                  <- modif
# [785] "Pacific menhaden"                  <- modif
# [786] "Pacific sand dab"                  <- modif
# [787] "Paguristes ulreyi"
# [788] "Pagurus"
# [789] "Pagurus samuelis"
# [790] "Palaemon adspersus"
# [791] "Palaemonetes"
# [792] "Palaemonetes vulgaris"
# [793] "Palaemonids"                       <- modif
# [794] "Paleomonetes"                      <- modif
# [795] "Panaeidae"
# [796] "Pandalids"                         <- modif
# [797] "Pandalus"
# [798] "Paracalanus"
# [799] "Paracalanus parvus"
# [800] "Paralichthys"
# [801] "Paraonis"
# [802] "Parapenaeus longirostris"
# [803] "Parapholas californica"
# [804] "Parathemisto gracilis"
# [805] "Particulate detritus"
# [806] "Passerines"                        <- modif
# [807] "Pelagagobia"
# [808] "Pelagic amphipod"                  <- modif
# [809] "Pelagic crustacean"                <- modif
# [810] "Pelagic detritus"
# [811] "Pelagic fish"
# [812] "Pelagic invertebrates"
# [813] "Pelagic seabirds"                  <- modif
# [814] "Pelagics"
# [815] "Pelagobia"
# [816] "Pelagobla"                         <- modif
# [817] "Pelecypod"                         <- modif
# [818] "Pelecypoda"                        <- modif
# [819] "Penaeid shrimp"                    <- modif
# [820] "Penaeus"
# [821] "Penaid"                            <- modif
# [822] "Penguins"                          <- modif
# [823] "Perinereis monterea"
# [824] "Perinereis nuntia"
# [825] "Periphyton"
# [826] "Perrierina turneri"
# [827] "Petrels"                           <- modif
# [828] "Petrolisthes"
# [829] "Phaeocystis"
# [830] "Philoscia"
# [831] "Phoca"
# [832] "Phoronida"
# [833] "Phoronopsis"
# [834] "Phoxocephalus regium"
# [835] "Physeter macrocephalus"
# [836] "Phytobenthos"
# [837] "Phytoplankton"
# [838] "Pilchard"                          <- modif
# [839] "Pimelometopon pulchrum"
# [840] "Pinfish"                           <- modif
# [841] "Pink sea perch"                    <- modif
# [842] "Pinnipeds"                         <- modif
# [843] "Pinnixa rathbuni"
# [844] "Pipefish"                          <- modif
# [845] "Pisaster"
# [846] "Pisaster brevispinus"
# [847] "Pisaster giganteus"
# [848] "Piscivorous fish"
# [849] "Piscivorous rockfish"
# [850] "Pisobia"
# [851] "Plactynemis"
# [852] "Planktivorous rockfish"
# [853] "Plankton"
# [854] "Planktonic bacteria"               <- modif
# [855] "Pleuromamma"
# [856] "Pleuromamma robusta"
# [857] "Pleuronectids"                     <- modif
# [858] "Pleuronectoids"                    <- modif
# [859] "Podophthalmus vigil"
# [860] "Poeciliids"                        <- modif
# [861] "Polinices"
# [862] "Pollicipes"
# [863] "Pollock"                           <- modif
# [864] "Polychaeta"
# [865] "Polychaete"                            <- modif
# [866] "Polynemidae"
# [867] "Pom"
# [868] "Pomatomus"
# [869] "Pomatoschistus minutus"
# [870] "Pontharpinia australis"
# [871] "Pore-water dissolved organic carbon"
# [872] "Porifera"
# [873] "Porolithon"
# [874] "Poronatus"
# [875] "Porpoise shark"                          <- modif
# [876] "Potamya"
# [877] "Prawns"                              <- modif
# [878] "Predaceous crabs"                  <- modif
# [879] "Predaceous fish"
# [880] "Predatory gastropods"              <- modif
# [881] "Predatory polychaete"                    <- modif
# [882] "Predatory shrimp"                  <- modif
# [883] "Prehistoric aleut man"
# [884] "Primary carnivores"
# [885] "Primary producers"
# [886] "Prinospio"
# [887] "Procambarus clarkii"
# [888] "Producers"
# [889] "Profilicollis antarcticus"
# [890] "Profilicollis novaezelandensis"
# [891] "Prokelisia"
# [892] "Protista"
# [893] "Protomyctophum bolini"
# [894] "Protomyctophum tenisoni"
# [895] "Protozoa"
# [896] "Protozoans"                          <- modif
# [897] "Pseudocalanus"
# [898] "Pseudopleuronectes"
# [899] "Pseudotolithus"
# [900] "Pteropods"                           <- modif
# [901] "Pterygophora californica"
# [902] "Pugettia"
# [903] "Purpura"
# [904] "Purpura patula"
# [905] "Pycnopodia"
# [906] "Pygospio elegans"
# [907] "Pyramidellidae"
# [908] "Raccoon"                             <- modif
# [909] "Radiolaria"
# [910] "Radiolarians"                        <- modif
# [911] "Raja"
# [912] "Rallus"
# [913] "Rangia"
# [914] "Raptors"                             <- modif
# [915] "Rattus"
# [916] "Rays"                                <- modif
# [917] "Red drum"                            <- modif
# [918] "Red hakes"                           <- modif
# [919] "Red-winged blackbird"                <- modif
# [920] "Redbait"                             <- modif
# [921] "Redfish"                         <- modif
# [922] "Redshank"                          <- modif
# [923] "Reef fish"
# [924] "Reithrodontomys"
# [925] "Rhabdosargus"
# [926] "Rhincalanus gigas"
# [927] "Rhombognathus"
# [928] "Right whales"                          <- modif
# [929] "Ringed seal"                         <- modif
# [930] "Roccus"
# [931] "Rockfish"
# [932] "Rorquals"                            <- modif
# [933] "Rose shrimp"                         <- modif
# [934] "Round herring"                       <- modif
# [935] "Rubber lip sea perch"                <- modif
# [936] "Ruditapes decussatus"
# [937] "Ruppia"
# [938] "Sablefish"                           <- modif
# [939] "Sagitta gazellae"
# [940] "Sagitta marri"
# [941] "Sagitta nagae"
# [942] "Salmon"                              <- modif
# [943] "Salpa"
# [944] "Salpa thompsoni"
# [945] "Salps"                               <- modif
# [946] "Sand bass"                           <- modif
# [947] "Sand lance"                          <- modif
# [948] "Saprophagous plankton"
# [949] "Sardina pilchardus"
# [950] "Sardine"                             <- modif
# [951] "Sardinella"
# [952] "Saury"                               <- modif
# [953] "Scallops"                            <- modif
# [954] "Scaphocalanus farrani"
# [955] "Schizoporella"
# [956] "School whiting"                        <- modif
# [957] "Sciaenidae"
# [958] "Scianids"                            <- modif
# [959] "Scolecithricella cenotelis"
# [960] "Scoloplos johnstonei"
# [961] "Scomber"
# [962] "Scomber japonicus"
# [963] "Scombrid"                            <- modif
# [964] "Scorpaenids"                         <- modif
# [965] "Scoter"                              <- modif
# [966] "Scrobicularia plana"
# [967] "Scylla serrata"
# [968] "Scyphomedusae"
# [969] "Sea cucumbers"                       <- modif
# [970] "Sea mammals"                         <- modif
# [971] "Sea otters"                          <- modif
# [972] "Sea raven"                           <- modif
# [973] "Sea robins"                          <- modif
# [974] "Sea urchins"                         <- modif
# [975] "Seabirds"                            <- modif
# [976] "Seahorses"                           <- modif
# [977] "Seals"                               <- modif
# [978] "Searlesia"
# [979] "Seaweeds"
# [980] "Secondary carnivores"
# [981] "Sedentary polychaete"                  <- modif
# [982] "Sediment"
# [983] "Sediment bacteria"                   <- modif
# [984] "Sedimentary labile detritus"
# [985] "Sedimentary refractory detritus"
# [986] "Sedimented detritus"
# [987] "Sepia officinalis"
# [988] "Sergestidae"
# [989] "Sergia lucens"
# [990] "Serpulid polychaete"                     <- modif
# [991] "Serranidae"
# [992] "Sertularia"
# [993] "Sesarma"
# [994] "Shad"                                  <- modif
# [995] "Shark"                                 <- modif
# [996] "Sharpnose sea perch"                   <- modif
# [997] "Sheepshead"                            <- modif
# [998] "Sheepshead minnow"                     <- modif
# [999] "Shelduck"                              <- modif
# [1000] "Shiner perch"                         <- modif
# [1001] "Shore crabs"                          <- modif
# [1002] "Shorebirds"                               <- modif
# [1003] "Shrimp"                             <- modif
# [1004] "Sialis"
# [1005] "Sicyases sanguineus"
# [1006] "Sigara"
# [1007] "Silicoflagellates"
# [1008] "Silver hakes"                           <- modif
# [1009] "Silversides"                              <- modif
# [1010] "Siphonophores"                        <- modif
# [1011] "Sipuncula"
# [1012] "Sipunculids"                          <- modif
# [1013] "Skates"                               <- modif
# [1014] "Smelt"                              <- modif
# [1015] "Smooth dogfish"                       <- modif
# [1016] "Snails"                               <- modif
# [1017] "Snake mackerel"                       <- modif
# [1018] "Snappers"                           <- modif
# [1019] "Snoek"                              <- modif
# [1020] "Snooks"                                 <- modif
# [1021] "Sockeye salmon"                       <- modif
# [1022] "Solea vulgaris"
# [1023] "Solemya"
# [1024] "Solen"
# [1025] "Solitary tunicates"                   <- modif
# [1026] "Song birds"                           <- modif
# [1027] "Sooty shearwaters"                    <- modif
# [1028] "Sorex"
# [1029] "Southern hakes"                       <-  modif
# [1030] "Sparids"                              <- modif
# [1031] "Spartina"
# [1032] "Spartina glabra"
# [1033] "Spartina patena"
# [1034] "Sperm whale"                          <- modif
# [1035] "Sphaerodorum"
# [1036] "Spider crabs"                         <- modif
# [1037] "Spiders"                              <- modif
# [1038] "Spinocalanus abyssalis"
# [1039] "Spiny dogfish"                        <- modif
# [1040] "Spio"
# [1041] "Spirobranchus giganteus"
# [1042] "Spirorbis"
# [1043] "Sponges"                              <- modif
# [1044] "Spot"                                 <- modif
# [1045] "Spotted hakes"                        <- modif
# [1046] "Sprattus"
# [1047] "Squid"                                <- modif
# [1048] "Starfish"                             <- modif
# [1049] "Steelhead trout"                      <- modif
# [1050] "Stenella"
# [1051] "Sterna"
# [1052] "Sterna striata"
# [1053] "Stictotarsus"
# [1054] "Stingray"                             <- modif
# [1055] "Stomatopods"                          <- modif
# [1056] "Storm-petrels"                        <- modif
# [1057] "Streblospio"
# [1058] "Streblospio shrubsolii"
# [1059] "Striped bass"                         <- modif
# [1060] "Striped mummichog"                    <- modif
# [1061] "Strongylocentrotus"
# [1062] "Strongylocentrotus franciscanus"
# [1063] "Strongylocentrotus purpuratus"
# [1064] "Styela montereyensis"
# [1065] "Sub-surface deposit-feeding polychaete"     <- modif
# [1066] "Suidasia"
# [1067] "Summer flounder"                      <- modif
# [1068] "Suprabenthos"
# [1069] "Surface deposit-feeding polychaete"           <- modif
# [1070] "Suspended detritus"
# [1071] "Suspended or deposited organic matter"
# [1072] "Suspension-feeding epifauna"
# [1073] "Suspension-feeding molluscs"
# [1074] "Suspension-feeding polychaete"                <- modif
# [1075] "Swimming crabs"                           <- modif
# [1076] "Sygnathus floridae"
# [1077] "Sygnathus fuscus"
# [1078] "Syllis spenceri"
# [1079] "Syllis vittata"
# [1080] "Sympetrum"
# [1081] "Symphodus bailloni"
# [1082] "Talorchestia"
# [1083] "Tanaidacidae"
# [1084] "Tanner crab"                          <- modif
# [1085] "Tanypodinae"
# [1086] "Tautogolabrus"
# [1087] "Tealia coriacea"
# [1088] "Tegula"
# [1089] "Tegula funebralis"
# !!!!!!! [1090] "Tendipes"                         <- *not found and article unavailable* !!!!!!
# [1091] "Tern"                                 <- modif
# [1092] "Terrestrial invertebrates"
# [1093] "Terrestrial plants"                 <- modif
# [1094] "Tertiary carnivores"
# [1095] "Tethya aurantia"
# [1096] "Tetraclita squamosa"
# [1097] "Thais"
# [1098] "Thais biserialis"
# [1099] "Thais canaliculata"
# [1100] "Thais emarginata"
# [1101] "Thais lapillus"
# [1102] "Thais lima"
# [1103] "Thais melones"
# [1104] "Thais triangularis"
# [1105] "Thaliacians"                          <- modif
# [1106] "Theobaldia"
# [1107] "Thorny skate"                         <- modif
# [1108] "Thyrsites atun"
# [1109] "Thysanoessa"
# [1110] "Thysanoessa macrura"
# [1111] "Thysanoessa spinifera"
# [1112] "Tilapia mossambica"
# [1113] "Tintinnids"                           <- modif
# [1114] "Tipulidae"
# [1115] "Toadfish"
# [1116] "Todarodes pacificus"
# [1117] "Tomopterids"                          <- modif
# [1118] "Tomopteris"
# [1119] "Tonguefish"                           <- modif
# [1120] "Tonicella"
# [1121] "Tooth-fish"                           <- modif
# [1122] "Toothed whales"                 <- modif
# [1123] "Top carnivores-tarpon"
# [1124] "Trachurus"
# [1125] "Trachurus japonicus"
# [1126] "Transient carnivores"
# [1127] "Trigla lucerna"
# [1128] "Trout"                          <- modif
# [1129] "Tuna"                           <- modif
# [1130] "Tunicates"                      <- modif
# [1131] "Turnstone"                      <- modif
# [1132] "Uca"
# [1133] "Ulothrix"
# [1134] "Ulva"
# [1135] "Upogebia"
# [1136] "Urchins"                        <- modif
# [1137] "Urechis"
# [1138] "Urosalpinx cinerea"
# [1139] "Vanellus miles"
# [1140] "Vermetids"
# [1141] "Vermilion rockfish"             <- modif
# [1142] "Vertically migrating mesopelagic fish"
# [1143] "Vibilia stebbingi"
# [1144] "Wading birds"                           <- modif
# [1145] "Walrus"                     <- modif
# [1146] "Warehous"                           <- modif
# [1147] "Water column dissolved organic carbon"
# [1148] "Water column labile detritus"
# [1149] "Water column refractory detritus"
# [1150] "Water plant"
# [1151] "Waterfowl"                      <- modif
# [1152] "Weakfish"               <- modif
# [1153] "Weddell seals"            <- modif
# [1154] "Whales"                   <- modif
# [1155] "White croaker"          <- modif
# [1156] "White hakes"              <- modif
# [1157] "Wigeongrass"              <- modif
# [1158] "Windowpane flounder"          <- modif
# [1159] "Winter flounder"              <- modif
# [1160] "Winter skate"               <- modif
# [1161] "Witch flounder"           <- modif
# [1162] "Wolffish"                 <- modif
# [1163] "Worms"                  <- modif
# [1164] "Xiphophorous helleri"
# [1165] "Yellowtail"                     <- modif
# [1166] "Yellowtail flounder"        <- modif
# [1167] "Zeacumantus subcarinatus"
# [1168] "Zooflagellates"
# [1169] "Zooplanktivorous fish"
# [1170] "Zooplankton"
# [1171] "Zooplankton herbivores"
# [1172] "Zostera"
# [1173] "Zostera marina"
# [1174] "Zostera noltii"

#!!!!!!!!!!!!!!!!!!!!!!!!!!! MANUAL RESOLVE 2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# [1] "!!!!!!!!! NO RESULTS OBTAINED FROM gnr_resolve() !!!!!!!!!"
#      tx.list                                 valid       rank
# 821  "Acanthocitona pygmaea"                 "valid"     NA <- modif
# 822  "Acetocina candei"                      "valid"     NA <- modif
# 66   "Algae"                                 "valid"     NA <- ok
# 173  "Algal feeders"                         "valid"     NA <- ok
# 825  "Amphitritidae"                         "valid"     NA <- modif
# 945  "Ardenna griseus"                       "no result" NA <- modif
# 13   "Attached plants"                       "no result" NA <- ok
# 833  "Bacterioplankton"                      "valid"     NA <- ok
# 1    "Basic food"                            "valid"     NA <- ok
# 175  "Benthic algae"                         "no result" NA <- ok
# 176  "Benthic carnivores"                    "valid"     NA <- ok
# 946  "Benthic detritus"                      "no result" NA <- ok
# 764  "Benthic epifauna"                      "no result" NA <- ok
# 1124 "Benthic filter-feeders"                "valid"     NA <- ok
# 947  "Benthic fish"                          "no result" NA <- modif
# 765  "Benthic infauna"                       "no result" NA <- ok
# 190  "Benthic invertebrates"                 "valid"     NA <- ok
# 109  "Benthic macrofauna"                    "valid"     NA <- ok
# 191  "Benthic vertebrates"                   "no result" NA <- ok
# 741  "Benthic-feeding fish"                  "no result" NA <- modif
# 948  "Benthivorous rockfish"                 "no result" NA <- ok
# 1021 "Benthopelagic fish"                    "no result" NA <- modif
# 2    "Benthos"                               "valid"     NA <- ok
# 394  "Benthos-eating fish"                   "no result" NA <- modif
# 520  "Blue-green algae"                      "no result" NA <- ok
# 834  "Brachiodontes exustus"                 "valid"     NA <- modif
# 836  "Cadulus carolinesis"                   "valid"     NA <- modif
# 3    "Carnivores"                            "no result" NA <- ok
# 950  "Carnivorous epifauna"                  "invalid"   NA <- ok
# 207  "Carnivorous plankton"                  "invalid"   NA <- ok
# 742  "Carnivorous zooplankton"               "invalid"   NA <- ok
# 321  "Chalina"                               "valid"     NA <- modif
# 156  "Chonophorous genivittatus"             "valid"     NA <- modif
# 280  "Coelenterata"                          "valid"     NA <- ok
# 495  "Colonial sessile invertebrates"        "valid"     NA <- ok
# 177  "Coral feeders"                         "valid"     NA <- ok
# 523  "Cyanoplax dientens"                    "valid"     NA <- modif
# 1023 "Deep-sea fish"                         "no result" NA <- modif
# 696  "Demersal fish"                         "invalid"   NA <- modif
# 1024 "Demersal piscivores"                   "invalid"   NA <- ok
# 113  "Demersal species"                      "invalid"   NA <- ok
# 766  "Demersals"                             "invalid"   NA <- ok
# 609  "Deposit feeders"                       "valid"     NA <- ok
# 5    "Detritus"                              "valid"     NA <- ok
# 178  "Detritus feeders"                      "invalid"   NA <- ok
# 164  "Dinoflagellata"                        "valid"     NA <- ok Infraphylum
# 1025 "Discard"                               "no result" NA <- ok
# 935  "Dissolved inorganic carbon"            "no result" NA <- ok
# 282  "Dissolved organic matter"              "valid"     NA <- ok
# 1051 "Dom"                                   "no result" NA <- ok
# 526  "Emplectonema gracilis"                 "valid"     NA <- modif
# 367  "Enchinometra lucunter"                 "valid"     NA <- modif
# 563  "Encrusting algae"                      "valid"     NA <- ok
# 847  "Erichsionella"                         "valid"     NA <- modif
# 444  "Erynnis japonica"                      "valid"     NA <- modif
# 619  "Fecal material"                        "no result" NA <- ok
# 610  "Filter feeders"                        "valid"     NA <- ok
# 54   "Fish"                                  "valid"     NA <- ok
# 6    "Fish carnivores"                       "invalid"   NA <- modif
# 956  "Fish eggs"                             "invalid"   NA <- modif
# 324  "Fish fry"                              "invalid"   NA <- modif
# 7    "Fish herbivores"                       "invalid"   NA <- modif
# 957  "Fishery offal"                         "no result" NA <- ok
# 115  "Flagellates"                           "valid"     NA <- ok
# 1026 "Gelatinous zooplankton"                "valid"     NA <- ok
# 719  "Gibulla umbilicalis"                   "no result" NA <- modif
# 1126 "Groundfish"                            "no result" NA <- modif
# 497  "Herbivorous fish"                      "no result" NA <- modif
# 209  "Herbivorous plankton"                  "valid"     NA <- ok
# 855  "Herbivorous shrimp"                    "no result" NA <- modif
# 747  "Herbivorous zooplankton"               "no result" NA <- ok
# 625  "High carnivores"                       "valid"     NA <- ok
# 912  "Hydromedusae"                          "no result" NA
# 858  "Hylina veliei"                         "valid"     NA <- modif
# 15   "Hypacanthus"                           "valid"     NA <- modif
# 185  "Ice algae"                             "valid"     NA <- ok
# 186  "Ice invertebrates"                     "valid"     NA <- ok
# 726  "Import"                                "no result" NA <- ok
# 799  "Infauna"                               "no result" NA <- ok
# 489  "Infusoria"                             "valid"     NA <- ok
# 55   "Intertidal invertebrates"              "no result" NA <- ok
# 960  "Invertebrate"                          "no result" NA <- ok
# 961  "Invertebrate eggs"                     "no result" NA <- ok
# 613  "Invertebrate predators"                "valid"     NA <- ok
# 248  "Invertebrates"                         "valid"     NA <- ok
# 328  "Lichenophora"                          "valid"     NA <- modif
# 626  "Lower carnivores-various fish"         "invalid"   NA <- ok
# 116  "Macroalgae"                            "valid"     NA <- ok
# 699  "Macrobenthos"                          "valid"     NA <- ok
# 862  "Macroepiphytes"                        "invalid"   NA <- ok
# 936  "Macrofauna"                            "no result" NA <- ok
# 792  "Macrophytes"                           "valid"     NA <- ok
# 963  "Macrozooplankton"                      "valid"     NA <- ok
# 627  "Mangrove leaf detritus"                "no result" NA <- ok
# 628  "Mangrove leaves"                       "valid"     NA <- ok
# 56   "Marine invertebrates"                  "valid"     NA <- ok
# 37   "Marsh plants"                          "no result" NA <- ok
# 566  "Mat-forming algae"                     "no result" NA <- ok
# 490  "Medusae"                               "no result" NA <- modif
# 611  "Meibenthos"                            "valid"     NA <- ok
# 305  "Meiobenthos"                           "invalid"   NA <- ok
# 419  "Meiofauna"                             "valid"     NA <- ok
# 152  "Melania indefinita"                    "valid"     NA <- modif
# 117  "Meroplankton"                          "valid"     NA <- ok
# 701  "Mesopelagic"                           "invalid"   NA <- ok
# 270  "Mesopelagic fish"                      "no result" NA <- modif
# 1028 "Mesozooplankton"                       "valid"     NA <- ok
# 158  "Metopograpsis messor"                  "valid"     NA <- modif
# 306  "Microbenthos"                          "invalid"   NA <- ok
# 867  "Microepiphytes"                        "invalid"   NA <- ok
# 868  "Microfauna"                            "valid"     NA <- ok
# 614  "Microflafellates"                      "invalid"   NA <- ok
# 731  "Microphytobenthos"                     "no result" NA <- ok
# 966  "Microzooplankton"                      "valid"     NA <- ok
# 702  "Middlebenthos"                         "no result" NA <- ok
# 179  "Midwater carnivores"                   "valid"     NA <- ok
# 180  "Midwater plankton feeders"             "no result" NA <- ok
# 218  "Mixed-food consumers"                  "valid"     NA <- ok
# 803  "Mojarras"                              "no result" NA <- modif
# 313  "Nearshore fish"                        "no result" NA <- modif
# 314  "Nearshore phytoplankton"               "no result" NA <- ok
# 159  "Neritina tahitiensis"                  "valid"     NA <- modif
# 371  "Nodolittorina tuberculata"             "valid"     NA <- modif
# 181  "Omnivores"                             "valid"     NA <- ok
# 595  "Organic matter"                        "valid"     NA <- ok
# 335  "Organic matter in mud"                 "valid"     NA <- ok
# 236  "Oxyurichthyes lonchotus"               "valid"     NA <- modif
# 9    "Panaeidae"                             "no result" NA <- modif
# 881  "Paracerces caudata"                    "valid"     NA <- modif
# 472  "Parathemisto gracilis"                 "valid"     NA <- modif
# 119  "Particulate detritus"                  "no result" NA <- ok
# 602  "Passeri"                               "no result" NA <- modif
# 883  "Pectanaridae"                          "valid"     NA <- modif
# 930  "Pelagagobia"                           "no result" NA <- modif
# 970  "Pelagic detritus"                      "no result" NA <- ok
# 615  "Pelagic fish"                          "no result" NA <- modif
# 1041 "Pelagic invertebrates"                 "no result" NA <- ok
# 704  "Pelagics"                              "invalid"   NA <- ok
# 437  "Penaeid shrimp"                        "invalid"   NA  <- modif
# 612  "Phytobenthos"                          "valid"     NA <- ok
# 20   "Phytoplankton"                         "valid"     NA <- ok
# 886  "Pinixia floridana"                     "valid"     NA <- modif
# 402  "Piscivorous fish"                      "no result" NA <- modif
# 971  "Piscivorous rockfish"                  "no result" NA <- modif
# 607  "Pisobia"                               "no result" NA <- modif
# 460  "Plactynemis"                           "valid"     NA <- modif
# 972  "Planktivorous rockfish"                "no result" NA <- modif
# 43   "Plankton"                              "valid"     NA <- ok
# 815  "Pom"                                   "invalid"   NA <- ok
# 937  "Pore-water dissolved organic carbon"   "no result" NA <- ok
# 403  "Potamya"                               "valid"     NA <- modif (Genus)
# 502  "Predaceous fish"                       "no result" NA <- modif
# 887  "Predatory shrimp"                      "no result" NA <- modif
# 315  "Prehistoric aleut man"                 "no result" NA <- ok
# 224  "Primary carnivores"                    "valid"     NA <- ok
# 680  "Prinospio"                             "no result" NA <- modif
# 439  "Producers"                             "valid"     NA <- ok
# 1110 "Protista"                              "valid"     NA <- ok (Kingdom)
# 79   "Raccoon"                               "no result" NA <- modif
# 374  "Reef fish"                             "no result" NA <- modif
# 978  "Rockfish"                              "no result" NA <- modif
# 226  "Saprophagous plankton"                 "valid"     NA <- ok
# 923  "Scyphomedusae"                         "valid"     NA <- modif
# 556  "Seaweeds"                              "valid"     NA <- ok
# 227  "Secondary carnivores"                  "valid"     NA <- ok
# 817  "Sediment"                              "valid"     NA <- ok
# 938  "Sedimentary labile detritus"           "no result" NA <- ok
# 939  "Sedimentary refractory detritus"       "no result" NA <- ok
# 427  "Sedimented detritus"                   "no result" NA <- ok
# 1075 "Silicoflagellates"                     "no result" NA <- ok
# 896  "Southern hakes"                        "no result" NA <- modif
# 608  "Spartina patena"                       "valid"     NA <- modif
# 1034 "Suprabenthos"                          "no result" NA <- ok
# 429  "Suspended detritus"                    "no result" NA <- ok
# 451  "Suspended or deposited organic matter" "valid"     NA <- ok
# 982  "Suspension-feeding epifauna"           "no result" NA <- ok
# 898  "Swartziella catesbyana"                "valid"     NA <- modif
# 1111 "Sygnathus fuscus"                      "invalid"   NA <- modif
# 900  "Symphurus plagisua"                    "valid"     NA <- modif
# 65   "Terrestrial invertebrates"             "valid"     NA <- ok
# 228  "Tertiary carnivores"                   "valid"     NA <- ok
# 506  "Thais biserialis"                      "valid"     NA <- modif
# 358  "Thais lima"                            "valid"     NA <- modif
# 485  "Theobaldia"                            "no result" NA <- modif
# 808  "Toadfish"                              "no result" NA <- modif
# 632  "Top carnivores-tarpon"                 "no result" NA <- ok
# 182  "Transient carnivores"                  "valid"     NA <- ok
# 360  "Tunicates"                             "valid"     NA <- modif
# 906  "Unident bivalves"                      "no result" NA <- modif
# 260  "Vermilion rockfish"                    "no result" NA <- modif
# 275  "Vertically migrating mesopelagic fish" "no result" NA <- ok
# 940  "Water column dissolved organic carbon" "no result" NA <- ok
# 941  "Water column labile detritus"          "no result" NA <- ok
# 942  "Water column refractory detritus"      "no result" NA <- ok
# 286  "Zooflagellates"                        "valid"     NA <- ok
# 761  "Zooplanktivorous fish"                 "no result" NA <- modif
# 24   "Zooplankton"                           "valid"     NA <- ok
# 10   "Zooplankton herbivores"                "invalid"   NA <- ok
# [1] "!!!!!!!!! INVALID RESULTS FROM gnr_resolve() !!!!!!!!!"
#       submitted_name                          canonical_form
 # [1,] "Anisotremus interreptus"               "Anisotremus interruptus" <- modif
 # [2,] "Archtocephalus gazella"                "Arctocephalus gazella" <- modif
 # [3,] "Aristeu varidens"                      "Aristeus varidens" <- modif
 # [4,] "Atherinomuros stipes"                  "Atherinomorus stipes"  <- modif
 # [5,] "Cailianassa"                           "Callianassa" <- modif
 # [6,] "Carnivorous epifauna"                  "Carnivorous" <- OK
 # [7,] "Carnivorous plankton"                  "Carnivorous" <- OK
 # [8,] "Carnivorous zooplankton"               "Carnivorous" <- OK
 # [9,] "Chthamalus microtretus"                "Chthamalus"  <- modif
# [10,] "Demersal fish"                         "Demersal"  <- OK
# [11,] "Demersal piscivores"                   "Demersal"  <- OK
# [12,] "Demersal species"                      "Demersal"  <- OK
# [13,] "Demersals"                             "Demersal"  <- OK
# [14,] "Detritus feeders"                      "Detritus"  <- OK
# [15,] "Diapterus peruvians"                   "Diapterus" <- modif
# [16,] "Dormitator latrifons"                  "Dormitator latifrons"  <- modif
# [17,] "Dorosoma petense"                      "Dorosoma"  <- modif
# [18,] "Fish carnivores"                       "Fish"  <- OK
# [19,] "Fish eggs"                             "Fish"  <- OK
# [20,] "Fish fry"                              "Fish"  <- OK
# [21,] "Fish herbivores"                       "Fish"  <- OK
# [22,] "Gaetanus tenuisplnus"                  "Gaetanus tenuispinus"  <- modif
# [23,] "Glicera tridactyla"                    "Glycera tridactyla"  <- modif
# [24,] "Gobiesoma bosci"                       "Gobiosoma bosci" <- modif
# [25,] "Green macroalgae"                      "Green" <- OK
# [26,] "Hemigrapsus edwardsii"                 "Hemigrapsus" <- modif
# [27,] "Hemiramphus brasilienis"               "Hemiramphus brasiliensis"  <- modif
# [28,] "Heterophoxus stephenseni"              "Heterophoxus"  <- OK
# [29,] "Lower carnivores-various fish"         "Lower" <- ok
# [30,] "Macroepiphytes"                        "Macro-epiphytes" <- ok
# [31,] "Marine plants"                         "Marine"  <- ok
# [32,] "Megabenthos"                           "Megapenthes" <- ok
# [33,] "Meiobenthos"                           "Meibenthos"  <- ok
# [34,] "Mesopelagic"                           "Mesopelagica"  <- ok
# [35,] "Mexican mojarras"                      "Mexican" <- modif
# [36,] "Microbenthos"                          "Macrobenthos"  <- ok
# [37,] "Microepiphytes"                        "Micro-epiphytes" <- ok
# [38,] "Microflafellates"                      "Microflagellates"  <- ok
# [39,] "Middle carnivores-a variety fish"      "Middle"  <- ok
# [40,] "Mista picta"                           "Mysta picta" <- modif
# [41,] "Oncacea"                               "Oncaea"  <- modif
# [42,] "Organic debris"                        "Organic" <- ok
# [43,] "Organic material"                      "Organic" <- ok
# [44,] "Pagurus mcglaughlini"                  "Pagurus"  <- modif
# [45,] "Pelagics"                              "Pelagica"  <- ok
# [46,] "Penaeid shrimp"                        "Penaeid" <- modif
# [47,] "Phoxocephalus regium"                  "Phoxocephalus" <- ok
# [48,] "Physeter macrocephalus"                "Physeter"  <- ok
# [49,] "Pleuronectiformes benthic feeder"      "Pleuronectiformes" <- modif
# [50,] "Pleuronectiformes water-column feeder" "Pleuronectiformes" <- modif
# [51,] "Pom"                                   NA  <- ok
# [52,] "Pontharpinia australis"                "Pontharpinia"  <- ok
# [53,] "Poronatus"                             "Poronotus" <- modif
# [54,] "Prunum aureocincta"                    "Prunum"  <- modif
# [55,] "Sea robins"                            "Sea" <- modif
# [56,] "Sygnathus floridae"                    "Syngnathus floridae"   <- modif
# [57,] "Sygnathus fuscus"                      "Sygnathus"         <- modif
# [58,] "Tanaidacidae"                          "Tanaididae"  <- modif
# [59,] "Xiphophorous helleri"                  "Xiphophorus helleri" <- modif
# [60,] "Zooplankton herbivores"                "Zooplankton" <- ok

#!!!!!!!!!!!!!!!!!!!!!!!!!!! MANUAL RESOLVE 3 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# [1] "!!!!!!!!! NO RESULTS OBTAINED FROM gnr_resolve() !!!!!!!!!"
#      tx.list                                 valid       rank
# 64   "Algae"                                 "valid"     NA
# 171  "Algal feeders"                         "valid"     NA
# 12   "Attached plants"                       "no result" NA
# 815  "Bacterioplankton"                      "valid"     NA
# 1    "Basic food"                            "valid"     NA
# 173  "Benthic algae"                         "no result" NA
# 174  "Benthic carnivores"                    "valid"     NA
# 931  "Benthic detritus"                      "no result" NA
# 746  "Benthic epifauna"                      "no result" NA
# 1097 "Benthic filter-feeders"                "valid"     NA
# 747  "Benthic infauna"                       "no result" NA
# 188  "Benthic invertebrates"                 "valid"     NA
# 107  "Benthic macrofauna"                    "valid"     NA
# 189  "Benthic vertebrates"                   "no result" NA
# 2    "Benthos"                               "valid"     NA
# 508  "Blue-green algae"                      "no result" NA
# 3    "Carnivores"                            "no result" NA
# 933  "Carnivorous epifauna"                  "invalid"   NA
# 205  "Carnivorous plankton"                  "invalid"   NA
# 725  "Carnivorous zooplankton"               "invalid"   NA
# 277  "Coelenterata"                          "valid"     NA
# 485  "Colonial sessile invertebrates"        "valid"     NA
# 175  "Coral feeders"                         "valid"     NA
# 999  "Demersal piscivores"                   "invalid"   NA
# 111  "Demersal species"                      "invalid"   NA
# 748  "Demersals"                             "invalid"   NA
# 596  "Deposit feeders"                       "valid"     NA
# 5    "Detritus"                              "valid"     NA
# 176  "Detritus feeders"                      "invalid"   NA
# 162  "Dinoflagellata"                        "valid"     NA
# 1000 "Discard"                               "no result" NA
# 921  "Dissolved inorganic carbon"            "no result" NA
# 279  "Dissolved organic matter"              "valid"     NA
# 1026 "Dom"                                   "no result" NA
# 551  "Encrusting algae"                      "valid"     NA
# 605  "Fecal material"                        "no result" NA
# 597  "Filter feeders"                        "valid"     NA
# 938  "Fishery offal"                         "no result" NA
# 113  "Flagellates"                           "valid"     NA
# 1001 "Gelatinous zooplankton"                "valid"     NA
# 207  "Herbivorous plankton"                  "valid"     NA
# 837  "Herbivorous shrimp"                    "no result" NA
# 730  "Herbivorous zooplankton"               "no result" NA
# 611  "High carnivores"                       "valid"     NA
# 183  "Ice algae"                             "valid"     NA
# 184  "Ice invertebrates"                     "valid"     NA
# 711  "Import"                                "no result" NA
# 784  "Infauna"                               "no result" NA
# 480  "Infusoria"                             "valid"     NA
# 53   "Intertidal invertebrates"              "no result" NA
# 941  "Invertebrate"                          "no result" NA
# 942  "Invertebrate eggs"                     "no result" NA
# 600  "Invertebrate predators"                "valid"     NA
# 246  "Invertebrates"                         "valid"     NA
# 612  "Lower carnivores-various fish"         "invalid"   NA
# 114  "Macroalgae"                            "valid"     NA
# 684  "Macrobenthos"                          "valid"     NA
# 842  "Macroepiphytes"                        "invalid"   NA
# 922  "Macrofauna"                            "no result" NA
# 774  "Macrophytes"                           "valid"     NA
# 944  "Macrozooplankton"                      "valid"     NA
# 613  "Mangrove leaf detritus"                "no result" NA
# 614  "Mangrove leaves"                       "valid"     NA
# 54   "Marine invertebrates"                  "valid"     NA
# 36   "Marsh plants"                          "no result" NA
# 554  "Mat-forming algae"                     "no result" NA
# 598  "Meibenthos"                            "valid"     NA
# 302  "Meiobenthos"                           "invalid"   NA
# 411  "Meiofauna"                             "valid"     NA
# 115  "Meroplankton"                          "valid"     NA
# 686  "Mesopelagic"                           "invalid"   NA
# 1003 "Mesozooplankton"                       "valid"     NA
# 303  "Microbenthos"                          "invalid"   NA
# 848  "Microepiphytes"                        "invalid"   NA
# 849  "Microfauna"                            "valid"     NA
# 601  "Microflafellates"                      "invalid"   NA
# 716  "Microphytobenthos"                     "no result" NA
# 947  "Microzooplankton"                      "valid"     NA
# 687  "Middlebenthos"                         "no result" NA
# 177  "Midwater carnivores"                   "valid"     NA
# 178  "Midwater plankton feeders"             "no result" NA
# 216  "Mixed-food consumers"                  "valid"     NA
# 310  "Nearshore phytoplankton"               "no result" NA
# 156  "Neripteron taitense"                   "valid"     NA
# 179  "Omnivores"                             "valid"     NA
# 583  "Organic matter"                        "valid"     NA
# 330  "Organic matter in mud"                 "valid"     NA
# 862  "Palaemonetes floridanus"               "valid"     NA
# 117  "Particulate detritus"                  "no result" NA
# 951  "Pelagic detritus"                      "no result" NA
# 1016 "Pelagic invertebrates"                 "no result" NA
# 689  "Pelagics"                              "invalid"   NA
# 599  "Phytobenthos"                          "valid"     NA
# 19   "Phytoplankton"                         "valid"     NA
# 8    "Pisces"                                "valid"     NA
# 42   "Plankton"                              "valid"     NA
# 797  "Pom"                                   "invalid"   NA
# 923  "Pore-water dissolved organic carbon"   "no result" NA
# 311  "Prehistoric aleut man"                 "no result" NA
# 222  "Primary carnivores"                    "valid"     NA
# 430  "Producers"                             "valid"     NA
# 1084 "Protista"                              "valid"     NA
# 224  "Saprophagous plankton"                 "valid"     NA
# 544  "Seaweeds"                              "valid"     NA
# 225  "Secondary carnivores"                  "valid"     NA
# 799  "Sediment"                              "valid"     NA
# 924  "Sedimentary labile detritus"           "no result" NA
# 925  "Sedimentary refractory detritus"       "no result" NA
# 419  "Sedimented detritus"                   "no result" NA
# 1049 "Silicoflagellates"                     "no result" NA
# 1009 "Suprabenthos"                          "no result" NA
# 421  "Suspended detritus"                    "no result" NA
# 442  "Suspended or deposited organic matter" "valid"     NA
# 959  "Suspension-feeding epifauna"           "no result" NA
# 63   "Terrestrial invertebrates"             "valid"     NA
# 226  "Tertiary carnivores"                   "valid"     NA
# 618  "Top carnivores-tarpon"                 "no result" NA
# 180  "Transient carnivores"                  "valid"     NA
# 272  "Vertically migrating mesopelagic fish" "no result" NA
# 926  "Water column dissolved organic carbon" "no result" NA
# 927  "Water column labile detritus"          "no result" NA
# 928  "Water column refractory detritus"      "no result" NA
# 283  "Zooflagellates"                        "valid"     NA
# 23   "Zooplankton"                           "valid"     NA
# 9    "Zooplankton herbivores"                "invalid"   NA
# [1] "!!!!!!!!! INVALID RESULTS FROM gnr_resolve() !!!!!!!!!"
#       submitted_name                     canonical_form
#  [1,] "Carnivorous epifauna"             "Carnivorous"
#  [2,] "Carnivorous plankton"             "Carnivorous"
#  [3,] "Carnivorous zooplankton"          "Carnivorous"
#  [4,] "Demersal piscivores"              "Demersal"
#  [5,] "Demersal species"                 "Demersal"
#  [6,] "Demersals"                        "Demersal"
#  [7,] "Detritus feeders"                 "Detritus"
#  [8,] "Green macroalgae"                 "Green"
#  [9,] "Heterophoxus stephenseni"         "Heterophoxus"
# [10,] "Lower carnivores-various fish"    "Lower"
# [11,] "Macroepiphytes"                   "Macro-epiphytes"
# [12,] "Marine plants"                    "Marine"
# [13,] "Megabenthos"                      "Megapenthes"
# [14,] "Meiobenthos"                      "Meibenthos"
# [15,] "Mesopelagic"                      "Mesopelagica"
# [16,] "Microbenthos"                     "Macrobenthos"
# [17,] "Microepiphytes"                   "Micro-epiphytes"
# [18,] "Microflafellates"                 "Microflagellates"
# [19,] "Middle carnivores-a variety fish" "Middle"
# [20,] "Organic debris"                   "Organic"
# [21,] "Organic material"                 "Organic"
# [22,] "Pelagics"                         "Pelagica"
# [23,] "Penaeus duoararum"                "Penaeus duorarum"
# [24,] "Phoxocephalus regium"             "Phoxocephalus"
# [25,] "Physeter macrocephalus"           "Physeter"
# [26,] "Pom"                              NA
# [27,] "Pontharpinia australis"           "Pontharpinia"
# [28,] "Prunum succineum"                 "Prunum"
# [29,] "Zooplankton herbivores"           "Zooplankton"
# [1] "!!!!!!!!! MULTIPLE TAXON FROM gnr_resolve() !!!!!!!!!"
    #  tx.list                      valid   rank
# 65   "Anisoptera"                 "valid" "genus - suborder"
# 313  "Annelida"                   "valid" "phylum - species"
# 470  "Anopheles"                  "valid" "genus - species"
# 172  "Anthozoa"                   "valid" "class - subphylum"
# 273  "Appendicularia"             "valid" "genus - class"
# 67   "Bacteria"                   "valid" "genus - kingdom"
# 424  "Batoidea"                   "valid" "order - superorder"
# 274  "Calanoida"                  "valid" "order - species"
# 68   "Capitella"                  "valid" "genus - species"
# 388  "Chironomidae"               "valid" "family - species"
# 443  "Chironomus"                 "valid" "genus - subgenus - species"
# 228  "Chlamydomonas"              "valid" "genus - species"
# 681  "Chondrichthyes"             "valid" "superclass - class"
# 276  "Ciliophora"                 "valid" "genus - phylum"
# 229  "Conger marginatus"          "valid" "species - subspecies"
# 549  "Crepidula"                  "valid" "genus - species"
# 1024 "Cryptophyta"                "valid" "class - phylum - species"
# 110  "Ctenophora"                 "valid" "genus - phylum"
# 389  "Diptera"                    "valid" "order - species"
# 1098 "Dussumieriidae"             "valid" "family - subfamily"
# 479  "Euphausia"                  "valid" "genus - species"
# 391  "Gomphus"                    "valid" "genus - subgenus"
# 348  "Halichondria"               "valid" "genus - subgenus"
# 322  "Haliclona"                  "valid" "genus - subgenus - species"
# 708  "Hediste diversicolor"       "valid" "species - genus"
# 1064 "Heliozoa"                   "valid" "class - phylum"
# 1087 "Hippolyte"                  "valid" "genus - species"
# 392  "Hirudinea"                  "valid" "class - subclass"
# 6    "Homo sapiens"               "valid" "species - subspecies"
# 940  "Hydrobatidae"               "valid" "family - subfamily"
# 14   "Hyporhamphus"               "valid" "genus - subgenus"
# 326  "Loligo"                     "valid" "genus - subgenus"
# 231  "Macrobrachium"              "valid" "genus - species"
# 56   "Microtus"                   "valid" "genus - species"
# 57   "Mus"                        "valid" "genus - subgenus"
# 412  "Mysidacea"                  "valid" "superorder - order"
# 329  "Mytilus"                    "valid" "genus - species"
# 581  "Nassarius"                  "valid" "genus - species"
# 93   "Nemertea"                   "valid" "phylum - species"
# 449  "Notonecta"                  "valid" "genus - subgenus"
# 248  "Octopus"                    "valid" "genus - species"
# 394  "Oecetis"                    "valid" "genus - species"
# 219  "Oikopleura"                 "valid" "genus - species"
# 74   "Oligochaeta"                "valid" "genus - class"
# 220  "Paracalanus"                "valid" "genus - species"
# 221  "Polychaeta"                 "valid" "genus - subphylum - class"
# 481  "Radiolaria"                 "valid" "order - genus"
# 529  "Rhombognathus"              "valid" "genus - species"
# 754  "Salpidae"                   "valid" "family - species"
# 254  "Sarda chiliensis lineolata" "valid" "species - subspecies"
# 492  "Serpulidae"                 "valid" "family - species"
# 452  "Sigara"                     "valid" "genus - subgenus"
# 493  "Sipuncula"                  "valid" "phylum - subclass"
# 62   "Sorex"                      "valid" "genus - subgenus"
# 991  "Stomatopoda"                "valid" "order - suborder"
# 340  "Strongylocentrotus"         "valid" "genus - species"
# 145  "Tegula"                     "valid" "genus - species"
# 888  "Terebra"                    "valid" "genus - species"
# 82   "Uca"                        "valid" "genus - species"

species <- c("Hediste diversicolor","Homo sapiens","Neripteron taitense","Palaemonetes floridanus","Heterophoxus stephenseni","Phoxocephalus regium","Pontharpinia australis","Prunum succineum")
genus <- c("Anopheles","Capitella","Chironomus","Chlamydomonas","Crepidula","Euphausia","Halichondria","Haliclona","Hippolyte","Hyporhamphus","Loligo","Macrobrachium","Microtus","Mus","Mytilus","Nassarius","Octopus","Oecetis","Oikopleura","Paracalanus","Rhombognathus","Sigara","Strongylocentrotus","Tegula","Terebra","Uca","Sorex","Notonecta","Gomphus")
family <- c("Chironomidae","Dussumieriidae","Hydrobatidae","Salpidae","Serpulidae")
class <- c("Anthozoa","Appendicularia","Chondrichthyes","Polychaeta")
subclass <- c("Hirudinea","Oligochaeta")
phylum <- c("Annelida","Ciliophora","Cryptophyta","Ctenophora","Heliozoa","Nemertea","Sipuncula")
order <- c("Calanoida","Diptera","Stomatopoda")
suborder <- c("Anisoptera")
superorder <- c("Batoidea")
kingdom <- c("Bacteria")
no.tx <- c("Megabenthos","Middle carnivores-a variety fish","Organic debris","Organic material","Marine plants","Green macroalgae")
rownames(tx.valid) <- tx.list

tx.valid[paste(species),3] <- "species"
tx.valid[paste(genus),3] <- "genus"
tx.valid[paste(family),3] <- "family"
tx.valid[paste(class),3] <- "class"
tx.valid[paste(subclass),3] <- "subclass"
tx.valid[paste(phylum),3] <- "phylum"
tx.valid[paste(order),3] <- "order"
tx.valid[paste(suborder),3] <- "suborder"
tx.valid[paste(superorder),3] <- "superorder"
tx.valid[paste(kingdom),3] <- "kingdom"
tx.valid[paste(no.tx),3] <- NA

#----------------------------------------------------------------------------------------------------------
#   Extract binary interactions from diet matrix
#----------------------------------------------------------------------------------------------------------
#List of binary interactions - column species (consumer) eats row species (resource)

for(k in 1: length(GlobalWeb)){
  GlobalWeb[[k]][[4]] <- bin_inter(GlobalWeb[[k]][[1]])
}

#----------------------------------------------------------------------------------------------------------
#   Extract taxon list for further analyses
#----------------------------------------------------------------------------------------------------------
for(k in 1: length(GlobalWeb)){
  tx.list <- unique(rownames(GlobalWeb[[k]][[1]]))
  GlobalWeb[[k]][[5]] <- as.data.frame(matrix(nrow=length(tx.list),ncol=2,data=NA))
  colnames(GlobalWeb[[k]][[5]]) <- c("taxon","rank")
  rownames(GlobalWeb[[k]][[5]]) <- tx.list
  GlobalWeb[[k]][[5]][,1] <- tx.list
  GlobalWeb[[k]][[5]][,2] <- tx.valid[tx.list,3]
}

n <-character()
m <- character()
for(k in 1:length(GlobalWeb)){
  n <- c(n,GlobalWeb[[k]][[5]][, 1])
  m <- c(m,GlobalWeb[[k]][[5]][, 2])
}
mat <- cbind(n,m)
mat <- unique(mat)


# Nombre d'interactions provenant de ces jeux de données
# x <- numeric(93)
# for(i in 1:93){x[i] <- nrow(GlobalWeb[[i]][[4]])}
# sum(x)
# [1] 90439

names(GlobalWeb) <- paste("GlobalWeb - ",names(GlobalWeb),sep = "")

save(x=GlobalWeb,file="RData/GlobalWeb.RData")
