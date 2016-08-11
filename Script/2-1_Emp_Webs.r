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
#      2.1 Barnes et al. 2008
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# FILES:
#   RawData <- file = "RawData/Web/Barnes_et_al_2008/Barnes2008.txt"
#   RData <- file = "RData/barnes2008.RData"
#   Script  <- file = "Script/2-1_Emp_Web.r"
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# NOTES:
#   Reference: Barnes, C., D. M. Bethea, R. D. Brodeur, J. Spitz, V. Ridoux,
#   C. Pusineri, B. C. Chase, et al. “Predator and Prey Body Sizes in Marine
#   Food Webs.” Ecology 89, no. 3 (March 1, 2008): 881–881. doi:10.1890/07-1551.1.
#
#   Original file modified manually to import in r
#     file = "RawData/Web/Barnes_et_al_2008/Predator_and_prey_body_sizes_in_marine
#        _food_webs_vsn4.txt"
#
#   Pinnegar et al. 2003 was substracted form this dataset, as it is contained
#     in Brose et al. 2005 (see 2-4_Emp_Webs)
#
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Verify whether there are other duplicated references,
# especially with GlobalWebs
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# TASKS LEFT:
#   - Modify taxon names
#   - Resolve taxon names
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# SCRIPT
# -----------------------------------------------------------------------------

#Import raw data from source
Barnes2008 <- vector("list", 5)
names(Barnes2008) <- c("Barnes_et_al_2008","Reference","Notes","Interactions","Species")
Barnes2008[[1]] <- read.table(file="RawData/Web/Barnes_et_al_2008/Barnes2008.txt",header = TRUE,sep="\t",quote = "")
Barnes2008[[1]] <- subset(Barnes2008[[1]],Barnes2008[[1]]$Reference != "Pinnegar et al. (2003)") #Already in Brose et al. 2005 as a diet matrix
Barnes2008[[1]]$Predator <- as.character(Barnes2008[[1]]$Predator)
Barnes2008[[1]]$Prey <- as.character(Barnes2008[[1]]$Prey)

Barnes2008[[2]] <- c("Barnes, C., D. M. Bethea, R. D. Brodeur, J. Spitz, V. Ridoux, C. Pusineri, B. C. Chase, et al. “Predator and Prey Body Sizes in Marine Food Webs.” Ecology 89, no. 3 (March 1, 2008): 881–881. doi:10.1890/07-1551.1.")

#Description of variable names
Barnes2008[[3]] <- c("PredLU = Predator length unit","PredDM = Predator dimension measured","PredSL = Predator standard length","PredFL = Predator fork length","PredTL = Predator total length","PredCR = Predator TL FL SL conversion reference","SPL = Standardised predator length","PredMT = Predator measurement type","PLMCR = Predator length-mass conversion reference","PQLMC = Predator quality of length-mass conversion","PMU = Predator mass unit","PMC = Predator mass check","PMCD = Predator mass check diff","PRMM = Predator ratio mass/mass","SI_PM = SI predator mass","PLU = Prey length unit","PCLM = Prey conversion to length method","PQCL = Prey quality of conversion to length","PCLR = Prey conversion to length reference","SI_PL = SI prey length","PDM = Prey dimension measured","PWU = Prey width unit","PreyMT = Prey measurement type","PMU = Prey mass unit","PMC = Prey mass check","PMCD = Prey mass check diff","PRMM = Prey ratio mass/mass","SI_PM = SI prey mass","PCMM = Prey conversion to mass method","PCMR = Prey conversion to mass reference","PQCM = Prey quality of conversion to mass")


#----------------------------------------------------------------------------------------------------------
#Name resolver - evaluate name validity
#----------------------------------------------------------------------------------------------------------
tx.list <- unique(c(as.character(Barnes2008[[1]]$Predator),as.character(Barnes2008[[1]]$Prey)))

Names_change <- function(x){
  x <- paste(x," ")
  x <- gsub(" sp ","",x) #Removing sp. from genus
  x <- gsub("\\(.*?\\)","",x) #removing parentheses
  x <- gsub("sp\\.","",x) #Removing sp. from genus
  x <- gsub("unidentified","",x) #Removing unidentified
  x <- gsub("/"," - ",x)
  x <- gsub("larvae","",x)
  x <- gsub("larva","",x)
  x <- gsub(" or "," - ",x)
  x <- gsub(" eggs","",x)
  x <- gsub(" egg","",x)
  x <- gsub("juv.","",x)
  x <- gsub("Unidentified crustacean","crustacean",x)
  x <- gsub(" - copepodite","",x)
  x <- gsub("copepodite","",x)
  x <- gsub("\"silversides, anchovies and 'other fish prey'\"","silverside - anchovy",x)
  x <- gsub("Euphausa","Euphausia",x)
  x <- gsub("E\\.","Euphausia ",x)
  x <- gsub(" - nauplius","",x)
  x <- gsub("nauplius","",x)
  x <- gsub("  "," ",x)
  x <- str_trim(x, side="both") #remove spaces
  x <- tolower(x)
  x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
  #Name resolve 1
  x <- paste(x," ")
  x <- gsub("Urophysis chuss","Urophycis chuss",x)
  x <- gsub("Crustaceans ","Crustacea",x)
  x <- gsub("Crustacean ","Crustacea ",x)
  x <- gsub("Crab megalope","Decapoda",x)
  x <- gsub("Squid","Teuthida",x)
  x <- gsub("Parathemisto gaudichaudi","Themisto gaudichaudi",x)
  x <- gsub("Gelatinous","Cnidaria",x)
  x <- gsub("Appendicularians house","Appendicularia",x)
  x <- gsub("Polychaete ","Polychaeta ",x)
  x <- gsub("Ostracod ","Ostracoda ",x)
  x <- gsub("Euphausid calyptopis","Euphausiacea",x)
  x <- gsub("Euphausid furcilia","Euphausiacea",x)
  x <- gsub("Cyclopoid ","Cyclopoida ",x)
  x <- gsub("Amphipod ","Amphipoda ",x)
  x <- gsub("Invert ","Invertebrate ",x)
  x <- gsub("Decapod zoea","Decapoda",x)
  x <- gsub("Hyperidae","Hyperiidae",x)
  x <- gsub("Onychoteuthis banksi ","Onychoteuthis banksii ",x)
  x <- gsub("Harpacticoid ","Harpacticoida ",x)
  x <- gsub("Isopod ","Isopoda ",x)
  x <- gsub("Roundworm","Nematoda ",x)
  x <- gsub("Medusa","Cnidaria ",x)
  x <- gsub("Pteropod ","Pteropoda ",x)
  x <- gsub("Tintinnid ","Tintinnida ",x)
  x <- gsub("Stephus longipes","Stephos longipes",x)
  x <- gsub("Bivalve ","Bivalvia ",x)
  x <- gsub("Gastropod ","Gastropoda ",x)
  x <- gsub("Teleosts ","Teleostei ",x)
  x <- gsub("Loligo pealei ","Doryteuthis pealeii ",x)
  x <- gsub("Loligo pealeii ","Doryteuthis pealeii ",x)
  x <- gsub("Silverside","Atheriniformes",x)
  x <- gsub("anchovy","Engraulidae",x)
  x <- gsub("Thysanoessa spinefera","Thysanoessa spinifera",x)
  x <- gsub("Scomber scrombus","Scomber scombrus",x)
  x <- gsub("Scyliorhinus caniculata","Scyliorhinus canicula",x)
  x <- gsub("Calanoid ","Calanoida ",x)
  x <- gsub("Appendicularians ","Appendicularia ",x)
  x <- gsub("Funchalia woodwardii","Funchalia woodwardi",x)
  x <- gsub("Insects ","Insecta ",x)
  x <- gsub("Metridia gerlacei","Metridia gerlachei",x)
  x <- gsub("Euphausia superba furcilia","Euphausia superba",x)
  x <- gsub("Micromesistius potassou","Micromesistius poutassou",x)
  x <- gsub("Molluscs","Mollusca",x)
  x <- gsub("Copepod ","Copepoda ",x)
  x <- gsub("Nauplius ","Alpheus ",x)
  x <- str_trim(x, side="both") #remove spaces
  return(x)
}

#Initial name change
Barnes2008[[1]]$Predator <- Names_change(Barnes2008[[1]]$Predator)
Barnes2008[[1]]$Prey <- Names_change(Barnes2008[[1]]$Prey)

#Rows to duplicate for prey (none for predator)
Barnes2008[[1]] <- dupl_row(as.matrix(Barnes2008[[1]]),Barnes2008[[1]]$Prey)
Barnes2008[[1]][,28] <- rownames(Barnes2008[[1]])
rownames(Barnes2008[[1]]) <- seq(1,nrow(Barnes2008[[1]]))
Barnes2008[[1]] <- as.data.frame(Barnes2008[[1]])

#Second name change
Barnes2008[[1]]$Predator <- Names_change(Barnes2008[[1]]$Predator)
Barnes2008[[1]]$Prey <- Names_change(Barnes2008[[1]]$Prey)

tx.list <- unique(c(as.character(Barnes2008[[1]]$Predator),as.character(Barnes2008[[1]]$Prey)))

#Taxonomic resolver
tx.res <- taxo_resolve(tx.list)
tx.valid <- taxo_valid(tx.res,tx.list,res.check=TRUE)

#!!!!!!!!!!!!!!!!!!!!!!!!!!! MANUAL RESOLVE 1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Incorporated in Names_change() above
# [1] "!!!!!!!!! NO RESULTS OBTAINED FROM gnr_resolve() !!!!!!!!!"
#     tx.list                    valid       rank
# 30  "Urophysis chuss"          "no result" NA <- modified
# 69  "Fish"                     "valid"     NA <- OK
# 70  "Crustacean"               "no result" NA <- modified
# 80  "Crab megalope"            "no result" NA <- modified
# 91  "Squid"                    "no result" NA <- modified
# 92  "Parathemisto gaudichaudi" "valid"     NA <- modified
# 93  "Gelatinous"               "no result" NA <- modified
# 99  "Invert"                   "no result" NA <- modified
# 100 "Appendicularians house"   "no result" NA <- modified
# 105 "Polychaete"               "no result" NA <- modified
# 107 "Ostracod"                 "no result" NA <- modified
# 108 "Euphausid calyptopis"     "no result" NA <- modified
# 109 "Cyclopoid"                "no result" NA <- modified
# 110 "Amphipod"                 "no result" NA <- modified
# 112 "Euphausid furcilia"       "no result" NA <- modified
# 115 "Decapod zoea"             "no result" NA <- modified
# 116 "Invertebrate"             "no result" NA <- OK
# 125 "Hyperidae"                "no result" NA <- modified
# 146 "Onychoteuthis banksi"     "valid"     NA <- modified
# 150 "Harpacticoid"             "no result" NA <- modified
# 152 "Egg"                      "no result" NA <- OK
# 156 "Isopod"                   "no result" NA <- modified
# 157 "Roundworm"                "no result" NA <- modified
# 172 "Medusa"                   "valid"     NA <- modified
# 173 "Pteropod"                 "no result" NA <- modified
# 174 "Tintinnid"                "valid"     NA <- modified
# 175 "Stephus longipes"         "no result" NA <- modified
# 183 "Bivalve"                  "valid"     NA <- modified
# 184 "Gastropod"                "no result" NA <- modified
# 197 "Teleosts"                 "no result" NA <- modified
# 199 "Crustaceans"              "invalid"   NA <- modified
# 200 "Loligo pealei"            "valid"     NA <- modified
# 202 "Silverside"               "no result" NA <- modified
# 203 "Anchovy"                  "no result" NA <- modified
# [1] "!!!!!!!!! INVALID RESULTS FROM gnr_resolve() !!!!!!!!!"
#    submitted_name               canonical_form
# 1  "Thysanoessa spinefera"      "Thysanoessa spinifera"
# 2  "Scomber scrombus"           "Scomber scombrus"
# 3  "Scyliorhinus caniculata"    "Scyliorhinus canicula"
# 4  "Calanoid"                   "Calanoida"
# 5  "Appendicularians"           "Appendicularia"
# 6  "Funchalia woodwardii"       "Funchalia woodwardi"
# 7  "Insects"                    "Insecta"
# 8  "Metridia gerlacei"          "Metridia gerlachei"
# 9  "Euphausia superba furcilia" "Euphausia superba"
# 10 "Micromesistius potassou"    "Micromesistius"
# 11 "Molluscs"                   "Mollusca"
# 12 "Crustaceans"                "Crustaceae"

#----------------------------------------------------------------------------------------------------------
#   Extract binary interactions from diet matrix
#----------------------------------------------------------------------------------------------------------
Barnes2008[[4]] <- unique(Barnes2008[[1]][,c("Predator","FeedInter","Prey")])
#----------------------------------------------------------------------------------------------------------
#   Extract taxon list for further analyses
#----------------------------------------------------------------------------------------------------------
#List of species with known interactions (dependant on binary interactions list modified in previous step)
Barnes2008[[5]] <- unique(c(levels(Barnes2008[[4]]$Predator),levels(Barnes2008[[4]]$Prey)))


Barnes2008[[5]] <- as.data.frame(matrix(nrow=length(tx.list),ncol=2,data=NA))
colnames(Barnes2008[[5]]) <- c("taxon","rank")
rownames(Barnes2008[[5]]) <- tx.list
Barnes2008[[5]][,1] <- tx.list
Barnes2008[[5]][,2] <- tx.valid[,3]

# [1] "!!!!!!!!! MULTIPLE TAXON FROM gnr_resolve() !!!!!!!!!"
#     tx.list                 valid   rank
# 9   "Cynoglossus"           "valid" "genus - species"
# 66  "Sebastes"              "valid" "genus - species"
# 71  "Euphausia"             "valid" "genus - species"
# 82  "Clupea harengus"       "valid" "species - infraspecies"
# 100 "Appendicularia"        "valid" "genus - class"
# 101 "Calanoida"             "valid" "order - species"
# 104 "Radiolaria"            "valid" "order - genus"
# 105 "Polychaeta"            "valid" "genus - subphylum - class"
# 135 "Notoscopelus kroeyeri" "valid" "species - subspecies"
# 143 "Chiroteuthis"          "valid" "genus - subgenus"
# 150 "Corycaeus"             "valid" "genus - subgenus"
# 153 "Oikopleura"            "valid" "genus - species"
# 175 "Mysidacea"             "valid" "superorder - order"

species <- c("Clupea harengus","Notoscopelus kroeyeri")
genus <- c("Cynoglossus","Sebastes","Euphausia","Chiroteuthis","Corycaeus","Oikopleura")
class <- c("Appendicularia","Polychaeta")
order <- c("Calanoida","Mysidacea")
phylum <- c("Radiolaria")
no.tx <- c("Unidentified")

Barnes2008[[5]][paste(species),2] <- "species"
Barnes2008[[5]][paste(genus),2] <- "genus"
Barnes2008[[5]][paste(class),2] <- "class"
Barnes2008[[5]][paste(order),2] <- "order"
Barnes2008[[5]][paste(phylum),2] <- "phylum"
Barnes2008[[5]][paste(no.tx),2] <- NA

Barnes <- vector("list",1)
Barnes[[1]] <- Barnes2008
names(Barnes) <- "Barnes_et_al_2008"
Barnes2008 <- Barnes

save(x=Barnes2008,file="RData/barnes2008.RData")
