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
#      2.5 Global Biotic Interactions (GloBI)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# FILES:
#   RData <- file = "RData/GloBI.RData"
#   Script  <- file = "Script/2-6_GloBI.r"
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# NOTES:
#   Reference:
#     Jorrit H. Poelen, James D. Simons and Chris J. Mungall. (2014).
#       Global Biotic Interactions: An open infrastructure to share and analyze
#       species-interaction datasets. Ecological Informatics.
#       http://dx.doi.org/10.1016/j.ecoinf.2014.08.005
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# FUNCTIONS:
# source("Script/bin_inter.R")
# source("Script/diet_mat_extend.R")
# source("Script/dup_multi_tx.R")
# source("Script/duplicate_row_col.R")
# source("Script/locate_string.R")
# source("Script/tax_rank.R")
# source("Script/taxo_resolve.R")
# source("Script/taxo_valid.R")
# source("Script/tx_valid_res.R")
# source("Script/binary_interaction.R")
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# LIBRARIES:
library(stringr)
library(rglobi)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# SCRIPT
# -----------------------------------------------------------------------------
load("RData/class_tx_tot.RData")

# Dataset to create for GloBI with usual structure
  GloBI <- vector("list",1)
  names(GloBI) <- "GloBI"
  GloBI[[1]] <- vector("list", 5)
  names(GloBI[[1]]) <- c("GloBI","Reference","Notes","Interactions","Species")

  GloBI[[1]][[2]] <- "Jorrit H. Poelen, James D. Simons and Chris J. Mungall. (2014). Global Biotic Interactions: An open infrastructure to share and analyze species-interaction datasets. Ecological Informatics. http://dx.doi.org/10.1016/j.ecoinf.2014.08.005"

# Extracting all families from EmpWebs & EGSL
  # Family list
    family.list <- sort(unique(tolower(as.character(class.tx.tot$family))))

  # Identifying those without families to select alternative taxon rank
    NAs <- class.tx.tot[which(is.na(class.tx.tot$family) == TRUE), ]
    for(i in 1:nrow(NAs)){
      if(is.na(NAs$subfamily[i]) == FALSE) {
        family.list <- c(family.list, as.character(NAs$subfamily[i]))
      } else if(is.na(NAs$superfamily[i]) == FALSE) {
        family.list <- c(family.list, as.character(NAs$superfamily[i]))
      } else if(is.na(NAs$tribe[i]) == FALSE) {
        family.list <- c(family.list, as.character(NAs$tribe[i]))
      } else if(is.na(NAs$genus[i]) == FALSE) {
        family.list <- c(family.list, as.character(NAs$genus[i]))
      } else if(is.na(NAs$species[i]) == FALSE) {
        family.list <- c(family.list, as.character(NAs$species[i]))
      } # if
    } # i

    family.list <- sort(unique(family.list))

  # Extracting taxa child from GloBI
    nb.fam <- length(family.list)
    pb <- txtProgressBar(min = 0,max = nb.fam, style = 3)
    child_taxa_matrix <- matrix(nrow = nb.fam, ncol = 1, data = NA, dimnames = list(c(),c("GloBI"))) # Row number equals to family.list vector elements

    for(i in 1:nb.fam) {

      GloBI_species <- get_child_taxa(taxon.names = family.list[i], rank = "Species", skip = 0, limit = 100, opts = list())
      child_taxa_matrix[i,1] <- if(is.null(GloBI_species) == TRUE) {NA} else {paste(GloBI_species, collapse = ";") } #Concatener les espèces pour chaque genre

      setTxtProgressBar(pb, i)
    }
    close(pb)

  # Percent EmpWebs & EGSL family rank w/o species in GloBI
    percent.missing.family <- (sum(is.na(child_taxa_matrix[,1]))/nb.fam)*100

  # New species list found in the GloBI database to validate St.Lawrence biotic interactions model - includes St.Lawrence species with documented interactions in GloBI
    GloBI_species <- unique(unlist(strsplit(paste(child_taxa_matrix[!is.na(child_taxa_matrix[, 1]), 1], collapse=";"), ";"))) #Liste d'espèces pour validation modèles

  # Modifying species names
  Names_change <- function(x){
    # Name resolve #1
    x <- str_trim(x, side="both") #remove spaces
    x <- tolower(x)
    x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
    x <- paste(x," ")
    x <- gsub("\\(.*?\\) ","",x) #remove species with additional names in parentheses
    x <- gsub("cf\\.[^\\.]","",x) #remove species cf.
    x <- gsub("sp\\.[^\\.] "," ",x) #remove sp.
    x <- gsub("Florida crotolaria witches'-broom phytoplasma ","Florida crotolaria ",x) #remove string
    x <- gsub("Lumbriculiid.*","Lumbriculiid",x) #remove string
    x <- str_trim(x, side="both") #remove spaces
    x <- tolower(x)
    x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
    return(x)
  }

  GloBI_species <- Names_change(GloBI_species)

  for(i in 1:length(GloBI_species)) {
    if(length(strsplit(GloBI_species[i]," ")[[1]]) > 2) {
      GloBI_species[i] <- paste(strsplit(GloBI_species[i]," ")[[1]][1], strsplit(GloBI_species[i]," ")[[1]][2], sep = " ")
    }
  }

  GloBI_species <- unique(GloBI_species)

  # Number of EmpWeb $ EGSL species found in GloBI
    sp.sl.globi <- c(GloBI_species, as.character(class.tx.tot$species[!is.na(class.tx.tot$species)]))
    sp.sl.globi <- length(subset(duplicated(sp.sl.globi), duplicated(sp.sl.globi) == TRUE))

  # Extract all known interactions for species identified in previous steps in the GloBI database
  nb.sp.globi <- length(GloBI_species)

  inter_GloBI <- as.data.frame(matrix(ncol=9, nrow=0, data=NA))
  colnames(inter_GloBI) <- c('interaction_type','target_taxon_name','source_taxon_name','target_taxon_external_id','source_taxon_external_id','target_taxon_path','source_taxon_path','target_taxon_path_ranks','source_taxon_path_ranks')

  pb <- txtProgressBar(min = 0, max = nb.sp.globi, style = 3)
  # pb <- txtProgressBar(min = 0, max = nb.sp.globi, style = 3)
  init.time <- Sys.time()
  for(i in 1:nb.sp.globi) {
  # for(i in 1:nb.sp.globi) {
  # for(i in 5688:6184) {
    temp <- get_interactions(GloBI_species[i],
                              interaction.type = "interactsWith",
                              showfield = c('interaction_type','target_taxon_name','source_taxon_name','target_taxon_external_id','source_taxon_external_id','target_taxon_path','source_taxon_path','target_taxon_path_ranks','source_taxon_path_ranks')
                            )

    inter_GloBI <- rbind(inter_GloBI,temp)
    setTxtProgressBar(pb, i)
  }
  Sys.time() - init.time
  close(pb)

  GloBI[[1]][[1]] <- inter_GloBI

  # save(x=GloBI,file="RData/GloBI.RData")

  # Extracting only trophic interactions
  GloBI[[1]][[4]] <- GloBI_trophic_inter(GloBI[[1]][[1]])

  # Checking taxon names and selecting only interactions that are at the resolution of the family or higher
  # GloBI_species_inter <- unique(c(GloBI[[1]][[4]]$source_taxon_name, GloBI[[1]][[4]]$target_taxon_name)) #10217 taxon
  GloBI_species_inter <- as.data.frame(GloBI[[1]][[1]])

  # Check
  Names_change <- function(x){
    # Name resolve #1
    x <- str_trim(x, side="both") #remove spaces
    x <- tolower(x)
    x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
    x <- paste(x," ")
    x <- gsub("\\(.*?\\) ","",x) #remove species with additional names in parentheses
    x <- gsub("cf\\.[^\\.] ","",x) #remove species cf.
    x <- gsub("sp\\.[^\\.] "," ",x) #remove sp.
    x <- gsub("var\\.[^\\.] "," ",x) #remove var.
    x <- gsub("subsp\\.[^\\.] "," ",x) #remove subsp.
    x <- gsub("Florida crotolaria witches'-broom phytoplasma ","Florida crotolaria ",x) #remove string
    x <- gsub("Lumbriculiid.*","Lumbriculiid",x) #remove string
    # Taxonomic resolve 1
    x <- gsub(" sp "," ",x)
    x <- gsub(" spp "," ",x)
    x <- gsub(" i "," ",x)
    x <- gsub(" x "," ",x)
    x <- gsub(" - "," ",x)
    x <- gsub("#15 "," ",x)
    x <- gsub("Acheloüs spinicarpus ","Achelous spinicarpus ",x)
    x <- gsub("Actitus macularia ","Actitis macularia ",x)
    x <- gsub("Alburniodes bipunctatus","Alburnoides bipunctatus ",x)
    x <- gsub("Amauropis rossiana","Amauropsis rossiana",x)
    x <- gsub("Animallia ","Animalia ",x)
    x <- gsub("Anthonantha acuminata ","Anthonotha acuminata ",x)
    x <- gsub("Anthonantha fragrans ","Anthonotha fragrans ",x)
    x <- gsub("Anthonantha macrophylla ","Anthonotha macrophylla ",x)
    x <- gsub("Athya marila ","Aythya marila",x)
    x <- gsub("Bossiella orbigniana ","Bossiella orbigniana ",x)
    x <- gsub("Campostylus mannii","Camptostylus mannii",x)
    x <- gsub("Capittelidae","Capitellidae ",x)
    x <- gsub("Castostmus occidentalis ","Catostomus occidentalis ",x)
    x <- gsub("Celthrionomys ","Clethrionomys ",x)
    x <- gsub("Ceracaria ","Cercaria ",x)
    x <- gsub("Chardrius vociferus ","Charadrius vociferus ",x)
    x <- gsub("Cyclopeterus lumpus ","Cyclopterus lumpus ",x)
    x <- gsub("Cylorrhynchus psittacula ","Cyclorrhynchus psittacula ",x)
    x <- gsub("Cynomus gunnisoni ","Cynomys gunnisoni ",x)
    x <- gsub("Cynomus ludovicianus ","Cynomys ludovicianus ",x)
    x <- gsub("Deplatsia dewevrei ","Desplatsia dewevrei ",x)
    x <- gsub("Dipdomys compactus ","Dipodomys compactus ",x)
    x <- gsub("Dipulmaris antarctica ","Diplulmaris antarctica ",x)
    x <- gsub("Eragrostris curvula ","Eragrostis curvula ",x)
    x <- gsub("Euhpagus cyanocephalus ","Euphagus cyanocephalus ",x)
    x <- gsub("Festuca ovina agg\\.[^\\.] ","Festuca ovina ",x)
    x <- gsub("Fratecula cirrhata ","Fratercula cirrhata ",x)
    x <- gsub("Galitheutis glacialis ","Galiteuthis glacialis ",x)
    x <- gsub("Gasterosterus ","Gasterosteus ",x)
    x <- gsub("Graminaea ","Graminea ",x)
    x <- gsub("Gymnoscoelus nicholsi ","Gymnoscopelus nicholsi ",x)
    x <- gsub("Healianthus ","Helianthus ",x)
    x <- gsub("Hypselodelphis violacea ","Hypselodelphys violacea ",x)
    x <- gsub("Illybius fuliginosus ","Ilybius fuliginosus ",x)
    x <- gsub("Jasminium abyssinicum ","Jasminum abyssinicum ",x)
    x <- gsub("Kröyeria ","Kroyeria ",x)
    x <- gsub("Large xiphideocercaria ","Xiphideocercaria",x)
    x <- gsub("Leplaca mayumbensis ","Leplaea mayumbensis ",x)
    x <- gsub("Letherocerus americanus ","Lethocerus americanus ",x)
    x <- gsub("Macrolymenella ","Macroclymenella ",x)
    x <- gsub("Macropolepia ","Macropelopia ",x)
    x <- gsub("Mildbraedeodendron excelsum ","Mildbraediodendron excelsum ",x)
    x <- gsub("Monanthocloe littoralis ","Monanthochloe littoralis ",x)
    x <- gsub("Morotheutis ingens ","Moroteuthis ingens ",x)
    x <- gsub("Neballidae ","Nebaliidae ",x)
    x <- gsub("Ostracoda sp. sbh266127 ","Ostracoda ",x)
    x <- gsub("Pandion haliaëtus haliaëtus ","Pandion haliaetus ",x)
    x <- gsub("Papulospora halima ","Papulaspora halima ",x)
    x <- gsub("Parapsia grewioides ","Paropsia grewioides ",x)
    x <- gsub("Permyscus maniculatus ","Peromyscus maniculatus ",x)
    x <- gsub("Pipinculus ","Pipunculus ",x)
    x <- gsub("Pleocyemete ","Pleocyemata ",x)
    x <- gsub("Pooecets gramineus ","Pooecetes gramineus ",x)
    x <- gsub("Prodiames olivacea ","Prodiamesa olivacea ",x)
    x <- gsub("Pseudiacris regilla ","Pseudacris regilla ",x)
    x <- gsub("Pseudophaeroma campbellensis ","Pseudosphaeroma campbellensis ",x)
    x <- gsub("Repitilia ","Reptilia ",x)
    x <- gsub("Sagartiogetum undatum ","Sagartiogeton undatus ",x)
    x <- gsub("Scapaloberis mucronata ","Scaphaloberis mucronata ",x)
    x <- gsub("Scieanops ocellatus ","Sciaenops ocellatus ",x)
    x <- gsub("Scolymasta joubini ","Scolymastra joubini ",x)
    x <- gsub("Scorpaenichtys marmoratus ","Scorpaenichthys marmoratus ",x)
    x <- gsub("Sebates ","Sebastes ",x)
    x <- gsub("Selachiomorpha ","Selachimorpha ",x)
    x <- gsub("Simocephalous vetulas ","Simocephalus vetulus ",x)
    x <- gsub("Stauditia stipitata ","Staudtia stipitata ",x)
    x <- gsub("Stigeidae ","Strigeidae ",x)
    x <- gsub("Syngnthidae ","Syngnathidae ",x)
    x <- gsub("Sysnsepalum stipulatum ","Synsepalum stipulatum ",x)
    x <- gsub("Syzgium guineensis ","Syzygium guineense ",x)
    x <- gsub("Tanaisus ","Tanaissus ",x)
    x <- gsub("Taraxacum officinale agg\\.[^\\.] ","Taraxacum officinale ",x)
    x <- gsub("Tenbrionidae ","Tenebrionidae ",x)
    x <- gsub("Trachelus troglodytus ","Trachelus troglodyta ",x)
    x <- gsub("Trypanorynch ","Trypanorhyncha ",x)
    x <- gsub("Vibillia antarctica ","Vibilia antarctica ",x)
    x <- gsub("Vibillia stebbingi ","Vibilia stebbingi ",x)
    x <- gsub("Xylosyctes ","Xyloryctes ",x)
    x <- gsub("A-pleurogona-stolidobranchiata ","Stolidobranchiata ",x)
    x <- gsub("Allochertes ptilocerus ","Parhyale ptilocerus ",x)
    x <- gsub("Asteroida ","Asteroidea ",x)
    x <- gsub("Atherestes stomias ","Atheresthes stomias ",x)
    x <- gsub("Bopyrid ","Bopyridae ",x)
    x <- gsub("Broadfin sculpin bolinia euryptera ","Bolinia euryptera ",x)
    x <- gsub("Burrowing shrimp ","Trypaea australiensis ",x)
    x <- gsub("Calodium hepaticum ","Capillaria hepatica ",x)
    x <- gsub("Cercariae lebouri ","Paramonostomum chabaudi ",x)
    x <- gsub("Chironomid type polyped ","Chironomidae ",x)
    x <- gsub("Cirrepedia ","Cirripedia ",x)
    x <- gsub("Coronadinium ","Amphora ",x)
    x <- gsub("Crangonid ","Crangonidae ",x)
    x <- gsub("Crustose coralline algae complex ","Corallinales ",x)
    x <- gsub("Tubularia ralphii ","Ectopleura crocea ",x)
    x <- gsub("Scopriones ","Scorpiones ",x)
    x <- gsub("Romalium ","Homalium ",x)
    x <- gsub("Osteichthys teleostei ","Osteichthyes ",x)
    x <- gsub("Onacea ","Oncaea ",x)
    x <- gsub("Noctuids ","Noctuides ",x)
    x <- gsub("Nematocysts ","Nematocystis ",x)
    x <- gsub("N-doridoidea ","Doridoidea ",x)
    x <- gsub("Iridaea vincent ","Iridaea ",x)
    x <- gsub("Fellostomatidae ","Bellostomatidae ",x)
    x <- gsub("Chironomid type polyped ","Chironomidae ",x)
    x <- gsub("Vetern poacher podothecus veternus ","Podothecus veternus ",x)
    x <- gsub("Urogryllus ","Anurogryllus ",x)
    x <- gsub("Tintinnididae ","Tintinnidae ",x)
    x <- gsub("Stratiomyid ","Stratiomyidae ",x)
    x <- gsub("Sphaerocidae ","Sphaerocinidae ",x)
    x <- gsub("Scarabidae ","Scarabaeidae ",x)
    x <- gsub("Scapaloberis mucronata ","Scapholeberis mucronata ",x)
    x <- gsub("Puget sound sculpin ruscarius meanyi ","Ruscarius meanyi ",x)
    x <- gsub("Pterepod ","Pteropoda ",x)
    x <- gsub("Prinospio sp. 1 ","Prionospio ",x)
    x <- gsub("Prinospio sp. 2 ","Prionospio ",x)
    x <- gsub("Pomphorynchus ","Pomphorhynchus ",x)
    x <- gsub("Moronrmississippiensis ","Morone mississippiensis ",x)
    x <- gsub("Mollusc ","Mollusca ",x)
    x <- gsub("Mertensiid ctenophore ","Mertensiidae ",x)
    x <- gsub("Lymnea ","Lymnaea ",x)
    x <- gsub("Lumbriculiid ","Lumbriculidae ",x)
    x <- gsub("Limicolae ","Limicola ",x)
    x <- gsub("Lagomorph ","Lagomorpha ",x)
    x <- gsub("Labratrema ","Bucephalus ",x)
    x <- gsub("Isodyctia steifera ","Isodictya setifera ",x)
    x <- gsub("Gomphonema vibrio bohemicum ","Gomphonema intricatum ",x)
    x <- gsub("Forminifera ","Foraminifera ",x)
    x <- gsub("Enchytraidae ","Enchytraeidae ",x)
    x <- gsub("Dorid nudibranch steinbergae ","Corambe steinbergae ",x)
    x <- gsub("Brachyura-cancridea ","Cancridae ",x)
    x <- gsub("Dusky/ dark rockfish sebastes variabilis/ ciliatus ","Sebastes variabilis - Sebastes ciliatus ",x)
    x <- gsub("Oulema rufocyanea/melanopus agg. ","Oulema rufocyanea - Oulema melanopus ",x)
    x <- gsub("Chimara ","Chimarra ",x)
    x <- gsub("Gomphonema cf. ","Gomphonema ",x)
    x <- gsub("Codium cf. ","Codium ",x)
    x <- gsub("Periclemenes ","Periclimenes ",x)
    x <- gsub("Sandlances ","Ammodytidae ",x)
    x <- gsub("Haplocrhomis mola ","Haplochromis mola ",x)
    x <- str_trim(x, side="both") #remove spaces
    x <- tolower(x)
    x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
    return(x)
  }

  GloBI_species_inter$target_taxon_name <- Names_change(GloBI_species_inter$target_taxon_name)
  GloBI_species_inter$source_taxon_name <- Names_change(GloBI_species_inter$source_taxon_name)

# Duplicate lines with multiple taxon seperated by " -  (only source_taxon_name in this case)
  GloBI_species_inter <- dupl_row(as.matrix(GloBI_species_inter),GloBI_species_inter$source_taxon_name)
  source_taxon_name <- rownames(GloBI_species_inter)
  rownames(GloBI_species_inter) <- seq(1,nrow(GloBI_species_inter))
  GloBI_species_inter <- as.data.frame(GloBI_species_inter)
  GloBI_species_inter$source_taxon_name <- source_taxon_name

# First letter only as capital
  Names_change <- function(x){
    # Name resolve #1
    x <- str_trim(x, side="both") #remove spaces
    x <- tolower(x)
    x <- paste(toupper(substr(x,nchar(x)-(nchar(x)-1),nchar(x)-(nchar(x)-1))),substr(x,nchar(x)-(nchar(x)-2),nchar(x)),sep="")
    return(x)
  }

  GloBI_species_inter$target_taxon_name <- Names_change(GloBI_species_inter$target_taxon_name)
  GloBI_species_inter$source_taxon_name <- Names_change(GloBI_species_inter$source_taxon_name)

# limiting number of words to two for each taxon name
  for(i in 1:length(GloBI_species_inter$target_taxon_name)) {
    if(length(strsplit(GloBI_species_inter$target_taxon_name[i]," ")[[1]]) > 2) {
      GloBI_species_inter$target_taxon_name[i] <- paste(strsplit(GloBI_species_inter$target_taxon_name[i]," ")[[1]][1], strsplit(GloBI_species_inter$target_taxon_name[i]," ")[[1]][2], sep = " ")
    }
  }

  for(i in 1:length(GloBI_species_inter$source_taxon_name)) {
    if(length(strsplit(GloBI_species_inter$source_taxon_name[i]," ")[[1]]) > 2) {
      GloBI_species_inter$source_taxon_name[i] <- paste(strsplit(GloBI_species_inter$source_taxon_name[i]," ")[[1]][1], strsplit(GloBI_species_inter$source_taxon_name[i]," ")[[1]][2], sep = " ")
    }
  }

  # Adjusted species interactions
  # One error found in the taxon path at the 10th row
  GloBI[[1]][[1]]$target_taxon_path <- as.character(GloBI[[1]][[1]]$target_taxon_path)
  GloBI[[1]][[1]]$target_taxon_path_ranks <- as.character(GloBI[[1]][[1]]$target_taxon_path_ranks)
  GloBI[[1]][[1]]$source_taxon_path <- as.character(GloBI[[1]][[1]]$source_taxon_path)
  GloBI[[1]][[1]]$source_taxon_path_ranks <- as.character(GloBI[[1]][[1]]$source_taxon_path_ranks)

  source_path <- GloBI[[1]][[1]][10, ]$target_taxon_path
  source_path_ranks <- GloBI[[1]][[1]][10, ]$target_taxon_path_ranks
  target_path <- GloBI[[1]][[1]][10, ]$source_taxon_path
  target_path_ranks <- GloBI[[1]][[1]][10, ]$source_taxon_path_ranks

  GloBI[[1]][[1]][10, ]$source_taxon_path <- source_path
  GloBI[[1]][[1]][10, ]$source_taxon_path_ranks <- source_path_ranks
  GloBI[[1]][[1]][10, ]$target_taxon_path <- target_path
  GloBI[[1]][[1]][10, ]$target_taxon_path_ranks <- target_path_ranks


  #Taxonomic resolver
    GloBI_taxon <- sort(unique(c(GloBI_species_inter$source_taxon_name, GloBI_species_inter$target_taxon_name))) #10217 taxon
    tx.res <- taxo_resolve(GloBI_taxon)
    tx.valid <- taxo_valid(tx.res,GloBI_taxon,res.check=TRUE)

    # [1] "!!!!!!!!! NO RESULTS OBTAINED FROM gnr_resolve() !!!!!!!!!"
    #       tx.list                                              valid       rank
    # 8737  "+113 species"                                       "no result" NA
    # 7776  "A-pleurogona-stolidobranchiata"                     "no result" NA
    # 8108  "Algal cysts"                                        "no result" NA
    # 3064  "Allocentrella magnicornis"                          "valid"     NA
    # 2961  "Allochertes ptilocerus"                             "valid"     NA
    # 10178 "Alpine vegetation"                                  "valid"     NA
    # 10019 "Amphicyrta"                                         "no result" NA
    # 3778  "Anaxilaus vesiculosus"                              "no result" NA
    # 9343  "Anthropogenic material"                             "no result" NA
    # 501   "Aphrotenidae"                                       "valid"     NA
    # 4378  "Apophallus lerouxi"                                 "valid"     NA
    # 5715  "Aquatic or marine worms"                            "valid"     NA
    # 2610  "Arctocorisa germari"                                "valid"     NA
    # 5352  "Asteroida"                                          "valid"     NA
    # 4852  "Atherestes stomias"                                 "no result" NA
    # 3780  "Athya erythrophthalma"                              "no result" NA
    # 8465  "Aucalosira"                                         "valid"     NA
    # 2774  "Austraclima jollyae"                                "valid"     NA
    # 8782  "Austrominius modestus"                              "valid"     NA
    # 6485  "Autothroph flagellat"                               "no result" NA
    # 9142  "Bacterioplankton"                                   "valid"     NA
    # 1725  "Baetis muticus"                                     "valid"     NA
    # 3874  "Bagous tubulus"                                     "valid"     NA
    # 8988  "Bait"                                               "no result" NA
    # 426   "Baraeoptera roria"                                  "valid"     NA
    # 6898  "Benthic autotrophs"                                 "valid"     NA
    # 6627  "Benthic carnivores"                                 "valid"     NA
    # 8188  "Benthic invertebrates"                              "valid"     NA
    # 9401  "Benthic microalgae/periphyton"                      "no result" NA
    # 7303  "Benthonic invertebrates"                            "valid"     NA
    # 9605  "Benthos"                                            "valid"     NA
    # 5520  "Benthos larvae"                                     "invalid"   NA
    # 9885  "Berries"                                            "valid"     NA
    # 4656  "Biofilm complex"                                    "no result" NA
    # 9353  "Bopyrid"                                            "no result" NA
    # 8162  "Bovine or equine dung"                              "valid"     NA
    # 9604  "Brachyura-cancridea"                                "no result" NA
    # 2481  "Brillia bifida"                                     "valid"     NA
    # 7096  "Broadfin sculpin bolinia euryptera"                 "no result" NA
    # 6262  "Burrowing shrimp"                                   "no result" NA
    # 3470  "Calodium hepaticum"                                 "valid"     NA
    # 1514  "Candorbulina"                                       "valid"     NA
    # 5311  "Carcass"                                            "valid"     NA
    # 9839  "Carpelimus manchuricus"                             "valid"     NA
    # 5599  "Carrion"                                            "no result" NA
    # 6020  "Centroptilium rufostrigatum"                        "valid"     NA
    # 3775  "Cephaloplatus fasciatus"                            "no result" NA
    # 3361  "Cercariae lebouri"                                  "valid"     NA
    # 3953  "Cerodontha lateralis"                               "valid"     NA
    # 3185  "Chimara"                                            "valid"     NA
    # 431   "Chironomid type polyped"                            "invalid"   NA
    # 4060  "Chorosoma schillingi"                               "valid"     NA
    # 8048  "Cirrepedia"                                         "no result" NA
    # 6875  "Coarse particulate organic matter"                  "valid"     NA
    # 10014 "Coniontis"                                          "valid"     NA
    # 8053  "Copepoda-calanoida"                                 "no result" NA
    # 8699  "Coronadinium"                                       "no result" NA
    # 8984  "Corophiida"                                         "no result" NA
    # 6823  "Cpom"                                               "no result" NA
    # 6389  "Crangonid"                                          "no result" NA
    # 5331  "Crustose coralline algae complex"                   "no result" NA
    # 9761  "Cryptolucilia"                                      "valid"     NA
    # 8833  "Cuspidium cernua"                                   "no result" NA
    # 6853  "Cydorus latus"                                      "valid"     NA
    # 7179  "Cymbella kappii"                                    "valid"     NA
    # 6294  "Desmidae"                                           "valid"     NA
    # 8049  "Digested remains"                                   "no result" NA
    # 1294  "Diplodina artemisiae"                               "valid"     NA
    # 1341  "Diplodina sonchi"                                   "valid"     NA
    # 3413  "Dorid nudibranch steinbergae"                       "no result" NA
    # 9951  "Dromopoda"                                          "valid"     NA
    # 2660  "Dusky/ dark rockfish sebastes variabilis/ ciliatus" "no result" NA
    # 8199  "Echinidea"                                          "no result" NA
    # 7449  "Egg"                                                "no result" NA
    # 5704  "Emergent vascular plants"                           "no result" NA
    # 8047  "Empty"                                              "no result" NA
    # 6858  "Enchytraidae"                                       "no result" NA
    # 8106  "Encrusting algae"                                   "valid"     NA
    # 6720  "Epipellic flora"                                    "no result" NA
    # 4206  "Erebia claudina"                                    "valid"     NA
    # 8658  "Euclymeninae"                                       "valid"     NA
    # 8890  "Euphausid calyptopis"                               "no result" NA
    # 9825  "Eupterygini"                                        "no result" NA
    # 9990  "Evolomys"                                           "valid"     NA
    # 3410  "Eye forward chiron"                                 "valid"     NA
    # 6241  "Fecal pellets"                                      "no result" NA
    # 9024  "Filter feeding"                                     "no result" NA
    # 6820  "Fine particulate organic matter"                    "valid"     NA
    # 7796  "Fishery offal"                                      "no result" NA
    # 6481  "Flagellate"                                         "valid"     NA
    # 9366  "Food waste"                                         "no result" NA
    # 5544  "Forage fish"                                        "no result" NA
    # 9514  "Forminifera"                                        "no result" NA
    # 6822  "Fpom"                                               "no result" NA
    # 8452  "Fragilaria capucina subsp. rumpens"                 "invalid"   NA
    # 8826  "Fruit"                                              "valid"     NA
    # 9590  "Gelatinous"                                         "no result" NA
    # 1241  "Glocianus pilosellus"                               "valid"     NA
    # 7273  "Gomphonema vibrio bohemicum"                        "valid"     NA
    # 6829  "Gongrosira incrustans"                              "valid"     NA
    # 9888  "Gravel"                                             "no result" NA
    # 7064  "Ground invertebrates"                               "valid"     NA
    # 8051  "Gut everted/regurgitated"                           "no result" NA
    # 9160  "Hair"                                               "no result" NA
    # 4604  "Haplocrhomis mola"                                  "valid"     NA
    # 2415  "Haploparaksis crassirostris"                        "valid"     NA
    # 1245  "Haplothrips hukkineni"                              "valid"     NA
    # 509   "Heliodidae"                                         "valid"     NA
    # 777   "Hemiuris communis"                                  "valid"     NA
    # 7009  "Hesperocorixa castanea"                             "valid"     NA
    # 6484  "Heterotroph flagellat"                              "no result" NA
    # 2936  "Himasthla interrupta"                               "valid"     NA
    # 6973  "Histanocerus"                                       "valid"     NA
    # 8989  "Hooks"                                              "no result" NA
    # 4220  "Hyalites acerata"                                   "invalid"   NA
    # 1285  "Hyalites rahira"                                    "valid"     NA
    # 418   "Hydora"                                             "valid"     NA
    # 362   "Hydora nitida"                                      "valid"     NA
    # 7093  "Hydrachnidia"                                       "valid"     NA
    # 9537  "Hylina veliei"                                      "valid"     NA
    # 6830  "Hyphomycete fungal hyphae"                          "no result" NA
    # 9870  "Hypocyphtus discoideus"                             "no result" NA
    # 4455  "Hyponigrus obsidianus"                              "valid"     NA
    # 6540  "Ice algae"                                          "valid"     NA
    # 7415  "Inorganic matter"                                   "no result" NA
    # 5410  "Invertebrata"                                       "no result" NA
    # 9603  "Invertebrate"                                       "no result" NA
    # 4594  "Invertebrate defoliators"                           "valid"     NA
    # 7291  "Invertebrate predators"                             "valid"     NA
    # 8057  "Invertebrate remains"                               "no result" NA
    # 6836  "Invertebrates"                                      "valid"     NA
    # 7363  "Isodyctia steifera"                                 "no result" NA
    # 9862  "Jassinae"                                           "no result" NA
    # 5355  "Khakista"                                           "no result" NA
    # 5859  "Labratrema"                                         "valid"     NA
    # 3790  "Lachnum carneolum var. longisporum"                 "invalid"   NA
    # 4790  "Lagomorph"                                          "no result" NA
    # 5152  "Large xiphideocercaria"                             "invalid"   NA
    # 3842  "Lasiommata paramegaera"                             "valid"     NA
    # 5970  "Lavonica"                                           "no result" NA
    # 8467  "Leaf fragments"                                     "valid"     NA
    # 3769  "Leptocorisa oratorius"                              "valid"     NA
    # 2909  "Life"                                               "valid"     NA
    # 9890  "Limicolae"                                          "valid"     NA
    # 8124  "Limnophilus"                                        "valid"     NA
    # 3796  "Limothrips schmutzi"                                "valid"     NA
    # 9591  "Livoneca tenuistylis"                               "valid"     NA
    # 9956  "Locustidae"                                         "no result" NA
    # 1376  "Longitarsus gracilis"                               "valid"     NA
    # 361   "Lumbriculiid"                                       "valid"     NA
    # 433   "Lymnea"                                             "no result" NA
    # 5693  "Macro-epiphytes"                                    "valid"     NA
    # 5621  "Macroalgae"                                         "valid"     NA
    # 5339  "Marine invertebrates"                               "valid"     NA
    # 9008  "Medusa"                                             "valid"     NA
    # 2926  "Meiofauna"                                          "valid"     NA
    # 3862  "Melampias"                                          "valid"     NA
    # 1324  "Melozone crissalis"                                 "valid"     NA
    # 2536  "Mertensiid ctenophore"                              "no result" NA
    # 1261  "Metopoplax ditomoides"                              "valid"     NA
    # 6990  "Micro-epiphytes"                                    "valid"     NA
    # 7468  "Microalgae"                                         "valid"     NA
    # 6213  "Microfauna"                                         "valid"     NA
    # 6835  "Microinvertebrates"                                 "valid"     NA
    # 10195 "Microprotozoa"                                      "valid"     NA
    # 8245  "Microsorium punctatum"                              "valid"     NA
    # 7424  "Microzooplankton"                                   "valid"     NA
    # 1034  "Mitoplinthus caliginosus"                           "valid"     NA
    # 6934  "Mollusc"                                            "no result" NA
    # 2645  "Monocaulus parvula"                                 "valid"     NA
    # 4868  "Moronrmississippiensis"                             "no result" NA
    # 6017  "Nanoplankton"                                       "invalid"   NA
    # 9662  "Nemertites"                                         "no result" NA
    # 4466  "Nestus mendicus"                                    "invalid"   NA
    # 9067  "Netting debris"                                     "no result" NA
    # 5839  "Non-gadoid fish remains"                            "no result" NA
    # 5338  "Non-insect arthropods"                              "valid"     NA
    # 3313  "Notocotyledae"                                      "valid"     NA
    # 8293  "Ochtocosmus africanus"                              "no result" NA
    # 9333  "Offal"                                              "no result" NA
    # 8624  "Ootetrastichus"                                     "no result" NA
    # 2673  "Opacelididii"                                       "valid"     NA
    # 8960  "Ophelina syringopyge"                               "valid"     NA
    # 6617  "Ophichthyidae"                                      "valid"     NA
    # 6978  "Organisms unidentified"                             "no result" NA
    # 8468  "Orthocladiariae"                                    "valid"     NA
    # 425   "Orychmontia"                                        "valid"     NA
    # 8045  "Osteichthys teleostei"                              "invalid"   NA
    # 5472  "Other worms"                                        "valid"     NA
    # 3871  "Oulema erichsoni"                                   "valid"     NA
    # 3855  "Oulema septentrionis"                               "valid"     NA
    # 9783  "Oxyloma sarsii"                                     "valid"     NA
    # 9066  "Paint debris"                                       "no result" NA
    # 9508  "Paracentropristes pomospilus"                       "valid"     NA
    # 4374  "Paralichthyes albigutta"                            "valid"     NA
    # 7296  "Paranyctiophylax"                                   "valid"     NA
    # 5975  "Parasite"                                           "no result" NA
    # 9329  "Parasites"                                          "valid"     NA
    # 9077  "Parasitic worms"                                    "no result" NA
    # 1313  "Paroxyna absinthii"                                 "valid"     NA
    # 1314  "Paroxyna misella"                                   "valid"     NA
    # 1238  "Paroxyna producta"                                  "valid"     NA
    # 5586  "Percarida"                                          "no result" NA
    # 8054  "Percarida-isopoda"                                  "no result" NA
    # 6440  "Periclemenes"                                       "no result" NA
    # 2394  "Perknaster fuscus antarcticus"                      "valid"     NA
    # 8924  "Phaephyceae"                                        "valid"     NA
    # 3960  "Phalacrus corruscus"                                "valid"     NA
    # 5718  "Phanerogams"                                        "valid"     NA
    # 4029  "Phoma arundinacea"                                  "valid"     NA
    # 2231  "Phyllosticta holosteae"                             "valid"     NA
    # 4660  "Phytoplankton"                                      "valid"     NA
    # 9723  "Pilumninae"                                         "valid"     NA
    # 4910  "Pisces"                                             "valid"     NA
    # 4804  "Plagyodes"                                          "no result" NA
    # 3039  "Plankton"                                           "valid"     NA
    # 6233  "Planktonic inverts cladocera etc�"                  "no result" NA
    # 6675  "Plastic"                                            "invalid"   NA
    # 3765  "Plautia affinis"                                    "valid"     NA
    # 532   "Podonomidae"                                        "valid"     NA
    # 9828  "Polyhydrus lineatus"                                "no result" NA
    # 2613  "Pomphorynchus"                                      "valid"     NA
    # 6792  "Posibacteria"                                       "no result" NA
    # 497   "Potamopurgus antipodarum"                           "valid"     NA
    # 7816  "Prey"                                               "no result" NA
    # 5766  "Prinospio sp. 1"                                    "no result" NA
    # 5774  "Prinospio sp. 2"                                    "no result" NA
    # 6475  "Protozooplankton"                                   "no result" NA
    # 6873  "Psammothidium lauenburgianum"                       "valid"     NA
    # 9873  "Pseudomopidae"                                      "no result" NA
    # 6834  "Pseudostaurosira elliptica"                         "valid"     NA
    # 1262  "Pseudostyphlus pillumus"                            "valid"     NA
    # 4518  "Psiloscops flammeolus"                              "valid"     NA
    # 8435  "Psychotha palustris"                                "no result" NA
    # 9009  "Pteropod"                                           "no result" NA
    # 9191  "Puget sound sculpin ruscarius meanyi"               "no result" NA
    # 7010  "Pulverized masses"                                  "valid"     NA
    # 6336  "Residue"                                            "invalid"   NA
    # 496   "Rhabdomastrix"                                      "valid"     NA
    # 1353  "Rhabdospora coriacea"                               "valid"     NA
    # 6286  "Rhoicoenia"                                         "valid"     NA
    # 3964  "Rhynchophanes mccownii"                             "no result" NA
    # 7806  "Rock"                                               "no result" NA
    # 8189  "Sandlances"                                         "no result" NA
    # 6885  "Sarortherdon macrochir"                             "valid"     NA
    # 6857  "Scapaloberis mucronata"                             "invalid"   NA
    # 6855  "Scaphaloberis mucronata"                            "valid"     NA
    # 7038  "Scarabidae"                                         "valid"     NA
    # 2545  "Scolex"                                             "valid"     NA
    # 3469  "Secernentia nematodes"                              "valid"     NA
    # 2067  "Sediment"                                           "valid"     NA
    # 8825  "Seeds"                                              "valid"     NA
    # 7011  "Sigara dorsalis"                                    "valid"     NA
    # 7369  "Sigaria semistraiata"                               "valid"     NA
    # 6480  "Silicioflagellates"                                 "invalid"   NA
    # 6900  "Silt"                                               "no result" NA
    # 9827  "Spanioconnus wetterhali"                            "invalid"   NA
    # 8632  "Sphaerocidae"                                       "no result" NA
    # 6474  "Spiroglyphus annulatus"                             "valid"     NA
    # 3702  "Spiruoidae"                                         "valid"     NA
    # 4054  "Stagonospora arenaria var. minor"                   "invalid"   NA
    # 9887  "Station refuse"                                     "no result" NA
    # 6846  "Staurostratum"                                      "valid"     NA
    # 3957  "Stenothrips graminum"                               "valid"     NA
    # 512   "Stratiomyid"                                        "valid"     NA
    # 9349  "Subviral agents"                                    "no result" NA
    # 5777  "Sumergent vascular"                                 "no result" NA
    # 9493  "Surface bait"                                       "valid"     NA
    # 9189  "Suspended or deposited organic matter"              "valid"     NA
    # 9352  "Synthetic material"                                 "invalid"   NA
    # 8406  "Taccazea apiculata"                                 "no result" NA
    # 3730  "Telicota bambusae"                                  "valid"     NA
    # 1342  "Tenthredo neobesa"                                  "valid"     NA
    # 6899  "Testate amoeba"                                     "no result" NA
    # 4599  "Theridion impressum"                                "valid"     NA
    # 8557  "Tintinnididae"                                      "no result" NA
    # 3939  "Tournotaris bimaculatus"                            "valid"     NA
    # 2888  "Trichopterna thorelli"                              "valid"     NA
    # 4942  "Trichoton"                                          "no result" NA
    # 10067 "Urogryllus"                                         "valid"     NA
    # 9808  "Vertigo moulinsiana"                                "valid"     NA
    # 9192  "Vetern poacher podothecus veternus"                 "no result" NA
    # 1380  "Vidalia cornuta"                                    "valid"     NA
    # 5980  "Wood"                                               "valid"     NA
    # 5041  "Woodhousii"                                         "no result" NA
    # 385   "Zealolessica cheira"                                "valid"     NA
    # 6884  "Zoobenthos"                                         "valid"     NA
    # 1742  "Zooplankton"                                        "valid"     NA
    # [1] "!!!!!!!!! INVALID RESULTS FROM gnr_resolve() !!!!!!!!!"
    #        submitted_name                            canonical_form
    #   [1,] "Acheloüs spinicarpus"                    "Achelous spinicarpus"
    #   [2,] "Actitus macularia"                       "Actitis macularia"
    #   [3,] "Alburniodes bipunctatus"                 "Alburnoides bipunctatus"
    #   [4,] "Amauropis rossiana"                      "Amauropsis rossiana"
    #   [5,] "Animallia"                               "Animalia"
    #   [6,] "Anthonantha acuminata"                   "Anthonotha acuminata"
    #   [7,] "Anthonantha fragrans"                    "Anthonotha fragrans"
    #   [8,] "Anthonantha macrophylla"                 "Anthonotha macrophylla"
    #   [9,] "Ascochyta hordei var. europaea"          "Ascochyta hordei europaea"
    #  [10,] "Athya marila"                            "Aythya marila"
    #  [11,] "Austropotamobius berndhauseri"           "Austropotamobius"
    #  [12,] "Benthos larvae"                          "Benthos"
    #  [13,] "Bossiella orbigniana var. orbigniana"    "Bossiella orbigniana orbigniana"
    #  [14,] "Callithamnion pikeanum var. pikeanum"    "Callithamnion pikeanum pikeanum"
    #  [15,] "Campostylus mannii"                      "Camptostylus mannii"
    #  [16,] "Capittelidae"                            "Capitellidae"
    #  [17,] "Castostmus occidentalis"                 "Catostomus occidentalis"
    #  [18,] "Celthrionomys"                           "Clethrionomys"
    #  [19,] "Ceracaria"                               "Cercaria"
    #  [20,] "Cerastium fontanum subsp. vulgare"       "Cerastium fontanum vulgare"
    #  [21,] "Chardrius vociferus"                     "Charadrius vociferus"
    #  [22,] "Chironomid type polyped"                 "Chironomid type"
    #  [23,] "Codium cf. bursa dho2-176"               "Codium bursa"
    #  [24,] "Crocicreas megalosporum var. gramineum"  "Crocicreas megalosporum gramineum"
    #  [25,] "Cyclopeterus lumpus"                     "Cyclopterus lumpus"
    #  [26,] "Cylorrhynchus psittacula"                "Cyclorrhynchus psittacula"
    #  [27,] "Cynomus gunnisoni"                       "Cynomys gunnisoni"
    #  [28,] "Cynomus ludovicianus"                    "Cynomys ludovicianus"
    #  [29,] "Deplatsia dewevrei"                      "Desplatsia dewevrei"
    #  [30,] "Diaporthe arctii var. artemisiae"        "Diaporthe arctii artemisiae"
    #  [31,] "Diastillidae"                            "Dascillidae"
    #  [32,] "Dicronychus equisetioides"               "Dicronychus"
    #  [33,] "Dipdomys compactus"                      "Dipodomys compactus"
    #  [34,] "Dipulmaris antarctica"                   "Diplulmaris antarctica"
    #  [35,] "Elytrigia atherica juncea = e. acuta"    "Elytrigia atherica"
    #  [36,] "Eragrostris curvula"                     "Eragrostis curvula"
    #  [37,] "Euhpagus cyanocephalus"                  "Euphagus cyanocephalus"
    #  [38,] "Eysarcoris trimaculatus"                 "Eysarcoris"
    #  [39,] "Fauchea laciniata var. pygmaea"          "Fauchea laciniata pygmaea"
    #  [40,] "Fellostomatidae"                         "Bellostomatidae"
    #  [41,] "Festuca ovina agg."                      "Festuca ovina"
    #  [42,] "Fragilaria capucina subsp. rumpens"      "Fragilaria capucina rumpens"
    #  [43,] "Fratecula cirrhata"                      "Fratercula cirrhata"
    #  [44,] "Fungal spores"                           "Fungal"
    #  [45,] "Galitheutis glacialis"                   "Galiteuthis glacialis"
    #  [46,] "Gasterosterus"                           "Gasterosteus"
    #  [47,] "Genaxinus bongraini"                     "Genaxinus"
    #  [48,] "Gomphonema cf. angustatum"               "Gomphonema angustatum"
    #  [49,] "Gomphonema cf. lagenula lk-2012"         "Gomphonema lagenula"
    #  [50,] "Graminaea"                               "Graminea"
    #  [51,] "Gymnoscoelus nicholsi"                   "Gymnoscopelus nicholsi"
    #  [52,] "Healianthus"                             "Helianthus"
    #  [53,] "Hyalites acerata"                        "Hyalites"
    #  [54,] "Hypnea valentiae var. valentiae"         "Hypnea valentiae valentiae"
    #  [55,] "Hypselodelphis violacea"                 "Hypselodelphys violacea"
    #  [56,] "Illybius fuliginosus"                    "Ilybius fuliginosus"
    #  [57,] "Iridaea vincent"                         "Iridaea"
    #  [58,] "Jania tenella var. zacae"                "Jania tenella zacae"
    #  [59,] "Jasminium abyssinicum"                   "Jasminum abyssinicum"
    #  [60,] "Kröyeria"                                "Kroyeria"
    #  [61,] "Lachnum carneolum var. longisporum"      "Lachnum carneolum longisporum"
    #  [62,] "Large xiphideocercaria"                  "Large"
    #  [63,] "Leplaca mayumbensis"                     "Leplaea mayumbensis"
    #  [64,] "Leptosphaeria culmifraga var. propinqua" "Leptosphaeria culmifraga propinqua"
    #  [65,] "Letherocerus americanus"                 "Lethocerus americanus"
    #  [66,] "Macrolymenella"                          "Macroclymenella"
    #  [67,] "Macropolepia"                            "Macropelopia"
    #  [68,] "Marine detritus"                         "Marine"
    #  [69,] "Mildbraedeodendron excelsum"             "Mildbraediodendron excelsum"
    #  [70,] "Monanthocloe littoralis"                 "Monanthochloe littoralis"
    #  [71,] "Morotheutis ingens"                      "Moroteuthis ingens"
    #  [72,] "Mucus"                                   "Hucus"
    #  [73,] "N-doridoidea"                            "Eudoridoidea"
    #  [74,] "Nanoplankton"                            "Nannoplankton"
    #  [75,] "Neballidae"                              "Nebaliidae"
    #  [76,] "Nematocysts"                             "Nematocystis"
    #  [77,] "Nestus mendicus"                         "Nestus"
    #  [78,] "Noctuids"                                "Noctuides"
    #  [79,] "Northern aplomado falcon adenovirus"     "Northern"
    #  [80,] "Nysius vinitor"                          "Nysius"
    #  [81,] "Onacea"                                  "Onawea"
    #  [82,] "Osteichthys teleostei"                   "Osteichthys"
    #  [83,] "Ostracoda sp. sbh266127"                 "Ostracoda"
    #  [84,] "Oulema rufocyanea/melanopus agg."        "Oulema"
    #  [85,] "Palythoa grandis author unknown"         "Palythoa grandis"
    #  [86,] "Pandion haliaëtus haliaëtus"             "Pandion haliaetus haliaetus"
    #  [87,] "Papulospora halima"                      "Papulaspora halima"
    #  [88,] "Parapsia grewioides"                     "Paropsia grewioides"
    #  [89,] "Permyscus maniculatus"                   "Peromyscus maniculatus"
    #  [90,] "Phalacrocorax atriceps georgianus"       "Phalacrocorax atriceps"
    #  [91,] "Phalacrocorax atriceps nivalis"          "Phalacrocorax atriceps"
    #  [92,] "Physeter macrocephalus"                  "Physeter"
    #  [93,] "Pica pica"                               "Pica"
    #  [94,] "Pipinculus"                              "Pipunculus"
    #  [95,] "Plastic"                                 "Plastica"
    #  [96,] "Pleocyemete"                             "Pleocyemata"
    #  [97,] "Pooecets gramineus"                      "Pooecetes gramineus"
    #  [98,] "Porphyra perforata var. perforata"       "Porphyra perforata perforata"
    #  [99,] "Porphyra porphyra var. nereocystis"      "Porphyra porphyra nereocystis"
    # [100,] "Prodiames olivacea"                      "Prodiamesa olivacea"
    # [101,] "Pseudiacris regilla"                     "Pseudacris regilla"
    # [102,] "Pseudophaeroma campbellensis"            "Pseudosphaeroma campbellensis"
    # [103,] "Repitilia"                               "Reptilia"
    # [104,] "Residue"                                 "Residua"
    # [105,] "Romalium"                                "Homalium"
    # [106,] "Sagartiogetum undatum"                   "Sagartiogeton undatus"
    # [107,] "Scapaloberis mucronata"                  "Scaphaloberis mucronata"
    # [108,] "Scieanops ocellatus"                     "Sciaenops ocellatus"
    # [109,] "Scolymasta joubini"                      "Scolymastra joubini"
    # [110,] "Scopriones"                              "Scorpiones"
    # [111,] "Scorpaenichtys marmoratus"               "Scorpaenichthys marmoratus"
    # [112,] "Sebates"                                 "Sebastes"
    # [113,] "Selachiomorpha"                          "Selachimorpha"
    # [114,] "Silene latifolia subsp. alba"            "Silene latifolia alba"
    # [115,] "Silicioflagellates"                      "Silicoflagellata"
    # [116,] "Simocephalous vetulas"                   "Simocephalus vetulus"
    # [117,] "Simocybe centunculus var. maritima"      "Simocybe centunculus maritima"
    # [118,] "Smithora naiadum var. naiadum"           "Smithora naiadum naiadum"
    # [119,] "Spanioconnus wetterhali"                 "Spanioconnus"
    # [120,] "Stagonospora arenaria var. minor"        "Stagonospora arenaria minor"
    # [121,] "Stauditia stipitata"                     "Staudtia stipitata"
    # [122,] "Stigeidae"                               "Strigeidae"
    # [123,] "Stones"                                  "Sitones"
    # [124,] "Syngnthidae"                             "Syngnathidae"
    # [125,] "Synthetic material"                      "Synthetic"
    # [126,] "Sysnsepalum stipulatum"                  "Synsepalum stipulatum"
    # [127,] "Syzgium guineensis"                      "Syzygium guineense"
    # [128,] "Tanaisus"                                "Tanaissus"
    # [129,] "Taraxacum officinale agg."               "Taraxacum officinale"
    # [130,] "Tenbrionidae"                            "Tenebrionidae"
    # [131,] "Threskiornis moluccus"                   "Threskiornis"
    # [132,] "Trachelus troglodytus"                   "Trachelus troglodyta"
    # [133,] "Trypanorynch"                            "Trypanorhyncha"
    # [134,] "Tubularia ralphii"                       "Tubularia"
    # [135,] "Vibillia antarctica"                     "Vibilia antarctica"
    # [136,] "Vibillia stebbingi"                      "Vibilia stebbingi"
    # [137,] "Xylosyctes"                              "Xyloryctes"
    # [1] "!!!!!!!!! MULTIPLE TAXON FROM gnr_resolve() !!!!!!!!!"
          # tx.list                                valid     rank
    # 5151  "Acanthocephala"                       "valid"   "genus - phylum - subgenus"
    # 9177  "Acaridae"                             "valid"   "family - species"
    # 8493  "Achilidae"                            "valid"   "family - species"
    # 4782  "Acipenser"                            "valid"   "genus - species"
    # 889   "Actinopterygii"                       "valid"   "class - superclass"
    # 1654  "Agabus"                               "valid"   "genus - subgenus - species"
    # 9418  "Agapetus"                             "valid"   "genus - subgenus"
    # 4279  "Agkistrodon piscivorus"               "valid"   "species - infraspecies"
    # 6197  "Aglaophenia"                          "valid"   "genus - species"
    # 9431  "Agonum"                               "valid"   "genus - subgenus"
    # 9257  "Agrostis stolonifera"                 "valid"   "species - infraspecies"
    # 9300  "Agrostis vinealis"                    "valid"   "species - infraspecies"
    # 7569  "Alcyoniidae"                          "valid"   "family - species"
    # 4453  "Aleochara"                            "valid"   "genus - subgenus"
    # 7534  "Alpheus"                              "valid"   "genus - species"
    # 5169  "Alsophis portoricensis"               "valid"   "infraspecies - species"
    # 9430  "Amara"                                "valid"   "genus - subgenus"
    # 8500  "Amaranthus"                           "valid"   "genus - species"
    # 8481  "Ameiva exsul"                         "valid"   "species - infraspecies"
    # 7460  "Amphidinium"                          "valid"   "genus - species"
    # 4878  "Amphisbaenia"                         "valid"   "suborder - parvorder"
    # 6902  "Anabaena"                             "valid"   "genus - species"
    # 4755  "Anas carolinensis"                    "valid"   "species - infraspecies - subspecies"
    # 989   "Anas crecca carolinensis"             "valid"   "infraspecies - subspecies - species"
    # 1687  "Anisoptera"                           "valid"   "genus - suborder"
    # 1168  "Annelida"                             "valid"   "phylum - species"
    # 8484  "Anolis"                               "valid"   "genus - species"
    # 560   "Anthopleura"                          "valid"   "genus - species"
    # 2105  "Anthozoa"                             "valid"   "class - subphylum"
    # 4340  "Anura"                                "valid"   "genus - order"
    # 2619  "Aphelocoma californica californica"   "valid"   "subspecies - infraspecies"
    # 3168  "Aphia minuta"                         "valid"   "species - infraspecies"
    # 1265  "Aphididae"                            "valid"   "family - species"
    # 2224  "Aphis fabae"                          "valid"   "infraspecies - species"
    # 8513  "Apidae"                               "valid"   "family - genus"
    # 5413  "Appendicularia"                       "valid"   "genus - class"
    # 5101  "Arachnida"                            "valid"   "class - species"
    # 9210  "Arion"                                "valid"   "genus - species"
    # 9417  "Asellus"                              "valid"   "genus - subgenus"
    # 8407  "Asplenium"                            "valid"   "genus - species"
    # 6610  "Astropecten"                          "valid"   "genus - species"
    # 7021  "Avena"                                "valid"   "genus - species"
    # 4659  "Bacteria"                             "valid"   "genus - kingdom"
    # 1730  "Balaenoptera musculus brevicauda"     "valid"   "infraspecies - subspecies"
    # 1731  "Balaenoptera musculus intermedia"     "valid"   "infraspecies - subspecies"
    # 810   "Bathyraja"                            "valid"   "genus - species"
    # 6709  "Batoidea"                             "valid"   "order - superorder"
    # 8495  "Blattidae"                            "valid"   "family - species - genus"
    # 2584  "Boiga dendrophila"                    "valid"   "species - infraspecies"
    # 8120  "Bosmina"                              "valid"   "genus - subgenus"
    # 6995  "Bossiella orbigniana var. orbigniana" "invalid" "variety - subspecies"
    # 4941  "Brachinus"                            "valid"   "genus - subgenus"
    # 7228  "Brachionus"                           "valid"   "genus - species"
    # 2225  "Brachycaudus"                         "valid"   "genus - species"
    # 1274  "Brachycaudus helichrysi"              "valid"   "species - infraspecies"
    # 494   "Brachycentrus"                        "valid"   "genus - subgenus"
    # 9437  "Braconidae"                           "valid"   "family - species - genus"
    # 8471  "Bryophyta"                            "valid"   "phylum - division"
    # 5408  "Calanoida"                            "valid"   "order - species"
    # 6877  "Calothrix"                            "valid"   "genus - species"
    # 9473  "Cambarus"                             "valid"   "genus - subgenus - species"
    # 4944  "Camponotus"                           "valid"   "genus - species"
    # 5341  "Canalipalpata"                        "valid"   "order - infraclass"
    # 3452  "Canis lupus dingo"                    "valid"   "subspecies - infraspecies"
    # 996   "Canis lupus familiaris"               "valid"   "subspecies - infraspecies"
    # 8361  "Canthium"                             "valid"   "genus - species"
    # 5095  "Canthon"                              "valid"   "genus - subgenus"
    # 445   "Capnia vidua"                         "valid"   "species - infraspecies"
    # 10188 "Cassia fistula"                       "valid"   "species - infraspecies"
    # 6401  "Caulerpa racemosa"                    "valid"   "species - varietas"
    # 6067  "Centaurea scabiosa"                   "valid"   "species - infraspecies"
    # 6741  "Cerastium fontanum subsp. vulgare"    "invalid" "subspecies - species"
    # 6385  "Cercopidae"                           "valid"   "family - genus - species"
    # 9769  "Cercyon"                              "valid"   "genus - subgenus"
    # 4676  "Chaetoceros"                          "valid"   "genus - subgenus"
    # 8433  "Chassalia"                            "valid"   "genus - species"
    # 2434  "Chelonia mydas"                       "valid"   "species - infraspecies"
    # 2374  "Chilomycterus antennatus"             "valid"   "species - infraspecies"
    # 7568  "Chilophiurina"                        "valid"   "infraorder - suborder"
    # 2777  "Chimarra"                             "valid"   "genus - subgenus"
    # 393   "Chironomidae"                         "valid"   "family - species"
    # 9312  "Chironomus"                           "valid"   "genus - subgenus - species"
    # 6699  "Chiroteuthis"                         "valid"   "genus - subgenus"
    # 4943  "Chlaenius"                            "valid"   "genus - subgenus"
    # 6910  "Chlorophyceae"                        "valid"   "class - species"
    # 6235  "Chlorophyta"                          "valid"   "infrakingdom - phylum"
    # 8606  "Chloropidae"                          "valid"   "family - species"
    # 859   "Chondrichthyes"                       "valid"   "superclass - class"
    # 6038  "Chrysanthemoides monilifera"          "valid"   "species - varietas - subspecies"
    # 2826  "Chrysemys picta"                      "valid"   "species - infraspecies"
    # 5054  "Chrysophyceae"                        "valid"   "class - species"
    # 9797  "Chrysopidae"                          "valid"   "family - species"
    # 4956  "Cicadidae"                            "valid"   "family - species - genus"
    # 3133  "Ciliophora"                           "valid"   "genus - phylum"
    # 3554  "Cinygmula"                            "valid"   "genus - species"
    # 1521  "Cliona"                               "valid"   "genus - species"
    # 4974  "Clitellata"                           "valid"   "class - subphylum - phylum"
    # 249   "Clupea harengus"                      "valid"   "species - infraspecies"
    # 10051 "Cnemidophorus gularis"                "valid"   "species - infraspecies"
    # 10007 "Cnemidophorus tesselatus"             "valid"   "species - infraspecies"
    # 5043  "Cnemidophorus tigris"                 "valid"   "species - infraspecies"
    # 3782  "Coleoptera"                           "valid"   "order - species"
    # 7250  "Collembola"                           "valid"   "order - class"
    # 5034  "Coluber constrictor"                  "valid"   "species - infraspecies"
    # 2544  "Contracaecum"                         "valid"   "genus - species"
    # 6608  "Conus"                                "valid"   "genus - species"
    # 2628  "Corvus corone corone"                 "valid"   "subspecies - infraspecies"
    # 6588  "Corycaeus"                            "valid"   "genus - subgenus"
    # 1015  "Corynosoma"                           "valid"   "genus - species"
    # 6953  "Coscinodiscus radiatus"               "valid"   "species - form"
    # 6106  "Crepidula"                            "valid"   "genus - species"
    # 382   "Cricotopus"                           "valid"   "genus - subgenus - species"
    # 5202  "Crotalus viridis"                     "valid"   "species - infraspecies"
    # 5124  "Crotaphytus collaris"                 "valid"   "species - infraspecies"
    # 8282  "Croton"                               "valid"   "genus - species"
    # 5484  "Cryptoblepharus virgatus"             "valid"   "infraspecies - species"
    # 26    "Ctenophora"                           "valid"   "genus - phylum"
    # 6113  "Cucumaria"                            "valid"   "genus - infraspecies"
    # 7107  "Culicoides"                           "valid"   "genus - subgenus - species"
    # 8060  "Cyanea"                               "valid"   "genus - species"
    # 6917  "Cyclotella"                           "valid"   "genus - species"
    # 7301  "Cyprinodon variegatus variegatus"     "valid"   "subspecies - infraspecies"
    # 9922  "Cytheropteron"                        "valid"   "genus - species"
    # 7233  "Daphnia"                              "valid"   "genus - species"
    # 6680  "Dasyatis"                             "valid"   "genus - species"
    # 9795  "Delphacidae"                          "valid"   "family - species"
    # 9796  "Delphacinae"                          "valid"   "subfamily - species - genus"
    # 9794  "Deltocephalinae"                      "valid"   "subfamily - species"
    # 4214  "Diabrotica virgifera virgifera"       "valid"   "subspecies - infraspecies"
    # 6351  "Diadema"                              "valid"   "genus - species"
    # 4930  "Diadophis punctatus"                  "valid"   "species - infraspecies"
    # 2929  "Diaptomidae"                          "valid"   "family - species"
    # 6362  "Digenea"                              "valid"   "subclass - genus"
    # 5396  "Dinophyceae"                          "valid"   "class - species"
    # 8617  "Dipogon"                              "valid"   "genus - species"
    # 3465  "Diptera"                              "valid"   "order - species"
    # 9930  "Dryomys nitedula"                     "valid"   "species - subspecies"
    # 9874  "Dryops"                               "valid"   "genus - subgenus"
    # 1369  "Dysaphis lappae"                      "valid"   "species - infraspecies"
    # 2614  "Echinorhynchus"                       "valid"   "genus - species"
    # 5834  "Echiura"                              "valid"   "phylum - subclass"
    # 7626  "Echiurida"                            "valid"   "order - subclass"
    # 6643  "Elasmobranchii"                       "valid"   "class - subclass"
    # 9274  "Elymus caninus"                       "valid"   "species - infraspecies"
    # 3487  "Enhydra lutris kenyoni"               "valid"   "subspecies - infraspecies"
    # 2096  "Enhydra lutris nereis"                "valid"   "subspecies - infraspecies"
    # 4253  "Enodia portlandia anthedon"           "valid"   "infraspecies - subspecies"
    # 10203 "Enoploteuthis"                        "valid"   "genus - subgenus"
    # 3409  "Ephemerella"                          "valid"   "genus - species"
    # 2430  "Eretmochelys imbricata"               "valid"   "species - infraspecies"
    # 8832  "Eriocephalus africanus paniculatus"   "valid"   "infraspecies - varietas"
    # 10124 "Eubosmina"                            "valid"   "genus - subgenus"
    # 8623  "Eucoilinae"                           "valid"   "subfamily - species"
    # 8755  "Euglena"                              "valid"   "genus - species"
    # 5547  "Euphausia"                            "valid"   "genus - species"
    # 7049  "Euphorbia"                            "valid"   "genus - species group - species - species subgroup"
    # 4204  "Eurytoma"                             "valid"   "genus - species"
    # 9547  "Fabricia"                             "valid"   "genus - subgenus"
    # 6721  "Fellodistomidae"                      "valid"   "family - species - genus"
    # 7029  "Ficus"                                "valid"   "genus - species"
    # 8331  "Ficus natalensis"                     "valid"   "species - infraspecies"
    # 8375  "Ficus sarmentosa luducca"             "valid"   "infraspecies - varietas"
    # 5660  "Galathea"                             "valid"   "genus - species"
    # 7554  "Galatheidae"                          "valid"   "family - species"
    # 8289  "Garcinia"                             "valid"   "genus - species"
    # 5094  "Geotrupes"                            "valid"   "genus - subgenus"
    # 1701  "Gerris"                               "valid"   "genus - subgenus"
    # 4515  "Glaucidium gnoma californicum"        "valid"   "infraspecies - subspecies"
    # 3182  "Glossosoma"                           "valid"   "genus - subgenus"
    # 5962  "Glycinde"                             "valid"   "genus - species"
    # 5574  "Gnathostomata"                        "valid"   "infraphylum - superorder"
    # 3178  "Golfingia margaritacea margaritacea"  "valid"   "infraspecies - subspecies - species"
    # 3179  "Golfingia margaritacea ohlini"        "valid"   "infraspecies - subspecies - species"
    # 1771  "Gonatopsis"                           "valid"   "genus - subgenus"
    # 5957  "Goniada"                              "valid"   "genus - species"
    # 6396  "Gracilaria"                           "valid"   "genus - species"
    # 1501  "Gymnodinium"                          "valid"   "genus - species"
    # 356   "Gyraulus"                             "valid"   "genus - subgenus"
    # 7515  "Halichondria"                         "valid"   "genus - subgenus"
    # 7079  "Haliotis kamtschatkana kamtschatkana" "valid"   "subspecies - infraspecies"
    # 9798  "Haplothrips"                          "valid"   "genus - subgenus"
    # 5097  "Harpalus"                             "valid"   "genus - subgenus"
    # 3565  "Hediste diversicolor"                 "valid"   "species - genus"
    # 5405  "Heliozoa"                             "valid"   "class - phylum"
    # 9457  "Helophorus"                           "valid"   "genus - subgenus"
    # 7015  "Hemiptera"                            "valid"   "order - species"
    # 3467  "Herpestes javanicus auropunctatus"    "valid"   "infraspecies - subspecies"
    # 5025  "Heterodon nasicus"                    "valid"   "species - infraspecies"
    # 7607  "Hippolyte"                            "valid"   "genus - species"
    # 2652  "Hirudinea"                            "valid"   "class - subclass"
    # 7438  "Holothuria"                           "valid"   "genus - species"
    # 17    "Homo sapiens"                         "valid"   "species - subspecies"
    # 9405  "Hyalella"                             "valid"   "genus - species"
    # 383   "Hydraena"                             "valid"   "genus - species"
    # 4914  "Hydrobatidae"                         "valid"   "family - subfamily"
    # 5870  "Hydrophilus"                          "valid"   "genus - subgenus"
    # 457   "Hydropsyche"                          "valid"   "genus - subgenus"
    # 8598  "Hylaeus"                              "valid"   "genus - subgenus - species"
    # 7516  "Hymeniacidon"                         "valid"   "genus - species"
    # 2418  "Hymenolepididae"                      "valid"   "family - species"
    # 4946  "Hymenoptera"                          "valid"   "order - species"
    # 8415  "Hypericum"                            "valid"   "genus - species"
    # 9593  "Hyporhamphus"                         "valid"   "genus - subgenus"
    # 5221  "Hystricognathi"                       "valid"   "suborder - infraorder"
    # 6383  "Ichneumonidae"                        "valid"   "family - species - genus"
    # 7632  "Jaera"                                "valid"   "genus - subgenus"
    # 4472  "Lacerta agilis"                       "valid"   "species - infraspecies"
    # 5313  "Lacerta media"                        "valid"   "species - infraspecies"
    # 439   "Lampetra"                             "valid"   "genus - species"
    # 1490  "Larus canus brachyrhynchus"           "valid"   "infraspecies - subspecies"
    # 1438  "Larus dominicanus dominicanus"        "valid"   "infraspecies - subspecies"
    # 4920  "Lepidoptera"                          "valid"   "order - species - genus"
    # 869   "Lepidorhombus whiffiagonis"           "valid"   "species - subspecies"
    # 9460  "Lepidostoma"                          "valid"   "genus - subgenus"
    # 7533  "Leptochelia"                          "valid"   "genus - species"
    # 8483  "Leptodactylus"                        "valid"   "genus - species"
    # 5816  "Leptophlebiidae"                      "valid"   "family - superfamily"
    # 4789  "Lepus"                                "valid"   "genus - subgenus"
    # 3219  "Leucocarbo albiventer purpurascens"   "valid"   "infraspecies - subspecies"
    # 1758  "Leucocarbo atriceps bransfieldensis"  "valid"   "infraspecies - subspecies"
    # 3337  "Limacina antarctica"                  "valid"   "subspecies - species"
    # 527   "Limnophila"                           "valid"   "genus - subfamily"
    # 5160  "Liolaemus"                            "valid"   "genus - species"
    # 4836  "Loligo"                               "valid"   "genus - subgenus"
    # 1105  "Lottia"                               "valid"   "genus - species"
    # 5492  "Lycosa"                               "valid"   "genus - species"
    # 4469  "Lycosidae"                            "valid"   "family - genus - species"
    # 5964  "Macrobrachium"                        "valid"   "genus - species"
    # 9608  "Magnoliophyta"                        "valid"   "division - phylum"
    # 7025  "Malus pumila"                         "valid"   "species - infraspecies"
    # 10042 "Mantidae"                             "valid"   "family - genus - species"
    # 5193  "Marmota"                              "valid"   "genus - subgenus"
    # 4929  "Masticophis flagellum"                "valid"   "species - infraspecies"
    # 10005 "Masticophis lateralis"                "valid"   "infraspecies - species"
    # 4928  "Masticophis taeniatus"                "valid"   "infraspecies - species"
    # 4657  "Mastocarpus"                          "valid"   "genus - species"
    # 8664  "Mazzaella"                            "valid"   "genus - species"
    # 4827  "Melanitta fusca deglandi"             "valid"   "infraspecies - subspecies"
    # 5087  "Melanoplus"                           "valid"   "genus - species"
    # 8605  "Meligethes"                           "valid"   "genus - species"
    # 3876  "Metopolophium dirhodum"               "valid"   "infraspecies - species"
    # 2780  "Micrasema"                            "valid"   "genus - species"
    # 5374  "Microphallus"                         "valid"   "genus - species"
    # 4710  "Microtus"                             "valid"   "genus - species"
    # 9318  "Millepora"                            "valid"   "genus - species"
    # 6382  "Miridae"                              "valid"   "family - species"
    # 7219  "Molanna"                              "valid"   "genus - subgenus"
    # 10131 "Monogononta"                          "valid"   "class - subclass"
    # 10030 "Mus"                                  "valid"   "genus - subgenus"
    # 4932  "Myotis"                               "valid"   "genus - species"
    # 6014  "Mysidacea"                            "valid"   "superorder - order"
    # 3374  "Mytilus"                              "valid"   "genus - species"
    # 8470  "Nais"                                 "valid"   "genus - species"
    # 9204  "Nassarius"                            "valid"   "genus - species"
    # 8899  "Nematocarcinus"                       "valid"   "genus - species"
    # 2107  "Nemertea"                             "valid"   "phylum - species"
    # 7567  "Neogastropoda"                        "valid"   "order - infraorder"
    # 6109  "Neoloricata"                          "valid"   "subclass - order"
    # 9592  "Nicholsina usta"                      "valid"   "species - infraspecies"
    # 4672  "Nitzschia"                            "valid"   "genus - species"
    # 7037  "Noctuidae"                            "valid"   "family - species - genus"
    # 10069 "Norops lineatopus"                    "valid"   "species - infraspecies"
    # 8756  "Nostoc"                               "valid"   "genus - species"
    # 8507  "Notonecta"                            "valid"   "genus - subgenus"
    # 2917  "Numenius phaeopus hudsonicus"         "valid"   "infraspecies - subspecies"
    # 5749  "Nymphaea"                             "valid"   "genus - species"
    # 5831  "Octopodidae"                          "valid"   "family - species"
    # 8856  "Octopus"                              "valid"   "genus - species"
    # 5811  "Odonata"                              "valid"   "order - species"
    # 3117  "Oecetis"                              "valid"   "genus - species"
    # 5587  "Oikopleura"                           "valid"   "genus - species"
    # 367   "Oligochaeta"                          "valid"   "genus - class"
    # 5379  "Opecoelidae"                          "valid"   "family - species"
    # 7636  "Ophelina"                             "valid"   "genus - species"
    # 6301  "Orchomene"                            "valid"   "genus - species"
    # 8970  "Orchomenella"                         "valid"   "genus - species"
    # 1212  "Orconectes virilis"                   "valid"   "species - subspecies"
    # 6854  "Oribatida"                            "valid"   "order - suborder"
    # 373   "Orthocladiinae"                       "valid"   "subfamily - genus - species"
    # 2264  "Orthocladius"                         "valid"   "genus - subgenus"
    # 5746  "Oryza"                                "valid"   "genus - species"
    # 6238  "Oscillatoria"                         "valid"   "genus - species"
    # 8929  "Ostracoda sp. sbh266127"              "invalid" "species - class"
    # 3854  "Oulema rufocyanea/melanopus agg."     "invalid" "genus - subgenus"
    # 3783  "Ovis"                                 "valid"   "genus - species"
    # 472   "Oxyethira"                            "valid"   "genus - subgenus"
    # 4485  "Pachygnatha"                          "valid"   "genus - superfamily"
    # 1671  "Pacifastacus"                         "valid"   "genus - subgenus"
    # 6244  "Palaemon"                             "valid"   "genus - species"
    # 1491  "Pandion haliaëtus haliaëtus"          "invalid" "subspecies - infraspecies"
    # 8695  "Panopeidae"                           "valid"   "family - species"
    # 6979  "Paracalanus"                          "valid"   "genus - species"
    # 128   "Parazoanthus"                         "valid"   "genus - species"
    # 6452  "Parthenope"                           "valid"   "genus - species"
    # 873   "Patagonotothen guntheri"              "valid"   "species - infraspecies"
    # 6110  "Patellogastropoda"                    "valid"   "order - class"
    # 9886  "Pelecanoides urinatrix exsul"         "valid"   "infraspecies - subspecies"
    # 1679  "Pentaneurini"                         "valid"   "tribe - species"
    # 6451  "Periclimenes"                         "valid"   "genus - species"
    # 6813  "Perinereis"                           "valid"   "genus - species"
    # 4385  "Peromyscus"                           "valid"   "genus - species"
    # 7346  "Peyssonnelia"                         "valid"   "genus - species"
    # 6368  "Phaeophyceae"                         "valid"   "class - phylum"
    # 3667  "Phalacrocorax varius varius"          "valid"   "infraspecies - subspecies"
    # 9649  "Phascolion"                           "valid"   "genus - subgenus"
    # 6815  "Phascolosoma"                         "valid"   "genus - subgenus"
    # 7585  "Phlebobranchia"                       "valid"   "suborder - order"
    # 1388  "Phomopsis"                            "valid"   "genus - species"
    # 9472  "Phryganeidae"                         "valid"   "family - superfamily"
    # 258   "Phycidae"                             "valid"   "family - subfamily"
    # 1279  "Phyciodes campestris"                 "valid"   "species - subspecies"
    # 8345  "Phyllanthus"                          "valid"   "genus - species"
    # 5090  "Phyllophaga"                          "valid"   "genus - subgenus"
    # 9403  "Physa"                                "valid"   "genus - species"
    # 5732  "Picornaviridae"                       "valid"   "family - species"
    # 7063  "Pinus"                                "valid"   "genus - species"
    # 8431  "Piper"                                "valid"   "genus - species"
    # 8642  "Pipinculus"                           "invalid" "genus - subgenus"
    # 9981  "Pipistrellus"                         "valid"   "genus - species"
    # 10060 "Pipistrellus subflavus"               "valid"   "species - infraspecies"
    # 5191  "Pituophis catenifer"                  "valid"   "species - infraspecies"
    # 3308  "Pituophis melanoleucus"               "valid"   "species - infraspecies"
    # 5148  "Plasmodium"                           "valid"   "genus - species - subgenus"
    # 221   "Platybelone argalus"                  "valid"   "species - infraspecies"
    # 8645  "Platycheirus"                         "valid"   "genus - species"
    # 5801  "Plecoptera"                           "valid"   "genus - order - species"
    # 9382  "Plecotus rafinesquii"                 "valid"   "species - infraspecies"
    # 10006 "Plestiodon gilberti"                  "valid"   "species - infraspecies"
    # 6975  "Poecilostomatoida"                    "valid"   "order - species"
    # 9851  "Poecilus"                             "valid"   "genus - subgenus"
    # 4232  "Poicephalus robustus suahelicus"      "valid"   "subspecies - infraspecies"
    # 4921  "Polistes"                             "valid"   "genus - species"
    # 3527  "Polychaeta"                           "valid"   "genus - subphylum - class"
    # 363   "Polypedilum"                          "valid"   "genus - subgenus - species"
    # 10012 "Polyphylla"                           "valid"   "genus - subgenus"
    # 6460  "Porcellanidae"                        "valid"   "family - superfamily"
    # 7653  "Priapulida"                           "valid"   "class - phylum"
    # 10018 "Prionus"                              "valid"   "genus - subgenus"
    # 5696  "Prochlorococcus"                      "valid"   "genus - species"
    # 6552  "Prorocentrum"                         "valid"   "genus - species"
    # 3068  "Prosimulium"                          "valid"   "genus - species"
    # 7024  "Prunus"                               "valid"   "genus - species"
    # 7940  "Pseudocuma"                           "valid"   "genus - subgenus"
    # 8233  "Psychotria"                           "valid"   "genus - species"
    # 6239  "Pteromalidae"                         "valid"   "family - species"
    # 10016 "Pterostichus"                         "valid"   "genus - subgenus"
    # 7936  "Pterygota"                            "valid"   "genus - subclass"
    # 8302  "Pycnanthus angolensis"                "valid"   "species - infraspecies"
    # 7034  "Quercus"                              "valid"   "genus - species"
    # 5398  "Radiolaria"                           "valid"   "order - genus"
    # 1704  "Rana"                                 "valid"   "genus - species"
    # 6728  "Renicolidae"                          "valid"   "family - species"
    # 9474  "Rhagovelia"                           "valid"   "genus - subgenus"
    # 7033  "Rhamnus purshiana"                    "valid"   "infraspecies - species"
    # 6285  "Rhopalodia"                           "valid"   "genus - species"
    # 3853  "Rhopalosiphum padi"                   "valid"   "species - genus"
    # 7030  "Rubus"                                "valid"   "genus - species"
    # 6756  "Sagina procumbens"                    "valid"   "species - infraspecies"
    # 8343  "Salacia"                              "valid"   "genus - species"
    # 5681  "Salicornia"                           "valid"   "genus - species"
    # 5699  "Salmo"                                "valid"   "genus - species"
    # 9482  "Salmo trutta fario"                   "valid"   "subspecies - species"
    # 5399  "Salpidae"                             "valid"   "family - species"
    # 8610  "Sarcophagidae"                        "valid"   "family - genus - species"
    # 5308  "Sauria"                               "valid"   "genus - order"
    # 9980  "Scarabaeus"                           "valid"   "genus - species"
    # 9640  "Scarus"                               "valid"   "genus - species"
    # 5042  "Sceloporus occidentalis"              "valid"   "species - infraspecies"
    # 6906  "Scenedesmus"                          "valid"   "genus - species"
    # 5015  "Sciurus"                              "valid"   "genus - subgenus"
    # 3764  "Scopula"                              "valid"   "genus - species"
    # 5861  "Scrippsiella"                         "valid"   "genus - species"
    # 2035  "Sebastes"                             "valid"   "genus - species"
    # 4777  "Sebates"                              "invalid" "genus - species"
    # 7647  "Sepia"                                "valid"   "genus - subgenus"
    # 6637  "Sepiola"                              "valid"   "genus - species"
    # 331   "Serpentes"                            "valid"   "infraorder - suborder"
    # 5343  "Serpulidae"                           "valid"   "family - species"
    # 8680  "Serranus"                             "valid"   "genus - species"
    # 7248  "Sigara"                               "valid"   "genus - subgenus"
    # 6736  "Silene acaulis"                       "valid"   "species - subspecies"
    # 6734  "Silene latifolia"                     "valid"   "species - infraspecies"
    # 6733  "Silene latifolia subsp. alba"         "invalid" "subspecies - infraspecies - species"
    # 6749  "Silene uniflora"                      "valid"   "species - infraspecies"
    # 437   "Simulium"                             "valid"   "genus - species"
    # 6962  "Siphonophora"                         "valid"   "genus - order"
    # 5907  "Sipuncula"                            "valid"   "phylum - subclass"
    # 6041  "Solidago californica"                 "valid"   "infraspecies - subspecies"
    # 5116  "Sorex"                                "valid"   "genus - subgenus"
    # 7972  "Spatangus"                            "valid"   "genus - species"
    # 1991  "Spinocalanus abyssalis pygmaeus"      "valid"   "infraspecies - subspecies"
    # 6872  "Spirulina"                            "valid"   "suborder - genus"
    # 2883  "Staphylinidae"                        "valid"   "family - species"
    # 2884  "Staphylininae"                        "valid"   "subfamily - family"
    # 8814  "Staudtia gabonensis"                  "valid"   "species - infraspecies"
    # 10111 "Staurastrum"                          "valid"   "genus - species"
    # 6740  "Stellaria alsine"                     "valid"   "species - infraspecies"
    # 6752  "Stellaria nemorum"                    "valid"   "species - infraspecies"
    # 8525  "Stenopelmatus"                        "valid"   "genus - species"
    # 3238  "Stercorarius antarcticus lonnbergi"   "valid"   "infraspecies - subspecies"
    # 9793  "Sternorrhyncha"                       "valid"   "suborder - order"
    # 5381  "Stigeidae"                            "invalid" "family - species"
    # 7561  "Stolidobranchia"                      "valid"   "order - suborder"
    # 5989  "Stomatopoda"                          "valid"   "order - suborder"
    # 4523  "Strongylocentrotus"                   "valid"   "genus - species"
    # 4812  "Sylvilagus"                           "valid"   "genus - subgenus - species"
    # 9504  "Synalpheus"                           "valid"   "genus - species"
    # 10129 "Synchaeta"                            "valid"   "genus - species"
    # 7772  "Syngnthidae"                          "invalid" "family - subfamily"
    # 6386  "Syrphidae"                            "valid"   "family - species"
    # 8646  "Tachinidae"                           "valid"   "family - species - genus"
    # 5162  "Tachymenis peruviana"                 "valid"   "species - infraspecies"
    # 2257  "Taeniothrips"                         "valid"   "genus - subgenus"
    # 5078  "Tamias"                               "valid"   "genus - subgenus"
    # 387   "Tanytarsus"                           "valid"   "genus - species"
    # 6032  "Taraxacum"                            "valid"   "genus - species"
    # 693   "Tegula"                               "valid"   "genus - species"
    # 10013 "Tenbrionidae"                         "invalid" "family - subfamily"
    # 8494  "Tenthredinidae"                       "valid"   "family - species"
    # 543   "Terebra"                              "valid"   "genus - species"
    # 2674  "Tetraphyllidea"                       "valid"   "order - species"
    # 4665  "Thalassiosira"                        "valid"   "genus - species"
    # 4889  "Thomomys"                             "valid"   "genus - subgenus"
    # 6653  "Thunnus"                              "valid"   "genus - subgenus"
    # 6024  "Tipula"                               "valid"   "genus - subgenus"
    # 697   "Todiramphus sanctus vagans"           "valid"   "subspecies - infraspecies"
    # 9383  "Trachemys scripta"                    "valid"   "species - infraspecies"
    # 6004  "Tracheophyta"                         "valid"   "phylum - division"
    # 5803  "Trichoptera"                          "valid"   "order - species - genus"
    # 8367  "Trichostachys"                        "valid"   "genus - species"
    # 7023  "Triticum"                             "valid"   "genus - species"
    # 9004  "Trophon"                              "valid"   "genus - species"
    # 5871  "Tropisternus"                         "valid"   "genus - subgenus"
    # 7648  "Truncatella"                          "valid"   "genus - species"
    # 7886  "Turbellaria"                          "valid"   "subphylum - class"
    # 6364  "Tylosurus crocodilus crocodilus"      "valid"   "subspecies - species"
    # 5979  "Uca"                                  "valid"   "genus - species"
    # 9953  "Uta stansburiana"                     "valid"   "species - infraspecies"
    # 4595  "Vertebrata"                           "valid"   "subphylum - genus"
    # 7039  "Vespidae"                             "valid"   "family - genus - species"
    # 7026  "Vitis"                                "valid"   "genus - species"
    # 5963  "Xanthidae"                            "valid"   "family - species"
    # 7044  "Zea"                                  "valid"   "genus - species"
    # 140   "Zoanthus"                             "valid"   "genus - species"

# Adjusting taxonomic rank for species with multiple hits in the analyses
  species <- c("Allocentrella magnicornis", "Anaxilaus vesiculosus", "Apophallus lerouxi", "Arctocorisa germari", "Austrominius modestus", "Baetis muticus", "Bagous tubulus", "Baraeoptera roria", "Brillia bifida", "Carpelimus manchuricus", "Centroptilium rufostrigatum", "Cephaloplatus fasciatus", "Cerodontha lateralis", "Chorosoma schillingi", "Cydorus latus", "Cymbella kappii", "Diplodina artemisiae", "Diplodina sonchi", "Erebia claudina", "Glocianus pilosellus", "Gongrosira incrustans", "Haploparaksis crassirostris", "Haplothrips hukkineni", "Hemiuris communis", "Hesperocorixa castanea", "Himasthla interrupta", "Hyalites rahira", "Hyalites acerata", "Hydora nitida", "Hylina veliei", "Hyponigrus obsidianus", "Lasiommata paramegaera", "Leptocorisa oratorius", "Limothrips schmutzi", "Livoneca tenuistylis", "Longitarsus gracilis", "Melozone crissalis", "Metopoplax ditomoides", "Microsorium punctatum", "Mitoplinthus caliginosus", "Monocaulus parvula", "Ophelina syringopyge", "Oulema erichsoni", "Oulema septentrionis", "Oxyloma sarsii", "Paracentropristes pomospilus", "Paralichthyes albigutta", "Paroxyna absinthii", "Paroxyna misella", "Paroxyna producta", "Phalacrus corruscus", "Phoma arundinacea", "Phyllosticta holosteae", "Plautia affinis", "Potamopurgus antipodarum", "Psammothidium lauenburgianum", "Pseudostaurosira elliptica", "Pseudostyphlus pillumus", "Psiloscops flammeolus", "Rhabdospora coriacea", "Sarortherdon macrochir", "Scaphaloberis mucronata", "Secernentia nematodes", "Sigara dorsalis", "Sigaria semistraiata", "Spiroglyphus annulatus", "Stenothrips graminum", "Telicota bambusae", "Tenthredo neobesa", "Theridion impressum", "Tournotaris bimaculatus", "Trichopterna thorelli", "Vertigo moulinsiana", "Vidalia cornuta", "Zealolessica cheira", "Agkistrodon piscivorus", "Agrostis stolonifera", "Agrostis vinealis", "Alsophis portoricensis", "Ameiva exsul", "Anas carolinensis","Aphia minuta", "Aphis fabae", "Boiga dendrophila", "Brachycaudus helichrysi", "Capnia vidua", "Cassia fistula", "Caulerpa racemosa", "Centaurea scabiosa", "Chelonia mydas", "Chilomycterus antennatus", "Chrysanthemoides monilifera", "Chrysemys picta", "Clupea harengus", "Cnemidophorus gularis", "Cnemidophorus tesselatus", "Cnemidophorus tigris", "Coluber constrictor", "Coscinodiscus radiatus", "Crotalus viridis", "Crotaphytus collaris", "Cryptoblepharus virgatus", "Diadophis punctatus", "Dryomys nitedula", "Dysaphis lappae", "Elymus caninus", "Eretmochelys imbricata", "Ficus natalensis", "Hediste diversicolor", "Heterodon nasicus", "Homo sapiens","Lacerta agilis", "Lacerta media", "Lepidorhombus whiffiagonis", "Limacina antarctica", "Malus pumila", "Masticophis flagellum", "Masticophis lateralis", "Masticophis taeniatus", "Metopolophium dirhodum", "Nicholsina usta", "Norops lineatopus", "Orconectes virilis", "Patagonotothen guntheri", "Phyciodes campestris", "Pipistrellus subflavus", "Pituophis catenifer", "Pituophis melanoleucus", "Platybelone argalus", "Plecotus rafinesquii", "Plestiodon gilberti", "Pycnanthus angolensis", "Rhamnus purshiana", "Rhopalosiphum padi", "Sagina procumbens", "Sceloporus occidentalis", "Silene acaulis", "Silene latifolia", "Silene uniflora", "Solidago californica", "Staudtia gabonensis", "Stellaria alsine", "Stellaria nemorum", "Tachymenis peruviana", "Trachemys scripta", "Uta stansburiana", "Austraclima jollyae", "Threskiornis moluccus", "Spanioconnus wetterhali", "Pica pica", "Nysius vinitor", "Genaxinus bongraini", "Eysarcoris trimaculatus", "Dicronychus equisetioides","Austropotamobius berndhauseri", "Rhynchophanes mccownii", "Sitobion avenae", "Eriocephalus africanus", "Gomphonema intricatum")

  genus <- c("Amphicyrta","Candorbulina", "Coniontis", "Cryptolucilia", "Acipenser", "Agabus", "Agapetus", "Aglaophenia", "Agonum", "Aleochara", "Alpheus", "Amara", "Amaranthus","Amphidinium", "Anabaena", "Anolis", "Anthopleura", "Arion", "Asellus", "Asplenium", "Astropecten", "Avena", "Bathyraja", "Bosmina", "Brachinus", "Brachionus", "Brachycaudus", "Brachycentrus", "Calothrix", "Cambarus", "Camponotus", "Canthium", "Canthon", "Cercyon", "Chaetoceros", "Chassalia", "Chimarra", "Chironomus", "Chiroteuthis", "Chlaenius", "Cinygmula", "Cliona", "Contracaecum", "Conus", "Corycaeus", "Corynosoma", "Crepidula", "Cricotopus", "Croton", "Cucumaria", "Culicoides", "Cyanea", "Cyclotella", "Cytheropteron", "Daphnia", "Dasyatis", "Diadema", "Dipogon", "Dryops", "Echinorhynchus", "Enoploteuthis", "Ephemerella", "Eubosmina", "Euglena", "Euphausia", "Euphorbia", "Eurytoma", "Fabricia", "Ficus", "Galathea", "Garcinia", "Geotrupes", "Gerris", "Glossosoma", "Glycinde", "Gonatopsis", "Goniada", "Gracilaria", "Gymnodinium", "Gyraulus", "Halichondria", "Haplothrips", "Harpalus", "Helophorus", "Hippolyte", "Holothuria", "Hyalella", "Hydraena", "Hydrophilus", "Hydropsyche", "Hylaeus", "Hymeniacidon", "Hypericum", "Hyporhamphus", "Jaera", "Lampetra", "Lepidostoma", "Leptochelia", "Leptodactylus", "Lepus", "Limnophila", "Liolaemus", "Loligo", "Lottia", "Lycosa", "Macrobrachium", "Marmota", "Mastocarpus", "Mazzaella", "Melanoplus", "Meligethes", "Micrasema", "Microphallus", "Microtus", "Millepora", "Molanna", "Mus", "Myotis", "Mytilus", "Nais", "Nassarius", "Nematocarcinus", "Nitzschia", "Nostoc", "Notonecta", "Nymphaea", "Octopus", "Oecetis", "Oikopleura", "Oligochaeta", "Ophelina", "Orchomene", "Orchomenella", "Orthocladius", "Oryza", "Oscillatoria", "Ovis", "Oxyethira", "Pachygnatha", "Pacifastacus", "Palaemon", "Paracalanus", "Parazoanthus", "Parthenope", "Periclimenes", "Perinereis", "Peromyscus", "Peyssonnelia", "Phascolion", "Phascolosoma", "Phomopsis", "Phyllanthus", "Phyllophaga", "Physa", "Pinus", "Piper", "Pipistrellus", "Plasmodium", "Platycheirus", "Poecilus", "Polistes", "Polypedilum", "Polyphylla", "Prionus", "Prochlorococcus", "Prorocentrum", "Prosimulium", "Prunus", "Pseudocuma", "Psychotria", "Pterostichus", "Quercus", "Rana", "Rhagovelia", "Rhopalodia", "Rubus", "Salacia", "Salicornia", "Salmo", "Sauria", "Scarabaeus", "Scarus", "Scenedesmus", "Sciurus", "Scopula", "Scrippsiella", "Sebastes", "Sepia", "Sepiola", "Serranus", "Sigara", "Simulium", "Sorex", "Spatangus", "Spirulina", "Staurastrum", "Stenopelmatus", "Strongylocentrotus", "Sylvilagus", "Synalpheus", "Synchaeta", "Taeniothrips", "Tamias", "Tanytarsus", "Taraxacum", "Tegula", "Terebra", "Thalassiosira", "Thomomys", "Thunnus", "Tipula", "Trichostachys", "Triticum", "Trophon", "Tropisternus", "Truncatella", "Uca", "Vitis", "Zea", "Zoanthus", "Scolex", "Nemertites", "Melampias", "Medusa", "Amphora", "Lymnaea")

  suborder <- c("Amphisbaenia", "Anisoptera", "Echiurida", "Hydrachnidia")
  superorder <- c("Batoidea")
  order <- c("Anura", "Calanoida", "Coleoptera", "Collembola", "Diptera", "Hemiptera", "Hymenoptera", "Lepidoptera", "Mysidacea", "Neogastropoda", "Neoloricata", "Odonata", "Oribatida", "Patellogastropoda", "Phlebobranchia", "Plecoptera", "Poecilostomatoida", "Serpentes", "Siphonophora", "Sternorrhyncha", "Stolidobranchia", "Stomatopoda", "Tetraphyllidea", "Trichoptera")
  infraorder <- c("Corophiida", "Chilophiurina", "Hystricognathi","Echinidea")
  phylum <- c("Acanthocephala", "Annelida", "Bryophyta", "Chlorophyta", "Ciliophora", "Ctenophora", "Heliozoa", "Nemertea", "Priapulida", "Radiolaria", "Sipuncula", "Tracheophyta")
  infraphylum <- c("Gnathostomata")
  subphylum <- c("Anthozoa", "Vertebrata", "Khakista")
  subfamily <- c("Deltocephalinae", "Eucoilinae", "Orthocladiinae", "Pilumninae", "Euclymeninae")
  family <- c("Acaridae","Achilidae", "Alcyoniidae", "Aphididae", "Apidae", "Blattidae", "Braconidae", "Cercopidae", "Chironomidae", "Chloropidae", "Chrysopidae", "Cicadidae", "Delphacidae", "Diaptomidae", "Fellodistomidae", "Galatheidae", "Hydrobatidae", "Hymenolepididae", "Ichneumonidae", "Leptophlebiidae", "Lycosidae", "Mantidae", "Miridae", "Noctuidae", "Octopodidae", "Opecoelidae", "Panopeidae", "Phryganeidae", "Phycidae", "Picornaviridae", "Porcellanidae", "Pteromalidae", "Renicolidae", "Salpidae", "Sarcophagidae", "Serpulidae", "Staphylinidae", "Staphylininae", "Strigeidae", "Syrphidae", "Tachinidae", "Tenthredinidae", "Vespidae", "Xanthidae", "Ophichthyidae", "Locustidae", "Delphacinae")
  infraclass <- c("Canalipalpata")
  class <- c("Actinopterygii", "Appendicularia", "Arachnida", "Chlorophyceae", "Chondrichthyes", "Chrysophyceae", "Clitellata", "Dinophyceae", "Echiura", "Monogononta", "Phaeophyceae", "Polychaeta", "Turbellaria")
  subclass <- c("Digenea", "Elasmobranchii", "Hirudinea", "Pterygota", "Dromopoda")
  kingdom <- c("Bacteria")
  no.tx <- c("Magnoliophyta", "Northern aplomado", "Northern", "Nestus mendicus", "Hyalites acerata", "Diastillidae", "Trichoton", "Testate amoeba", "Taccazea apiculata", "Marine detritus", "Organic", "Bovine or", "Fungal spores", "Mucus", "Stones")
  tribe <- c("Pentaneurini")
  superfamily <- c("Doridoidea")

  rownames(tx.valid) <- tx.valid[,1]

  tx.valid[paste(species),3] <- "species"
  tx.valid[paste(genus),3] <- "genus"
  tx.valid[paste(tribe),3] <- "tribe"
  tx.valid[paste(family),3] <- "family"
  tx.valid[paste(subfamily),3] <- "subfamily"
  tx.valid[paste(superfamily),3] <- "superfamily"
  tx.valid[paste(class),3] <- "class"
  tx.valid[paste(subclass),3] <- "subclass"
  tx.valid[paste(infraclass),3] <- "infraclass"
  tx.valid[paste(infraphylum),3] <- "infraphylum"
  tx.valid[paste(phylum),3] <- "phylum"
  tx.valid[paste(subphylum),3] <- "subphylum"
  tx.valid[paste(infraorder),3] <- "infraorder"
  tx.valid[paste(order),3] <- "order"
  tx.valid[paste(suborder),3] <- "suborder"
  tx.valid[paste(superorder),3] <- "superorder"
  tx.valid[paste(kingdom),3] <- "kingdom"
  tx.valid[paste(no.tx),3] <- NA

#----------------------------------------------------------------------------------------------------------
#   Extract binary interactions
#----------------------------------------------------------------------------------------------------------
GloBI[[1]][[4]] <- matrix(nrow = nrow(GloBI[[1]][[1]]), ncol = 3, data = NA)
colnames(GloBI[[1]][[4]]) <- c("Predator","FeedInter","Prey")
GloBI[[1]][[4]][,1] <- GloBI[[1]][[1]]$source_taxon_name
GloBI[[1]][[4]][,2] <- 1
GloBI[[1]][[4]][,3] <- GloBI[[1]][[1]]$target_taxon_name

#----------------------------------------------------------------------------------------------------------
#   Extract taxon list
#----------------------------------------------------------------------------------------------------------
  GloBI[[1]][[5]] <- tx.valid[, c(1,3)]
  colnames(GloBI[[1]][[5]]) <- c("taxon", "rank")
  rownames(GloBI[[1]][[5]]) <- seq(1, nrow(GloBI[[1]][[5]]))
  GloBI[[1]][[5]] <- as.data.frame(GloBI[[1]][[5]])


  # save.image(file="GloBI_in_case.R")
save(x=GloBI,file="RData/GloBI.RData")
