# Run readme.r before other scripts
rm(list=ls())
setwd("/Users/davidbeauchesne/Dropbox/PhD/PhD_obj2/Structure_Comm_EGSL/Interaction_catalog")
# -----------------------------------------------------------------------------
# PROJECT:
#    Evaluating the structure of the communities of the estuary
#    and gulf of St.Lawrence
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# REPOSITORY
#   The first step of this project is to compile a catalog of interactions
#   from empirical data
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# PROCESS STEPS:
#   1. Species list for estuary and gulf of St. Lawrence
#       Script  <- file = "Script/1_EGSL_SpList.r"
#       RData <- file = "RData/sp_egsl.RData" - EGSL species list
#
#   2. Import and format empirical food web data
#
#     2.1 Barnes et al. 2008
#       Script  <- file = "Script/2-1_Emp_Webs.r"
#       RData <- file = "RData/barnes2008.RData"
#
#     2.2 Kortsch et al. 2015
#       Script  <- file = "Script/2-2_Emp_Webs.r"
#       RData <- file = "RData/Kortsch2015.RData"
#
#     2.3 GlobalWeb data http://globalwebdb.com
#       Script  <- file = "Script/2-3_Emp_Web.r"
#       RData <- file = "RData/GlobalWeb.RData"
#
#     2.4 Brose et al. 2005
#       Script  <- file = "Script/2-4_Emp_Webs.r"
#       RData <- file = "RData/brose2005.RData"
#
#     2.5 Ecopath with Ecosim models *** !!!!! Still needs to be done
#       Script  <- file = "Script/2-5_Emp_Webs.r"
#       RData <- file = "RData/EwE.RData"
#
#   3. List of binary interations to include in the analysis
#     Script <- file = "Script/3-Bin_inter_tot.r"
#     RData1 <- file = "RData/interactions.RData"
#     RData2 <- file = "RData/taxon_list.RData"
#
#   4. Classification of taxon from empirical webs and St. Lawrence species
#     Script <- file = "Script/4-Classification_Emp-Webs.r"
#     RData <- file = "RData/class_tx_tot.RData"
#
#   5. Global Biotic Interactions (GloBI)
#       http://www.globalbioticinteractions.org
#       Script <- file = "Script/5-GloBI.r"
#       RData <- file = "RData/GloBI.RData"
#
#   6. List of binary interactions from GloBI
#     Script <- file = "Script/6-Bin_inter_GloBI.r"
#     RData1 <- file = "RData/GloBI_interactions.RData"
#     RData2 <- file = "RData/GloBI_taxon.RData"
#
#   7. Classification of taxon from GloBI
#     Script <- file = "Script/7-Classification_GloBI.r"
#     RData <- file = "RData/GloBI_classification.RData"
#
#   8. Final list of taxon and interations from EmpWeb & GloBI *** Final DB
#     Script <- file = "Script/8-Taxon_final.r"
#     RData <- file = "RData/Interaction_catalog.RData"
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# FUNCTIONS (add a description of the functions eventually)
source("Script/bin_inter.R")
source("Script/diet_mat_extend.R")
source("Script/dup_multi_tx.R")
source("Script/duplicate_row_col.R")
source("Script/locate_string.R")
source("Script/tax_rank.R")
source("Script/taxo_resolve.R")
source("Script/taxo_valid.R")
source("Script/tx_valid_res.R")
source("Script/binary_interaction.R")
source("Script/inter_taxo_resolution.R")
source("Script/taxon_resolve_for_classification.R")
source("Script/extract_id.R")
source("Script/class_taxo.R")
source("Script/GloBI_trophic_inter.R")
source("Script/GloBI_taxon_classification.R")
functions_to_keep <- ls()
functions_to_keep <- ls()
# -----------------------------------------------------------------------------
