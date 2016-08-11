#Extraction des identifiants
extract_id <- function(tx.list) {

  tx.id <- as.data.frame(matrix(nrow = nb.tx, ncol = 3, data = 0, dimnames = list(c(), c("TSN", "EoL", "GBIF"))))

  pb <- txtProgressBar(min = 1, max = nb.tx, style = 3)
  options(eolApiKey="49e7bc01917e30ca4b5bc1db0fa52cdbc2dc9afc") #identifiant EoL requis pour requêtes
  oldw <- getOption("warn")
  options(warn = -1)
  time.init <- Sys.time()

  for(j in 1:nb.tx){
    tsn <- try(get_tsn_(tx.list.tot[j,1], verbose=FALSE, accepted=TRUE,rows=1))
    eol <- try(get_eolid_(tx.list.tot[j,1], verbose=FALSE, rows=1)) #EoL ID
    gbif <- try(get_gbifid_(tx.list.tot[j,1], verbose=FALSE, rows=1)) #GBIF ID
    tx.id$TSN[j] <- try(if(is.null(tsn[[1]]) == TRUE) {NA} else {tsn[[1]]$tsn[1]})
    tx.id$EoL[j] <- try(if(is.null(eol[[1]]) == TRUE) {NA} else {eol[[1]]$eolid[1]})
    tx.id$GBIF[j] <- try(if(is.null(gbif[[1]]) == TRUE) {NA} else {gbif[[1]]$key[1]})
    remove(tsn,eol,gbif)
    setTxtProgressBar(pb, j)
  }

  print(Sys.time() - time.init)
  close(pb)
  options(warn = oldw)

  tx.id$TSN[which(tx.id$TSN == "Error in eol[[1]]$eolid : $ operator is invalid for atomic vectors\n")] <- NA
  tx.id$EoL[which(tx.id$EoL == "Error in eol[[1]]$eolid : $ operator is invalid for atomic vectors\n")] <- NA
  tx.id$GBIF[which(tx.id$GBIF == "Error in eol[[1]]$eolid : $ operator is invalid for atomic vectors\n")] <- NA

  return(tx.id)
}
