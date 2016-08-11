taxo_resolve <- function(x) { # x = list of taxon tx.list
  library(taxize)
  options(eolApiKey="49e7bc01917e30ca4b5bc1db0fa52cdbc2dc9afc") #identifiant EoL requis pour requêtes
  db <- c("Catalogue of Life","ITIS","NCBI","WoRMS","EOL","OBIS","FishBase") #datasources to use
  datasrc <- gnr_datasources()
  db.gnr <- NA
  for(i in 1:length(db)){db.gnr <- c(db.gnr,datasrc[datasrc$title == db[i], "id"])}
  db.gnr <- db.gnr[-1]
  tx.lg <- length(x)
  tx.res <- vector("list",tx.lg)
  pb <- txtProgressBar(min = 0,max = tx.lg, style = 3)
  for(i in 1:tx.lg){
    tx.res[[i]] <- gnr_resolve(x[i],data_source_ids=db.gnr,canonical = TRUE,fields="all")
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(tx.res)
}
