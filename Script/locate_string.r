locate <- function(x,y) {
subset(x,str_locate(x,y)[,1] > 0)
}

locate2 <- function(x,y) {
  x[which(x == y),]
}


# # To search in all the food webs - used for GlobalWeb  fw
# n <-character()
# m <- numeric()
# for(k in 1:length(GlobalWeb)){
#   n <- c(n,unique(colnames(GlobalWeb[[k]][[1]])))
#   m <- c(m,rep(names(GlobalWeb[[k]])[1],length(unique(colnames(GlobalWeb[[k]][[1]])))))
# }
# mat <- cbind(n,m)
#
# n <-character()
# for(k in 1:length(tx.list)){
#   n <- c(n,unique(tx.list))
# }
# mat <- n
