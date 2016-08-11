Names_change_size <- function(x) {
  x <- paste(x," ")
  x <- tolower(x)
  x <- gsub("sm "," ",x)
  x <- gsub("lg "," ",x)
  x <- gsub("md "," ",x)
  x <- gsub("medium "," ",x)
  x <- gsub("large "," ",x)
  x <- gsub("small "," ",x)
  # x <- gsub("","",x)
  # x <- gsub("","",x)
  # x <- gsub("","",x)
  # x <- gsub("","",x)
  # x <- gsub("","",x)
  # x <- gsub("","",x)
  # x <- gsub("","",x)
  # x <- gsub("","",x)
  # x <- gsub("","",x)
  # x <- gsub("","",x)
  # x <- gsub("","",x)
  # x <- gsub("","",x)
  # x <- gsub("","",x)
  x <- str_trim(x, side="both") #remove spaces

}

Names_change_misc <- function(x){
  x <- paste(x," ")
  x <- tolower(x)
  x <- gsub("misc","",x)
  # x <- gsub("etc ","",x)
  # x <- gsub("","",x)
  # x <- gsub("","",x)
  # x <- gsub("","",x)
  # x <- gsub("","",x)
  # x <- gsub("","",x)


}
