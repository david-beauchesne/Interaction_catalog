# Removing objects while keeping functions
remove_objects_keep_functions <- function() {

  ENV <- globalenv()
  object_list <- ls(envir = ENV)

  which_function_to_keep <- numeric()
  for(i in 1:length(functions_to_keep)) {
    which_function_to_keep <- c(which_function_to_keep, which(object_list == functions_to_keep[i]))

  }

  rm(list = object_list[-which_function_to_keep], envir = ENV)

}
