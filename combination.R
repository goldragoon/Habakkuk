combination.named <- function(...){
  # Support named list only, all parameters should be same type
  args <- list(...)
  args_names <- lapply(args, names)
  names_ <- do.call(expand.grid, list(args_names))
  values_ <- do.call(expand.grid, args)
  names_ <- apply(names_, 1, unlist)
  values_ <- apply(values_, 1, unlist)
  
  combinations <- list()
  for(index in seq(1, NCOL(names_), 1)){
      print(values_)
      print(index)
    temp <- c(values_[, index])

    names(temp) <- c(names_[, index])
    
    combinations[[index]] <- as.list(temp)
  }
  return(combinations)
}

combination.unnamed <- function(...){
  # Support unnamed vector and list, all parameters should be same type
  args <- list(...)
  args_names <- lapply(args, names)
  values_ <- do.call(expand.grid, args)
  values_ <- unname(apply(values_,1, unlist))
  return(values_)
}

############ combination.named examples ################
#cc <- combination.named(list(k1=1, k2=2), list(k3=3, k4=4), list(k5=5, k6=6))

############ combination.unnamed examples ##############
#cc <- combination.unnamed(list(1, 2), list(3, 4), list(5, 6))
#cc <- combination.unnamed(c(1, 2), c(3, 4), c(5, 6))
