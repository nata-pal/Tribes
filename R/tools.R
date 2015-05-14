GetLibs  <- function(names){  
  for (name in names){
    if (!require(name, quietly = TRUE, character.only = TRUE)) {
      stop(paste(name, " package needed for this function to work. Please install it."),
           call. = FALSE)
    }
  } 
}

