# Loads libraries if they have not been loaded yet
# Throws error and displays info if one of libs is not installed
getLibs  <- function(names){  
  for (name in names){
    if (!require(name, quietly = TRUE, character.only = TRUE)) {
      stop(paste(name, " package needed for this function to work. Please install it."),
           call = FALSE)
    }
  } 
}

requiredPackages <<- c("tm", "stringi", "proxy", "SnowballC", "RCurl", "XML", "qdap")
