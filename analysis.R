loadFunctions <- function(){
  files <- list.files(path = "R/", full.names = TRUE, pattern = ".*R", recursive = TRUE, include.dirs = FALSE)
  for(file in files){
    source(file)
  }
}

VERBOSE  <<- TRUE
loadFunctions()

# Creating directory in /data directory with text information for topics listed in /data/topics.txt
# Function requires specifing classes for downloaded articles
# prepareAll()


classify(dtm, classes)
