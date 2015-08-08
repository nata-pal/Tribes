loadFunctions <- function(){
  files <- list.files(path = "R/", full.names = TRUE, pattern = ".*R", recursive = TRUE, include.dirs = FALSE)
  for(file in files){
    source(file)
  }
}

VERBOSE  <<- TRUE
loadFunctions()


# ***GOOGLE***
# Creating directory in /data directory with text information for topics listed in /data/topics.txt
# Function requires specifing classes for downloaded articles
# prepareAll()
# classify(dtm, classes)

#Downloading data
download <- function(){
  topics <- getTopics("data/topics.txt")
  for (topic in topics){
    downloadWithGBlogSearchAPI(topic)
  }
  alarm()
  m <<- merge_corpuses()
  p <<- cleanDocs2(m)
  
}

label <- function(){
  c <- setClasses(meta(docs, "link", "local"))
  saveClassified(docs, c, meta(docs, "topic", "local")[[1]])
}