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

loadAllLibs <- function(){
  requiredPackages <- c("tm", "stringi", "proxy", "SnowballC", "RCurl", "XML", "qdap", "RTextTools", "tm.plugin.webmining")
  getLibs(requiredPackages)
}

setClasses <- function(links){
  classes <- c()
  i <- 1
  for (link in links){
    if (stri_startswith_fixed(link, "/")) {
      link <- stri_paste("http://www.google.pl", link)
    }
    browseURL(link)
    print(stri_paste(i, " Wprowadź klasę decyzyjną dla strony ", link, "\n"))
    print("[F] - 'for' artykuł opowiada się za tematem")
    print("[A] - 'against' artykuł opowiada się przeciw tematowi")
    print("[N] - 'neither' artykuł zachowuje neutralny charakter")
    print("[q] - 'quit' anuluj")
    
    class <- readline(prompt = "class: ")
    classes <- c(classes, class)
    if(stri_cmp_eq(class, "q")){
      cat("Function canceled")
      return()
    }
    
    i <- i+1
    
    
  }

  cat("\nAll classes set: \n")
  print(classes)
  
  unlist(classes, recursive=FALSE)
}

fixAllIds <- function(){
  filepattern <- "*.Rdata"
  dirpath <- "data/corpuses/"
  corpuses <- dir(dirpath, pattern = filepattern, recursive = FALSE)
  if(length(corpuses)>0){
    for(file in corpuses){
      objName <- load(file = stri_paste(dirpath, file))
      corp <- get(objName)
      corp <- tm_map(corp, function(x){
        id <- meta(x, "id", "local")
        id <- stri_replace_all_fixed(str = id, pattern = " ", replacement = "_")
        meta(x, "id", "local") <- id
        x
      })
      save(corp, file=stri_paste(dirpath, file))
    }
  }
}

merge_corpuses <- function(){
  #   filepattern <- "^[A-Za-z _]*_corpus.Rdata"
  filepattern <- "^[A-Za-z _]*_labeled.Rdata"
  dirpath <- "data/corpuses/"
  corpuses <- dir(dirpath, pattern = filepattern)
  cat(length(corpuses))
  if(length(corpuses)>0){
    objName <- load(file = stri_paste(dirpath, corpuses[1]))
    merged <- get(objName)
    corpuses <- corpuses[2:length(corpuses)]
    for(file in corpuses){
      objName <- load(file = stri_paste(dirpath, file))
      merged <- c(get(objName), merged)
    }
    save(merged, file="data/corpuses/_merged_corpus.Rdata")
    merged
  }
}

saveClassified <- function(corpus, classVector, name){
  meta(corpus, tag = "class", type = "local") <- classVector
  save(corpus, file=stri_paste("data/corpuses/", name, "_labeled.Rdata"))
  corpus
}

getTopics <- function(filepath, VERBOSE = FALSE){
  # Returns vector of topics loaded from given filepath
  # The file should contain one topic in one line
  # Lines in the file starting with "-" are not included
  getLibs("stringi")
  topics  <- scan(filepath, blank.lines.skip = TRUE, sep="\n", what = "character", quiet = TRUE)
  remove <- c()
  
  for (topic in topics){
    if (stri_startswith_fixed(stri_trim(topic), "-")){
      i <- which(topics %in% topic)
      remove <- c(remove, i)
    }
  }
  
  topics <- topics[-remove]
  
  if (VERBOSE) {
    cat("The topics are: ")
    cat(topics, sep = ", ")
  }
  
  topics
}
