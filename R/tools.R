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
  requiredPackages <- c("tm", "stringi", "proxy", "SnowballC", "RCurl", "XML", "qdap", "RTextTools")
  getLibs(requiredPackages)
}

setClasses <- function(links){
  classes <- c()
  for (link in links){
    if (stri_startswith_fixed(link, "/")) {
      link <- stri_paste("http://www.google.pl", link)
    }
    browseURL(link)
    print(stri_paste("Wprowadź klasę decyzyjną dla strony ", link, "\n"))
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
    
  }

  cat("\nAll classes set: \n")
  print(classes)
  classes
}


setDFClasses <- function(dataframe){
  classes <- c()
  for (i in 1:nrow(x = dataframe)){
    if (stri_startswith_fixed(dataframe[i,]$metalink, "/")) {
      url <- stri_paste("http://www.google.pl", dataframe[i,]$metalink)
    } else {
      url <- dataframe[i,]$metalink
    }
    browseURL(url)
    cat(stri_paste("Wprowadź klasę decyzyjną dla strony ", url, "\n"))
    classes[[i]] <- readline(prompt = "class: ")
    if(classes[[i]]=="q"){
      print(classes[[i]])
      return
    }
  }
  dataframe$class <- classes
  dataframe
}

setCorpClasses <- function(corp){
  classes <- c()
  for (i in 1:length(corp)){
    if (stri_startswith_fixed(corp[[i]]$meta$link, "/")) {
      url <- stri_paste("http://www.google.pl", corp[[i]]$meta$link)
    } else {
      url <- corp[[i]]$meta$link
    }
    browseURL(url)
    cat(stri_paste("Wprowadź klasę decyzyjną dla strony ", url, "\n"))
    classes[[i]] <- readline(prompt = "class: ")
    if(classes[[i]]=="q"){
      print(class[[i]])
      return
    }
  }
  classes
}

saveAllCorps <- function(cv){
  for (i in 1:length(cv)){
    
  }
}
