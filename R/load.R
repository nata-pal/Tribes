# Flag determining whether processing infos should be printed out
VERBOSE <<- FALSE

# Returns corpora of wikipedia articles about given topics
loadTopicsFromWiki <- function(topics, lang){
  getLibs(c("tm", "stringi", "proxy"))
  
  wiki <- stri_paste("http://", lang, ".wikipedia.org/wiki/")
  articles <- character(length(topics))
  
  for (i in 1:length(topics)) {
    articles[i] <- stri_flatten(readLines(stri_paste(wiki, topics[i])), col = " ")
  }
  
  docs <- Corpus(VectorSource(articles))
}

# Returns vector of topics loaded from given filepath
# The file should contain one topic in one line
# Lines in the file starting with "-" are not included
getTopics <- function(filepath){
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

# Returns vector of corporas containing n docs each about given topics
loadTopicsFromGoogle <- function(topics, n){
  corpArray <- list();
  print(corpArray)
    for (topic in topics) {
#     corpArray <- c(corpArray, getTopicFromGoogle(topic, n))
      corp <- getTopicFromGoogle(topic, n)
      cat(typeof(corp))
      
      corpArray[[topic]] <- corp
    }
  corpArray
}

 
getTopicFromGoogle  <- function(topic, n){
  getLibs(c("tm", "stringi", "RCurl", "XML"))
  
  site <- getForm("http://www.google.com/search", q=stri_paste("~",topic), gws_rd="cr", num=n, pws="0", gfe_rd="cr")
  doc  <- htmlParse(site, asText = TRUE)
  saveXML(doc, "output/out.html", indent = TRUE)
    
  links  <- xpathSApply(doc, "//li[@class='g']//h3[@class='r']/a", xmlGetAttr, "href")
  
  if(VERBOSE){
    cat("\n\n================================\nRESULTS:\n\n")
    print(links)
  }
  
  articles <- character(n)
  titles <- character(n)
  
  for (i in 1:n) {  
#      articles[i] <- stri_flatten(scan(stri_paste("http://www.google.pl",links[[i]]), what = 'character', allowEscapes = FALSE,  encoding = "UTF-8"), col = " ")
    articles[i] <- stri_flatten(readLines(stri_paste("http://www.google.pl",links[[i]]), warn=FALSE, encoding = "UTF-8"), col = " ")
    titles[i] <- stri_extract_first_regex(str = articles[[i]], pattern = "(?<=<title).*?(?=[^<>]*</title>)")
    cat(titles[i])
    cat("\n")
  }

  
   
  docs <- Corpus(VectorSource(articles))
  meta(docs, type = "local", tag = "id") <- links
  meta(docs, type = "corpus", tag = "topic") <- topic
  docs
}

