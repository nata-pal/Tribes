
# Loads R objects reffering given topic to Global Environment
loadTopic <- function(topic){
  getLibs("stringi")
  path <- stri_paste("data/", topic, "/")
  files <- list.files(path = path, full.names = TRUE, pattern = ".*Rdata", recursive = TRUE, include.dirs = FALSE)
  for(file in files){
    load(file, env = .GlobalEnv)
  }
}

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
loadTopicsFromGoogleNews <- function(topics, n){
  corpArray <- list();
  print(corpArray)
  for (topic in topics) {
    #     corpArray <- c(corpArray, getTopicFromGoogle(topic, n))
    corp <- getTopicFromGoogleNews(topic, n)
    cat(typeof(corp))
    
    corpArray[[topic]] <- corp
  }
  corpArray
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

getTopicFromGoogleNews  <- function(topic, n){
  getLibs(c("tm", "stringi", "RCurl", "XML"))
  
  site <- getForm("http://news.google.com/news/section?", q=stri_paste("~",topic), gws_rd="cr", num=n, pws="0", gfe_rd="cr")
  doc  <- htmlParse(site, asText = TRUE)
  saveXML(doc, "output/out.html", indent = TRUE)
  
  
  links  <- xpathSApply(doc, "//h2[@class='esc-lead-article-title']/a", xmlGetAttr, "href")
  
  if(VERBOSE){
    cat("\n\n================================\nRESULTS:\n\n")
    print(links)
  }
  
  articles <- character(n)
  titles <- character(n)
  
  for (i in 1:n) {  
    articles[i] <- stri_flatten(readLines(stri_paste(links[[i]]), warn=FALSE, encoding = "UTF-8"), col = " ")
    
    hm <- gregexpr("(.*?)(?=</head>)", articles[i], perl=TRUE)
    h <- as.character(regmatches(articles[i], hm))
    sm <- gregexpr("(?<=<title)(.*?)(?=</title>)", h, perl=TRUE)
    s <- as.character(regmatches(h, sm))
    if(s == "character(0)"){
      s <- stri_paste(topic, " ", i)
    }
    titles[i] <- stri_replace_all_regex(s, "", pattern = "> *")
  }
  
  docs <- Corpus(VectorSource(articles))
  meta(docs, type = "local", tag = "link") <- links
  meta(docs, type = "local", tag = "title") <- titles
  meta(docs, type = "corpus", tag = "topic") <- topic
  docs
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
    articles[i] <- stri_flatten(readLines(stri_paste("http://www.google.pl",links[[i]]), warn=FALSE, encoding = "UTF-8"), col = " ")
    
    hm <- gregexpr("(.*?)(?=</head>)", articles[i], perl=TRUE)
    h <- as.character(regmatches(articles[i], hm))
    sm <- gregexpr("(?<=<title)(.*?)(?=</title>)", h, perl=TRUE)
    s <- as.character(regmatches(h, sm))
    if(s == "character(0)"){
      s <- stri_paste(topic, " ", i)
    }
    titles[i] <- stri_replace_all_regex(s, "", pattern = "> *")
  }
  
  docs <- Corpus(VectorSource(articles))
  meta(docs, type = "local", tag = "link") <- links
  meta(docs, type = "local", tag = "title") <- titles
  meta(docs, type = "corpus", tag = "topic") <- topic
  docs
}





