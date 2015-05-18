

loadTopicsFromWiki <- function(topics, lang){
  GetLibs(c("tm", "stringi", "proxy"))
  
  wiki <- stri_paste("http://", lang, ".wikipedia.org/wiki/")
  articles <- character(length(topics))
  
  for (i in 1:length(topics)) {
    articles[i] <- stri_flatten(readLines(stri_paste(wiki, topics[i])), col = " ")
  }
  
  docs <- Corpus(VectorSource(articles))
}


getTopics <- function(filepath){
  topics  <- scan(filepath,blank.lines.skip = TRUE, sep="\n", what = "character")
  print(topics)
}

loadTopicsFromGoogle <- function(topics, n){
  for(topic in topics){
    getTopicFromGoogle(topic, n)
  }
}


getTopicFromGoogle  <- function(topic, n){
  GetLibs(c("tm", "stringi", "RCurl", "XML"))
  
  site <- getForm("http://www.google.com/search", q=stri_paste("~",topic), gws_rd="cr", num=n, pws="0", gfe_rd="cr")
  doc  <- htmlParse(site, asText = TRUE)
  saveXML(doc, "output/out.html", indent = TRUE)
  #     print(htmlTreeParse(site))
    
  links  <- xpathSApply(doc, "//li[@class='g']//h3[@class='r']/a", xmlGetAttr, "href")
  
  cat("\n\n================================\nRESULTS:\n\n")
  print(links)
  
  articles <- character(n)
  
  for (i in 1:n) {  
    articles[i] <- stri_flatten(readLines(stri_paste("http://www.google.pl",links[[i]]), warn=FALSE))
  }
  
  docs <- Corpus(VectorSource(articles))
}


load