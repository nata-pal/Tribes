downloadArticles <- function(topic){
#   getLibs("curl")
  getLibs(c("tm", "stringi", "RCurl", "XML", "httr"))
  n <- 15
  set_config( config( ssl.verifypeer = 0L ) )
  
  site <- getForm("http://www.google.com/search", 
                  q=stri_paste("~",topic), 
                  safe="active",
                  as_sitesearch="wordpress.com", 
                  as_qdr="all", 
                  lr="lang_en", 
                  hl="en", 
                  gl="en", 
                  gws_rd="cr", 
                  num=n, 
                  pws="0", 
                  gfe_rd="cr"
                  )
  print("Google search result ready")
  doc  <- htmlParse(site, asText = TRUE)
#   print(doc)
  saveXML(doc, "output/out.html", indent = TRUE)
  
  links  <<- xpathSApply(doc, "//li[@class='g']//h3[@class='r']/a", xmlGetAttr, "href")
  print("Links ready")
  
#   if(VERBOSE){
#     cat("\n\n================================\nRESULTS:\n\n")
#     print(links)
#   }
  
  articles <- c(n)
  topics <- character(n)
  ids <- character(n)
  sitesNotFound  <- c()

  for (i in 1:n) {  
    link <- links[i]
    cat(stri_paste("Processing ", link, "\n\n"))
    site <- tryCatch(stri_flatten(readLines(stri_paste("http://www.google.pl",link), warn=FALSE, encoding = "UTF-8"), col = " "), error = function(e) "Not found")
#     site <- tryCatch(GET(stri_paste("http://www.google.pl",link)), error = function(e) "Not found")


#     site <- getURL(stri_paste("http://www.google.pl",link), ssl.verifypeer = FALSE, .opts = curlOptions(
#       cookiejar="",  useragent = "Mozilla/5.0", followlocation = TRUE
#     ))
    if(stri_cmp_eq(site, "Not found")){
      sitesNotFound <- c(sitesNotFound, i)
    }

    articles[i] <- site
    topics[i] <- topic
    ids[i] <- stri_paste(topic, formatC(i, width = 2, format=, flag="0") )
  }
  if(length(sitesNotFound)){
    articles <- articles[-sitesNotFound]
    links <- links[-sitesNotFound]
    topics <- topics[-sitesNotFound]
    ids <- ids[-sitesNotFound]
  }
  
  docs <- Corpus(VectorSource(articles))
  meta(docs, type = "local", tag = "link") <- links
  meta(docs, type = "local", tag = "topic") <- topics
  meta(docs, type = "local", tag = "id") <- ids

  docs <<- docs

  writeCorpus(x = docs, path = "data/articles")
  save(docs, file=stri_paste("data/corpuses/", topic, "_corpus.Rdata"))
}

merge_corpuses <- function(){
#   filepattern <- "^[A-Za-z ]*_corpus.Rdata"
  filepattern <- "^[A-Za-z ]*_labeled.Rdata"
  dirpath <- "data/corpuses/"
  corpuses <- dir(dirpath, pattern = filepattern)
  if(length(corpuses)>0){
    load(file = stri_paste(dirpath, corpuses[1]))
    merged <- docs
    corpuses <- corpuses[2:length(corpuses)]
    for(file in corpuses){
      load(file = stri_paste(dirpath, file))
      merged <- c(docs, merged)
    }
    save(merged, file="data/corpuses/_merged_corpus.Rdata")
    merged
  }
}

saveClassified <- function(corpus, classVector, name){
  meta(corpus, tag = "class", type = "local") <- classVector
  save(docs, file=stri_paste("data/corpuses/", name, "_labeled.Rdata"))
  corpus
}












test <- function(){
  testCurlSpeed()
  cat("\n")
  testRCurlSpeed()  
}

testCurlSpeed <- function(){
  getLibs("curl")
  start <- Sys.time()
  site <<- curl_fetch_memory("http://www.google.com/search?q=abortion+site:wordpress.com&as_sitesearch=wordpress.com&gws_rd=cr&num=30&pws=0&gfe_rd=cr")
  end <- Sys.time()
  cat("Czas curl: ")
  cat(end - start)
}

testRCurlSpeed <- function(){
  getLibs("RCurl")
  start <- Sys.time()
  site <<- getForm("http://www.google.com/search", q="abortion+site:wordpress.com", as_sitesearch="wordpress.com", gws_rd="cr", num="30", pws="0", gfe_rd="cr")
  end <- Sys.time()
  cat("Czas RCurl: ")
  cat(end - start)
}

