downloadWithGBlogSearchAPI <- function(topic, n = 15){
  getLibs(c("jsonlite", "RCurl"))
  start <- 0
  rn <- 8 #[1:8]
  links <- c()
  articles <- c(n)
  topics <- character(n)
  ids <- character(n)
  sitesNotFound  <- c()
  
  while (start < n) {
    rsz <- min(rn, (n-start))
    results <- fromJSON(URLencode(paste0("https://ajax.googleapis.com/ajax/services/search/blogs?v=1.0&q=", topic, "&scoring=d&rsz=", rsz, "&start=", start)))
    links <- c(links, results$responseData$results$postUrl)        
    start <- start + rn
  }
  
  print(links)
  
  for (i in 1:n){
    link <- links[i]
    cat(stri_paste("Processing ", link, "\n\n"))
    site <- tryCatch(stri_flatten(readLines(link, warn=FALSE, encoding = "UTF-8"), col = " "), error = function(e) "Not found")
    
    if(stri_cmp_eq(site, "Not found")){
      sitesNotFound <- c(sitesNotFound, i)
    }
    
    articles[i] <- site
    topics[i] <- topic
    ids[i] <- stri_paste(topic, formatC(i, width = 2, format=, flag="0") )
    ids[i] <- stri_replace_all_fixed(str = ids[i], pattern = " ", replacement = "_")
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

downloadFromGoogleBlg <- function(topic){
  #   getLibs("curl")
  getLibs(c("tm", "stringi", "RCurl", "XML", "httr"))
  n <- 15
  set_config( config( ssl.verifypeer = 0L ) )
  
  site <- getForm("http://www.google.com/search", 
                  q=stri_paste("~",topic),                           
                  hl="en", 
                  lr="lang_en",
                  num=n,
                  pws="0",
                  ie="UTF-8",
                  tbm="blg",
                  source="univ",
                  tbs="blgt:b",
                  tbo="u",
                  sa="X",
                  ei="pzahVYWlAcWiygPKlZ2oDA",
                  ved="0CBQQ-Ag",
                  gws_rd="cr,ssl"
  )
  print("Google search result ready")
  doc  <- htmlParse(site, asText = TRUE)
  print(doc)
  saveXML(doc, "output/out.html", indent = TRUE)
  
  links  <<- xpathSApply(doc, "//li[@class='g']//h3[@class='r']/a", xmlGetAttr, "href")
  print("Links ready")
  
  articles <- c(n)
  topics <- character(n)
  ids <- character(n)
  sitesNotFound  <- c()
  
  for (i in 1:n) {  
    link <- links[i]
    cat(stri_paste("Processing ", link, "\n\n"))
    site <- tryCatch(stri_flatten(readLines(stri_paste("http://www.google.pl",link), warn=FALSE, encoding = "UTF-8"), col = " "), error = function(e) "Not found")
    if(stri_cmp_eq(site, "Not found")){
      sitesNotFound <- c(sitesNotFound, i)
    }
    
    articles[i] <- site
    topics[i] <- topic
    ids[i] <- stri_paste(topic, formatC(i, width = 2, format=, flag="0") )
    ids[i] <- stri_replace_all_fixed(str = ids[i], pattern = " ", replacement = "_")
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

downloadFromGoogleSearch  <- function(topic, n){
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
