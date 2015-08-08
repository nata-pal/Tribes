downloadArticles <- function(topic){
#   getLibs("curl")
  getLibs(c("tm", "stringi", "RCurl", "XML", "httr"))
  n <- 15
  set_config( config( ssl.verifypeer = 0L ) )
  
  site <- getForm("http://www.google.com/search", 
                  q=stri_paste("~",topic), 
#                   safe="active",
#                   as_sitesearch="wordpress.com", 
#                   as_qdr="all", 
#                   lr="lang_en", 
#                   hl="en", 
#                   gl="en", 
#                   gws_rd="cr", 
#                   num=n, 
#                   pws="0", 
#                   gfe_rd="cr"
                              
                  hl="en", 
#                   gl="en",
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

merge_corpuses <- function(){
  filepattern <- "^[A-Za-z _]*_corpus.Rdata"
#   filepattern <- "^[A-Za-z _]*_labeled.Rdata"
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




downloadFromIceRocket <- function(topic){
  getLibs(c("tm", "stringi", "RCurl", "XML", "httr"))
  n <- 15
  set_config( config( ssl.verifypeer = 0L ) )
  
  articles <- c(n)
  topics <- character(n)
  ids <- character(n)
  sitesNotFound  <- c()
  links <- c()
  
  
  p <- 0
  while(p*10<n){
    
    site <- getForm("http://blogs.icerocket.com/search", 
                    tab="blog",
                    p=(p+1),
                    fr="h",
                    q=topic,
                    dl="",
                    dh=""      
    )
    print("IceRocket search result ready")
    doc  <- htmlParse(site, asText = TRUE)
    print(doc)
    saveXML(doc, "output/out.html", indent = TRUE)
  #   
    next_links  <- xpathSApply(doc, "//div[@id='blog-results']//a[@class='main_link']", xmlGetAttr, "href")
    print("Links ready")
    
    #   if(VERBOSE){
        cat("\n\n================================\nRESULTS:\n\n")
        print(next_links)
    #   }
    
  
  
    links <- c(links, next_links)
    end <- min((p*10+10), n)
    
    for (i in (p*10+1):end) {  
      link <- links[i]
      cat(stri_paste("Processing ", link, "\n\n"))
      site <- tryCatch(stri_flatten(readLines(link, warn=FALSE, encoding = "UTF-8"), col = " "), error = function(e) "Not found")
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
      ids[i] <- stri_replace_all_fixed(str = ids[i], pattern = " ", replacement = "_")
    }
    p <- p+1
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
  
  writeCorpus(x = docs, path = "data/articles2")
  save(docs, file=stri_paste("data/corpuses2/", topic, "_corpus.Rdata"))
}




downloadFromWordpress <- function(topic){
  getLibs(c("tm", "stringi", "RCurl", "XML", "httr"))
  n <- 15
#   set_config( config( ssl.verifypeer = 0L ) )
  
  articles <- c(n)
  topics <- character(n)
  ids <- character(n)
  sitesNotFound  <- c()
  links <- c()
  
  
  p <- 0
  while(p*10<n){
    
    site <- getForm("http://en.search.wordpress.com/", 
                    src="organic",
                    q="abortion",
                    t="post",
                    s="date",
                    page=p     
    )
    print("Wordpress search result ready")
    doc  <- htmlParse(site, asText = TRUE)
    print(doc)
    saveXML(doc, "output/out.html", indent = TRUE)
    #   
    next_links  <- xpathSApply(doc, "//div[@class='results']//a[@class='result-media-link']", xmlGetAttr, "href")
    print("Links ready")
    
    #   if(VERBOSE){
    cat("\n\n================================\nRESULTS:\n\n")
    print(next_links)
    #   }
    
    
    
    links <- c(links, next_links)
    end <- min((p*10+10), n)
    
    for (i in (p*10+1):end) {  
      link <- links[i]
      cat(stri_paste("Processing ", link, "\n\n"))
      site <- tryCatch(stri_flatten(readLines(link, warn=FALSE, encoding = "UTF-8"), col = " "), error = function(e) "Not found")
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
      ids[i] <- stri_replace_all_fixed(str = ids[i], pattern = " ", replacement = "_")
    }
    p <- p+1
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
  
  writeCorpus(x = docs, path = "data/articles2")
  save(docs, file=stri_paste("data/corpuses2/", topic, "_corpus.Rdata"))
}

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


