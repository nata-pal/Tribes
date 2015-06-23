



# PROCESSING:
prepareAll <- function(){
  ARTICLES_PER_TOPIC <- 10
  LANG <- "en"
  
  # ===LOADING===
  
  topics <- getTopics("data/topics.txt")
  wikiArticles <- loadTopicsFromWiki(topics, LANG)
  #   corpsVector <- loadTopicsFromGoogle(topics, ARTICLES_PER_TOPIC)
  corpsVector <- loadTopicsFromGoogleNews(topics, ARTICLES_PER_TOPIC)
  for (i in 1:length(topics)){
    wiki <- wikiArticles[[i]]
    corp <- corpsVector[[topics[[i]]]]
    #         corp <- Corpus(VectorSource(corpsVector[[i]]))
    
    # ===CLEANING===
    cleanCorp <- cleanDocs(corp, LANG)
     
    dtm.params = list (
      weighting=weightTf
      ,bounds=list(
        global = c(2, Inf), local=c(2,Inf))
      ,wordLengths = c(2,Inf)
    )
    
    dtm <- DocumentTermMatrix(cleanCorp, control = dtm.params)
    classes <- setClasses(meta(cleanCorp, "link", type="local"))
    saveTopic(cleanCorp, corp, wiki, dtm, classes);
    
    #   dtm.df <- as.data.frame(inspect(dtm))
    #   dtm.df$metalink <- meta(cleanCorp, type="local", tag="link")
    
    #         save(corp, file = "output/corp", ascii = TRUE)
    #         save(cleanCorp, file = "output/cleanCorp", ascii = TRUE)
    #   saveRDS(dtm.df, file=stri_paste("data/", topics[[i]], ".rds"))
    #   readRDS(stri_paste("data/", topics[[i]], ".rds"))
  }
}


saveTopic <- function(cleanCorp, corp, wiki, dtm, classes){
  path <- stri_paste("data/", meta(cleanCorp, "topic", type="corpus"), "/")
  cat(path)
  dir.create(path)
  dir.create(stri_paste(path, "original"))
  dir.create(stri_paste(path, "clean"))
  writeCorpus(corp, path = stri_paste(path, "original"))
  writeCorpus(cleanCorp, path = stri_paste(path, "clean"))
  save(wiki, file = stri_paste(path, "original/wiki.txt"))
  save(dtm, file = stri_paste(path, "dtm.Rdata"))
  save(classes, file = stri_paste(path, "classes.Rdata"))
  save(cleanCorp, file = stri_paste(path, "cleanCorp.Rdata"))
  save(corp, file = stri_paste(path, "originCorp.Rdata"))
}


