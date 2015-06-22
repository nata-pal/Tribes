# SETTINGS:
ARTICLES_PER_TOPIC <<- 10
TOPICS_SOURCE_PATH <<- "data/topics.txt"
LANG <<- "en"
# TESTMODE <<- FALSE
TESTMODE <<- TRUE



# PROCESSING:
# createDataframes <- function(){
  VERBOSE  <<- TESTMODE
  topics <- getTopics(TOPICS_SOURCE_PATH)
  wikiArticles <- loadTopicsFromWiki(topics, LANG)
#   corpsVector <- loadTopicsFromGoogle(topics, ARTICLES_PER_TOPIC)
  corpsVector <- loadTopicsFromGoogleNews(topics, ARTICLES_PER_TOPIC)
  for (i in 1:length(topics)){
        wiki <- wikiArticles[[i]]
        corp <- corpsVector[[topics[[i]]]]
#         corp <- Corpus(VectorSource(corpsVector[[i]]))
        cleanCorp <- cleanDocs(corp, LANG)
#         writeLines(as.character(cleanCorp), con=paste("data/clean_", topics[[i]], ".txt"))
#         save.image(cleanCorp, file=paste("data/clean_", topics[[i]], ".corp"))
#         load(paste("data/clean_", topics[[i]], ".txt"))
#         classes <- setClasses(cleanCorp);        

        dtm.params = list (
          weighting=weightTf
          ,bounds=list(
            global = c(2, Inf), local=c(2,Inf))
          ,wordLengths = c(2,Inf)
        )
        dtm <- DocumentTermMatrix(cleanCorp, control = dtm.params)
#         dtm2 <- DocumentTermMatrix(cleanCorp)
        dtm.sparse <- removeSparseTerms(dtm, 0.5)

        


        dtm.df <- as.data.frame(inspect(dtm))
        dtm.df$metalink <- meta(cleanCorp, type="local", tag="link")
        
#         save(corp, file = "output/corp", ascii = TRUE)
#         save(cleanCorp, file = "output/cleanCorp", ascii = TRUE)
        saveRDS(dtm.df, file=stri_paste("data/", topics[[i]], ".rds"))
        readRDS(stri_paste("data/", topics[[i]], ".rds"))
  }
# }

