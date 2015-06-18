# SETTINGS:
ARTICLES_PER_TOPIC <<- 10
TOPICS_SOURCE_PATH <<- "data/topics.txt"
LANG <<- "en"
TESTMODE <<- FALSE
# TESTMODE <<- TRUE



# PROCESSING:
# go <- function(){
  VERBOSE  <<- TESTMODE
  topics <- getTopics(TOPICS_SOURCE_PATH)
  wikiArticles <- loadTopicsFromWiki(topics, LANG)
  corpsVector <- loadTopicsFromGoogle(topics, ARTICLES_PER_TOPIC)
  for (i in 1:length(topics)){
        wiki <- wikiArticles[[i]]
        corp <- corpsVector[[topics[[i]]]]
#         corp <- Corpus(VectorSource(corpsVector[[i]]))
        cleanCorp <- cleanDocs(corp, LANG)
        dtm.params = list (
          weighting=weightTfIdf, 
          bounds=list(global = c(2, Inf)),
          wordLengths = c(2,Inf),
          termFreq= c(2,10)
        )
        dtm <- DocumentTermMatrix(cleanCorp, control = dtm.params)
        frTerms <- findFreqTerms(dtm, 0.5)

        dtm.df <- as.data.frame(inspect(dtm))
        
        save(corp, file = "output/corp", ascii = TRUE)
        save(cleanCorp, file = "output/cleanCorp", ascii = TRUE)
  }
# }

