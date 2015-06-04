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
#         i <- 1
        wiki <- wikiArticles[[i]]
        corp <- Corpus(VectorSource(corpsVector[[i]]))
        cleanCorp <- cleanDocs(corp, LANG)
        getDocsDissimPlot(cleanCorp)
  }
# }

