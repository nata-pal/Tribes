# Returns cleaned corpora
cleanDocs <- function(corpora, lang){
  getLibs(c("tm", "SnowballC"))
  
  corpora <- tm_map(corpora, content_transformer(function(x) stri_replace_all_regex(as.character(x), "<!--.+?-->", " ")))
  corpora <- tm_map(corpora, content_transformer(function(x) stri_replace_all_regex(as.character(x), "<style.+?</style>", " ")))
  corpora <- tm_map(corpora, content_transformer(function(x) stri_replace_all_regex(as.character(x), "<script.+?</script>", " ")))
  corpora <- tm_map(corpora, content_transformer(function(x) stri_replace_all_regex(as.character(x), "<head.+?</head>", " ")))
  corpora <- tm_map(corpora, content_transformer(function(x) stri_replace_all_regex(as.character(x), "<.+?>", " ")))
  corpora <- tm_map(corpora, content_transformer(function(x) gsub("http[[:alnum:]]*", "", x)))
  
  corpora <- tm_map(corpora, content_transformer(tolower))
  corpora <- tm_map(corpora, removeNumbers)
  corpora <- tm_map(corpora, removePunctuation)
  corpora <- tm_map(corpora, removeWords, stopwords(lang))
  corpora <- tm_map(corpora, stemDocument, language = lang) 
  corpora <- tm_map(corpora, stripWhitespace)
  
}


getTermsDissimPlot <- function(corpora){
  getLibs(c("tm", "proxy"))
  
  docsTDM <- TermDocumentMatrix(corpora)
  tdm <- as.matrix(docsTDM)
#   docsdissim <- dissimilarity(docsDTM, method="cosine") #no longer supported
  termsdissim <- proxy::dist(tdm, method="cosine")

#   cat(docsdissim)
  
  h <<- hclust(termsdissim, method = "ward.D2" ) 
  plot(h, sub = meta(corpora, tag="topic", type="corpus"), hang = -1)

  termsdissim
}


getDocsDissimPlot <- function(corpora){
  getLibs(c("tm", "proxy"))
  
  docsDTM <- DocumentTermMatrix(corpora)
  dtm <- as.matrix(docsDTM)
  #   docsdissim <- dissimilarity(docsDTM, method="cosine") #no longer supported
  docsdissim <- proxy::dist(dtm, method="cosine")
  
  #   cat(docsdissim)
  
  h <<- hclust(docsdissim, method = "ward.D2") 
  plot(h, sub = meta(corpora, tag="topic", type="corpus"), hang = -1)
  
  docsdissim
}