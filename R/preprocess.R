# Returns cleaned corpora
cleanDocs <- function(corpora, lang){
  getLibs(c("tm", "SnowballC"))
  
#   
#   corpora <- tm_map(corpora, function(x) stri_replace_all_regex(as.character(x), "<.+?>", " "))
#   
#   corpora <- tm_map(corpora, function(x) stri_replace_all_fixed(x, "\t", " "))
  corpora <- tm_map(corpora, PlainTextDocument)
  corpora <- tm_map(corpora, stripWhitespace)
  corpora <- tm_map(corpora, content_transformer(tolower))
  corpora <- tm_map(corpora, removeWords, stopwords(lang))
  corpora <- tm_map(corpora, stemDocument) 


}


getDocsDissimPlot <- function(corpora){
  getLibs(c("tm", "proxy"))
  
  docsTDM <- DocumentTermMatrix(corpora)
  tdm <- as.matrix(docsTDM)
#   docsdissim <- dissimilarity(docsDTM, method="cosine") #no longer supported
  docsdissim <- proxy::dist(tdm, method="cosine")

#   cat(docsdissim)
  
  h <<- hclust(docsdissim, method = "ward.D2" ) 

  
  plot(h, sub = "", hang = -1)
  
}