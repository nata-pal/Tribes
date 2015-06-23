
getTermsDissimPlot <- function(corpora){
  getLibs(c("tm", "proxy"))
  
  docsTDM <- TermDocumentMatrix(corpora)
  tdm <- as.matrix(docsTDM)
  #   docsdissim <- dissimilarity(docsDTM, method="cosine") #no longer supported
  termsdissim <- proxy::dist(tdm, method="cosine")
  
  #   cat(docsdissim)
  
  h <- hclust(termsdissim, method = "ward.D2" ) 
  plot(h, sub = meta(corpora, tag="topic", type="corpus"), hang = -1)
  
  docsTDM
}


getDocsDissimPlot <- function(corpora){
  getLibs(c("tm", "proxy"))
  
  docsDTM <- DocumentTermMatrix(corpora)
  dtm <- as.matrix(docsDTM)
  #   docsdissim <- dissimilarity(docsDTM, method="cosine") #no longer supported
  docsdissim <- proxy::dist(dtm, method="cosine")
  
  #   cat(docsdissim)
  
  h <- hclust(docsdissim, method = "ward.D2") 
  plot(h, sub = meta(corpora, tag="topic", type="corpus"), hang = -1)
  
  docsDTM
}