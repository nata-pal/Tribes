

# Returns cleaned corpora
cleanDocs <- function(corpora, lang = "en"){
  getLibs(c("tm", "SnowballC", "stringi"))
  
  corpora <- tm_map(corpora, content_transformer(function(x) stri_replace_all_regex(as.character(x), "<!--.+?-->", " ")))
  corpora <- tm_map(corpora, content_transformer(function(x) stri_replace_all_regex(as.character(x), "<style.+?</style>", " ")))
  corpora <- tm_map(corpora, content_transformer(function(x) stri_replace_all_regex(as.character(x), "<script.+?</script>", " ")))
  corpora <- tm_map(corpora, content_transformer(function(x) stri_replace_all_regex(as.character(x), "<head.+?</head>", " ")))
  corpora <- tm_map(corpora, content_transformer(function(x) stri_replace_all_regex(as.character(x), "<.+?>", " ")))
  corpora <- tm_map(corpora, content_transformer(function(x) gsub("http[[:alnum:]]*", "", x)))
  corpora <- tm_map(corpora, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
  
  
  corpora <- tm_map(corpora, content_transformer(tolower))
  corpora <- tm_map(corpora, removeNumbers)
  corpora <- tm_map(corpora, removePunctuation)
  corpora <- tm_map(corpora, removeWords, stopwords(lang))
  corpora <- tm_map(corpora, stemDocument, language = lang) 
  corpora <- tm_map(corpora, stripWhitespace)
  
  save(corpora, file="data/corpuses/_cleaned_corpus.Rdata")
  corpora
}

ClassTermMatrix <- function(corp){
  getLibs(c("plyr"))
  dtm <- DocumentTermMatrix(corp)
  dtm.df <- as.data.frame(inspect(dtm))
  row.class <- unlist(meta(corp, "class", "local"), recursive = FALSE)
  dtm.df <- cbind(dtm.df, row.class)
  
  dtm.df <- dtm.df[dtm.df$row.class!='N',]
  row.class <- dtm.df$row.class
  new_df <- dtm.df[sapply(dtm.df,is.numeric)]
  
  new_df <- new_df[,colSums(new_df)>1]
  dtm.df <- cbind(new_df, row.class)
  
#   ctm <- aggregate(. ~ row.class, data=dtm.df, FUN=sum)
  ctm <- ddply(dtm.df, "row.class", numcolwise(sum))
  
#   View(ctm)
#   View(new_df)
#   n <- ncol(dtm.df)
#   View(dtm.df[(n-50):n])
  ctm
}

getConditionalProbs <- function(ctm){
  rownames(ctm) <- ctm$row.class
  ctm <- ctm[sapply(ctm, is.numeric)]
  
  nfor <- sum(ctm["F",])
  nagainst <- sum(ctm["A",])
  
  f <- ctm["F",] / nfor
  a <- ctm["A",] / nagainst
  d <- abs(f-a)
  rownames(d) <- "F-A"
  
  r <- rbind(a, f, d)
  
  View(r)
  r
}


