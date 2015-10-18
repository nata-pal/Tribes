cleanDocs2 <- function(corpora, lang = "en"){
  getLibs(c("tm", "boilerpipeR"))
  
  # Removing documents assigned to class 'N'
  corpora <- corpora[meta(corpora, "class", "local")!="N"]
  
  # Modifying document's content
  corpora <- tm_map(corpora, content_transformer(function(x) ArticleExtractor(x)))
  corpora <- tm_map(corpora, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
  corpora <- tm_map(corpora, content_transformer(tolower))
  corpora <- tm_map(corpora, removeNumbers)
  corpora <- tm_map(corpora, removePunctuation)
  corpora <- tm_map(corpora, removeWords, tm::stopwords(lang))
  corpora <- tm_map(corpora, stemDocument, language = lang) 
  corpora <- tm_map(corpora, stripWhitespace)
    
  save(corpora, file="data/corpuses/_cleaned_corpus.Rdata")
  alarm()
  corpora
  
}

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
  getLibs(c("plyr", "tm"))
  
  l <- ceiling(length(corp) * 0.1)
  u <- ceiling(length(corp) * 0.5)
  dtm <- DocumentTermMatrix(corp, control=list(bounds = list(global = c(l,u))))
  
#   dtm <- DocumentTermMatrix(corp)
  ncol(as.matrix(dtm))
  dtm.df <- as.data.frame(as.matrix(dtm))
  print(ncol(dtm.df))
  row.class <- unlist(meta(corp, "class", "local"), recursive = FALSE)
  dtm.df <- cbind(dtm.df, row.class)
  print(ncol(dtm.df))
  
  dtm.df <- dtm.df[dtm.df$row.class!='N',]
  row.class <- dtm.df$row.class
  new_df <- dtm.df[sapply(dtm.df,is.numeric)]
  print(ncol(dtm.df))
  
#   new_df <- new_df[,colSums(new_df)>1]
  dtm.df <- cbind(new_df, row.class)
  print(ncol(dtm.df))
  
  ctm <- ddply(dtm.df, "row.class", numcolwise(sum))
  
#   View(ctm)
#   View(new_df)
  n <<- ncol(dtm.df)
#   View(dtm.df[(n-50):n])
  ctm
}

# returns conditional probs P(term|class)
getConditionalProbs <- function(ctm){
  rownames(ctm) <- ctm$row.class
  ctm <- ctm[sapply(ctm, is.numeric)]
  
  nfor <<- sum(ctm["F",])
  nagainst <<- sum(ctm["A",])
#   cat("for total: ")
#   cat(nagainst)
#   cat("\nagainst total: ")
#   cat(nfor)
  
  f <- ctm["F",] / nfor
  a <- ctm["A",] / nagainst
  d <- abs(f-a)
  rownames(d) <- "F-A"
  
  r <- rbind(a, f, d)
  # Order by F-A
  r <- r[,order(-r[3,])]
  prob <<- r
  r
}

removeNegligibleWords <- function(corpus, breakpoint = 0.6){
#   if(breakpoint!=1){
    getLibs(c("tm"))
    cond  <- getConditionalProbs(ctm = ClassTermMatrix(corpus))
    bp <- ceiling(ncol(cond) * breakpoint)
    negligibleWords <- colnames(cond)[bp:ncol(cond)]
  #   View(negligibleWords)
    
    corpus <- tm_map(corpus, removeWords, negligibleWords)
#   }
  corpus
}

estimateNBClasses <- function(corp){
  getLibs("Brobdingnag")
  
  zero <- 1^(-300)
  factor <- 10^10
  ctm <- ClassTermMatrix(corp)
  cp <- getConditionalProbs(ctm)
  dtm <- as.matrix(DocumentTermMatrix(corp))
  dtm  <- dtmA <- dtmF<- dtm[,colnames(cp)]

  for(col in colnames(dtm)){
    dtmA[,col] <- cp["A",col]^dtm[,col]
    dtmF[,col] <- cp["F",col]^dtm[,col]
  }

  dtmA[dtmA==0] <- zero
  dtmF[dtmF==0] <- zero

  dtmA <- log(dtmA)
  dtmF <- log(dtmF)
  
  pxc <- cbind(A = apply(dtmA, 1, sum), F = apply(dtmF, 1, sum))
  
  meta <- meta(corp, "class", "local")
  pc.a <- length(meta[meta=="A"])/(length(meta[meta=="A"])+length(meta[meta=="F"]))
  pc.f <- length(meta[meta=="F"])/(length(meta[meta=="A"])+length(meta[meta=="F"]))
  
  pxc.log.a <- pxc[,"A"] + log(pc.a) 
  pxc.log.f <- pxc[,"F"] + log(pc.f) 

  pxc.log <- cbind(A = pxc.log.a, F = pxc.log.f)
  
  cnb <- colnames(pxc.log)[max.col(pxc.log,ties.method="first")]
  row.names(cnb) <- row.names(pxc.log)

#   class <- max(classProbs)
  cnb
}

removeTopicTypicalWords <- function(corpus, minDocs = 0.5){
  if(minDocs!=0){
    getLibs(c("tm", "stringi", "qdap"))
    topics <- unlist(unique(meta(corpus, "topic", "local")))
    # Minimal number of topics in which word has to occur
    min <- ceiling(length(topics)*minDocs)
    words <- c()
    for(t in topics){
      tc <- corpus[meta(corpus, "topic", "local") == t]
      dtm <- DocumentTermMatrix(tc, )
      content <- dtm$dimnames$Terms
      words <- c(as.vector(words), as.vector(content))
    }
    freq <<- all_words(words)
    rmWords <<- freq$WORD[freq$FREQ < min ]
    cat(paste(length(rmWords), "... "))
    
    while(length(rmWords)>0){
      b <- min(2000, length(rmWords))
      words <- rmWords[1:b]
      corpus <- tm_map(corpus, removeWords, words)
      
      if(b == length(rmWords)){
        
        rmWords <- c()
        cat(length(rmWords))
        cat("last\n")
        
      } else {
        rmWords <- rmWords[(b+1):length(rmWords)]  
        cat(paste(length(rmWords), "... "))
      }
      
    }
  }
  corpus
}