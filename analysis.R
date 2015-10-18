loadFunctions <- function(){
  files <- list.files(path = "R/", full.names = TRUE, pattern = ".*R", recursive = TRUE, include.dirs = FALSE)
  for(file in files){
    source(file)
  }
}

# ***GOOGLE***
# Creating directory in /data directory with text information for topics listed in /data/topics.txt
# Function requires specifing classes for downloaded articles
# prepareAll()
# classify(dtm, classes)

#Downloading data
downloadData <- function(){
  topics <- getTopics("data/topics.txt")
  for (topic in topics){
    downloadWithGBlogSearchAPI(topic)
  }
  alarm()
  m <<- merge_corpuses()
  p <<- cleanDocs2(m)
  
}

labelData <- function(){
  c <- setClasses(meta(docs, "link", "local"))
  saveClassified(docs, c, meta(docs, "topic", "local")[[1]])
}

testMinDocs <- function(){
  for(i in 0:10){
    processAll(minDocs = (i*0.1), lower = 0, upper = 1)
  }
}



processAll2 <- function(minDocs){
  corpName <- load("C:/Users/Natalia/OneDrive/Magisterka/WORKSPACE/Tribes/Tribes/data/corpuses.train/_merged_corpus.Rdata")
  c <- get(corpName)
  corpName <- load("C:/Users/Natalia/OneDrive/Magisterka/WORKSPACE/Tribes/Tribes/data/corpuses/_merged_corpus.Rdata")
  ct <- get(corpName)
  rm(corpName)
  
  print("Cleaning documents...")
  ct1 <- cleanDocs2(ct)
  c1 <- cleanDocs2(c)
  print("Removing words typical for given topics...")
  c2 <- removeTopicTypicalWords(corpus = c1, minDocs = minDocs)

  print("Classifing with Naive Bayes...")
  for(i in 1:9){
    for(j in (i+1):10){
      print("================================")
      print(paste("minDocs = ", minDocs))
      print(paste("lower = ", (i*0.1)))
      print(paste("upper = ", (j*0.1)))
      e <- tryCatch(classify(c2, ct1, lower = (i*0.1), upper = (j*0.1)), error = function(cond)"skip")
      if(e == "skip") next
    }
  }  
}



processAll <- function(minDocs, breakpoint, lower, upper){
  corpName <- load("C:/Users/Natalia/OneDrive/Magisterka/WORKSPACE/Tribes/Tribes/data/corpuses.train/_merged_corpus.Rdata")
  c <- get(corpName)
  corpName <- load("C:/Users/Natalia/OneDrive/Magisterka/WORKSPACE/Tribes/Tribes/data/corpuses/_merged_corpus.Rdata")
  ct <- get(corpName)
#   print(levels(meta(c, "class", "local")))
  rm(corpName)
  print("Cleaning documents...")
  ct1 <- cleanDocs2(ct)
  c1 <- cleanDocs2(c)
  print("Removing words typical for given topics...")
  c2 <- removeTopicTypicalWords(corpus = c1, minDocs = minDocs)
  print("Removing negligible words...")
  c3 <- removeNegligibleWords(corpus = c2, breakpoint = breakpoint)
#   print("Classifing with Naive Bayes...")
#   for(i in 1:9){
#     for(j in (i+1):10){
#       print("================================")
#       print(paste("minDocs = ", minDocs))
#       print(paste("lower = ", (i*0.1)))
#       print(paste("upper = ", (j*0.1)))
#       e <- tryCatch(classify(c2, ct1, lower = (i*0.1), upper = (j*0.1)), error = function(cond)"skip")
# #       e <- tryCatch(classify(c2, ct1, lower = (0.6), upper = (0.7)), error = function(cond) next)
#       if(e == "skip") next
#       else {
#         print("ok")
#         print(e)
#       }
#     }
#   }
#   classify(c2, ct1, lower = lower, upper = upper)
#   classify(c3, ct1, lower = lower, upper = upper)

}

classify <- function(corp.train, corp.test, lower = 0, upper = 1){
  getLibs(c("e1071", "caret"))
  l <- ceiling(length(corp.train) * lower)
  u <- ceiling(length(corp.train) * upper)
#   print(paste("l = ", l))
#   print(paste("u = ", u))
  dtm.train <- DocumentTermMatrix(corp.train, control=list(bounds = list(global = c(l,u))))
  dtm.test <- DocumentTermMatrix(corp.test)
  
  df.train <- as.data.frame(as.matrix(dtm.train))
  my.dtm <<- df.train
  df.test <- as.data.frame(as.matrix(dtm.test))
  rownames(df.train) <- unlist(meta(corp.train, "id", "local"))
  rownames(df.test) <- unlist(meta(corp.test, "id", "local"))
  
  class.train <- unlist(meta(corp.train, "class", "local"))
  class.test <- unlist(meta(corp.test, "class", "local"))
#   print(unique(class.train))
#   print(unique(class.test))
  df.train <- cbind(df.train, CLASS = class.train)
  df.test <- cbind(df.test, CLASS = class.test)
  
  model <- naiveBayes(CLASS ~ ., data = df.train)
#   model <- svm(CLASS ~ ., data=df.train)
  p <<- predict(model, df.test)
#   print(levels(df.train$CLASS))
#   print(levels(p))
#   print(levels(df.test$CLASS))

  conf.mx <<-  confusionMatrix(data = p, reference = df.test$CLASS, positive = "F")
  print(conf.mx)
  conf.mx
#   conf.mx <- table(p, df.test$CLASS)
#   print(conf.mx)
#   
#   tp <- conf.mx[1,1]    
#   fp <- conf.mx[2,1]    
#   tn <- conf.mx[2,2]    
#   fn <- conf.mx[1,2]    
#   
#   print(stri_paste("Accuracy = ", acc <- (tp + tn) / (tp + tn + fp + fn)))
#   print(stri_paste("Recall/sensitivity = ", recall <-  tp / (tp + fn)))
#   print(stri_paste("Specifity = ", spec <-  tn / (tn + fp)))
#   print(stri_paste("Precision = ", precision <- tp / (tp + fp)))
#   print(stri_paste("F1 = ", f1 <- 2 * precision * recall / (precision + recall)))
#   
  
}
