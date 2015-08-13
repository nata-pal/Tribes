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

classify(corp.train, corp.test){
  getLibs("e1071")
  dtm.train <- DocumentTermMatrix(corp.train)
  dtm.test <- DocumentTermMatrix(corp.test)
  
  df.train <- as.data.frame(as.matrix(dtm.train))
  df.test <- as.data.frame(as.matrix(dtm.test))
  rownames(df.train) <- unlist(meta(corp.train, "id", "local"))
  rownames(df.test) <- unlist(meta(corp.test, "id", "local"))
  
  class.train <- unlist(meta(corp.train, "class", "local"))
  class.test <- unlist(meta(corp.test, "class", "local"))
  df.train <- cbind(df.train, CLASS = class.train)
  df.test <- cbind(df.test, CLASS = class.test)
  
  model <- naiveBayes(CLASS ~ ., data = df.train)
  p <- predict(model, df.test)
  conf.mx <- table(p, df.test$CLASS)
  print(conf.mx)
  
  tp <- conf.mx[1,1]    
  fp <- conf.mx[2,1]    
  tn <- conf.mx[2,2]    
  fn <- conf.mx[1,2]    
  
  print(stri_paste("Error rate = ", error.rate <- (tp + tn) / (tp + tn + fp + fn)))
  print(stri_paste("Recall = ", recall <-  tp / (tp + fn)))
  print(stri_paste("Precision = ", precision <- tp / (tp + fp)))
  print(stri_paste("F1 = ", f1 <- 2 * precision * recall / (precision + recall)))
  
  
}
