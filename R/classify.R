classify <- function(dtm, classes, VERBOSE= TRUE){
  getLibs("RTextTools")
  if(VERBOSE){
    print("INFO: # Error in drop(y %*% rep(1, nc)) może oznaczać, że w zbiorze testowym/trenującym znajdują się dokumenty tylko z jednej klasy\n")
  }
  n <- length(classes)
  x <- round(0.7 * n)
  print(x)
  combined.matrix <- ResortDtm(dtm)
  
  classes <- as.numeric.class(classes)
#   print(classes)
  container <- create_container(combined.matrix, classes, trainSize = 1:x, testSize = (x+1):n, virgin=FALSE)
  
  SVM <- train_model(container,"SVM")
  GLMNET <- train_model(container,"GLMNET")
  MAXENT <- train_model(container,"MAXENT")
  SLDA <- train_model(container,"SLDA")
  BOOSTING <- train_model(container,"BOOSTING")
  BAGGING <- train_model(container,"BAGGING")
  RF <- train_model(container,"RF")
#   NNET <- train_model(container,"NNET")
#   TREE <- train_model(container,"TREE")
  
  SVM_CLASSIFY <- classify_model(container, SVM)
  GLMNET_CLASSIFY <- classify_model(container, GLMNET)
  MAXENT_CLASSIFY <- classify_model(container, MAXENT)
  SLDA_CLASSIFY <- classify_model(container, SLDA)
  BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
  BAGGING_CLASSIFY <- classify_model(container, BAGGING)
  RF_CLASSIFY <- classify_model(container, RF)
#   NNET_CLASSIFY <- classify_model(container, NNET)
#   TREE_CLASSIFY <- classify_model(container, TREE)
  
  analytics <- create_analytics(container,
                                cbind(SVM_CLASSIFY, 
                                      SLDA_CLASSIFY,
                                      BOOSTING_CLASSIFY, 
                                      BAGGING_CLASSIFY,
                                      RF_CLASSIFY, 
                                      GLMNET_CLASSIFY,
#                                       NNET_CLASSIFY, 
#                                       TREE_CLASSIFY,
                                      MAXENT_CLASSIFY))
  summary(analytics)
  # CREATE THE data.frame SUMMARIES
  topic_summary <- analytics@label_summary
  alg_summary <- analytics@algorithm_summary
  ens_summary <-analytics@ensemble_summary
  doc_summary <- analytics@document_summary
  
}

ResortDtm <- function(working.dtm) {
  # sorts a sparse matrix in triplet format (i,j,v) first by i, then by j.
  # Args:
  #   working.dtm: a sparse matrix in i,j,v format using $i $j and $v respectively. Any other variables that may exist in the sparse matrix are not operated on, and will be returned as-is.
  # Returns:
  #   A sparse matrix sorted by i, then by j.
  working.df <- data.frame(i = working.dtm$i, j = working.dtm$j, v = working.dtm$v)  # create a data frame comprised of i,j,v values from the sparse matrix passed in.
  working.df <- working.df[order(working.df$i, working.df$j), ] # sort the data frame first by i, then by j.
  working.dtm$i <- working.df$i  # reassign the sparse matrix' i values with the i values from the sorted data frame.
  working.dtm$j <- working.df$j  # ditto for j values.
  working.dtm$v <- working.df$v  # ditto for v values.
  return(working.dtm) # pass back the (now sorted) data frame.
}

as.numeric.class <- function(cl){
  cl <- replace(cl, cl=='F', 1)
  cl <- replace(cl, cl=='A', -1)
  cl <- replace(cl, cl=='N', 0)
  as.numeric(cl)  
}
