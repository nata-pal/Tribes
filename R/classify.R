classify <- function(dtm, classes){
  getLibs("RTextTools")
  n <- length(classes)
  x <- round(0.8 * n)
  container <- create_container(dtm, classes, trainSize = 1:x, testSize = (x+1):n, virgin=FALSE)
  
  SVM <- train_model(container,"SVM")
  GLMNET <- train_model(container,"GLMNET")
  MAXENT <- train_model(container,"MAXENT")
  SLDA <- train_model(container,"SLDA")
  BOOSTING <- train_model(container,"BOOSTING")
  BAGGING <- train_model(container,"BAGGING")
  RF <- train_model(container,"RF")
  NNET <- train_model(container,"NNET")
  TREE <- train_model(container,"TREE")
  
  SVM_CLASSIFY <- classify_model(container, SVM)
  GLMNET_CLASSIFY <- classify_model(container, GLMNET)
  MAXENT_CLASSIFY <- classify_model(container, MAXENT)
  SLDA_CLASSIFY <- classify_model(container, SLDA)
  BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
  BAGGING_CLASSIFY <- classify_model(container, BAGGING)
  RF_CLASSIFY <- classify_model(container, RF)
  NNET_CLASSIFY <- classify_model(container, NNET)
  TREE_CLASSIFY <- classify_model(container, TREE)
  
  analytics <- create_analytics(container,
                                cbind(SVM_CLASSIFY, SLDA_CLASSIFY,
                                      BOOSTING_CLASSIFY, BAGGING_CLASSIFY,
                                      RF_CLASSIFY, GLMNET_CLASSIFY,
                                      NNET_CLASSIFY, TREE_CLASSIFY,
                                      MAXENT_CLASSIFY))
  summary(analytics)
  # CREATE THE data.frame SUMMARIES
  topic_summary <- analytics@label_summary
  alg_summary <- analytics@algorithm_summary
  ens_summary <-analytics@ensemble_summary
  doc_summary <- analytics@document_summary
  
}