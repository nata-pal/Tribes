evaluate <- function(prediction, original){
  getLibs(caret)
#   conf.mx <- table(p, df.test$CLASS)
  conf.mx <-  confusionMatrix(data = prediction, reference = original, positive = "F")
  print(conf.mx)
  
  
  tp <- conf.mx[1,1]    
  fp <- conf.mx[2,1]    
  tn <- conf.mx[2,2]    
  fn <- conf.mx[1,2]    
  
  print("Results for class A:")
  print(stri_paste("Accuracy = ", acc <- (tp + tn) / (tp + tn + fp + fn)))
  print(stri_paste("Recall/sensitivity = ", recall <-  tp / (tp + fn)))
  print(stri_paste("Specifity = ", spec <-  tn / (tn + fp)))
  print(stri_paste("Precision = ", precision <- tp / (tp + fp)))
  print(stri_paste("F1 = ", f1 <- 2 * precision * recall / (precision + recall)))
  
  
  tp <- conf.mx[2,2]    
  fp <- conf.mx[2,1]    
  tn <- conf.mx[2,2]    
  fn <- conf.mx[1,2]    
  
  print("Results for class F:")
  print(stri_paste("Accuracy = ", acc <- (tp + tn) / (tp + tn + fp + fn)))
  print(stri_paste("Recall/sensitivity = ", recall <-  tp / (tp + fn)))
  print(stri_paste("Specifity = ", spec <-  tn / (tn + fp)))
  print(stri_paste("Precision = ", precision <- tp / (tp + fp)))
  print(stri_paste("F1 = ", f1 <- 2 * precision * recall / (precision + recall)))

  
}