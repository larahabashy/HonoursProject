##Support Vector Machine
if (! require ("e1071")){
  install.packages ("e1071")
  library (e1071)
}
svm_model_1 <- svm(diagnosis~.,data=s1_train,type='C-classification')
svm_model_2 <- svm(diagnosis~.,data=s2_train,type='C-classification',probability=TRUE)
svm_model_3 <- svm(diagnosis~.,data=s3_train,type='C-classification')
svm_model_4 <- svm(diagnosis~.,data=s4_train,type='C-classification')
svm_model_5 <- svm(diagnosis~.,data=s5_train,type='C-classification')
svm_model_6 <- svm(diagnosis~.,data=s6_train,type='C-classification')

#Summary will list the respective parameters such as cost, gamma, etc.
summary(svm_model_1)

#Predictions
pred1=predict(svm_model_1,newdata=s1_test)
pred2=predict(svm_model_2,newdata=s2_test,probability=TRUE) 
pred3=predict(svm_model_3,newdata=s3_test)
pred4=predict(svm_model_4,newdata=s4_test)
pred5=predict(svm_model_5,newdata=s5_test)
pred6=predict(svm_model_6,newdata=s6_test)

table(pred1,s1_test$diagnosis)
#% prediction accuracy 
confusionMatrix(pred1, s1_test$diagnosis) #97.06% 
confusionMatrix(pred2, s2_test$diagnosis) #96.47% 
confusionMatrix(pred3, s3_test$diagnosis) #93.53%  - 9646
confusionMatrix(pred4, s4_test$diagnosis) #93.53% 
confusionMatrix(pred5, s5_test$diagnosis) #96.47% -95
confusionMatrix(pred6, s6_test$diagnosis) #98.24% , Kappa : 0.962  -97

library(kernlab,quietly = TRUE)
# Build a Support Vector Machine model .
set.seed (42)
ksvm <- ksvm (diagnosis.bin ~ ., data = training,
              kernel ="rbfdot",
              prob.model = TRUE )
ksvm

# tune svm model
svm.m1 <- train(diagnosis~., data=s1, preProc = c("center","scale"), method = "svmLinear",trControl = train_control)
svm.m2 <- train(diagnosis~., data=s2, preProc = c("center","scale"), method = "svmLinear",trControl = train_control)
svm.m3 <- train(diagnosis~., data=s3, preProc = c("center","scale"), method = "svmLinear",trControl = train_control)
svm.m4 <- train(diagnosis~., data=s4, preProc = c("center","scale"), method = "svmLinear",trControl = train_control)
svm.m5 <- train(diagnosis~., data=s5, preProc = c("center","scale"), method = "svmLinear",trControl = train_control)
svm.m6 <- train(diagnosis~., data=s6, preProc = c("center","scale"), method = "svmLinear",trControl = train_control)

pred.svm.m1 <- predict(svm.m1, newdata = s1_test)
pred.svm.m2 <- predict(svm.m2, newdata = s2_test)
pred.svm.m3 <- predict(svm.m3, newdata = s3_test)
pred.svm.m4 <- predict(svm.m4, newdata = s4_test)
pred.svm.m5 <- predict(svm.m5, newdata = s5_test)
pred.svm.m6 <- predict(svm.m6, newdata = s6_test)

confusionMatrix(pred.svm.m1, s1_test$diagnosis) #0.9882
confusionMatrix(pred.svm.m2, s2_test$diagnosis) #0.9824 
confusionMatrix(pred.svm.m3, s3_test$diagnosis) #0.9765 
confusionMatrix(pred.svm.m4, s4_test$diagnosis) #0.9647
confusionMatrix(pred.svm.m5, s5_test$diagnosis) #0.9647
confusionMatrix(pred.svm.m6, s6_test$diagnosis) #0.9647

#---------------------------------Naive Bayes--------------------------#
if (! require ("naivebayes" )){
  install.packages ("naivebayes")
  library (naivebayes)
}

nb_1 <- naive_bayes(diagnosis ~ ., s1_train)
nb_2 <- naive_bayes(diagnosis ~ ., s2_train)
nb_3 <- naive_bayes(diagnosis ~ ., s3_train)
nb_4 <- naive_bayes(diagnosis ~ ., s4_train)
nb_5 <- naive_bayes(diagnosis ~ ., s5_train)
nb_6 <- naive_bayes(diagnosis ~ ., s6_train)
#nb_7 <- naive_bayes(diagnosis ~ ., s7_train)

pred_nb_1=predict(nb_1,newdata=s1_test)
pred_nb_2=predict(nb_2,newdata=s2_test,probability=TRUE) 
pred_nb_3=predict(nb_3,newdata=s3_test)
pred_nb_4=predict(nb_4,newdata=s4_test) 
pred_nb_5=predict(nb_5,newdata=s5_test)
pred_nb_6=predict(nb_6,newdata=s6_test, type = "class") 

confusionMatrix(pred_nb_1, s1_test$diagnosis) #90.59%
confusionMatrix(pred_nb_2, s2_test$diagnosis) #94.12% , Kappa : 0.8722 
confusionMatrix(pred_nb_3, s3_test$diagnosis) #91.18%
confusionMatrix(pred_nb_4, s4_test$diagnosis) #92.35%
confusionMatrix(pred_nb_5, s5_test$diagnosis) #92.35% 
confusionMatrix(pred_nb_6, s6_test$diagnosis) #92.35%
#they misclassify the same number of points
#similar classification errors but different confusion matrices, 

# set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 10
)

# train model
nb.m1 <- train(s1_train, s1_train$diagnosis, method = "nb",trControl = train_control)
nb.m2 <- train(s2_train, s2_train$diagnosis, method = "nb",trControl = train_control)
nb.m3 <- train(s3_train, s3_train$diagnosis, method = "nb",trControl = train_control)
nb.m4 <- train(s4_train, s4_train$diagnosis, method = "nb",trControl = train_control)
nb.m5 <- train(s5_train, s5_train$diagnosis, method = "nb",trControl = train_control)
nb.m6 <- train(s6, s6$diagnosis, method = "nb",trControl = train_control)

# results
confusionMatrix(nb.m1)
plot(nb.m2)

pred.nb.m1 <- predict(nb.m1, newdata = s1_test)
pred.nb.m2 <- predict(nb.m2, newdata = s2_test)
pred.nb.m3 <- predict(nb.m3, newdata = s3_test)
pred.nb.m4 <- predict(nb.m4, newdata = s4_test)
pred.nb.m5 <- predict(nb.m5, newdata = s5_test)
pred.nb.m6 <- predict(nb.m6, newdata = s6_test)

confusionMatrix(pred.nb.m1, s1_test$diagnosis) #0.9529 
confusionMatrix(pred.nb.m2, s2_test$diagnosis) #0.9824 
confusionMatrix(pred.nb.m3, s3_test$diagnosis) #0.9765 
confusionMatrix(pred.nb.m4, s4_test$diagnosis) #0.9647
confusionMatrix(pred.nb.m5, s5_test$diagnosis) #0.9647
confusionMatrix(pred.nb.m6, s6_test$diagnosis) #0.9647


# set up tuning grid
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)

# train model
nb.m2 <- train(train, train$diagnosis, method = "nb",trControl = train_control,
               tuneGrid = search_grid,
               preProc = c("BoxCox", "center", "scale", "pca")
)

# top 5 models
nb.m2$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

plot(nb.m2)
confusionMatrix(nb.m2)

#----------------------------------------------------------------------#
#Logistic Regression Modelling

control <- trainControl(method="cv",
                        number = 10,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary)

logit_1 <- train(diagnosis ~., data = s1_train, method = "glm", 
               metric = "ROC", preProcess = c("scale", "center"),
               trControl = control)
logit_2 <- train(diagnosis ~., data = s2_train, method = "glm", 
                 metric = "ROC", preProcess = c("scale", "center"),
                 trControl = control)
logit_3 <- train(diagnosis ~., data = s3_train, method = "glm", 
                 metric = "ROC", preProcess = c("scale", "center"),
                 trControl = control)
logit_4 <- train(diagnosis ~., data = s4_train, method = "glm", 
                 metric = "ROC", preProcess = c("scale", "center"),
                 trControl = control)
logit_5 <- train(diagnosis ~., data = s5_train, method = "glm", 
                 metric = "ROC", preProcess = c("scale", "center"),
                 trControl = control)
logit_6 <- train(diagnosis ~., data = s6_train, method = "glm", 
                 metric = "ROC", preProcess = c("scale", "center"),
                 trControl = control)

#predictions
pred_logit_1 <- predict(logit_1, s1_test)
pred_logit_2 <- predict(logit_2, s2_test)
pred_logit_3 <- predict(logit_3, s3_test)
pred_logit_4 <- predict(logit_4, s4_test)
pred_logit_5 <- predict(logit_5, s5_test)
pred_logit_6 <- predict(logit_6, s6_test)

cm_logit <- confusionMatrix(pred_logit_1, s1_test$diagnosis, positive = "B")
cm_logit #94.12%

confusionMatrix(pred_logit_1, s1_test$diagnosis, positive = "B") #94.12%
confusionMatrix(pred_logit_2, s2_test$diagnosis, positive = "B") #95.29%
confusionMatrix(pred_logit_3, s3_test$diagnosis, positive = "B") #97.06%, Kappa: 0.9243
confusionMatrix(pred_logit_4, s4_test$diagnosis, positive = "B") #93.53%
confusionMatrix(pred_logit_5, s5_test$diagnosis, positive = "B") #94.71%
confusionMatrix(pred_logit_6, s6_test$diagnosis, positive = "B") #96.47%

plot(varImp((logit),top=22),main="Logistic Regression Variable Importance")

