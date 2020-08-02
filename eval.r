#Load required packages
if (! require ("Metrics")){
  install.packages ("Metrics")
  library (Metrics)
}
if (! require ("ROCit")){
  install.packages ("ROCit")
  library (ROCit)
}
if (! require ("ROCR")){
  install.packages ("ROCR")
  library (ROCR)
}
if (! require ("pROC")){
  install.packages ("pROC")
  library (pROC)
}
if (! require ("PRROC")){
  install.packages ("PRROC")
  library (PRROC)
}
if (! require ("precrec")){
  install.packages ("precrec")
  library (precrec)
}
if (! require ("rsq")){
  install.packages ("rsq")
  library (rsq)
}
if (! require ("MLmetrics")){
  install.packages ("MLmetrics")
  library (MLmetrics)
}
if (! require ("plotROC")){
  install.packages ("plotROC")
  library (plotROC)
}

#selected models: logit s3, nb s2, svm s6 VSURF

#---------------------------------------------------------------------------------------#
#effeciency

#f score - logit
F1_Score(y_pred = pred_logit_3, y_true = s3_test$diagnosis, positive = "M") #0.9606299
F1_Score(y_pred = pred_logit_3, y_true = s3_test$diagnosis, positive = "B") #0.9765258

#f score - svm
F1_Score(y_pred = pred6, y_true = s6_test$diagnosis, positive = "M") #0.976
F1_Score(y_pred = pred6, y_true = s6_test$diagnosis, positive = "B") #0.9860465
F1_Score(y_pred = pred.svm.m1, y_true = s1_test$diagnosis, positive = "M") # 0.983871
F1_Score(y_pred = pred.svm.m1, y_true = s1_test$diagnosis, positive = "B") #0.9907407

#f score - nb
F1_Score(y_pred = pred.nb.m2, y_true = s2_test$diagnosis, positive = "M") #0.976
F1_Score(y_pred = pred.nb.m2, y_true = s2_test$diagnosis, positive = "B") #0.9860465

#---------------------------------------------------------------------------------------#
# ROC cuve
rocplot <- ggplot(data, aes(m = pred6, d = s6_test$diagnosis))+ geom_roc(n.cuts=20,labels=FALSE)
rocplot
rocplot + style_roc(theme = theme_grey) + geom_rocci(fill="pink") 

roc.curve(s6_test$diagnosis,pred6,curve = TRUE) #0.5029412
roc.curve(s3_test$diagnosis,pred_logit_3,curve = TRUE) #0.5029412
roc.curve(s2_test$diagnosis,pred_nb_2,curve = TRUE) #0.5029412

#roc curve
ROCit_obj <- rocit(score=as.numeric(pred6),class=s6_test$diagnosis)
plot(ROCit_obj)
ksplot(ROCit_obj)

ROCit_obj_logit <- rocit(score=as.numeric(pred_logit_3),class=s3_test$diagnosis)
plot(ROCit_obj_logit)
ksplot(ROCit_obj_logit)

ROCit_obj_nb <- rocit(score=as.numeric(pred.nb.m2),class=s2_test$diagnosis)
plot(ROCit_obj_nb)
ksplot(ROCit_obj_nb)

roc.curve(s6_test$diagnosis,pred6,curve = TRUE) #0.5029412
roc.curve(s3_test$diagnosis,pred_logit_3,curve = TRUE) #0.5029412

pred_ksvm <- predict (ksvm , testing)
roc<- roc.curve(testing$diagnosis.bin, pred_ksvm, curve = T)
plot(roc)

#Evaluation metrics - effeciency of models - found in appendix
precrec_logit <- evalmod(score = as.numeric(pred_logit_3),labels=s3_test$diagnosis, mode="basic")
autoplot(precrec_logit)  

precrec_svm <- evalmod(score = as.numeric(pred6),labels=s6_test$diagnosis, mode="basic")
autoplot(precrec_svm)  

precrec_nb <- evalmod(score = as.numeric(pred_nb_2),labels=s2_test$diagnosis, mode="basic")
autoplot(precrec_nb)  

#AUC-ROC
AUC(y_pred = pred_nb_2, y_true = as.vector(s2_test$diagnosis))


#---------------------------------------------------------------------------------------#
#effectiveness

#accuracy - svm
accuracy(as.numeric(s6_test$diagnosis), as.numeric(pred6)) #0.9823529

#accuracy - logit
accuracy(as.numeric(s3_test$diagnosis), as.numeric(pred_logit_3)) #0.9705882

#accuracy - nb 
accuracy(as.numeric(s2_test$diagnosis), as.numeric(pred_nb_2)) #0.9411765
accuracy(as.numeric(s2_test$diagnosis), as.numeric(pred.nb.m2)) #0.9823529

#auc scores
auc(as.numeric(s6_test$diagnosis), as.numeric(pred6)) #0.9795
auc(as.numeric(s6_test$diagnosis), as.numeric(pred.svm.m6)) #0.9715

auc(as.numeric(s3_test$diagnosis), as.numeric(pred_logit_3)) # 0.9701

auc(as.numeric(s2_test$diagnosis), as.numeric(pred.nb.m2)) #0.9795
auc(as.numeric(s2_test$diagnosis), as.numeric(pred_nb_2)) #0.9304


#computational complecxity - measure run-time
if (! require ("tictoc")){
  install.packages ("tictoc")
  library (tictoc)
}

#logit run-time
tic("sleeping")
print("falling asleep...")
logit_3 <- train(diagnosis ~., data = s3_train, method = "glm", 
                 metric = "ROC", preProcess = c("scale", "center"),
                 trControl = control)
pred_logit_3 <- predict(logit_3, s3_test)
print("...waking up")
toc()

#svm run-time
tic("sleeping")
print("falling asleep...")
svm_model_6 <- svm(diagnosis~.,data=s6_train,type='C-classification')
#pred6=predict(svm_model_6,newdata=s6_test)
print("...waking up")
toc()

tic("sleeping")
print("falling asleep...")
svm.m6 <- train(diagnosis~., data=s6, preProc = c("center","scale"), method = "svmLinear",trControl = train_control)
pred.svm.m6 <- predict(svm.m6, newdata = s6_test)
print("...waking up")
toc()

#nb run-time
tic("sleeping")
print("falling asleep...")
nb.m2 <- train(s2_train, s2_train$diagnosis, method = "nb",trControl = train_control)
pred.nb.m2 <- predict(nb.m2, newdata = s2_test)
print("...waking up")
toc()

#---------------------------------------------------------------------------------------#
#extra accuracy 
mae(as.numeric(s6_test$diagnosis), as.numeric(pred6)) #0.01764706
mse(as.numeric(s6_test$diagnosis), as.numeric(pred6)) #0.01764706
accuracy(as.numeric(s6_test$diagnosis), as.numeric(pred6)) #0.9823529
mae(as.numeric(test_no_corr$diagnosis), as.numeric(pred2)) #0.02941176
mse(as.numeric(test$diagnosis), as.numeric(pred1)) #0.02352941
mse(as.numeric(test_no_corr$diagnosis), as.numeric(pred2)) #0.02941176
accuracy(as.numeric(test$diagnosis), as.numeric(pred1))
auc(as.numeric(test$diagnosis), as.numeric(pred1))
ll(as.numeric(test$diagnosis), as.numeric(pred1))
f1(as.numeric(test$diagnosis), as.numeric(pred1))
test_no_corr$diagnosis
test_no_corr$diagnosis2 <- ifelse(test_no_corr$diagnosis == 'M',1,0)

accuracy(pred,testing$diagnosis)
bias(pred,testing$diagnosis)
auc(pred,testing$diagnosis)
#classification error
ce(pred,testing$diagnosis) #0.0201005
f1(pred,testing$diagnosis) #1
fbeta_score(pred,testing$diagnosis)
#log loss
ll(pred,testing$diagnosis)
#mean log loss
logLoss(pred6,s6_test$diagnosis)
#mean absolute error
mae(pred,testing$diagnosis)
#mean absolute percentage error
mape(pred,testing$diagnosis)
#mean absolute  scaled error
mase(pred,testing$diagnosis,step_size = 1)
#Median Absolute Error
mdae(pred,testing$diagnosis)
#mean squared error
mse(pred,testing$diagnosis)
#msle Mean Squared Log Error
msle(pred,testing$diagnosis)
#rmse Root Mean Squared Error
#rmsle Root Mean Squared Log Error

