#stacking Ensemble on Attrition dataset

library(caTools)
library(caret)
library(RCurl)
library(pROC)
library(rpart)
library(rgdal)
library(raster)

set.seed(12345)
emp_df <- data.frame(Employeeattrition)
emp_df<-emp_df[,c(1,3:35,2)]
str(emp_df)

emp_df<-subset(emp_df,select=-c(StandardHours,RelationshipSatisfaction,PerformanceRating,Over18,JobInvolvement,Gender))
#str(emp_df)
character_vars <- lapply(emp_df, class) == "character"
#str((emp_df))
emp_df[, character_vars] <- lapply(emp_df[, character_vars], as.factor)
emp_df$Attrition<-as.factor(emp_df$Attrition)
emp_df$JobRole<-as.factor(emp_df$JobRole)
#str(emp_df)

split <- sample(nrow(emp_df), floor(0.7*nrow(emp_df)))
traindf <- emp_df[split,]
testdf <-  emp_df[-split,]

ctrl <- trainControl(method = "cv",number = 5,savePredictions = 'final',classProbs = T)

#Training decision tree
dec_tree <-train(Attrition~., data=traindf, method="rpart",trControl=ctrl, tuneLength=5)
print(dec_tree$results$Accuracy[5])
print(dec_tree$pred)

#Training logistic regression
log_reg <-train(Attrition~., data=traindf, method="glm",trControl=ctrl, tuneLength=5)
print(log_reg$results$Accuracy)

#Training knn model
knn <-train(Attrition~., data=traindf, method="knn",trControl=ctrl,tuneLength=5)
print(knn$results$Accuracy[5])

#training the level 0 of the ensemble model
pred_dec_tree<-predict(dec_tree,newdata = testdf)
pred_dec_tree
confusionMatrix(pred_dec_tree,testdf$Attrition)$overall[1]
pred_log_reg<-predict(log_reg,newdata = testdf)
pred_log_reg
confusionMatrix(pred_log_reg,testdf$Attrition)$overall[1]
pred_knn<-predict(knn,newdata = testdf)
pred_knn
confusionMatrix(pred_knn,testdf$Attrition)$overall[1]
preddf<-data.frame(pred_dec_tree, pred_log_reg,pred_knn,class=testdf$Attrition,stringsAsFactors = F )
str(preddf)
#preddf_bc <- undersample_ds(preddf, "Attrition", nsamples_class)

#training ensemble
modelstack<-train(class~.,data=preddf, method="glm")
pred_modelstack<-predict(modelstack,newdata=testdf)
head(pred_modelstack)
confusionMatrix(pred_modelstack,testdf$Attrition)$overall[1]
pred_modelstack







