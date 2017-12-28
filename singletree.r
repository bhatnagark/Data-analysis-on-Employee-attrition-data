
library(rpart.plot)
library(caTools)
library(caTools)
library(caret)
library(RCurl)
library(pROC)
library(rpart)
library(rgdal)
library(raster)

emp_df <- data.frame(Employeeattrition)
emp_df<-emp_df[,c(1,3:35,2)]
str(emp_df)


set.seed(1234)
split <- sample.split(emp_df,SplitRatio = 0.7)
train <- subset(emp_df, split == TRUE)
str(train)
test <- subset(emp_df, split == FALSE)
str(test)

tree <- rpart(Attrition ~ . , method='class', data= train)

pred <- predict(object = tree, test, type="class")
t <- table(test$Attrition,pred)

confusionMatrix(t)

#tree 1
prp(tree, uniform = TRUE, main='Attrition Tree', type = 4, extra = 109, tweak=1, cex=0.6)
