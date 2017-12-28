library(RCurl)
library(caret)
library(gbm)
library(mlbench)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(psych)
library(e1071)
library(ipred)

#library(plotly)

set.seed(1234)
emp_df <- data.frame(Employeeattrition)
emp_df<-emp_df[,c(1,3:35,2)]

emp_df<-subset(emp_df,select=c(MonthlyIncome, TotalWorkingYears,JobRole,Attrition,YearsAtCompany,JobLevel))
str(emp_df)
emp_df$Attrition<-as.factor(emp_df$Attrition)
emp_df$JobRole<-as.factor(emp_df$JobRole)
str(emp_df)
emp.bagging<-bagging(Attrition~.,data = emp_df,mfinal=15,control=rpart.control(maxdepth=3, minsplit=1))
emp_df$pred.class <- predict(emp.bagging,emp_df)
?confusionMatrix
str(emp_df$pred.class)
head(emp_df$pred.class)
str(emp_df$Attrition)
confusionMatrix(emp_df$Attrition, emp_df$pred.class ,positive = "Yes")

# binarize data
#charcolumns <- names(emp_df[sapply(emp_df, is.character)])
#for (colname in charcolumns) {
#  print(paste(colname,length(unique(emp_df[,colname]))))
#  for (newcol in unique(emp_df[,colname])) {
#    if (!is.na(newcol))
#      emp_df[,paste0(colname,"_",newcol)] <- ifelse(emp_df[,colname]==newcol,1,0)
#  }
#  emp_df <- emp_df[,setdiff(names(emp_df),colname)]
#}
#str(emp_df)

#splitting the data
#split <- sample(nrow(emp_df), floor(0.5*nrow(emp_df)))
#traindf <- emp_df[split,]
#testdf <-  emp_df[-split,]

#str(testdf)

#model <- naiveBayes(Attrition~., data = traindf)
#class(model) 
#pred <- predict(model,testdf)
#table(pred)
