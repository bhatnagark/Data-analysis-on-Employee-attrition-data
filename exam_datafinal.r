library(caret)
library(gbm)
library(mlbench)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(psych)
#library(plotly)

emp_df <- data.frame(Employeeattrition)
str(emp_df)
emp_df<-emp_df[,c(1,3:35,2)]
str(emp_df)
character_vars <- lapply(emp_df, class) == "character"
#str((emp_df))
emp_df[, character_vars] <- lapply(emp_df[, character_vars], as.factor)
str(emp_df)
fit<-rpart(Attrition~.,data=emp_df)
varimp<-data.frame(varImp(fit))
print(varimp)
varimp<-varimp[order(varimp$Overall),]
#print(varimp)


y0<-ggplot(emp_df, aes(JobRole))+
  geom_bar(aes(fill=Attrition),position = "dodge")+
  xlab('EMP satisfaction') + ylab('COUNT') + ggtitle('EMP ATTRITION')+ scale_y_continuous(breaks = seq(0,400,by=50))
#plot_y0<-plotly(y0)
print(y0)


y1<-ggplot(emp_df, aes(x=Attrition,y=MonthlyIncome))+
  geom_boxplot(aes(color=Attrition))
print(y1)

y2<-ggplot(emp_df, aes(x=JobRole,y=MonthlyIncome), alpha=0.5)+
  geom_boxplot(aes(fill=Attrition))+ scale_y_continuous(breaks = seq(0,20000,by=1000))#+ geom_smooth()
print(y2)

y3<-ggplot(emp_df, aes(x=TotalWorkingYears,y=MonthlyIncome), alpha=0.5)+
  geom_point(aes(color=Attrition))+ geom_smooth() #+ scale_y_continuous(breaks = seq(0,20000,by=1000))
print(y3)


