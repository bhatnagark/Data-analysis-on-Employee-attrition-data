library(caret)
library(gbm)
emp_df <- data.frame('Employeeattrition')
#str(emp_df)
# split data set into train and test portion
set.seed(1234)
splitIndex <- sample(nrow(emp_df), floor(0.5*nrow(emp_df)))
trainDF <- emp_df[ splitIndex,]
testDF  <- emp_df[-splitIndex,]

outcomeName <- 'Attrition'
predictorNames <- setdiff(names(trainDF),outcomeName)

# transform outcome variable to text as this is required in caret for classification 
trainDF[,outcomeName] <- ifelse(trainDF[,outcomeName]==1,'yes','nope')

# create caret trainControl object to control the number of cross-validations performed
objControl <- trainControl(method='cv', number=2, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)

objGBM <- train(trainDF[,predictorNames],  as.factor(trainDF[,outcomeName]),
                method='gbm',
                trControl=objControl,
                metric = "ROC",
                tuneGrid = expand.grid(n.trees = 5, interaction.depth = 3, shrinkage = 0.1)
)

predictions <- predict(object=objGBM, testDF[,predictorNames], type='prob')

GetROC_AUC = function(probs, true_Y){
  # AUC approximation
  probsSort = sort(probs, decreasing = TRUE, index.return = TRUE)
  val = unlist(probsSort$x)
  idx = unlist(probsSort$ix) 
  
  roc_y = true_Y[idx];
  stack_x = cumsum(roc_y == 0)/sum(roc_y == 0)
  stack_y = cumsum(roc_y == 1)/sum(roc_y == 1)   
  
  auc = sum((stack_x[2:length(roc_y)]-stack_x[1:length(roc_y)-1])*stack_y[2:length(roc_y)])
  return(auc)
}

refAUC <- GetROC_AUC(predictions[[2]],testDF[,outcomeName])
print(paste('AUC score:', refAUC))

# Shuffle predictions for variable importance
AUCShuffle <- NULL
shuffletimes <- 500

featuresMeanAUCs <- c()
for (feature in predictorNames) {
  featureAUCs <- c()
  shuffledData <- testDF[,predictorNames]
  for (iter in 1:shuffletimes) {
    shuffledData[,feature] <- sample(shuffledData[,feature], length(shuffledData[,feature]))
    predictions <- predict(object=objGBM, shuffledData[,predictorNames], type='prob')
    featureAUCs <- c(featureAUCs,GetROC_AUC(predictions[[2]], testDF[,outcomeName]))
  }
  featuresMeanAUCs <- c(featuresMeanAUCs, mean(featureAUCs < refAUC))
}
AUCShuffle <- data.frame('feature'=predictorNames, 'importance'=featuresMeanAUCs)
AUCShuffle <- AUCShuffle[order(AUCShuffle$importance, decreasing=TRUE),]
print(AUCShuffle)