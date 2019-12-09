#Anurag Das - 2577859
#Redion Xhepa -2581221
#Tutorial 1, Monday
##Presume that the data is downloaded and it placed at the local directory
library(stringr)
library ( MASS )
library(boot)
library (glmnet) # needed for the ridge regression
datasetAll=read.table("prostate.txt")

set.seed (10)  #so the grader can check the results he can change the seed value

print("Part 1")
#standatize each feature vector(column)
standartize <- function(columnFeature) {
  std <-sd(columnFeature)
  mean <-mean(columnFeature)
  standartized <- (columnFeature-mean)/std
  return(standartized)
}
#apply the normalization function to each featue vector
normalizedDataSet<-as.data.frame(apply(datasetAll[1:8], 2, standartize))
normalizedDataSet$lpsa <- datasetAll[,9] #append it to the dataframe,we had discard it earlier
#divide into training and test data
trainData=normalizedDataSet[str_detect(unlist(datasetAll["train"]), "TRUE"),]
testData=normalizedDataSet[str_detect(unlist(datasetAll["train"]), "FALSE"),]

print("Part 2")
print("*************************************")
print("With One Out Leave Cross Validation")
LOOCLinearRegression=glm (lpsa ~.,data = trainData )
errorLOOC= cv.glm(trainData ,LOOCLinearRegression) $delta [1]
print("error LOOC")
print(errorLOOC)
print("*************************************")
print("*************************************")
print("for k=5")
LinearRegressionK_5 = glm (lpsa ~., data = trainData )
errorK5= cv.glm(trainData ,LinearRegressionK_5, K =5) $delta [1]
print("error K=5:")
print(errorK5)
print("*************************************")
print("*************************************")
print("for k=10")
LinearRegressionK_5 = glm (lpsa ~., data = trainData )
errorK10= cv.glm(trainData ,LinearRegressionK_5, K =10) $delta [1]
print("error K=10:")
print(errorK10)







