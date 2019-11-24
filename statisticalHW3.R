
#Anurag Das
#Redion Xhepa
##Presume that the data is downloaded and it placed at the local directory
library(stringr)
library ( MASS )
#load the dataset
#part 1
print("part 1")
datasetAll=read.csv("phoneme.csv")
#divide into dataset and trainset
#check if the speaker has "train" or "test" substring and slice aacordingly
#remove 1st and 259th,which are irrelevant for our purpouses
trainData=datasetAll[str_detect(unlist(datasetAll["speaker"]), "train"),2:258]
testData=datasetAll[str_detect(unlist(datasetAll["speaker"]), "test"),2:258]

#I kept 258the column since it is required by the lda model

print("summary of train data")
#print(summary(trainData))
print("summary of test data")
#print(summary(testData))

print("part 2")
ldaModel=lda(g~., data=trainData)
print(ldaModel)       
#perform the prediciton on train data
print("perform predictions on train data")
predictionTrain= predict (ldaModel,trainData)$class
#calculate the train error
print(table(predictionTrain,trainData[,257]))

print("perform predictions on test data")
predictionTest= predict (ldaModel,testData)$class
#calculate the test error
#calculate the train error
print(table(predictionTest,testData[,257]))


print("part 3")
print("skiped")

print("part 4")
#select only the g=ao or g=aa
dataPart4=datasetAll[str_detect(unlist(datasetAll["g"]), "aa")|str_detect(unlist(datasetAll["g"]), "ao"),]
#somehow using logical operators I couldnt join this two steps into one ....
#divide into test and train data
Part4Train=dataPart4[str_detect(unlist(dataPart4["speaker"]), "train"),2:258]
Part4Test=dataPart4[str_detect(unlist(dataPart4["speaker"]), "test"),2:258]
#right now the training and testing sets containing only aa and ao is created
#fit the lda model on the new dataset
ldaModelPart4=lda(g~., data=Part4Train)
#check the issue of g


print("part 4 continuation")
#perform the prediciton on train data
print("perform predictions on train data")
predTrainPart4= predict (ldaModelPart4,Part4Train)$class
#calculate the train error
print(table(predTrainPart4,Part4Train[,257]))

print("perform predictions on test data")
predTestPart4= predict (ldaModelPart4,Part4Test)$class
#calculate the test error
#calculate the train error
print(table(predTestPart4,Part4Test[,257]))


print("part 6")


  
