

#Redion Xhepa and Anurag Das  #remember to put group and stuff like that
#Elements of Statistrical Learning HW1

#library for KNN
library(FNN)

#Presume that the data is downloaded and it placed at the local directory
datasetOzone=load("ozone.RData")
# 
# #Inspect the structure of the objects
print("Question 4.2")
print("Inspecting the structure of the  data")
print("*************************************")
print("*************************************")
print("The names of the parts included in the ozone dataset")
print(datasetOzone)
#check the types
cat("Ozone is type of ",typeof(ozone),"\n")
cat("Testset is type of ",typeof(testset),"\n")
cat("Trainset is type of ",typeof(trainset),"\n")

#  #create the dataset
print("**************************")
print("**************************")
 training_data=ozone[trainset,]
 test_data=ozone[testset,]
print("Training and test sets created")

# # ls() method used
print("**************************")
print("**************************")
  print("Memory contents for  training set objects")
  print(ls(training_data))
  print("Memory contents for  test set objects")
  print(ls(test_data))

# # # #str() method
  print("**************************")
  print("**************************")
 print("Display internal structure of the particular objects in training set")
 str(training_data)
 print("Display internal structure of the particular objects in test set")
 str(test_data)

# # # #summary() method
 print("**************************")
 print("**************************")
 print("Display summary and mean of the training set")
 print(summary(training_data))
 print("Display summary and mean of the test set")
 print(summary(test_data))
 
# #  #dim() method  
 print("**************************")
 print("**************************")
print("Dimensions (# of rows/cols) for training set")
  print(dim(training_data))
  print("Dimensions (# of rows/cols) for test set")
   print(dim(test_data))
 
# 
# # # #length() method 
  print("**************************")
  print("**************************")
  print("length of objects of training set")
  print(length(training_data))
  print("length of objects of test set")
  print(length(test_data))
  
  
# # # #range() method for ozone
 print("**************************")
 print("**************************")
 print("Range of the values,min and max respectively in training set")
 print(range(training_data))
 print("Range of the values,min and max respectively in test set")
 print(range(test_data))


 print("*************************************")
 print("*************************************")
  print("Question 4.3")
 #apply the same function to each variable 
 stds=apply(ozone[2:4],2,sd)
 means=apply(ozone[2:4],2,mean)
 ranges=apply(ozone[2:4],2,range)
 print("Respective means for each input variable(whole dataset considered):")
 print(means)
 print("Respective stds for each input variable(whole dataset considered):")
print(stds)
print("Respective ranges for each input variable(whole dataset considered):")
print(ranges)

 print("*************************************")
 print("*************************************")
#  print("Question 4.4")
# par(mfrow=c(2,2))
 pdf("plotsPart4_4.pdf") 
   print("plot1") #debuggin 
  plot(unname(unlist(ozone["radiation"])),unname(unlist(ozone["ozone"])), main = "Ozone vs Radiation",
       xlab = "Radiation level", ylab = "Ozone level",frame = TRUE)
  print("plot2") #debuggin 
 plot(unname(unlist(ozone["temperature"])),unname(unlist(ozone["ozone"])),main = "Ozone vs Temperature",
      xlab = "Temperature level", ylab = "Ozone level",frame = TRUE)
 print("plot3") #debuggin 
  plot(unname(unlist(ozone["wind"])),unname(unlist(ozone["ozone"])),  main = "Ozone  vs Wind",
       xlab = "Wind level", ylab = "Ozone level",frame = TRUE)
  print("plot4") #debuggin 
  plot(unname(unlist(ozone["temperature"])),unname(unlist(ozone["wind"])),  main = "Wind vs Temperature",
       xlab = "Temperature level", ylab = "Wind level",frame = TRUE)
  print("plot5") #debuggin 
  plot(unname(unlist(ozone["radiation"])),unname(unlist(ozone["wind"])),  main = "Wind vs Radiation",
       xlab = "Radiation Level", ylab = "Wind level",frame = TRUE)
  print("plot 6")
  plot(unname(unlist(ozone["radiation"])),unname(unlist(ozone["temperature"])),  main = "Temperature vs Radiation",
       xlab = "Radiation Level", ylab = "Temperature level",frame = TRUE)
   dev.off() 
   print("plots done and save at a pdf file called plotsPart4_4.pdf")
  
 
  print("Correlation")
#  #converts the list-like object to a numeric matrix(can be used)
  dataMatrix2=matrix(unlist(ozone)[!is.na(unlist(ozone))], ncol = 4, byrow = F)
  correlations=cor(ozone,ozone, use = "everything",
      method = c("pearson"))
  print("Correlations displayed:")
  print(correlations)
  print("Range of the correlations")
  print(range(correlations))
  
 print("*************************************")
 print("*************************************")
  print("Question 4.5")
# #  #testing it 
   vec1=c(1,2,3)
  vec2=c(1,1,1)
   rss = function(vector_predicted,vector_true){
      return (sum(((vector_predicted-vector_true)^2)))
   }
  print("Testing purpousese,vector 1 and vector 2 displayed below")
  print("Test vector 1")
  print(vec1)
  print("Test Vector 2")
  print(vec2)
  print("RSS")
  print(rss(vec1,vec2))
   
 print("*************************************")
 print("*************************************")
  print("Question 4.6")

#  # #train the model
#  #             #at 0 u have the ozone(the y variable),(1,2,3 are the respective features radiation,temperature and wind)
linearModel=lm(ozone~radiation+temperature+wind, data=training_data)
print("The trained linear model")
print(linearModel)

# #test the model
 predictions=predict(linearModel, newdata =test_data)
 print("Predictions,testing the model")
 print(predictions)
# 
# #calculate the RSS  (do we have to do it for training data ?????)
#                  #predictions        ,#ozone data from the training data
RSS_test =rss(predictions,unname(unlist(test_data["ozone"])))
 print("Rss for the test data ")
 print(RSS_test)
# #perform correlation
 correlations=cor(predictions,unname(unlist(test_data["ozone"])), use = "everything",
                  method = c("pearson"))
 print("Pearson correlation")
 print(correlations)
 pdf("plotsPart4_6.pdf") 
 ##the scatter plot 
 plot(seq(1,31,1),predictions, main = "Scatter plot",
      xlab = "data point", ylab = "level",frame = FALSE,)
 points(seq(1,31,1),unname(unlist(test_data["ozone"])),col="red")
 # add a legend 
 legend(10, 10, legend=c("Predictions", "True Values"),
        col=c("black", "red"), lty=1:2, cex=0.8)
 dev.off() 


print("*************************************")
 print("*************************************")
print("Question 4.7")

RSS_final_test = vector()
RSS_final_train = vector()

for (i in 1:30) {
     predictions_test = FNN::knn.reg(train=training_data[,2:4], test = test_data[,2:4], y = training_data[,1], k = i)
     RSS_final_test[i] = rss(predictions_test$pred, unname(unlist(test_data[1])))
     predictions_train = FNN::knn.reg(train=training_data[,2:4], test = training_data[,2:4], y = training_data[,1], k = i)
     RSS_final_train[i] = rss(predictions_train$pred, unname(unlist(training_data[1])))
}

plot(seq(1,30,1),RSS_final_train, main = "Scatter plot for RSS in Train vs Test time",
     xlab = "'k' neighbors", ylab = "RSS",frame = FALSE,)
points(seq(1,30,1),RSS_final_test,col="red")




 
 

 

