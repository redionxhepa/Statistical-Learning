
#Redion Xhepa and Anurag
#Elements of Statistrical Learning HW1


#Presume that the data is downloaded and it placed at the local directory
datasetOzone=load("ozone.RData")

#Inspect the structure of the objects
print("Question 4.2")
print("Inspecting the structure of the  data")
print("*************************************")
print("*************************************")
print("The names of the objects included in the ozone dataset")
print(datasetOzone)
#check the types
cat("Ozone is type of ",typeof(ozone),"\n")

cat("Testset is type of ",typeof(testset),"\n")

cat("Trainset is type of ",typeof(trainset),"\n")

#for the ozone data object
print("Memory contents for ozone object")
print(ls(ozone))

# #str() method
print("Display internal structure of the ozon object")
str(ozone)

# #summary() method
 print("Display summary and mean")
 print(summary(ozone))

 #dim() method for ozone
 print("Dimensions (# of rows/cols)")
 print(dim(ozone))
 

 # # ncol() for number of columns  or variables
 print("Number of colums(extra seperate function)")
 print(ncol(ozone))
 #
 # # nrow() for number of rows (observations)
 print("Number of rows(extra seperate function")
 print(nrow(ozone))
 

# #length() method for ozone
 print("length() method")
 print(length(ozone))

# #range() method for ozone
 print("Range of the values,min and max respectively")
 print(range(ozone))

# #range colnames for ozone
 print("Names of the columns(variables)")
 print(colnames(ozone))


# print("*************************************")
# print("*************************************")
# print("Trainset Object ")
#print(ls(testset))

#print("Testset Object ")


# print("*************************************")
# print("*************************************")
 print("Question 4.3")
 #apply the same function to each column
 stds=apply(ozone,2,sd)
 means=apply(ozone,2,mean)
 ranges=apply(ozone,2,range)
 print("Respective means from left to right are:")
 print(means)
 
 print("Respective stds from left to right are:")
 print(stds)
 
 print("Respective ranges from left to right are:")
 print(ranges)
 
 
 # print("*************************************")
 # print("*************************************")
 print("Question 4.4")
par(mfrow=c(2,2))
print("Plot 1")  # for debugging purpouses
 plot(seq(1,nrow(ozone),1),unname(unlist(ozone["ozone"])), main = "Scatter plot of ozone feature",
      xlab = "data point", ylab = "level",frame = FALSE,)
 print("Plot 2")  # for debugging purpouses
 plot(seq(1,nrow(ozone),1),unname(unlist(ozone["radiation"])), main = "Scatter plot of radiation feature",
      xlab = "data point", ylab = "level",frame = FALSE)

 plot(seq(1,nrow(ozone),1),unname(unlist(ozone["temperature"])),main = "Scatter plot of temperature feature",
     xlab = "data point", ylab = "level",frame = FALSE)

 plot(seq(1,nrow(ozone),1),unname(unlist(ozone["wind"])),  main = "Scatter plot of wind feature",
      xlab = "data point", ylab = "level",frame = FALSE)

 print("Correlation")
 #converts the list-like object to a numeric matrix
 dataMatrix2=matrix(unlist(ozone)[!is.na(unlist(ozone))], ncol = 4, byrow = F)
 correlations=cor(dataMatrix2,dataMatrix2, use = "everything",
     method = c("pearson"))
 

 
 # print("*************************************")
 # print("*************************************")
 print("Question 4.5")
 
 #testing it 
 vec1=c(1,2,3)
 vec2=c(1,1,1)
 rss = function(vector_predicted,vector_true){
    return (sum(((vector_predicted-vector_true)^2)))

 }
 print(rss(vec1,vec2))
 
 
 # print("*************************************")
 # print("*************************************")
 print("Question 4.6")
 
 


 
 

 

