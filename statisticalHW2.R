
#Redion Xhepa and Anurag Das  #Matriculation Numbers
#Elements of Statistrical Learning HW2
#load the library
library ( ISLR )
#retrieve the dataset
fix(Auto)
?Auto

#part 1
print("Part 1 of Question 4")
pdf("plotsHw2Part4_1.pdf") 

#mpg vs remaining
plot(unname(unlist(Auto["cylinders"])),unname(unlist(Auto["mpg"])), main = "Scatter plot",
     xlab = "cylinders", ylab = "mpg",frame = FALSE,)
plot(unname(unlist(Auto["displacement"])),unname(unlist(Auto["mpg"])), main = "Scatter plot",
     xlab = "displacement", ylab = "mpg",frame = FALSE,)
plot(unname(unlist(Auto["horsepower"])),unname(unlist(Auto["mpg"])), main = "Scatter plot",
     xlab = "horsepower", ylab = "mpg",frame = FALSE,)
plot(unname(unlist(Auto["weight"])),unname(unlist(Auto["mpg"])), main = "Scatter plot",
     xlab = "weight", ylab = "mpg",frame = FALSE,)
plot(unname(unlist(Auto["acceleration"])),unname(unlist(Auto["mpg"])), main = "Scatter plot",
     xlab = "acceleration", ylab = "mpg",frame = FALSE,)
plot(unname(unlist(Auto["year"])),unname(unlist(Auto["mpg"])), main = "Scatter plot",
     xlab = "year", ylab = "mpg",frame = FALSE,)
plot(unname(unlist(Auto["origin"])),unname(unlist(Auto["mpg"])), main = "Scatter plot",
     xlab = "origin", ylab = "mpg",frame = FALSE,)
#cylinders vs remaining
plot(unname(unlist(Auto["displacement"])),unname(unlist(Auto["cylinders"])), main = "Scatter plot",
     xlab = "displacement", ylab = "cylinders",frame = FALSE,)
plot(unname(unlist(Auto["horsepower"])),unname(unlist(Auto["cylinders"])), main = "Scatter plot",
     xlab = "horsepower", ylab = "cylinders",frame = FALSE,)
plot(unname(unlist(Auto["weight"])),unname(unlist(Auto["cylinders"])), main = "Scatter plot",
     xlab = "weight", ylab = "cylinders",frame = FALSE,)
plot(unname(unlist(Auto["acceleration"])),unname(unlist(Auto["cylinders"])), main = "Scatter plot",
     xlab = "acceleration", ylab = "cylinders",frame = FALSE,)
plot(unname(unlist(Auto["year"])),unname(unlist(Auto["cylinders"])), main = "Scatter plot",
     xlab = "year", ylab = "cylinders",frame = FALSE,)
plot(unname(unlist(Auto["origin"])),unname(unlist(Auto["cylinders"])), main = "Scatter plot",
     xlab = "origin", ylab = "cylinders",frame = FALSE,)

#displacement vs remaining
plot(unname(unlist(Auto["horsepower"])),unname(unlist(Auto["displacement"])), main = "Scatter plot",
     xlab = "horsepower", ylab = "displacement",frame = FALSE,)
plot(unname(unlist(Auto["weight"])),unname(unlist(Auto["displacement"])), main = "Scatter plot",
     xlab = "weight", ylab = "displacement",frame = FALSE,)
plot(unname(unlist(Auto["acceleration"])),unname(unlist(Auto["displacement"])), main = "Scatter plot",
     xlab = "acceleration", ylab = "displacement",frame = FALSE,)
plot(unname(unlist(Auto["year"])),unname(unlist(Auto["displacement"])), main = "Scatter plot",
     xlab = "year", ylab = "displacement",frame = FALSE,)
plot(unname(unlist(Auto["origin"])),unname(unlist(Auto["displacement"])), main = "Scatter plot",
     xlab = "origin", ylab = "displacement",frame = FALSE,)

#horsepower vs remaining
plot(unname(unlist(Auto["weight"])),unname(unlist(Auto["horsepower"])), main = "Scatter plot",
     xlab = "weight", ylab = "horsepower",frame = FALSE,)
plot(unname(unlist(Auto["acceleration"])),unname(unlist(Auto["horsepower"])), main = "Scatter plot",
     xlab = "acceleration", ylab = "horsepower",frame = FALSE,)
plot(unname(unlist(Auto["year"])),unname(unlist(Auto["horsepower"])), main = "Scatter plot",
     xlab = "year", ylab = "horsepower",frame = FALSE,)
plot(unname(unlist(Auto["origin"])),unname(unlist(Auto["horsepower"])), main = "Scatter plot",
     xlab = "origin", ylab = "horsepower",frame = FALSE,)

#weight vs remaining
plot(unname(unlist(Auto["acceleration"])),unname(unlist(Auto["weight"])), main = "Scatter plot",
     xlab = "acceleration", ylab = "weight",frame = FALSE,)
plot(unname(unlist(Auto["year"])),unname(unlist(Auto["weight"])), main = "Scatter plot",
     xlab = "year", ylab = "weight",frame = FALSE,)
plot(unname(unlist(Auto["origin"])),unname(unlist(Auto["weight"])), main = "Scatter plot",
     xlab = "origin", ylab = "weight",frame = FALSE,)

#acceleration vs remaining
plot(unname(unlist(Auto["year"])),unname(unlist(Auto["acceleration"])), main = "Scatter plot",
     xlab = "year", ylab = "acceleration",frame = FALSE,)
plot(unname(unlist(Auto["origin"])),unname(unlist(Auto["acceleration"])), main = "Scatter plot",
     xlab = "origin", ylab = "acceleration",frame = FALSE,)

#year vs remaining
plot(unname(unlist(Auto["origin"])),unname(unlist(Auto["year"])), main = "Scatter plot",
     xlab = "origin", ylab = "year",frame = FALSE,)

dev.off() 
print("plots done and save at a pdf file called plotsHw2Part4_1.pdf")

#Question 4 part 2
#perform the correlations
AutoWithoutName=Auto[,1:8] #discard the names column
correlations=cor(AutoWithoutName,AutoWithoutName, use = "everything",
                 method = c("pearson"))
print(correlations)
#Question 4 part 3
print("QUestion 4 part 3")  #One vs one all one vs three ?
print("Linear Model with cylinders,horsepower,year")
linearModel_cylinders=lm(mpg~cylinders+horsepower+year, data=AutoWithoutName)
print(summary(linearModel_cylinders))

print("Linear Model with cylinders")
linearModel_cylinders=lm(mpg~cylinders, data=AutoWithoutName)
print(summary(linearModel_cylinders))

print("Linear Model with horsepower")
linearModel_horsepower=lm(mpg~horsepower, data=AutoWithoutName)
print(summary(linearModel_horsepower))

print("Linear Model with year")
linearModel_year=lm(mpg~year, data=AutoWithoutName)
print(summary(linearModel_year))



#Question 4 part 4
print("QUestion 4 part 4")
linearModel_multi=lm(mpg~., data=AutoWithoutName)
print(summary(linearModel_multi))


# #Question 4 part 5
# print("QUestion 4 part 5")
#  plot ( AutoWithoutName[] ,)
#  abline (linearModel_multi)
# 
#  par(mfrow=c(2,2))
#  plot( AutoWithoutName["mpg"], AutoWithoutName["mpg"] )
#  abline(lm(wt ~ mpg, mtcars))
#  plot(mtcars$disp,mtcars$wt)
#  abline(lm(wt ~ disp, mtcars))


#Question 4 part 5
print("Question 4 part 5")
model_cyl_weight_logX=lm(mpg~cylinders*weight+I(log(displacement)), data=AutoWithoutName)
print("done A")
model_cyl_year_squareRootX=lm(mpg~cylinders*year+I(sqrt(displacement)), data=AutoWithoutName)
print("done B")
model_weight_year_squareX=lm(mpg~weight*year+I(displacement^2), data=AutoWithoutName)
print("done C")
print(model_cyl_year_squareRootX)

