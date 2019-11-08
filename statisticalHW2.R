
#Redion Xhepa and Anurag Das  #Matriculation Numbers
#Elements of Statistrical Learning HW2
#load the library
library ( ISLR )
#retrieve the dataset
fix(Auto)
?Auto

#part 1
plot(unname(unlist(Auto["cylinders"])),unname(unlist(Auto["mpg"])), main = "Scatter plot",
     xlab = "data point", ylab = "level",frame = FALSE,)
