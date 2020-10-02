#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
View(iris)
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length
versicolor<-filter(iris, iris$Species == "versicolor")
x<-c(versicolor[1], versicolor[3], versicolor[1])
y<-c(versicolor[2], versicolor[4], versicolor[3])
reg<- list()
for (i in 1:3) {reg[[i]]<-lm(y[[i]]~x[[i]])}

#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
					Height.cm = c(60,100,11.8))

left_join(iris,height)

#####################################
##### Part 3: plots in ggplot2  #####
#####################################
#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
ggplot(iris,aes(x=Sepal.Length, y=Sepal.Width)) + geom_point(aes(color = "red")) +coord_cartesian() 

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
ggplot(iris,aes(x=Sepal.Length, y=Sepal.Width)) + geom_point(aes(color = "red")) +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"), panel.background = element_blank())

#3c.make a scatter plot with ggplot and get rid of grid lines,
#show species by color, and increase the point size
ggplot(iris,aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point(aes(color= Species, size = 2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"), panel.background = element_blank())

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################

#the commands in ggplot create a blank plot for a data frame (iris) and then the variables are added using the aes() command
#whereas in the plot function, you give the variables using the $ rather than giving the data frame and the variables seperately