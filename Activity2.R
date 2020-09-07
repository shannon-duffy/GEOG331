#Activity 2 tutorial
#make a vector of tree heights in meters
heights<-c(30,41,20,22)
#convert to cm
heights_cm<-heights*100
#look at first tree height
heights[1]
help(matrix)
#make a matrix
mat<-matrix(c(1,2,3,4,5,6), ncol = 2, byrow = TRUE)
#view matrix
mat
#make a matrix by column instead of row
mat.bycol<-matrix(c(1,2,3,4,5,6), ncol = 2, byrow = FALSE)
#view new matrix
mat.bycol
#look at column 2
mat.bycol[,2]
#look at row 2
mat.bycol[2,]
#read in noaa data
datw<- read.csv("data/noaa_weather/2011124.csv")
