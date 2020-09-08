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
#get info about data frame
str(datw)
#reformat date
datw$dateF <- as.Date(datw$DATE, "%Y-%m-%d")
#add year column
datw$year <-as.numeric(format(datw$dateF, "%y"))

#investigating data modes
#create numeric vector
watervolume_ml<- c(12.3, 54.3, 2.9, 5.0, 38.6)
#create integer vector
speciescount<- c(1,3,3,4,2)
#check mode
typeof(speciescount)
#force to integer
speciescount<- c(1L,3L,3L,4L,2L)
#check mode again
typeof(speciescount)
#create character vector
sitename<- c("field","stream","woods","yard","hill")
#create factor vector
sex<- c("male","female","female","unknown","female")
#force to factor
sex_factor<- as.factor(sex)
#check mode
class(sex_factor)

#manipulating NOAA data
#find out site names
levels(datw$NAME)
#mean max temp for Aberdeen
mean(datw$TMAX[datw$NAME == "ABERDEEN, WA US"], na.rm = TRUE)
#calculate average temp
datw$TAVE <- datw$TMIN + ((datw$TMAX+datw$TMIN)/2)
#aggregate sites and take mean temp
averagetemp<- aggregate(datw$TAVE, by=list(datw$NAME), FUN="mean", na.rm=TRUE)
averagetemp
#change column names
colnames(averagetemp)<-c("NAME","MAAT")
#numbering location names
datw$siteN<- as.numeric(as.factor(datw$NAME))
hist(datw$TAVE[datw$siteN==1],freq=FALSE,main=paste(levels(datw$NAME)[1]),xlab = "Average Daily Temperature (degrees C)",ylab = "Relative Frequency", col = "grey50",border = "white")
#looking up functions
help("hist")
help("paste")
help("levels")
#add mean line to histogram
hist(datw$TAVE[datw$siteN==1],freq=FALSE, main = paste(levels(datw$NAME[1])), xlab = "Average Daily Temperature (degrees C)",ylab = "Relative Frequency", col = "grey50",border = "white")
abline(v=mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE), col = "tomato3", lwd = 3)
#add standard deviation below mean
abline(v=mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE)-sd(datw$TAVE[datw$siteN == 1], na.rm= TRUE), col = "tomato3", lwd = 3, lty = 3)
#add standard deviatoin above mean
abline(v=mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE)+sd(datw$TAVE[datw$siteN == 1], na.rm= TRUE), col = "tomato3", lwd = 3, lty = 3)
#make histograms for three other sites
#group 4 charts together
par(mfrow=c(2,2))
#copy site 1
hist(datw$TAVE[datw$siteN==1],freq=FALSE, main = paste(levels(datw$NAME[1])), xlab = "Average Daily Temperature (degrees C)",ylab = "Relative Frequency", col = "grey50",border = "white")
abline(v=mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE), col = "tomato3", lwd = 3)
abline(v=mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE)-sd(datw$TAVE[datw$siteN == 1], na.rm= TRUE), col = "tomato3", lwd = 3, lty = 3)
abline(v=mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE)+sd(datw$TAVE[datw$siteN == 1], na.rm= TRUE), col = "tomato3", lwd = 3, lty = 3)
#site 2
hist(datw$TAVE[datw$siteN==2],freq=FALSE, main = paste(levels(datw$NAME[2])), xlab = "Average Daily Temperature (degrees C)",ylab = "Relative Frequency", col = "blue",border = "white")
abline(v=mean(datw$TAVE[datw$siteN == 2], na.rm = TRUE), col = "tomato3", lwd = 3)
abline(v=mean(datw$TAVE[datw$siteN == 2], na.rm = TRUE)-sd(datw$TAVE[datw$siteN == 2], na.rm= TRUE), col = "tomato3", lwd = 3, lty = 3)
abline(v=mean(datw$TAVE[datw$siteN == 2], na.rm = TRUE)+sd(datw$TAVE[datw$siteN == 2], na.rm= TRUE), col = "tomato3", lwd = 3, lty = 3)
#site 3
hist(datw$TAVE[datw$siteN==3],freq=FALSE, main = paste(levels(datw$NAME[3])), xlab = "Average Daily Temperature (degrees C)",ylab = "Relative Frequency", col = "green",border = "white")
abline(v=mean(datw$TAVE[datw$siteN == 3], na.rm = TRUE), col = "tomato3", lwd = 3)
abline(v=mean(datw$TAVE[datw$siteN == 3], na.rm = TRUE)-sd(datw$TAVE[datw$siteN == 3], na.rm= TRUE), col = "tomato3", lwd = 3, lty = 3)
abline(v=mean(datw$TAVE[datw$siteN == 3], na.rm = TRUE)+sd(datw$TAVE[datw$siteN == 3], na.rm= TRUE), col = "tomato3", lwd = 3, lty = 3)
#site 4
hist(datw$TAVE[datw$siteN==4],freq=FALSE, main = paste(levels(datw$NAME[4])), xlab = "Average Daily Temperature (degrees C)",ylab = "Relative Frequency", col = "black",border = "white")
abline(v=mean(datw$TAVE[datw$siteN == 4], na.rm = TRUE), col = "tomato3", lwd = 3)
abline(v=mean(datw$TAVE[datw$siteN == 4], na.rm = TRUE)-sd(datw$TAVE[datw$siteN == 4], na.rm= TRUE), col = "tomato3", lwd = 3, lty = 3)
abline(v=mean(datw$TAVE[datw$siteN == 4], na.rm = TRUE)+sd(datw$TAVE[datw$siteN == 4], na.rm= TRUE), col = "tomato3", lwd = 3, lty = 3)

#probability distribution
par(mfrow=c(1,1))
h1 <- hist(datw$TAVE[datw$siteN == 1],
           freq=FALSE, 
           main = paste(levels(datw$NAME)[1]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")
x.plot <- seq(-10,30, length.out = 100)
y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datw$TAVE[datw$siteN == 1],na.rm=TRUE),
                 sd(datw$TAVE[datw$siteN == 1],na.rm=TRUE))
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot
points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)
#finding probability that daily average temp is below freezing
help(dnorm)
pnorm(0, mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE), sd(datw$TAVE[datw$siteN == 1], na.rm = TRUE))
#finding probability that daily average temp is below 5 degrees
pnorm(5, mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE), sd(datw$TAVE[datw$siteN == 1], na.rm = TRUE))
#finding probability that daily average temp is between 0 and 5 degrees
pnorm(5, mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE), sd(datw$TAVE[datw$siteN == 1], na.rm = TRUE))-pnorm(0, mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE), sd(datw$TAVE[datw$siteN == 1], na.rm = TRUE))
#finding probability that daily average temp is above 20 degrees
1 - pnorm(20, mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE), sd(datw$TAVE[datw$siteN == 1], na.rm = TRUE))
#finding the temp above which ther is only a 5% chance of occurring
qnorm(0.95, mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE), sd(datw$TAVE[datw$siteN == 1], na.rm = TRUE))
#question 6: 4 degree increase in mean, what is probability above current high extreme
1 - pnorm(qnorm(0.95, mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE), sd(datw$TAVE[datw$siteN == 1], na.rm = TRUE)), mean(datw$TAVE[datw$siteN == 1] + 4, na.rm = TRUE), sd(datw$TAVE[datw$siteN == 1], na.rm = TRUE))

#Evaluating Precipitation Data
#Question 7 daily precip for Aberdeen histogram
aberdeen_precip_hist<-hist(datw$PRCP[datw$siteN == 1], freq=FALSE, main = paste(levels(datw$NAME[4])), xlab = "Daily Precipitation (cm)",ylab = "Relative Frequency", col = "black",border = "white")
#Question 8 yearly precipitation
annualprecip<- aggregate(datw$PRCP, by=list(datw$NAME,datw$year), FUN="sum", na.rm = TRUE)
aberdeen_annual_precip_hist<-hist(annualprecip$x[annualprecip$Group.1 == "ABERDEEN, WA US"], freq=FALSE, main = paste(levels(datw$NAME[4])), xlab = "Annual Precipitation (cm)",ylab = "Relative Frequency", col = "grey50",border = "white")
livermore_annual_precip_hist<-hist(annualprecip$x[annualprecip$Group.1 == "LIVERMORE, CA US"], freq=FALSE, main = paste(levels(datw$NAME[4])), xlab = "Annual Precipitation (cm)",ylab = "Relative Frequency", col = "grey50",border = "white")
morrisville_annual_precip_hist<-hist(annualprecip$x[annualprecip$Group.1 == "MORRISVILLE 6 SW, NY US"], freq=FALSE, main = paste(levels(datw$NAME[4])), xlab = "Annual Precipitation (cm)",ylab = "Relative Frequency", col = "grey50",border = "white")
mormonflat_annual_precip_hist<-hist(annualprecip$x[annualprecip$Group.1 == "MORMON FLAT, AZ US"], freq=FALSE, main = paste(levels(datw$NAME[4])), xlab = "Annual Precipitation (cm)",ylab = "Relative Frequency", col = "grey50",border = "white")
#mean annual precip
meanannualprecip<- aggregate(annualprecip$x, by=list(annualprecip$Group.1), FUN="mean", na.rm = TRUE)
