#Activity 2
#read in noaa data
datw<- read.csv("data/noaa_weather/2011124.csv", stringsAsFactors = TRUE)
#get info about data frame
str(datw)
#Question 1- there are 157,849 rows and 9 columns
#reformat date
datw$dateF <- as.Date(datw$DATE, "%Y-%m-%d")
#add year column
datw$year <-as.numeric(format(datw$dateF, "%y"))

#Question 2- investigating data modes
#create numeric vector
watervolume_ml<- c(12.3, 54.3, 2.9, 5.0, 38.6)
#create integer vector
speciescount<- c(1,3,3,4,2)
#force to integer
speciescount<- as.integer(speciescount)
#create character vector
sitename<- c("field","stream","woods","yard","hill")
#create factor vector
sex<- c("male","female","female","unknown","female")
#force to factor
sex<- as.factor(sex)

#manipulating NOAA data
#find out site names
levels(datw$NAME)
#mean max temp for Aberdeen
mean(datw$TMAX[datw$NAME == "ABERDEEN, WA US"], na.rm = TRUE)
#calculate average temp
datw$TAVE <- datw$TMIN + ((datw$TMAX-datw$TMIN)/2)
#aggregate sites and take mean temp
averagetemp<- aggregate(datw$TAVE, by=list(datw$NAME), FUN="mean", na.rm=TRUE)
#change column names
colnames(averagetemp)<-c("NAME","MAAT")
#numbering location names
datw$siteN<- as.numeric(as.factor(datw$NAME))
#make a histogram for Aberdeen
hist(datw$TAVE[datw$siteN==1],freq=FALSE,main= paste(levels(datw$NAME)[1]),xlab = "Average Daily Temperature (degrees C)",ylab = "Relative Frequency", col = "grey50",border = "white")

#Question 3- look up and describe functions
#looking up functions
help("hist")
help("paste")
help("levels")

#add mean line to histogram
hist(datw$TAVE[datw$siteN==1],freq=FALSE, main= paste(levels(datw$NAME)[1]), xlab = "Average Daily Temperature (degrees C)",ylab = "Relative Frequency", col = "grey50",border = "white")
abline(v=mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE), col = "tomato3", lwd = 3)
#add standard deviation below mean
abline(v=mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE)-sd(datw$TAVE[datw$siteN == 1], na.rm= TRUE), col = "tomato3", lwd = 3, lty = 3)
#add standard deviation above mean
abline(v=mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE)+sd(datw$TAVE[datw$siteN == 1], na.rm= TRUE), col = "tomato3", lwd = 3, lty = 3)

#Question 4- make histograms for three other sites
#group 4 charts together
par(mfrow=c(2,2))
#copy site 1
hist(datw$TAVE[datw$siteN==1],freq=FALSE, main= paste(levels(datw$NAME)[1]), xlab = "Average Daily Temperature (degrees C)",ylab = "Relative Frequency", col = "grey50",border = "white")
abline(v=mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE), col = "tomato3", lwd = 3)
abline(v=mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE)-sd(datw$TAVE[datw$siteN == 1], na.rm= TRUE), col = "tomato3", lwd = 3, lty = 3)
abline(v=mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE)+sd(datw$TAVE[datw$siteN == 1], na.rm= TRUE), col = "tomato3", lwd = 3, lty = 3)
#site 2
hist(datw$TAVE[datw$siteN==2],freq=FALSE, main = paste(levels(datw$NAME)[2]), xlab = "Average Daily Temperature (degrees C)",ylab = "Relative Frequency", col = "blue",border = "white")
abline(v=mean(datw$TAVE[datw$siteN == 2], na.rm = TRUE), col = "tomato3", lwd = 3)
abline(v=mean(datw$TAVE[datw$siteN == 2], na.rm = TRUE)-sd(datw$TAVE[datw$siteN == 2], na.rm= TRUE), col = "tomato3", lwd = 3, lty = 3)
abline(v=mean(datw$TAVE[datw$siteN == 2], na.rm = TRUE)+sd(datw$TAVE[datw$siteN == 2], na.rm= TRUE), col = "tomato3", lwd = 3, lty = 3)
#site 3
hist(datw$TAVE[datw$siteN==3],freq=FALSE, main= paste(levels(datw$NAME)[3]), xlab = "Average Daily Temperature (degrees C)",ylab = "Relative Frequency", col = "green",border = "white")
abline(v=mean(datw$TAVE[datw$siteN == 3], na.rm = TRUE), col = "tomato3", lwd = 3)
abline(v=mean(datw$TAVE[datw$siteN == 3], na.rm = TRUE)-sd(datw$TAVE[datw$siteN == 3], na.rm= TRUE), col = "tomato3", lwd = 3, lty = 3)
abline(v=mean(datw$TAVE[datw$siteN == 3], na.rm = TRUE)+sd(datw$TAVE[datw$siteN == 3], na.rm= TRUE), col = "tomato3", lwd = 3, lty = 3)
#site 4
hist(datw$TAVE[datw$siteN==4],freq=FALSE, main= paste(levels(datw$NAME)[4]), xlab = "Average Daily Temperature (degrees C)",ylab = "Relative Frequency", col = "black",border = "white")
abline(v=mean(datw$TAVE[datw$siteN == 4], na.rm = TRUE), col = "tomato3", lwd = 3)
abline(v=mean(datw$TAVE[datw$siteN == 4], na.rm = TRUE)-sd(datw$TAVE[datw$siteN == 4], na.rm= TRUE), col = "tomato3", lwd = 3, lty = 3)
abline(v=mean(datw$TAVE[datw$siteN == 4], na.rm = TRUE)+sd(datw$TAVE[datw$siteN == 4], na.rm= TRUE), col = "tomato3", lwd = 3, lty = 3)

#probability distribution
#get just one plot per frame
par(mfrow=c(1,1))
h1 <- hist(datw$TAVE[datw$siteN == 1],
           freq=FALSE, 
           main= paste(levels(datw$NAME)[1]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")
#plot the normal distribution
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

#calculating probability
#finding probability that daily average temp is below freezing
pnorm(0, mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE), sd(datw$TAVE[datw$siteN == 1], na.rm = TRUE))
#finding probability that daily average temp is below 5 degrees
pnorm(5, mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE), sd(datw$TAVE[datw$siteN == 1], na.rm = TRUE))
#finding probability that daily average temp is between 0 and 5 degrees
pnorm(5, mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE), sd(datw$TAVE[datw$siteN == 1], na.rm = TRUE))-pnorm(0, mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE), sd(datw$TAVE[datw$siteN == 1], na.rm = TRUE))
#finding probability that daily average temp is above 20 degrees
1 - pnorm(20, mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE), sd(datw$TAVE[datw$siteN == 1], na.rm = TRUE))
#finding the temp above which there is only a 5% chance of occurring
qnorm(0.95, mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE), sd(datw$TAVE[datw$siteN == 1], na.rm = TRUE))

#Question 6- What is the probability of current extreme temperatures if there is a 4 degree increase in mean
1 - pnorm(qnorm(0.95, mean(datw$TAVE[datw$siteN == 1], na.rm = TRUE), sd(datw$TAVE[datw$siteN == 1], na.rm = TRUE)), mean(datw$TAVE[datw$siteN == 1] + 4, na.rm = TRUE), sd(datw$TAVE[datw$siteN == 1], na.rm = TRUE))

#Evaluating Precipitation Data
#Question 7- daily precip for Aberdeen histogram
aberdeen_precip_hist<-hist(datw$PRCP[datw$siteN == 1], freq=FALSE, main= paste(levels(datw$NAME)[1]), xlab = "Daily Precipitation (cm)",ylab = "Relative Frequency", col = "black",border = "white", xlim = c(0,60))

#Question 8- annual precipitation for each year and site
#create new dataframe
annualprecip<- aggregate(datw$PRCP, by=list(datw$NAME,datw$year), FUN="sum", na.rm = TRUE)
#rename columns
colnames(annualprecip)<-c("NAME","year","Annual_Precipitation")
#create histogram for annual precipitation for Aberdeen
aberdeen_annual_precip_hist<-hist(annualprecip$Annual_Precipitation[annualprecip$NAME == "ABERDEEN, WA US"], freq=FALSE, main = "ABERDEEN, WA US", xlab = "Annual Precipitation (cm)",ylab = "Relative Frequency", col = "grey50",border = "white")
#create histograms for other sites to observe normality
livermore_annual_precip_hist<-hist(annualprecip$Annual_Precipitation[annualprecip$NAME == "LIVERMORE, CA US"], freq=FALSE, main = "LIVERMORE, CA US", xlab = "Annual Precipitation (cm)",ylab = "Relative Frequency", col = "grey50",border = "white")
morrisville_annual_precip_hist<-hist(annualprecip$Annual_Precipitation[annualprecip$NAME == "MORRISVILLE 6 SW, NY US"], freq=FALSE, main = "MORRISVILLE 6 SW, NY US", xlab = "Annual Precipitation (cm)",ylab = "Relative Frequency", col = "grey50",border = "white")
mormonflat_annual_precip_hist<-hist(annualprecip$Annual_Precipitation[annualprecip$NAME == "MORMON FLAT, AZ US"], freq=FALSE, main = "MORMON FLAT, AZ US", xlab = "Annual Precipitation (cm)",ylab = "Relative Frequency", col = "grey50",border = "white")
mandan_annual_precip_hist<-hist(annualprecip$Annual_Precipitation[annualprecip$NAME == "MANDAN EXPERIMENT STATION, ND US"], freq=FALSE, main = "MANDAN EXPERIMENT STATION, ND US", xlab = "Annual Precipitation (cm)",ylab = "Relative Frequency", col = "grey50",border = "white")

#Question 9- get mean annual precip for each site
meanannualprecip<- aggregate(annualprecip$Annual_Precipitation, by=list(annualprecip$NAME), FUN="mean", na.rm = TRUE)
#rename columns
colnames(meanannualprecip)<-c("NAME","Mean_Annual_Precipitation")
