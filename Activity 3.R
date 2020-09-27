#Shannon Duffy
#9/27/2020
#Activity 3

#Question 3: what's the difference between skip and nrows?
#read in data
datW<-read.csv("data/bewkes/bewkes_weather.csv", skip = 3, na.strings = c("#N/A"),header = FALSE)
#make column names
sensorInfo<-read.csv("data/bewkes/bewkes_weather.csv", na.strings = c("#N/A"), nrows = 2)
colnames(datW)<- colnames(sensorInfo)

#use lubridate package to reformat dates
install.packages(c("lubridate"))
library("lubridate")
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")
#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)

#Question 4: Is temp data reliable?
#plor air temp over time to visually check irregularities
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")
#removing temps below freezing with ifelse command
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)
#look at range of temps
quantile(datW$air.tempQ1)
#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]
#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]  

#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy

#Question 5: use assert function to explain why lightscale can subset datW
assert<-function(statement, err.message){if(statement == FALSE){print(err.message)}}
assert(length(lightscale) == length(datW$DD), "error: unequal length")

#create a new wind speed column filtering out storm days (2mm rain +lightning or >5mm rain)
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA, ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

#Question 6: do QA/QC for wind speed
#create a new wind speed column filtering out storm days (2mm rain +lightning or >5mm rain)
datW$wind.speedQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA, ifelse(datW$precipitation > 5, NA, datW$wind.speed))
#use assert function to see if it filtered properly
assert(length(which(is.na(datW$wind.speedQ2))) > 0, "error: no NA values found")
#plot wind speed over time with new wind speed data
plot(datW$DD , datW$wind.speedQ2, xlab = "Day of Year", ylab = "Wind Speed", type = "b", pch=19)

#Question 7: check soil sensor validity
#check soil moisture validity
#find extreme soil moisture values
quantile(datW$soil.moisture, na.rm = TRUE)
#find extreme precip values (excluding time with no rain)
quantile(datW$precipitation[datW$precipitation>0])
#check to see if precip values on extreme soil moisture times (>0.25) are also high
datW[datW$soil.moisture >0.25,]

#check soil temp validity
#find extreme soil temp values
quantile(datW$soil.temp, na.rm = TRUE)
#find extreme air temp values
quantile(datW$air.tempQ2, na.rm = TRUE)
#check to see if air temp values on high extreme soil temp time (>24) are also high
datW[datW$soil.temp > 24,]
#check to see if air temp values on low extreme soil temp times (>10) are also low
datW[datW$soil.temp < 10,]

#Question 8:making a table of averages
#make object for each average rounded to the appropriate decimal
#temp ends in 0 so use sprintf function to keep it
avetemp<-sprintf("%.1f", mean(datW$air.tempQ2, na.rm = TRUE))
avewind<-round(mean(datW$wind.speedQ2, na.rm = TRUE), digits = 2)
avesoiltemp<-round(mean(datW$soil.temp, na.rm = TRUE), digits = 1)
avesoilmoist<-round(mean(datW$soil.moisture, na.rm = TRUE), digits = 4)
totalprecip<-round(sum(datW$precipitation), digits = 3)
#aggregate values in data frame
avetable<-data.frame(avetemp, avewind, avesoiltemp, avesoilmoist, totalprecip)
#create headers
headers<-c("Average Air Temperature", "Average Wind Speed", "Average Soil Temperature", "Average Soil Moisture", "Total Precipitation")
colnames(avetable)<-headers
#count the number of observations for each variable and compile in a vector
nobs<- c(length(datW$air.tempQ2[!is.na(datW$air.tempQ2)]), length(datW$wind.speedQ2[!is.na(datW$wind.speedQ2)]), length(datW$soil.temp[!is.na(datW$soil.temp)]), length(datW$soil.moisture[!is.na(datW$soil.moisture)]), length(datW$precipitation[!is.na(datW$precipitation)]))
#convert number of observations to text with "n = " and compile in a vector
nobstext<-c("n = 2105", "n = 2105", "n = 1411", "n = 1141", "n = 2188")
#add row of number of observations to table of averages in new dataframe
avetablenobs<-rbind(avetable,nobstext)

#Question 9: making plots of weather variables
#group plots together
par(mfrow=c(2,2), mar=c(3,4,0.5,0), mgp=c(1.5,0.5,0))
#air temp
plot(datW$DD , datW$air.tempQ2, xlab = "Day of Year", ylab = "Air Temperature (°C)", type = "l")
#precipitation
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation (mm)", type = "l")
#soil temp
plot(datW$DD , datW$soil.temp, xlab = "Day of Year", ylab = "Soil Temperature (°C)", type = "l")
#soil moisture
plot(datW$DD , datW$soil.moisture, xlab = "Day of Year", ylab = "Soil Moisture (m^3/m^3)", type = "l")



