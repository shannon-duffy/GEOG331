#Tutorial
#creating a function
assert<-function(statement, err.message){if(statement == FALSE){print(err.message)}}
assert(1 == 2, "error: unequal values")
assert(2 == 2, "error: unequal values")
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")

#read in data
datW<-read.csv("data/bewkes/bewkes_weather.csv", skip = 3, na.strings = c("#N/A"),header = FALSE)
#make column names
sensorInfo<-read.csv("data/bewkes/bewkes_weather.csv", na.strings = c("#N/A"), nrows = 2)
colnames(datW)<- colnames(sensorInfo)


#QA/QC
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
#quick preview of new date calcualtions
datW[1,]
#see how many values have missing data for each sensor observation
#air temperature
length(which(is.na(datW$air.temperature)))
length(which(is.na(datW$wind.speed)))
length(which(is.na(datW$precipitation)))
length(which(is.na(datW$soil.moisture)))
length(which(is.na(datW$soil.temp)))
#plotting soil moisture over time to find out missing data
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")
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
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)
#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)
#use assert function to explain why lightscale can subset datW
assert(length(lightscale) == length(datW$DD), "error: unequal length")
#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA, ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))
#create a new wind speed column filtering out storm days (2mm rain +lightning or >5mm rain)
datW$wind.speedQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA, ifelse(datW$precipitation > 5, NA, datW$wind.speed))
#use assert function to see if it filtered properly
assert(length(which(is.na(datW$wind.speedQ2))) > 0, "error: no NA values found")
#plot wind speed over time with new wind speed data
plot(datW$DD , datW$wind.speedQ2, xlab = "Day of Year", ylab = "Wind Speed", type = "b", pch=19)

#check soil moisture sensor validity
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

#making a table of averages
avetemp<-sprintf("%.1f",round(mean(datW$air.tempQ2, na.rm = TRUE), digits = 1))
avewind<-round(mean(datW$wind.speedQ2, na.rm = TRUE), digits = 2)
avesoiltemp<-round(mean(datW$soil.temp, na.rm = TRUE), digits = 1)
avesoilmoist<-round(mean(datW$soil.moisture, na.rm = TRUE), digits = 4)
totalprecip<-round(sum(datW$precipitation), digits = 3)
avetable<-data.frame(avetemp, avewind, avesoiltemp, avesoilmoist, totalprecip)
headers<-c("Average Air Temperature", "Average Wind Speed", "Average Soil Temperature", "Average Soil Moisture", "Total Precipitation")
nobs<- c(length(datW$air.tempQ2[!is.na(datW$air.tempQ2)]), length(datW$wind.speedQ2[!is.na(datW$wind.speedQ2)]), length(datW$soil.temp[!is.na(datW$soil.temp)]), length(datW$soil.moisture[!is.na(datW$soil.moisture)]), length(datW$precipitation[!is.na(datW$precipitation)]))
colnames(avetable)<-headers
nobstext<-c("n = 2105", "n = 2105", "n = 1411", "n = 1141", "n = 2188")
avetablenobs<-rbind(avetable,nobstext)