#reading in flow data
library(ncdf4)
ncar<-nc_open("coastal-stns-Vol-monthly.updated-Aug2014.nc")
{
  sink("coastal-stns-Vol-monthly.updated-Aug2014-metadata.txt")
  print(ncar)
  sink()
}
time<-ncvar_get(ncar, ncar$dim$time)
station<-ncvar_get(ncar, ncar$dim$station)
flow<-ncvar_get(ncar, ncar$var$FLOW)
#subset flow for Kinshasa 2000-2010
flow.kinshasa<-flow[2,1201:1332]
station_name<-ncvar_get(ncar, ncar$var$stn_name)
river_name<-ncvar_get(ncar, ncar$var$riv_name)
plot(flow.kinshasa, type="l")

#read in tree cover data and convert from m^2 to km^2
coverstats<-read.csv('coverstats.csv')
cover2000<-(coverstats$treecover2000)/100000000
gainstats<-read.csv('gainstats.csv')
gain2000_2019<-(gainstats$gain)/1000000
gain2000_2019/cover2000
#only 0.006% of forest area has been gained from 2000-2019
lossstats<-read.csv('lossstats.csv')
lossstats$Loss<-(lossstats$Loss)/1000000
sum(lossstats$Loss)/cover2000

#read in precip from World Clim
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)
library(tiff)
library(sf)

#set up months to read in
precmonth <- c("2000-01","2000-02","2000-03","2000-04","2000-05","2000-06","2000-07","2000-08","2000-09","2000-10","2000-11","2000-12","2001-01","2001-02","2001-03","2001-04","2001-05","2001-06","2001-07","2001-08","2001-09","2001-10","2001-11","2001-12","2002-01","2002-02","2002-03","2002-04","2002-05","2002-06","2002-07","2002-08","2002-09","2002-10","2002-11","2002-12","2003-01","2003-02","2003-03","2003-04","2003-05","2003-06","2003-07","2003-08","2003-09","2003-10","2003-11","2003-12","2004-01","2004-02","2004-03","2004-04","2004-05","2004-06","2004-07","2004-08","2004-09","2004-10","2004-11","2004-12","2005-01","2005-02","2005-03","2005-04","2005-05","2005-06","2005-07","2005-08","2005-09","2005-10","2005-11","2005-12","2006-01","2006-02","2006-03","2006-04","2006-05","2006-06","2006-07","2006-08","2006-09","2006-10","2006-11","2006-12","2007-01","2007-02","2007-03","2007-04","2007-05","2007-06","2007-07","2007-08","2007-09","2007-10","2007-11","2007-12","2008-01","2008-02","2008-03","2008-04","2008-05","2008-06","2008-07","2008-08","2008-09","2008-10","2008-11","2008-12","2009-01","2009-02","2009-03","2009-04","2009-05","2009-06","2009-07","2009-08","2009-09","2009-10","2009-11","2009-12","2010-01","2010-02","2010-03","2010-04","2010-05","2010-06","2010-07","2010-08","2010-09","2010-10","2010-11","2010-12")

#read all files into a list
precraster <- list() 
for(i in 1:length(precmonth)){
  precraster[[i]] <- raster(paste0("wc2.1_2.5m_prec_2000-2009 (1)/wc2.1_2.5m_prec_",precmonth[i],".tif"))
  
}

#read in boundary shapefile
congo_polygon<-readOGR("congoshape/congoshape.shp")


precextract <- list()
sumprec <- numeric(0)
#loop through all NDVI years
for(i in 1:length(precmonth)){
  #get raster values in the difference polygon
  precextract[[i]] <- extract(precraster[[i]],congo_polygon)[[1]]
  #calculate the mean of the NDVI values
  sumprec[i] <- sum(precextract[[i]], na.rm=TRUE)
}

#create date variable
dateprecmonth <- as.Date(paste(precmonth,"-01",sep=""))
#create year variable
library(lubridate)
Year<-year(dateprecmonth)
#add precip and discharge data to date and year in data frame
congohydro<-data.frame(dateprecmonth,Year,sumprec,flow.kinshasa)
#calculate SDR
congohydro$SDR<-(flow.kinshasa/sumprec)
#aggregate flow and SDR min, max, mean, and standard deviation by year and get CV
library(stats)
flowmin<-stats::aggregate(congohydro$flow.kinshasa, by=list(congohydro$Year), FUN=min)
flowmax<-stats::aggregate(congohydro$flow.kinshasa, by=list(congohydro$Year), FUN=max)
flowmean<-stats::aggregate(congohydro$flow.kinshasa, by=list(congohydro$Year), FUN=mean)
flowsd<-stats::aggregate(congohydro$flow.kinshasa, by=list(congohydro$Year), FUN=sd)
flowcv<-flowmean$x/flowsd$x
sdrmin<-stats::aggregate(congohydro$SDR, by=list(congohydro$Year), FUN=min)
sdrmax<-stats::aggregate(congohydro$SDR, by=list(congohydro$Year), FUN=max)
sdrmean<-stats::aggregate(congohydro$SDR, by=list(congohydro$Year), FUN=mean)
sdrsd<-stats::aggregate(congohydro$SDR, by=list(congohydro$Year), FUN=sd)
sdrcv<-sdrmean$x/sdrsd$x
hydrostats<-data.frame(flowmin$Group.1,flowmin$x,flowmax$x,flowmean$x,flowsd$x,flowcv,sdrmin$x,sdrmax$x,sdrmean$x,sdrsd$x,sdrcv)
colnames(hydrostats)<-c("Year","Minimum Monthly Discharge", "Maximum Monthly Discharge","Mean Monthly Discharge","Monthly Discharge Standard Deviation","Monthly Discharge CV","Minimum Monthly SDR", "Maximum Monthly SDR","Mean Monthly SDR","Monthly SDR Standard Deviation","Monthly SDR CV")

#monthly times series plots
par(mar = c(4,5,3,1))
plot(congohydro$dateprecmonth, congohydro$flow.kinshasa, type = "l", main = "Congo River Monitoring Station at Kinshasa", xlab = "Time", ylab = expression(paste("Monthly Discharge (m"^3,"/s)")))
par(mar = c(4,4,3,1))
plot(congohydro$dateprecmonth, congohydro$SDR, type = "l", main = "Congo River Monitoring Station at Kinshasa", xlab = "Time", ylab = "Monthly Specific Discharge Ratio")
plot(congohydro$dateprecmonth, congohydro$sumprec, type = "l", main = "Congo River Monitoring Station at Kinshasa", xlab = "Time", ylab = "Monthly Rainfall (mm)")

#join deforestation data to flow data
foresthydro<-join(hydrostats, lossstats, by="Year", type="left")

#make 2000 0
foresthydro[is.na(foresthydro)]<-0
#calculate remaining forest
foresthydro$cover<-cover2000-foresthydro$Loss-cumsum(foresthydro$Loss)

#regressions and plots
library(mblm)
par(mar = c(4,5,3,1))
mean.discharge.reg<-mblm(`Mean Monthly Discharge` ~ cover, data = foresthydro)
plot(foresthydro$cover, foresthydro$`Mean Monthly Discharge`, main="Congo River Monitoring Station at Kinshasa", xlab = expression(paste("Forest Cover (km"^2,")")), ylab=expression(paste("Mean Discharge (m"^3,"/s)")))
lines(c(min(foresthydro$cover),max(foresthydro$cover)),c(mean.discharge.reg$fitted.values[which.min(foresthydro$cover)],mean.discharge.reg$fitted.values[which.max(foresthydro$cover)]),col = "red")
min.discharge.reg<-mblm(`Minimum Monthly Discharge` ~ cover, data = foresthydro)
plot(foresthydro$cover, foresthydro$`Minimum Monthly Discharge`, main="Congo River Monitoring Station at Kinshasa", xlab = expression(paste("Forest Cover (km"^2,")")), ylab=expression(paste("Minimum Monthly Discharge (m"^3,"/s)")))
lines(c(min(foresthydro$cover),max(foresthydro$cover)),c(min.discharge.reg$fitted.values[which.min(foresthydro$cover)],min.discharge.reg$fitted.values[which.max(foresthydro$cover)]),col = "red")
max.discharge.reg<-mblm(`Maximum Monthly Discharge` ~ cover, data = foresthydro)
plot(foresthydro$cover, foresthydro$`Maximum Monthly Discharge`, main="Congo River Monitoring Station at Kinshasa", xlab = expression(paste("Forest Cover (km"^2,")")), ylab=expression(paste("Maximum Monthly Discharge (m"^3,"/s)")))
lines(c(min(foresthydro$cover),max(foresthydro$cover)),c(max.discharge.reg$fitted.values[which.min(foresthydro$cover)],max.discharge.reg$fitted.values[which.max(foresthydro$cover)]),col = "red")
discharge.CV.reg<-mblm(`Monthly Discharge CV` ~ cover, data = foresthydro)
plot(foresthydro$cover, foresthydro$`Monthly Discharge CV`, main="Congo River Monitoring Station at Kinshasa", xlab = expression(paste("Forest Cover (km"^2,")")), ylab=expression(paste("Monthly Discharge CV (m"^3,"/s)")))
lines(c(min(foresthydro$cover),max(foresthydro$cover)),c(discharge.CV.reg$fitted.values[which.min(foresthydro$cover)],discharge.CV.reg$fitted.values[which.max(foresthydro$cover)]),col = "red")
mean.SDR.reg<-mblm(`Mean Monthly SDR` ~ cover, data = foresthydro)
plot(foresthydro$cover, foresthydro$`Mean Monthly SDR`, main="Congo River Monitoring Station at Kinshasa", xlab = expression(paste("Forest Cover (km"^2,")")), ylab="Mean SDR")
lines(c(min(foresthydro$cover),max(foresthydro$cover)),c(mean.SDR.reg$fitted.values[which.min(foresthydro$cover)],mean.SDR.reg$fitted.values[which.max(foresthydro$cover)]),col = "red")
min.SDR.reg<-mblm(`Minimum Monthly SDR` ~ cover, data = foresthydro)
plot(foresthydro$cover, foresthydro$`Minimum Monthly SDR`, main="Congo River Monitoring Station at Kinshasa", xlab = expression(paste("Forest Cover (km"^2,")")), ylab="Minimum Monthly SDR")
lines(c(min(foresthydro$cover),max(foresthydro$cover)),c(min.SDR.reg$fitted.values[which.min(foresthydro$cover)],min.SDR.reg$fitted.values[which.max(foresthydro$cover)]),col = "red")
max.SDR.reg<-mblm(`Maximum Monthly SDR` ~ cover, data = foresthydro)
plot(foresthydro$cover, foresthydro$`Maximum Monthly SDR`, main="Congo River Monitoring Station at Kinshasa", xlab = expression(paste("Forest Cover (km"^2,")")), ylab="Maximum Monthly SDR")
lines(c(min(foresthydro$cover),max(foresthydro$cover)),c(max.SDR.reg$fitted.values[which.min(foresthydro$cover)],max.SDR.reg$fitted.values[which.max(foresthydro$cover)]),col = "red")
SDR.CV.reg<-mblm(`Monthly SDR CV` ~ cover, data = foresthydro)
plot(foresthydro$cover, foresthydro$`Monthly SDR CV`, main="Congo River Monitoring Station at Kinshasa", xlab = expression(paste("Forest Cover (km"^2,")")), ylab="Monthly SDR CV")
lines(c(min(foresthydro$cover),max(foresthydro$cover)),c(SDR.CV.reg$fitted.values[which.min(foresthydro$cover)],SDR.CV.reg$fitted.values[which.max(foresthydro$cover)]),col = "red")

summary(mean.discharge.reg)
summary(min.discharge.reg)
summary(max.discharge.reg)
summary(discharge.CV.reg)
summary(mean.SDR.reg)
summary(min.SDR.reg)
summary(max.SDR.reg)
summary(SDR.CV.reg)


#monthly times series plots
par(mar = c(4,5,3,1))
plot(congohydro$dateprecmonth, congohydro$flow.kinshasa, type = "l", main = "Congo River Monitoring Station at Kinshasa", xlab = "Time", ylab = expression(paste("Monthly Discharge (m"^3,"/s)")))
par(mar = c(4,4,3,1))
plot(congohydro$dateprecmonth, congohydro$SDR, type = "l", main = "Congo River Monitoring Station at Kinshasa", xlab = "Time", ylab = "Monthly Specific Discharge Ratio")
                                                                                                      