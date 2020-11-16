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

#read in tree cover data
coverstats<-read.csv('coverstats.csv')
cover2000<-coverstats$treecover2000
gainstats<-read.csv('gainstats.csv')
gain2000_2019<-gainstats$gain
gain2000_2019/cover2000
#only 0.006% of forest area has been gained from 2000-2019
lossstats<-read.csv('lossstats.csv')

#read in precip from World Clim
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)
library(tiff)
install.packages("sf")
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
#rasterize
congoraster<-rasterize(congo_polygon,prec01.2000)
plot(congoraster)


precextract <- list()
sumprec <- numeric(0)
#loop through all NDVI years
for(i in 1:length(precmonth)){
  #get raster values in the difference polygon
  precextract[[i]] <- extract(precraster[[i]],congo_polygon)[[1]]
  #calculate the mean of the NDVI values
  sumprec[i] <- sum(precextract[[i]], na.rm=TRUE)
}
View(sumprec)

#create date variable
dateprecmonth <- as.Date(paste(precmonth,"-01",sep=""))
#create year variable
Year<-year(dateprecmonth)
#add precip and discharge data to date and year in data frame
congohydro<-data.frame(dateprecmonth,Year,sumprec,flow.kinshasa)
#calculate SDR
congohydro$SDR<-(flow.kinshasa/sumprec)
#aggregate flow and SDR sum, mean, and standard deviation by year
library(stats)
flowsum<-stats::aggregate(congohydro$flow.kinshasa, by=list(congohydro$Year), FUN=sum)
colnames(flowsum)<-c("Year","Annual Discharge")
flowmean<-stats::aggregate(congohydro$flow.kinshasa, by=list(congohydro$Year), FUN=mean)
flowsd<-stats::aggregate(congohydro$flow.kinshasa, by=list(congohydro$Year), FUN=sd)
sdrsum<-stats::aggregate(congohydro$SDR, by=list(congohydro$Year), FUN=sum)
colnames(sdrsum)<-c("Year","Annual SDR")
sdrmean<-stats::aggregate(congohydro$SDR, by=list(congohydro$Year), FUN=mean)
sdrsd<-stats::aggregate(congohydro$SDR, by=list(congohydro$Year), FUN=sd)
#get CV for flow and SDR
flowcv<-data.frame(c(2000:2010),flowmean$x/flowsd$x)
colnames(flowcv)<-c("Year","Discharge CV")
sdrcv<-data.frame(c(2000:2010),sdrmean$x/sdrsd$x)
colnames(sdrcv)<-c("Year","SDR CV")
#join deforestation data to flow and sdr sum and cv data
foresthydro1<-join(flowcv, lossstats, by="Year", type="left")
foresthydro1<-join(foresthydro1,sdrcv, by="Year", type= "left")
foresthydro1<-join(foresthydro1,flowsum, by="Year", type= "left")
foresthydro1<-join(foresthydro1,sdrsum, by="Year", type= "left")
#make 2000 0
foresthydro1[is.na(foresthydro1)]<-0
#calculate remaining forest
foresthydro1$cover<-cover2000-foresthydro1$Loss-cumsum(foresthydro1$Loss)
#plot forest change against flow
plot(foresthydro1$cover,foresthydro1$`Discharge CV`)
plot(foresthydro1$cover,foresthydro1$`SDR CV`)
plot(foresthydro1$Year,foresthydro1$`SDR CV`)
#plot against raw values
plot(foresthydro1$cover, foresthydro1$`Annual Discharge`)
plot(foresthydro1$cover, foresthydro1$`Annual SDR`)
