library(ncdf4)
ncar<-nc_open("coastal-stns-Vol-monthly.updated-Aug2014.nc")
{
  sink("coastal-stns-Vol-monthly.updated-Aug2014-metadata.txt")
  print(ncar)
  sink()
}
time<-ncvar_get(ncar, ncar$dim$time)
time
View(time)
station<-ncvar_get(ncar, ncar$dim$station)
station
flow<-ncvar_get(ncar, ncar$var$FLOW)
View(flow)
dim(flow)
flow.kinshasa<-flow[2,1201:1332]
station_name<-ncvar_get(ncar, ncar$var$stn_name)
station_name
river_name<-ncvar_get(ncar, ncar$var$riv_name)
View(river_name)
View(flow.kinshasa)
plot(flow.kinshasa, type="l")

coverstats<-read.csv('coverstats.csv')
cover2000<-coverstats$treecover2000
gainstats<-read.csv('gainstats.csv')
gain2000_2019<-gainstats$gain
gain2000_2019/cover2000
#only 0.006% of forest area has been gained from 2000-2019
lossstats<-read.csv('lossstats.csv')
head(lossstats)


precip<-nc_open("congoprecip.nc")
{
  sink("congoprecip-metadata.txt")
  print(precip)
  sink()
}
congoprecip<-ncvar_get(precip, precip$var$precip)
View(congoprecip)
dim(congoprecip)
cover2000
