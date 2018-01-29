#calculating radiation for each plot using Yoder et al (2005), Meza and Varas (2000) & Allen (1993) FAO 56
#with input from Nico Raab [can look more at Campbell and Norm (1998)--Intro to Environmental Biophysics]

#will use Turc 1961 equation
#ET0=aT*0.013*meanT/(meanT+15)*(23.8856*Rs+50)/lambda
#latent heat vaporization, lambda = 2.45 [MJ/kg]
lambda<-2.45
#meanT=mean daily temperature [(Tmax+Tmin)/2]
#aT[for RHmean=50%]=1.0 or aT[for RHmean<50%]=1.0+(50-RHmean)/70
#Rs solar radiation at atmosphere

#calculating Rs [MJ/m2/day] from Ra [extra-terrestrial radiation], Meza and Varas (2000)
#Ra=86400*1360/pi*(dm/d)^2*[Hs*sin(phi)*sin(gamma)+cos(phi)*cos(gamma)*sin(Hs)]
#where:
#d=distance from sun to earth (km)
#dm=mean distance sun-earth (km)
#or dr=dm/d given by, given by eqn: 1+0.033*cos(2*pi/365*JulianDay)
#phi=latitude (radians), multiply lattitude (in degrees) by pi/180
#gamma=solar declination (radians), given by eqn: 0.409*sin(2*pi/365*JulianDay-1.39)
#Hs=solar angle at sunrise/sunset (radians), given by eqn: acos(-tan(phi)*tan(gamma))

#or FAO equation, which is
#Ra=24*60/pi*Gsc*dr*(Hs*sin(phi)*sin(gamma)+cos(phi)*cos(gamma)*sin(Hs))
#where Gsc is the solar constant = 0.082 MJ/m2/min
Gsc<-0.082
#Rs is a subset of Ra given by eqn: Rs/Ra=a + b*n/N Angstrom model (1924)
#where a=0.25, b=0.5, n=actual sunshine hours (or time period with solar radiation > 120 W/m2) and N=theoretical sunshine hours, by the eqn: N=24/pi*Hs
#whereby Rs=Ra*(a+b*n/N)
a<-0.25
b<-0.5

#calculate infiltration factor using Kostiakov equation F=alpha*kappa*t^(alpha-1), alpha and kappa taken from tables

library(lubridate)
library(plyr)
library(ggplot2)
library(gridExtra)
library(reshape)
library(stringr)
library(tidyverse)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu/MetData")
names<-data.frame(read.csv(paste0(getwd(),"/MS_names.csv")), stringsAsFactors=FALSE)
ns<-read.csv("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu/plotnums.csv")
dnames<-data.frame(read.csv(paste0(getwd(),"/DL_names.csv")), stringsAsFactors=FALSE)
latlon<-data.frame(read.csv(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/GIS/shapefiles/yayu_plots_all.final.csv")),stringsAsFactors = F)
latlon.ms<-data.frame(read.csv(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/GIS/shapefiles/yayu/metstation_latlon.csv")),stringsAsFactors = F)
ppt.low<-data.frame(read.csv(paste0(getwd(),"/ECO_11_dailyppt.csv")))
ppt.high<-data.frame(read.csv(paste0(getwd(),"/ECO_12_dailyppt.csv")))
soil<-data.frame(read.csv("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu/Nutrients/Soils/Soil_variables.csv"))
  
#open met station data 
met.low<-data.frame(read.csv(paste0(getwd(),"/ECO_11.csv"), stringsAsFactors=FALSE))
met.low[,1]<-as.POSIXct(met.low[,1],format="%d/%m/%Y %H:%M")
colnames(met.low)<-c("date","no","voltage","ptemp","temp","rh","slrkw","slrmj","wsms","wind.dir","ppt")

met.high<-data.frame(read.csv(paste0(getwd(),"/ECO_12.csv"), stringsAsFactors=FALSE))
met.high[,1]<-as.POSIXct(met.high[,1],format="%d/%m/%Y %H:%M")
colnames(met.high)<-c("date","no","voltage","ptemp","temp","rh","slrkw","slrmj","wsms","wind.dir","ppt")

#remove extraneous rows and get hourly radiation
met.high<-na.omit(met.high)

#identify days
met.high$day<-as.Date(cut(met.high[,1],breaks="day"))
dates1<-unique(met.high$day)
dates1<-dates1[!is.na(dates1)]

met.mj<-data.frame(as.Date(met.high$day),met.high$date,stringsAsFactors = F)
colnames(met.mj)<-c("day","date")
met.mj$slrmj<-met.high$slrmj
met.mj$hour<-hour(cut(met.high[,1],breaks="hour"))

met.rad<-data.frame(ddply(met.mj,.(day,hour),summarise,radiation=mean(as.numeric(slrmj),na.rm=T)),stringsAsFactors = F)
nas<-which(is.na(met.rad$radiation))

#fill in missing values
for(i in 1:length(nas)){
  met.rad[nas[i],"radiation"]<-mean(met.rad[(nas[i]-1):(nas[i]+1),"radiation"],na.rm=T)
}

#sum hours of sunshine per day
met.rad$sunshine<-met.rad$radiation>.01
met.rad$sunshine<-met.rad$sunshine+0
met.rad$month<-as.Date(cut(as.Date(met.rad$day),breaks="month"))

met.kw<-data.frame(as.Date(met.high$day),met.high$date)
colnames(met.kw)<-c("day","date")
met.kw$slrkw<-met.high$slrkw

#sum hours of sunshine per day
met.kw$sunshine<-met.kw$slrkw>.12
met.kw$sunshine<-met.kw$sunshine+0

sunshine<-ddply(met.kw,.(day),summarise,daylight=sum(sunshine)/2)
sunshine$month<-as.Date(cut(as.Date(sunshine$day),breaks="month"))
daycount <- table(sunshine$day) 

# generate vector of all dates[taken from http://www.r-bloggers.com/fix-missing-dates-with-r/]
alldays <- seq(as.Date(min(sunshine$day,na.rm=T)),length=as.numeric(as.Date(sunshine[!is.na(sunshine$day),"day"][length(sunshine[!is.na(sunshine$day),1])])-as.Date(min(sunshine$day,na.rm=T))+1),by="+1 day")  
allcount <- table(alldays) # create table object from alldays.
actindex <- match(names(allcount),names(daycount),nomatch = 0)  
# create "active" index: vector of length(allcount), i.e. all days. 
# on days with no activity (i.e. a missing day in daycount), this has value 0 (nomatch = 0). 
# For days with activity, actindex holds the index of the matching position in daycount.
# function to get entries of daycount corresponding to actindex
# indexing is a bit tricky. i loops over all days. get correct date by
# substracting all "zero-activity" days accumulated so far.
days <- function(actindex,daycount){
  n <- length(actindex)
  x <- rep(NA,times=n)
  zero <- 0
  for (i in 1:n){
    if (actindex[i]==0) {
      zero <- zero +1
      x[i] <- 0
    } else {
      x[i] <- daycount[i-zero]
    }			
  }
  return(x)
}

alldaycount <- data.frame(days(actindex,daycount))   # construct vector with number of hits per day
alldaycount[,2] <- names(allcount)           # name entries by consecutive dates.

#create hourly dataset for inserting filled data
allhours <-data.frame(seq(from = as.POSIXct(paste0(min(met.rad$day)+1," 01:00")), 
                          to = as.POSIXct(paste0(max(met.rad$day)-1," 23:00")), by = "hour"),stringsAsFactors = F)
colnames(allhours)<-c("time")
allhours$day<-as.Date(allhours$time)
allhours$hour<-hour(allhours$time)
allhours$radiation<-met.rad[match(interaction(allhours$day,allhours$hour),interaction(met.rad$day,met.rad$hour)),"radiation"]

#for all days where daylight data is missing put diurnal average for month
ds.1<-data.frame(day=as.Date(character()),month=as.Date(character()),stringsAsFactors = F)
ds.1[1:nrow(alldaycount[alldaycount[,1]==0,]),1]<-alldaycount[alldaycount[,1]==0,2]

#add month
ds.1$month<-as.Date(cut(as.Date(ds.1[,1]),breaks="month"))
m<-unique(ds.1$month)
d.s.1<-list()
for(i in 1:length(m)){
  d.s.1[[i]]<-ddply(met.rad[met.rad$month==m[i],],.(month,hour),summarise,radiation.hr=mean(radiation,na.rm=T))
}
d.s.1<-do.call(rbind.data.frame,d.s.1)

for(i in 1:nrow(ds.1)){
  tmp<-d.s.1[d.s.1$month==ds.1[i,"month"],]
  allhours[allhours$day==ds.1$day[i],"radiation"]<-tmp[match(allhours[allhours$day==ds.1$day[i],"hour"],tmp$hour),"radiation.hr"]
}

#for all days where daylight data is missing put daily average for month
ds<-data.frame(alldaycount[alldaycount[,1]==0,2])
colnames(ds)<-"day"
#add month
ds$month<-as.Date(cut(as.Date(ds[,1]),breaks="month"))
m<-unique(ds$month)
for(i in 1:length(m)){
  ds[ds$month==m[i],"avg"]<-mean(sunshine[sunshine$month==m[i],"daylight"],na.rm=T)
}

#add average ppt for missing points with daily ppt measures
alldaycount[match(as.Date(sunshine$day),as.Date(alldaycount$V2),nomatch=0),"daylight"]<-sunshine$daylight
alldaycount[match(ds$day,alldaycount$V2),"daylight"]<-ds$avg

alldaycount$JulianDay<-yday(alldaycount$V2)
#calculate latitude in radians
phi<-latlon.ms$Y[1]*pi/180
alldaycount$gamma<-0.409*sin(2*pi/365*alldaycount$JulianDay-1.39)
alldaycount$Hs<-acos(-tan(phi)*tan(alldaycount$gamma))
alldaycount$n_N<-alldaycount$daylight/(24/pi*alldaycount$Hs)

#sum hourly radiation levels to get daily radiation
alldaycount.1<-ddply(allhours,.(day),summarise,Rs=sum(radiation,na.rm=T)*2)
alldaycount.1$month<-as.Date(cut(as.Date(alldaycount.1$day),breaks="month"))

#identify days with missing hours of measurement and supply average monthly value
alldaycount.1[alldaycount.1$Rs<1,"Rs"]<-NA
miss<-alldaycount.1[is.na(alldaycount.1$Rs),]

PET<-list()
n2<-names
n2<-n2[n2$DataLogger!="ECO_7",]
n2<-n2[grep("Met_",n2$Plot,invert=T),]
for(i in 1:nrow(n2)){
  dF<-data.frame(read.csv(paste0(getwd(),"/",n2[i,"DataLogger"],"_summary.csv")),stringsAsFactors = F)
  dF<-dF[!is.na(dF$day),]
  #insert avg for missing RH
  dF[is.na(dF$RHmean),"RHmean"]<-mean(dF$RHmean,na.rm=T)
  #calculate meanT from max and min T
  dF$meanT<-(dF$Tmax+dF$Tmin)/2
  #calculate Tavg from all Tavg measures
  dF$Tavg<-rowMeans(cbind(dF[,grep("Tavg",colnames(dF))]),na.rm=T)
  #convert dates to Julian Day
  dF$JulianDay<-yday(dF$day)
  #calculate dr
  dF$dr<-1+0.033*cos(2*pi/365*dF$JulianDay)
  #calculate latitude in radians
  phi<-latlon[match(n2[i,"Plot"],latlon$Plot),"Latitude"]*pi/180
  #calculate gamma and Hs
  dF$gamma<-0.409*sin(2*pi/365*dF$JulianDay-1.39)
  dF$Hs<-acos(-tan(phi)*tan(dF$gamma))
  #calculate daily Ra
  #dF$Ra<-86400*1360/pi*(dF$dr)^2*(dF$Hs*sin(phi)*sin(dF$gamma)+cos(phi)*cos(dF$gamma)*sin(dF$Hs))
  dF$Ra<-24*60/pi*Gsc*dF$dr*(dF$Hs*sin(phi)*sin(dF$gamma)+cos(phi)*cos(dF$gamma)*sin(dF$Hs))
  #convert Ra to Rs
  dF$Rs<-dF$Ra*(a+b*alldaycount[match(dF$day,alldaycount$V2),"n_N"])
  #compare with measured radiation
  dF$Rs.1<-alldaycount.1[match(as.Date(dF$day),alldaycount.1$day),"Rs"]
  #assign aT depending RH
  dF[dF$RHmean>=50,"aT"]<-1
  dF[dF$RHmean<50,"aT"]<-1.0+(50-dF[dF$RHmean<50,"RHmean"])/70
  #calculate evapotranspiration
  dF$ET0<-dF$aT*0.013*dF$meanT/(dF$meanT+15)*(23.8856*dF$Rs+50)/lambda
  dF$ET0.1<-dF$aT*0.013*dF$meanT/(dF$meanT+15)*(23.8856*dF$Rs.1+50)/lambda
  #remove ET0 NAs
  dF<-dF[!is.na(dF$ET0)&!is.na(dF$ET0.1),]
  pet<-cbind(dF[,2:3],dF[,6:7],dF$Tavg,dF$day,dF$Rs,dF$Rs.1,dF$ET0,dF$ET0.1)
  colnames(pet)<-c(colnames(pet[,1:4]),"Tavg","day","Rs","Rs.1","ET0","ET0.1")
  pet$Plot<-n2[i,"Plot"]
  PET[[i]]<-pet
}

#do again for dataloggers
dns<-list.files(path=paste0(getwd(),"/summary"),pattern=".csv",all.files = FALSE,full.names = TRUE)

for(i in 1:(length(dns))){
  dF<-read.csv(dns[i])
  dF<-dF[!is.na(dF$day),]
  #insert avg for missing RH
  dF[is.na(dF$RHmean),"RHmean"]<-mean(dF$RHmean,na.rm=T)
  #calculate meanT from max and min T
  dF$meanT<-(dF$Tmax+dF$Tmin)/2
  #calculate Tavg from all Tavg measures
  dF$Tavg<-rowMeans(cbind(dF[,grep("Tavg",colnames(dF))]),na.rm=T)
  #convert dates to Julian Day
  dF$JulianDay<-yday(dF$day)
  #calculate dr
  dF$dr<-1+0.033*cos(2*pi/365*dF$JulianDay)
  #find plot number and start date
  p<-str_split(str_split(dns[i],"summary/")[[1]][2],"_summary.csv")
  p<-p[[1]][p[[1]]!=""]
  #calculate latitude in radians
  phi<-latlon[match(dnames[dnames$Datalogger.id==p,"Plot"],latlon$Plot),"Latitude"]*pi/180
  #calculate gamma and Hs
  dF$gamma<-0.409*sin(2*pi/365*dF$JulianDay-1.39)
  dF$Hs<-acos(-tan(phi)*tan(dF$gamma))
  #calculate daily Ra
  #dF$Ra<-86400*1360/pi*(dF$dr)^2*(dF$Hs*sin(phi)*sin(dF$gamma)+cos(phi)*cos(dF$gamma)*sin(dF$Hs))
  dF$Ra<-24*60/pi*Gsc*dF$dr*(dF$Hs*sin(phi)*sin(dF$gamma)+cos(phi)*cos(dF$gamma)*sin(dF$Hs))
  #convert Ra to Rs
  #dF$Rs<-dF$Ra*(a+b*alldaycount[match(dF$day,alldaycount$V2),"n_N"])
  dF$Rs<-dF$Ra*(a+b*alldaycount[match(dF$day,alldaycount$V2),"n_N"])
  #compare with measured radiation
  dF$Rs.1<-alldaycount.1[match(as.Date(dF$day),alldaycount.1$day),"Rs"]
  #assign aT depending RH
  dF[dF$RHmean>=50,"aT"]<-1
  dF[dF$RHmean<50,"aT"]<-1.0+(50-dF[dF$RHmean<50,"RHmean"])/70
  #calculate evapotranspiration
  dF$ET0<-dF$aT*0.013*dF$meanT/(dF$meanT+15)*(23.8856*dF$Rs+50)/lambda
  dF$ET0.1<-dF$aT*0.013*dF$meanT/(dF$meanT+15)*(23.8856*dF$Rs.1+50)/lambda
  #remove ET0 NAs
  dF<-dF[!is.na(dF$ET0)&!is.na(dF$ET0.1),]
  pet<-cbind(dF[,2:5],dF$Tavg,dF$day,dF$Rs,dF$Rs.1,dF$ET0,dF$ET0.1)
  colnames(pet)<-c(colnames(pet[,1:4]),"Tavg","day","Rs","Rs.1","ET0","ET0.1")
  pet$Plot<-dnames[dnames$Datalogger.id==p,"Plot"]
  PET[[i+9]]<-pet
  rm(pet,dF,p)
}

et.0<-data.frame(do.call(rbind.data.frame,PET),stringsAsFactors = F)
#add month
et.0$month<-as.Date(cut(as.Date(et.0$day),breaks="months"))

#calculate average temperature of all plots
et.1<-ddply(et.0[et.0$Tavg!=0,], .(day), summarise,Tavg=mean(Tavg,na.rm=T))
#save Tavg values
write.csv(et.1,paste0(getwd(),"/Daily_Tavg.csv"))

#calculate deviations from average for all measured plots
et.0$Tdiff<- et.0$Tavg- et.1[match(et.0$day,et.1$day),"Tavg"]

#add elevation class
et.0$elevation<-ns[match(et.0$Plot,ns$name),"elevation"]
et.0$elev.class<-"Low"
et.0[et.0$elevation>1600,"elev.class"]<-"High"
et.0$kebele<-as.character(ns[match(et.0$Plot,ns$name),"Kebele"])
et.0$day<-as.Date(et.0$day)
et.0$Plot<-as.character(et.0$Plot)
et.0$patcharea<-as.character(ns[match(et.0$Plot,ns$name),"PatchArea"])
ns$elev.class<-"Low"
ns[ns$elevation>1600,"elev.class"]<-"High"
c.g<-quantile(ns$Gap_Nov14)
ns[ns$Gap_Nov14>c.g[3],"Gap.class"]<-"High"
ns[ns$Gap_Nov14<=c.g[3],"Gap.class"]<-"Low"

#save Tdiff values
write.csv(et.0,paste0(getwd(),"/Daily_Tdiff_byplot.csv"))


#identify which plots are missing
p<-as.character(dnames[match(str_split_fixed(str_split_fixed(dns,"summary/",2)[,2],"_summary.csv",2)[,1],dnames$Datalogger.id),"Plot"])
ps<-c(as.character(n2$Plot),p)
ns$check<-match(as.character(ns$name),ps)
ns[ns$check>0&!is.na(ns$check),"check"]<-1
ns[is.na(ns$check),"check"]<-0
ns<-ns[grep("Met_",ns$name,invert=T),]

#identify plots to use for missing plots
maf<-unique(ns$Kebele)
maf.1<-list()
maf.2<-list()
for(i in 1:length(maf)){
  tmp<-ns[ns$Kebele==maf[i],]
  if(nrow(tmp[tmp$check==0,])==0) next
  maf.1[[i]]<-cbind(as.character(maf[i]),as.character(tmp[tmp$check==1,"name"]))
  maf.2[[i]]<-cbind(as.character(maf[i]),as.character(tmp[tmp$check==0,"name"]))
}
root.plts<-do.call(rbind.data.frame,maf.1)
blank.plts<-do.call(rbind.data.frame,maf.2)
rm(maf.1,maf.2)

#write existing et0 to figure out relationship between elevation and patchsize
#write.csv(et.0,paste0(getwd(),"/Unfilled_et.0.csv"))
#number of days to fill
#write.csv(alldaycount.1,paste0(getwd(),"/Numberofdays.forfilling.csv"))
#et.0<-read.csv(paste0(getwd(),"/Unfilled_et.0.csv"))
#met.fill<-read.csv(paste0(getwd(),"/Filled_metdata.csv"))

#Calculate ET values for "missing plots"
#kebs<-as.character(unique(blank.plts$V2))
#for(i in 1:length(kebs)){
  #identify elevation class,canopy gap and patchsize
#c.de<-paste0(ns[ns$name==kebs[i],"elev.class"],".",ns[ns$name==kebs[i],"Gap.class"],".",ns[ns$name==kebs[i],"PatchSize"])
  
#tmp<-data.frame(met.fill[,grep(c.de,colnames(met.fill))])
#colnames(tmp)<-c("meanT","RHmean","VPDmax")
#tmp$day<-met.fill$day
  #tmp<-ddply(et.1,.(day),summarise,Tmax=mean(Tmax,na.rm=T),Tmin=mean(Tmin,na.rm=T),VPDmax=mean(VPDmax,na.rm=T),RHmean=mean(RHmean,na.rm=T))
  #tmp$meanT<-(tmp$Tmax+tmp$Tmin)/2
  #add Rs to dataframe
#tmp$Rs<-et.0[match(as.Date(tmp$day),as.Date(et.0$day)),"Rs"]
#tmp$Rs.1<-et.0[match(as.Date(tmp$day),as.Date(et.0$day)),"Rs.1"]
  #remove dates without Rs values
  #tmp<-tmp[!is.na(tmp$Rs),]
 
  #assign aT depending RH
#tmp[tmp$RHmean>=50&!is.na(tmp$RHmean),"aT"]<-1
#tmp[tmp$RHmean<50&!is.na(tmp$RHmean),"aT"]<-1.0+(50-tmp[tmp$RHmean<50&!is.na(tmp$RHmean),"RHmean"])/70
  #calculate evapotranspiration
#tmp$ET0<-tmp$aT*0.013*tmp$meanT/(tmp$meanT+15)*(23.8856*tmp$Rs+50)/lambda
#tmp$ET0.1<-tmp$aT*0.013*tmp$meanT/(tmp$meanT+15)*(23.8856*tmp$Rs.1+50)/lambda
  
  #for(j in 1:length(p)){
# et.0[(nrow(et.0)+1):(nrow(et.0)+nrow(tmp)),3:4]<-tmp[,2:3]
#et.0[(nrow(et.0)-nrow(tmp)+1):nrow(et.0),"day"]<-as.Date(tmp[,4])
# et.0[(nrow(et.0)-nrow(tmp)+1):nrow(et.0),"ET0"]<-tmp$ET0
#et.0[(nrow(et.0)-nrow(tmp)+1):nrow(et.0),"ET0.1"]<-tmp$ET0.1
#et.0[(nrow(et.0)-nrow(tmp)+1):nrow(et.0),"Plot"]<-kebs[i]
#et.0[(nrow(et.0)-nrow(tmp)+1):nrow(et.0),"month"]<-as.Date(cut(as.Date(tmp$day),breaks="months"))
#et.0[(nrow(et.0)-nrow(tmp)+1):nrow(et.0),"elevation"]<-ns[ns$name==kebs[i],"elevation"]
#et.0[(nrow(et.0)-nrow(tmp)+1):nrow(et.0),"elev.class"]<-ns[ns$name==kebs[i],"elev.class"]
#et.0[(nrow(et.0)-nrow(tmp)+1):nrow(et.0),"kebele"]<-ns[ns$name==kebs[i],"Kebele"]
#et.0[(nrow(et.0)-nrow(tmp)+1):nrow(et.0),"PatchSize"]<-ns[ns$name==kebs[i],"PatchSize"]
    
#}
#remove na rows
#et.0<-et.0[!is.na(et.0$day),]

#find unique infiltrates
irates<-data_frame(Plot=as.character(ns[grep("Met_",ns$name,invert=T),"name"]), elevation=ns[grep("Met_",ns$name,invert=T),"elevation"])
irates <- irates %>% mutate(elev.class="Low") %>% mutate(elev.class=replace(elev.class, elevation>1600, "High"))

irates$infil<-soil[match(irates$Plot,soil$Plot),"Infiltrate"]
#compare combinations of infiltration rates and elevation class
comp<-data.frame(table(irates$elev.class,irates$infil),stringsAsFactors = F)
comp<-comp[comp$Freq!=0,]
#separate infiltrates for low and high elevations
irates.low<-as.numeric(as.character(comp[comp$Var1=="Low",2]))
irates.high<-as.numeric(as.character(comp[comp$Var1=="High",2]))

#calculate daily ppt available for each elevaation and infiltration (left after runoff)
#met data
#identify days
met.low$day<-as.Date(cut(met.low$date,breaks="day"))
met.high$day<-as.Date(cut(met.high$date,breaks="day"))

#identify hours
met.low$hour<-cut(met.low$date,breaks="hour")
met.high$hour<-cut(met.high$date,breaks="hour")

#remove extraneous rows
met.low<-na.omit(met.low)
met.high<-na.omit(met.high)

#go through each hour and figure out hourly ppt
ppt.h1<-data.frame(Day=as.Date(character()),Hour=numeric(),ppt=numeric(),stringsAsFactors = F)

for(i in 1:length(alldays)){
  ppt.h1[(nrow(ppt.h1)+1):(nrow(ppt.h1)+24),"Day"]<-alldays[i]
  ppt.h1[(nrow(ppt.h1)-23):nrow(ppt.h1),"Hour"]<-c(1:23,0)
  m<-met.low[met.low$day==alldays[i],]
  if(nrow(m)==0) next

  m$hour<-hour(m$date)
  #find hours for that day
  h<-unique(m$hour)
  #sum for hours
  for(j in 1:length(h)){
    ppt.h1[ppt.h1$Hour==h[j]&ppt.h1$Day==alldays[i],"ppt"]<-sum(as.numeric(m[m$hour==h[j],"ppt"]),na.rm=T)
    #calculate run-off per texture, negative values are run-off and positive values refer to infiltration potential remaining
    for(k in 1:length(irates.low)){
      if(ppt.h1[ppt.h1$Hour==h[j]&ppt.h1$Day==alldays[i],"ppt"]==0) ppt.h1[ppt.h1$Hour==h[j]&ppt.h1$Day==alldays[i],paste0("ir.",irates.low[k])]<-0  else ppt.h1[ppt.h1$Hour==h[j]&ppt.h1$Day==alldays[i],paste0("ir.",irates.low[k])]<-irates.low[k]-ppt.h1[ppt.h1$Hour==h[j]&ppt.h1$Day==alldays[i],"ppt"]
    }
  }
}
#colnames(ppt.h1)<-c(colnames(ppt.h1[,1:3]),gsub(" ","",colnames(ppt.h1[,4:17])))
#from hourly rates calculate daily runoff
runoff1<-ddply(ppt.h1,.(Day),summarise,ir.0.3302=sum(ir.0.3302[ir.0.3302<0],na.rm=T),ir.0.381=sum(ir.0.381[ir.0.381<0],na.rm=T),ir.0.4826=sum(ir.0.4826[ir.0.4826<0],na.rm=T),ir.0.635=sum(ir.0.635[ir.0.635<0],na.rm=T),
               ir.0.7874=sum(ir.0.7874[ir.0.7874<0],na.rm=T),ir.1.27=sum(ir.1.27[ir.1.27<0],na.rm=T),ir.1.524=sum(ir.1.524[ir.1.524<0],na.rm=T),ir.1.905=sum(ir.1.905[ir.1.905<0],na.rm=T))
ppt.h1$sum<-0
ppt.h1[ppt.h1$ppt>0&!is.na(ppt.h1$ppt),"sum"]<-1

ppt.h2<-data.frame(Day=as.Date(character()),Hour=numeric(),ppt=numeric(),stringsAsFactors = F)
for(i in 1:length(alldays)){
  ppt.h2[(nrow(ppt.h2)+1):(nrow(ppt.h2)+24),"Day"]<-alldays[i]
  ppt.h2[(nrow(ppt.h2)-23):nrow(ppt.h2),"Hour"]<-c(1:23,0)
  m<-met.high[met.high$day==alldays[i],]
  if(nrow(m)==0) next
  
  m$hour<-hour(m$date)
  #find hours for that day
  h<-unique(m$hour)
  #sum for hours
  for(j in 1:length(h)){
    ppt.h2[ppt.h1$Hour==h[j]&ppt.h1$Day==alldays[i],"ppt"]<-sum(as.numeric(m[m$hour==h[j],"ppt"]),na.rm=T)
    #calculate run-off per texture, negative values are run-off and positive values refer to infiltration potential remaining
    for(k in 1:length(irates.high)){
      if(ppt.h2[ppt.h2$Hour==h[j]&ppt.h2$Day==alldays[i],"ppt"]==0) ppt.h2[ppt.h2$Hour==h[j]&ppt.h2$Day==alldays[i],paste0("ir.",irates.high[k])]<-0  else ppt.h2[ppt.h2$Hour==h[j]&ppt.h2$Day==alldays[i],paste0("ir.",irates.high[k])]<-irates.high[k]-ppt.h2[ppt.h2$Hour==h[j]&ppt.h2$Day==alldays[i],"ppt"]
    }
  }
}
#colnames(ppt.h2)<-c(colnames(ppt.h2[,1:3]),gsub(" ","",colnames(ppt.h2[,4:8])))
#from hourly rates calculate daily runoff
runoff2<-ddply(ppt.h2,.(Day),summarise,ir.0.2032=sum(ir.0.2032[ir.0.2032<0],na.rm=T),ir.0.4826=sum(ir.0.4826[ir.0.4826<0],na.rm=T),ir.0.635=sum(ir.0.635[ir.0.635<0],na.rm=T),
               ir.0.7874=sum(ir.0.7874[ir.0.7874<0],na.rm=T),ir.1.1176=sum(ir.1.1176[ir.1.1176<0],na.rm=T),ir.1.27=sum(ir.1.27[ir.1.27<0],na.rm=T),ir.1.3716=sum(ir.1.3716[ir.1.3716<0],na.rm=T),
               ir.1.905=sum(ir.1.905[ir.1.905<0],na.rm=T))
ppt.h2$sum<-0
ppt.h2[ppt.h2$ppt>0&!is.na(ppt.h2$ppt),"sum"]<-1

#for days with missing data, calculate average number of hours of a rain a day (by month)
correct<-ddply(ppt.h1,.(Day),summarise,freq=sum(sum))
#add month to get daily average of rain hours
correct$month<-as.Date(cut(correct$Day,breaks="month"))
ppt.d1<-cbind(ppt.low$V2,ppt.low$Tppt,runoff1[match(as.Date(ppt.low$V2),as.Date(runoff1$Day)),2:ncol(runoff1)])
ppt.d1<-ppt.d1[as.Date(ppt.d1[,1])>min(runoff1$Day),]
ppt.d1<-ppt.d1[as.Date(ppt.d1[,1])<max(runoff1$Day),]
ppt.d1[is.na(ppt.d1[,2]),2]<-0
ppt.d1[ppt.d1[,2]>0&ppt.d1$ir.0.3302==0,3:ncol(ppt.d1)]<-NA
m.correct<-ddply(correct,.(month),summarise,avg=mean(freq))
ppt.d1$month<-as.Date(cut(as.Date(ppt.d1[,1]),breaks="months"))
m<-unique(ppt.d1[is.na(ppt.d1$ir.0.3302),"month"])

for(i in 1:length(m)){
  ppt.m<-ppt.d1[ppt.d1$month==m[i],]
  mis<-unique(ppt.m[is.na(ppt.m$ir.0.3302),2])
 
  for(k in 1:length(mis)){
    if(nrow(ppt.m)==nrow(ppt.m[is.na(ppt.m[,2]),])) run.est[,1:length(irates.low)]<-0 else run.est<-t(irates.low)*m.correct[m.correct$month==m[i],"avg"]-mis[k]
    for(j in 3:(length(run.est)+1)){
      if(is.na(mis)) ppt.m[is.na(ppt.m[,j]),j]<-run.est[j-2] else ppt.m[ppt.m[,2]==mis[k]&is.na(ppt.m[,j]),j]<-run.est[j-2]
    }
  }
  ppt.m[is.na(ppt.m[,2]),2]<-0
  ppt.d1[ppt.d1$month==m[i],]<-ppt.m
}

#match daily ppt
tmp<-et.0[et.0$elev.class=="Low",]
tmp$ppt<-ppt.d1[match(tmp$day,as.Date(ppt.d1[,1])),2]
et.0[et.0$elev.class=="Low","ppt"]<-tmp$ppt

#for days with missing data, calculate average number of hours of a rain a day (by month?)
correct<-ddply(ppt.h2,.(Day),summarise,freq=sum(sum))
#add month to get daily average of rain hours
correct$month<-as.Date(cut(correct$Day,breaks="month"))
ppt.d2<-cbind(ppt.high$V2,ppt.high$Tppt,runoff2[match(as.Date(ppt.high$V2),as.Date(runoff2$Day)),2:ncol(runoff2)])
ppt.d2<-ppt.d2[as.Date(ppt.d2[,1])>min(runoff2$Day),]
ppt.d2<-ppt.d2[as.Date(ppt.d2[,1])<max(runoff2$Day),]
ppt.d2[is.na(ppt.d2[,2]),2]<-0
ppt.d2[ppt.d2[,2]>0&ppt.d2$ir.0.2032==0,3:ncol(ppt.d2)]<-NA
m.correct<-ddply(correct,.(month),summarise,avg=mean(freq))
ppt.d2$month<-as.Date(cut(as.Date(ppt.d2[,1]),breaks="months"))
m<-unique(ppt.d2[is.na(ppt.d2$ir.0.2032),"month"])

for(i in 1:length(m)){
  ppt.m<-ppt.d2[ppt.d2$month==m[i],]
  mis<-unique(ppt.m[is.na(ppt.m$ir.0.2032),2])
  for(k in 1:length(mis)){
    if(nrow(ppt.m)==nrow(ppt.m[is.na(ppt.m[,2]),])) run.est[,1:length(irates.high)]<-0 else run.est<-t(irates.high)*m.correct[m.correct$month==m[i],"avg"]-mis[k]
    for(j in 3:(length(run.est)+2)){
      if(is.na(mis)) ppt.m[is.na(ppt.m[,j]),j]<-run.est[j-2] else ppt.m[ppt.m[,2]==mis[k]&is.na(ppt.m[,j]),j]<-run.est[j-2]
    }
  }
  ppt.d2[ppt.d2$month==m[i],]<-ppt.m
}

#match daily ppt
tmp<-et.0[et.0$elev.class=="High",]
tmp$ppt<-ppt.d2[match(tmp$day,as.Date(ppt.d2[,1])),2]
et.0[et.0$elev.class=="High","ppt"]<-tmp$ppt

#add infiltrate to et.0
et.0$Infiltrate<-soil[match(et.0$Plot,soil$Plot),"Infiltrate"]
#add daily run-off estimates per soil texture by day
elev.class<-c("Low","High")

#need to calculate for "all" plots
for(j in 1:length(elev.class)){
  et.1<-et.0[et.0$elev.class==elev.class[j],]
  if(j==1) i.r<-irates.low else i.r<-irates.high
  if(j==1) runoff<-ppt.d1 else runoff<-ppt.d2
  #match days to daily runoff estimates
  for(i in 1:length(i.r)){
    et.2<-et.1[as.numeric(et.1$Infiltrate)==i.r[i],]
    et.1[et.1$Infiltrate==i.r[i],"runoff"]<-as.numeric(runoff[match(as.Date(et.2$day),as.Date(runoff[,1])),paste0("ir.",i.r[i])])
    et.1[is.na(et.1$runoff)&as.numeric(et.1$Infiltrate)==i.r[i],"runoff"]<-0
    et.0[et.0$Infiltrate==i.r[i]&et.0$elev.class==elev.class[j],"runoff"]<- et.1[et.1$Infiltrate==i.r[i],"runoff"]
  }
}

#calculate infiltration by I=P+(-R)
et.0$infil<-et.0$ppt+et.0$runoff

#calculate water stress by subtracting evapotranspiration from "infiltrated water" and taking cumulative sum per plot per month
et.0$diff<-et.0$infil-et.0$ET0
et.0$diff1<-et.0$infil-et.0$ET0.1

#count number of days "diff" is negative
et.0[et.0$diff>0&!is.na(et.0$diff),"stress"]<-0
et.0[et.0$diff<0&!is.na(et.0$diff),"stress"]<-1

et.0[et.0$diff1>0&!is.na(et.0$diff1),"stress1"]<-0
et.0[et.0$diff1<0&!is.na(et.0$diff1),"stress1"]<-1

#save ET0 data
write.csv(et.0,paste0(getwd(),"/ET0_allplots.csv"))

et.0<-data.frame(read.csv(paste0(getwd(),"/ET0_allplots.csv")),stringsAsFactors = F)
#calculate monthly maxT, minT, meanT, maxVPD, stress incidence (number of negative days), sum infiltration and ETO differences 
tmp<-ddply(et.0,.(Plot,month),summarise,maxVPD=mean(VPDmax,na.rm=T),meanRH=mean(RHmean,na.rm=T),stress.nodays=sum(stress,na.rm=T),stress.nodays1=sum(stress1,na.rm=T),stress=sum(diff,na.rm=T),stress1=sum(diff1,na.rm=T))

#and number of continuous days of  drought
plts<-unique(et.0$Plot)
for(i in 1:length(plts)){
  et.1<-et.0[et.0$Plot==plts[i],]
  mos<-unique(et.1$month)
  for(j in 1:length(mos)){
    et.2<-et.1[et.1$month==mos[j],]
    tmp[tmp$Plot==plts[i]&tmp$month==mos[j],"stress.daysinarow"]<-max(do.call(cbind.data.frame,rle(et.2$stress))[2]*do.call(cbind.data.frame,rle(et.2$stress))[1],na.rm=T)
    tmp[tmp$Plot==plts[i]&tmp$month==mos[j],"stress.daysinarow1"]<-max(do.call(cbind.data.frame,rle(et.2$stress1))[2]*do.call(cbind.data.frame,rle(et.2$stress1))[1],na.rm=T)
  }
}

#write out stress indices per month
write.csv(tmp,paste0(getwd(),"/MonthlyStress_estimates.csv"))

#max(do.call(cbind.data.frame,rle(x1$stress))[2]*do.call(cbind.data.frame,rle(x1$stress))[1],na.rm=T)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#calculating soil water capacity 
et.0<-data.frame(read.csv(paste0(getwd(),"/ET0_allplots.csv")),stringsAsFactors = F)
ps<-as.character(unique(ns[grep("Met_",ns$name,invert=T),"name"]))
et.1<-et.0[as.Date(et.0$month)>"2014-12-01"&as.Date(et.0$month)<"2016-01-01",]
#create figures comparing precip, ET0 and infiltration
g.top<-list()
g.mid<-list()
g.bottom<-list()
for(i in 1:length(ps)){
  et.2<-et.1[et.1$Plot==ps[i],]
  g.top[[i]]<-ggplot(et.2,aes(day,ppt))+geom_bar(stat="identity")+ylab("Daily \nPrecipitation (mm)")+theme(axis.text.x=element_blank(),axis.title.x=element_blank())+
    ggtitle(paste0(ps[i]," [",unique(signif(et.2$elevation,digits=4))," (m)]"))
  g.mid[[i]]<-ggplot(et.2)+geom_line(aes(as.Date(day),ET0))+ylab("Daily \nEvapotranspiration (mm)")+theme(axis.text.x=element_blank(),axis.title.x=element_blank())
  g.bottom[[i]]<-ggplot(et.2)+geom_line(aes(as.Date(day),diff))+ylab("Daily \nInfiltration-ET0 (mm)")+geom_hline(yintercept=0,color="red",linetype="dashed")+xlab("Day")
  g<-grid.arrange(g.top[[i]],g.mid[[i]],g.bottom[[i]],heights = c(1/4,1/4, 1/2))
  ggsave(paste0(getwd(),"/Figures/Ppt_ET_dif_",ps[i],".pdf"),g,width=11,height=8)
}


