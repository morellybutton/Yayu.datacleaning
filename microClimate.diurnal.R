#calculating monthly diurnal cycles of microclimate measures (RH,Tmax,Tmin,VPDmax,SoilT,VWC,occurrance of LW) by elevation and patch/shade
library(lubridate)
#library(ggplot2)
#library(plyr)
library(grid)
library(stringr)
library(tidyverse)

#setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu/MetData/")

setwd("/users/alex/Documents/Research/Africa/ECOLIMITS/Data/Yayu")

names<-data.frame(read.csv(paste0(getwd(),"/MetData/MS_names.csv")), stringsAsFactors=FALSE)
ns<-read.csv(paste0(getwd(),"/plotnums.csv"))
#load reference for T and VPD
#svp<-data.frame(read.csv(paste0(getwd(),"/T_SVP.csv")), stringsAsFactors=FALSE)
#based on equation(Tetens formula from Buck, 1981) 
#SVP (hPa)= A*exp(mT/(T+Tn)) 
#A=6.11 hPa, m=17.502,Tn=240.97 C
#VPD equation = ((100-RH)/100)*SVP [hPa]
#AH equation = 2.16679*SVP/T(in K)
#to calculate confidence interval at 95% is:
#1.96*stdev/sqrt(n)
#time periods to average over:
#brks<-data.frame(x1=0:2,x2=3:5,x3=6:8,x4=9:11,x5=12:14,x6=15:17,x7=18:20,x8=21:23)
root="ECO_"
nums=c(1:10)

combo<-list()
for (i in 1:length(nums)){
  if(i == 7) next
  dat<-data.frame(read.csv(paste0(getwd(),"/MetData/",root,nums[i],".csv"), stringsAsFactors=FALSE))
  colnames(dat)<-c("date","no","voltage","temp","rh","vwc","ec","t","p","pa","vr","lw")
                   #"vpd","ah","lwa")
  #remove 0 values
  #dat[dat$rh==0,"rh"]<-NA
  #calculate svp
  svp<-6.11*exp(17.502*as.numeric(dat$temp)/(240.97+as.numeric(dat$temp)))
  #calculate VPD in hPa
  dat$vpd<-(100-as.numeric(dat$rh))/100*svp
  #dat$VPD2=(100-as.numeric(dat[,5]))/100*6.116441*10^(7.591386*as.numeric(dat[,4])/(240.7263+as.numeric(dat[,4])))
  #calculate AH
  dat$ah=2.16679*svp/(273.15+as.numeric(dat$temp))*as.numeric(dat$rh)
  #dat$AH=2.16679*6.116441*10^(7.591386*as.numeric(dat[,4])/(240.7263+as.numeric(dat[,4])))/(as.numeric(dat[,4])+273.15)*as.numeric(dat[,5])
  #find LWA values >500 kohms
  dat$lw<-as.numeric(dat$lw)
  dat[is.na(dat$lw),"lw"]=0
  dat[dat$lw>500,"lwa"]<-1
  dat[is.na(dat$lwa),"lwa"]<-0
  
  #generate dates
  dys<-as.POSIXct(dat$date,format="%d/%m/%Y %H:%M")
  if(length(dys[is.na(dys)])==nrow(dat)|year(dys[5])<2014) dat$date<-as.POSIXct(dat[,1],format="%d/%m/%y %H:%M") else dat$date<-dys
  
  #seq1<-seq(as.POSIXct("2014-05-15 00:00:00"), as.POSIXct("2014-11-30 00:00:00"), by="day")
  #d <- which(outer(as.Date(seq1),as.Date(dat[,2]),"=="),arr.ind = TRUE)
  #remove extraneous rows
  dat<-na.omit(dat)
  
  #identify days
  dat$day<-as.Date(cut(dat$date,breaks="day"))
  dates<-unique(dat$day)
  dat$month<-as.Date(cut(dat$date,breaks="month"))
  #add in hour
  dat$hour<-hour(dat$date)
  #remove factors
  #dat<-data.frame(dat, stringsAsFactors=FALSE)
  #addNA(dat$vwc)
  #remove odd values
  dat$vwc[as.character(dat$vwc)>1]=NA
  dat$temp[as.numeric(dat$temp)>100]=NA
  dat$temp[as.numeric(dat$temp)< -50]=NA
  dat$vpd[as.numeric(dat$vpd)>100]=NA
  dat$ah[as.numeric(dat$ah)>100]=NA
  dat$rh[as.numeric(dat$rh)>100]=100
  dat[,3:12]<-sapply(dat[,3:12],as.numeric)
  
  #take hourly averages of all measures, for each day
  dat2<-dat %>% group_by(month,day,hour) %>% summarise(Tavg=mean(temp,na.rm=T),Tmax=max(temp,na.rm=T),Tmin=min(temp,na.rm=T),RH=mean(rh,na.rm=T),vwc=mean(vwc,na.rm=T),Tsoil=mean(t,na.rm=T),vpd=max(vpd,na.rm=T),AH=mean(ah,na.rm=T),lwa=max(lwa,na.rm=T))
  #take hourly averages and confidence intervals of all measures, for each month
  dat3<-dat %>% group_by(month,hour) %>% summarise(Tavg=mean(temp,na.rm=T),Tavg.ci=1.96*sd(temp,na.rm=T)/sqrt(length(temp)),Tmax=max(temp,na.rm=T),Tmax.ci=1.96*sd(temp,na.rm=T)/sqrt(length(temp)),Tmin=min(temp,na.rm=T),Tmin.ci=1.96*sd(temp,na.rm=T)/sqrt(length(temp)),RH=mean(rh,na.rm=T),RH.ci=1.96*sd(rh,na.rm=T),
                                                   vwc=mean(vwc,na.rm=T),Tsoil=mean(t,na.rm=T),Tsoil.ci=1.96*sd(t,na.rm=T)/sqrt(length(t)),vpd=max(vpd,na.rm=T),AH=mean(ah,na.rm=T),AH.ci=1.96*sd(ah,na.rm=T)/sqrt(length(ah)),lwa=mean(lwa,na.rm=T))
  dat4<-dat %>% group_by(month,hour) %>%summarise(vwc.ci=1.96*sd(vwc,na.rm=T)/sqrt(length(vwc)),vpd.ci=1.96*sd(vpd,na.rm=T)/sqrt(length(vpd)),lwa.ci=1.96*sd(lwa,na.rm=T)/sqrt(length(lwa)))
  dat3<-left_join(dat3,dat4,by=c("month","hour"))
  
  write.csv(dat2,paste0(getwd(),"/MetData/",root,nums[i],"_diurnal.csv"))
  write.csv(dat3,paste0(getwd(),"/MetData/",root,nums[i],"_diurnal.monthavg.csv"))
  
  dat3$Plot<-as.character(names[names$DataLogger==paste0(root,nums[i]),"Plot"])
  
  combo[[i]]<-dat3
  rm(dat,dat2,dat3,dat4)
}

final<-do.call(rbind.data.frame,combo)
write.csv(final,paste0(getwd(),"/MetData/Diurnal.avgmonth.allundercanopy.csv"))
rm(combo,final)

#based on equation SVP (Pa)= A*10^(mT/(T+Tn))
#A=6.116441, m=7.591386,Tn=240.7263
#VPD equation = (100-RH)/100*SVP
combo<-list()
for (i in 11:12){
  met<-data.frame(read.csv(paste0(getwd(),"/MetData/",root,i,".csv"), stringsAsFactors=FALSE))
  colnames(met)<-c("date","no","voltage","ptemp","temp","rh","slrkw","slrmj","ws_ms","w_dir","ppt")
  
  #calculate svp
  svp<-6.11*exp(17.502*as.numeric(met$temp)/(240.97+as.numeric(met$temp)))
  
  #calculate VPD
  met$vpd<-(100-as.numeric(met$rh))/100*svp
  #calculate AH
  met$ah=2.16679*svp/(273.15+as.numeric(met$temp))*as.numeric(met$rh)
  
  #generate dates
  dys<-as.POSIXct(met$date,format="%d/%m/%Y %H:%M")
  if(length(dys[is.na(dys)])==nrow(met)|year(dys[5])<2014) met$date<-as.POSIXct(met[,1],format="%d/%m/%y %H:%M") else met$date<-dys
  #remove odd values
  met$temp[as.numeric(met$temp)>100]=NA
  met$temp[as.numeric(met$temp)< -50]=NA
  met$vpd[as.numeric(met$vpd)>100]=NA
  met$ah[as.numeric(met$ah)>100]=NA
  met$rh[as.numeric(met$rh)>100]=100
  
  met[,3:11]<-sapply(met[,3:11],as.numeric)
  
  #generate dates
  dys<-as.POSIXct(met[,1],format="%d/%m/%Y %H:%M")
  if(length(dys[is.na(dys)])==nrow(met)) met[,1]<-as.POSIXct(met[,1],format="%d/%m/%y %H:%M") else met[,1]<-dys
  
  #seq1<-seq(as.POSIXct("2014-05-15 00:00:00"), as.POSIXct("2014-11-30 00:00:00"), by="day")
  #d <- which(outer(as.Date(seq1),as.Date(dat[,2]),"=="),arr.ind = TRUE)
  colnames(met)<-c("date","no","voltage","ptemp","temp","rh","slrkw","slrmj","wsms","wind.dir","ppt","vpd","ah")
  
  #identify days
  met$day<-as.Date(cut(met$date,breaks="day"))
  dates<-unique(met$day)
  met$month<-as.Date(cut(met$date,breaks="month"))
  #add in hour
  met$hour<-hour(met$date)
  #remove extraneous rows
  met<-na.omit(met)
  
  #take hourly averages of all measures, for each day
  dat2<-met %>% group_by(month,day,hour) %>% summarise(Tavg=mean(temp,na.rm=T),Tmax=max(temp,na.rm=T),Tmin=min(temp,na.rm=T),RH=mean(rh,na.rm=T),slrkw=mean(slrkw,na.rm=T),slrmj=mean(slrmj,na.rm=T),vpd=max(vpd,na.rm=T),AH=mean(ah,na.rm=T),ppt=sum(ppt,na.rm=T)) %>%
    filter(Tavg!=0)
  #take hourly averages and confidence intervals of all measures, for each month
  dat3<-met %>% filter(temp!=0) %>% group_by(month,hour) %>% summarise(Tavg=mean(temp,na.rm=T),Tavg.ci=1.96*sd(temp,na.rm=T)/sqrt(length(temp)),Tmax=max(temp,na.rm=T),Tmax.ci=1.96*sd(temp,na.rm=T)/sqrt(length(temp)),Tmin=min(temp,na.rm=T),Tmin.ci=1.96*sd(temp,na.rm=T)/sqrt(length(temp)),RH=mean(rh,na.rm=T),RH.ci=1.96*sd(rh,na.rm=T),
                                                   slrkw=mean(slrkw,na.rm=T),slrmj=mean(slrmj,na.rm=T),vpd=max(vpd,na.rm=T),AH=mean(ah,na.rm=T),AH.ci=1.96*sd(ah,na.rm=T)/sqrt(length(ah)),ppt=mean(ppt,na.rm=T))
  dat4<-met %>% filter(temp!=0) %>% group_by(month,hour) %>%summarise(slrkw.ci=1.96*sd(slrkw,na.rm=T)/sqrt(length(slrkw)),slrmj.ci=1.96*sd(slrmj,na.rm=T)/sqrt(length(slrmj)),vpd.ci=1.96*sd(vpd,na.rm=T)/sqrt(length(vpd)),ppt.ci=1.96*sd(ppt,na.rm=T)/sqrt(length(ppt)))
  dat3<-left_join(dat3,dat4,by=c("month","hour"))
  
  write.csv(dat2,paste0(getwd(),"/MetData/",root,i,"_diurnal.csv"))
  write.csv(dat3,paste0(getwd(),"/MetData/",root,i,"_diurnal.monthavg.csv"))
  
  dat3$Plot<-as.character(names[names$DataLogger==paste0(root,i),"Plot"])
  combo[[i]]<-dat3
  
  rm(met,dat2,dat3,dat4)
}
final<-do.call(rbind.data.frame,combo)
write.csv(final,paste0(getwd(),"/MetData/Diurnal.avgmonth.largemetstations.csv"))
rm(combo,final)

############USB dataloggers
#root="NCRC_"
#find list of files
names<-list.files(path=paste0(getwd(),"/MetData/USB_raw"),pattern=".csv",all.files = FALSE,full.names = TRUE)

#load datalogger names
name<-read.csv(paste0(getwd(),"/MetData/DL_names.csv"))
name[is.na(name$Time.offset),"Time.offset"]<-0
#remove "no data" lines
name<-name[name[,4]!="no data",]
#ns<-read.csv("/Users/alex/Documents/Research/Africa/ECOLIMITS/Data/Kakum/plots.csv")
combo<-as.data.frame(matrix(nrow=length(name[,1]),ncol=7),stringsAsFactors =FALSE)
for(i in 1:length(name[,1])){
  p<-grep(as.character(name[i,3]),ns$name)
  if(as.character(name[i,3])=="H1") p<-p[1]
  combo[i,1:3]<-cbind(as.character(name[i,2]),as.character(name[i,3]),as.numeric(name[i,5]))
  combo[i,4]<-cbind(as.character(ns[p,5]))
  combo[i,5:7]<-cbind(ns[p,2],ns[p,3],ns[p,6])
}
rm(p)

comb<-list()
for(i in 1:length(names)){
  met<-data.frame(read.csv(names[i]), stringsAsFactors=FALSE)
  #find plot number and start date
  p<-str_split(str_split(names[i],"USB_raw/")[[1]][2],".csv")
  p<-p[[1]][p[[1]]!=""]
  n<-as.Date(name[grep(p,as.character(name$Datalogger.id)),"Date"],format="%d/%m/%Y")+1
  if(p=="NCRC_1") n=n[1]
  
  colnames(met)<-c("no","date","temp","rh","dew","sno")
  #identify days
  time<-as.POSIXct(met$date,format="%d/%m/%Y %H:%M")
  #if(is.na(time[1])) time<-as.POSIXct(met$date,format="%d/%m/%Y %H:%M")
  met$day<-as.Date(cut(time,breaks="day"))
  met$month<-as.Date(cut(time,breaks="month"))
  #add in hour
  met$hour<-hour(time)
  
  #extract data collected after start date
  out<-match(n,met$day)
  if(is.na(out)&min(met$day,na.rm=T)>n) out<-match(min(met$day,na.rm=T)+1,met$day)
  if(is.na(out)) next
  
  met<-met[out:length(met[,1]),]
  dates<-unique(met$day)
  
  #calculate svp
  svp<-6.11*exp(17.502*as.numeric(met$temp)/(240.97+as.numeric(met$temp)))
  #calculate VPD
  met$vpd<-(100-as.numeric(met$rh))/100*svp
  #calculate AH
  met$ah=2.16679*svp/(273.15+as.numeric(met$temp))*as.numeric(met$rh)
  
  #remove odd values
  met$temp[as.numeric(met$temp)>100]=NA
  met$temp[as.numeric(met$temp)< -50]=NA
  met$vpd[as.numeric(met$vpd)>100]=NA
  met$ah[as.numeric(met$ah)>100]=NA
  met$rh[as.numeric(met$rh)>100]=100
  
  #take hourly averages of all measures, for each day
  dat2<-met %>% filter(temp!=0) %>% group_by(month,day,hour) %>% summarise(Tavg=mean(temp,na.rm=T),Tmax=max(temp,na.rm=T),Tmin=min(temp,na.rm=T),RH=mean(rh,na.rm=T),vpd=max(vpd,na.rm=T),AH=mean(ah,na.rm=T))
    
  #take hourly averages and confidence intervals of all measures, for each month
  dat3<-met %>% filter(temp!=0) %>% group_by(month,hour) %>% summarise(Tavg=mean(temp,na.rm=T),Tavg.ci=1.96*sd(temp,na.rm=T)/sqrt(length(temp)),Tmax=max(temp,na.rm=T),Tmax.ci=1.96*sd(temp,na.rm=T)/sqrt(length(temp)),Tmin=min(temp,na.rm=T),Tmin.ci=1.96*sd(temp,na.rm=T)/sqrt(length(temp)),RH=mean(rh,na.rm=T),RH.ci=1.96*sd(rh,na.rm=T),
                                                                       vpd=max(vpd,na.rm=T),AH=mean(ah,na.rm=T),AH.ci=1.96*sd(ah,na.rm=T)/sqrt(length(ah)))
  dat4<-met %>% filter(temp!=0) %>% group_by(month,hour) %>%summarise(vpd.ci=1.96*sd(vpd,na.rm=T)/sqrt(length(vpd)))
  dat3<-left_join(dat3,dat4,by=c("month","hour"))
  
  write.csv(dat2,paste0(getwd(),"/MetData/",p,"_diurnal.csv"))
  write.csv(dat3,paste0(getwd(),"/MetData/",p,"_diurnal.monthavg.csv"))
  
  dat3$Plot<-as.character(name[name$Datalogger.id==p,"Plot"])
  comb[[i]]<-dat3
  rm(met,dat2,dat3,dat4)
}

final<-do.call(rbind.data.frame,comb)
write.csv(final,paste0(getwd(),"/MetData/Diurnal.avgmonth.usbloggers.csv"))
rm(final,comb)