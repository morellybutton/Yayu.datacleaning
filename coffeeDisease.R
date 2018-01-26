#code for combining branch measures and final yield berry counts
library(tidyverse)
library(lubridate)
#library(stringr)
#library(plyr)
#library(ggplot2)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu/")

#dates<-"nov14"
years<-c("2014","2015","2016")

plts<-read.csv(paste0(getwd(),"/plotnums.csv"))
#remove Met station and Forest plots
plts<-plts[grep("Met_",plts$name,invert=T),]
plts<-plts[grep("FC",plts$name,invert=T),]

#npp<-npp[grep("FP",npp,invert=T)]
p<-as.character(plts$name)

for(j in 1:length(years)){
  final<-list()
  final.s<-list()
  
  Bcount<-data.frame(read.csv(paste0(getwd(),"/Yield/Final.BerryCounts_",years[j],".csv"), stringsAsFactors=F))
  if(ncol(Bcount)==8) Bcount <- data_frame(Plot=Bcount$Plot,Shrub.id = Bcount$ShrubId,Branch.cm=Bcount$Branch.cm,Berry.no=Bcount$Berry.no,NoCBB.end=NA)  else Bcount <- data_frame(Plot=Bcount$Plot,Shrub.id = Bcount$ShrubId,Branch.cm=Bcount$Branch.cm,Berry.no=Bcount$Berry.no,NoCBB.end=Bcount$NoCBB) 
  
  #fs.3<-list()
  for(i in 1:length(p)){
    if(p[i]=="H7") next
    if(p[i]=="H11") next
    if(years[j]==2014&p[i]=="H1") next
    
  Fset<-data.frame(read.csv(paste0(getwd(),"/Yield/",years[j],"/Branchdata_",p[i],".csv"), stringsAsFactors=F))
  Fset<-Fset[!is.na(Fset$Date),]
  
  #if(snames[i]=="B2") next
    #if(snames[i]=="B3") next
    #if(snames[i]=="B4") next
  
  d.f<-data_frame(Plot=Fset$plot,Shrub.id=Fset$Shrub.id,Branch.cm=as.character(Fset$Branch.cm),No.of.buds=as.numeric(Fset$No.of.buds),No.of.flower=as.numeric(Fset$No.of.flower),No.of.eberry=as.numeric(Fset$No.of.eberry),
                    No.of.fruits=as.numeric(Fset$No.of.fruits),NoCBB=as.numeric(Fset$NoCBB),NoCBD=as.numeric(Fset$NoCBD),No.of.leaves=as.numeric(Fset$No.of.leaves),NoLM=as.numeric(Fset$NoLM),NoCLR=as.numeric(Fset$NoCLR),iCLR=Fset$iCLR,NoChl=as.numeric(Fset$NoChl),
                    NoWilt=as.numeric(Fset$NoWilt),NoHerb=as.numeric(Fset$NoHerb),month=month(Fset$Date),day=day(Fset$Date),date=as.Date(Fset$Date),Notes=Fset$Notes)
  #replace NA values for disease counts if just not entered
  d.f <- d.f %>% mutate(NoCBB=replace(NoCBB,is.na(NoCBB)&!is.na(No.of.fruits),0),NoCBD=replace(NoCBD,is.na(NoCBD)&!is.na(No.of.fruits),0),NoLM=replace(NoLM,is.na(NoLM)&!is.na(No.of.leaves),0),NoWilt=replace(NoWilt,is.na(NoWilt)&!is.na(No.of.leaves),0))
  
  #add in final berry count
  d.f <- left_join(d.f,Bcount %>% filter(Plot==p[i]) %>% select(Shrub.id,Branch.cm,Berry.no,NoCBB.end),by=c("Shrub.id","Branch.cm"))
  
  #calculate intensity score using Zeru’s et. al. scale 1= no pustule, 2 = 1 pustule, 3 = 2 pustules and 4 = > 3 pustules
  d.f <- d.f %>% group_by(Plot,Shrub.id,Branch.cm,month,day) %>% mutate(two=str_count(iCLR,"1")-str_count(iCLR,"10"),three=str_count(iCLR,"2"),four=sum(str_count(iCLR,"3"),str_count(iCLR,"4"),str_count(iCLR,"5"),str_count(iCLR,"6"),str_count(iCLR,"7"),str_count(iCLR,"8"),str_count(iCLR,"9"),str_count(iCLR,"10"))) %>%
    mutate(iCLR2=sum(two*2,three*3,four*4,na.rm=T)/sum(two,three,four,na.rm=T)) %>% mutate(iCLR2=replace(iCLR2,is.na(iCLR2),1)) %>% mutate(iCLR=replace(iCLR,!is.na(iCLR),as.numeric(iCLR2)))
  
  #note whether measurements have been on same branch/shoot throughout monitoring period
  d.f <- d.f %>% group_by(Plot,Shrub.id,Branch.cm,month,day) %>% mutate(same=1) %>% mutate(same=replace(same,grep("changed",Notes),0))
  
  #calculate fruitset
  #sum buds, flowers and eberries per monitoring event
  d.f <-d.f %>% group_by(Plot,Shrub.id,Branch.cm,month,day) %>% mutate(Tot.flowers=sum(No.of.buds,No.of.flower,No.of.eberry,na.rm=T)) %>% ungroup()
  
  
  combo <- d.f %>% group_by(Plot,Shrub.id,Branch.cm) %>% 
    summarise(No.fruits=max(No.of.fruits,na.rm=T), fruitset=max(No.of.fruits,na.rm=T)/max(Tot.flowers,na.rm=T),propCBB=max(NoCBB,na.rm=T)/max(No.of.fruits,na.rm=T),propCBD=max(NoCBD,na.rm=T)/max(No.of.fruits,na.rm=T),
              fruit.drop=max(No.of.fruits,na.rm=T)-mean(Berry.no,na.rm=T),No.leaves=max(No.of.leaves,na.rm=T),leaf.drop=max(No.of.leaves,na.rm=T)-min(No.of.leaves,na.rm=T),PropLM=max(NoLM,na.rm=T)/max(No.of.leaves,na.rm=T),
              PropCLR=max(NoCLR,na.rm=T)/max(No.of.leaves,na.rm=T),iCLR=mean(No.of.leaves*as.numeric(iCLR)/No.of.leaves,na.rm=T),PropChl=max(NoChl,na.rm=T)/max(No.of.leaves,na.rm=T),PropWilt=max(NoWilt,na.rm=T)/max(No.of.leaves,na.rm=T),
              PropHerb=max(NoHerb,na.rm=T)/max(No.of.leaves,na.rm=T),prop.ldrop = leaf.drop/No.leaves, same=prod(same,na.rm=T)) %>% ungroup()
  
  #if year is 2014, cannot calculate fruitset
  #replace fruitset values > 1 with 1 and CBB, CBD and fruitset values with 0 if fruits are 0
  if(years[j]!=2014) combo <- combo %>% mutate(fruitset = replace(fruitset,fruitset>1,1), propCBD=replace(propCBD,No.fruits==0,0), propCBB = replace(propCBB,No.fruits==0,0), fruitset=replace(fruitset,No.fruits==0,0)) else combo <- combo %>% mutate(fruitset = replace(fruitset,fruitset>1,NA),fruitset=replace(fruitset,No.fruits==0,NA),propCBD=replace(propCBD,No.fruits==0,0), propCBB = replace(propCBB,No.fruits==0,0))
  
  #replace leaf disease measures if leaves are 0
  combo <- combo %>% mutate(PropLM = replace(PropLM,No.leaves==0,0), PropCLR = replace(PropCLR,No.leaves==0,0), iCLR = replace(iCLR,No.leaves==0|PropCLR==0,0), PropChl = replace(PropChl,No.leaves==0,0),PropWilt = replace(PropWilt,No.leaves==0,0),PropHerb = replace(PropHerb,No.leaves==0,0),prop.ldrop=replace(prop.ldrop,No.leaves==0,0)) 
  
  #replace negative fruit drop values
  tmp<-d.f %>% select(Plot,Shrub.id,Branch.cm,No.of.fruits) %>% group_by(Plot,Shrub.id,Branch.cm) %>% summarise(fruit.drop2=max(No.of.fruits,na.rm=T)-min(No.of.fruits[No.of.fruits!=0],na.rm=T))
  
  combo<-left_join(combo,tmp, by=c("Plot","Shrub.id","Branch.cm"))
  combo <- combo %>% group_by(Plot,Shrub.id,Branch.cm)  %>% mutate(fruit.drop=replace(fruit.drop,fruit.drop<0|is.na(fruit.drop),fruit.drop2[fruit.drop<0|is.na(fruit.drop)]),propCBB=replace(propCBB,is.na(propCBB),0),propCBD=replace(propCBD,is.na(propCBD),0),fruit.drop=replace(fruit.drop,No.fruits==0,0))

  
  final[[i]]<- combo %>% filter(same==1)
  #take shrub-level averages
  combo2 <- combo %>% filter(same==1) %>% group_by(Plot,Shrub.id) %>% summarise(Tot.fruits=sum(No.fruits,na.rm=T),fruitset=sum(No.fruits,na.rm=T)/sum(No.fruits/fruitset,na.rm=T),propCBB=sum(propCBB*No.fruits,na.rm=T)/sum(No.fruits),propCBD=sum(propCBD*No.fruits,na.rm=T)/sum(No.fruits),fruit.drop=sum(fruit.drop,na.rm=T),
              Tot.leaves=sum(No.leaves,na.rm=T),leaf.drop=sum(leaf.drop,na.rm=T),prop.ldrop=sum(leaf.drop,na.rm=T)/sum(No.leaves,na.rm=T),PropLM=sum(PropLM*No.leaves,na.rm=T)/sum(No.leaves),PropCLR=sum(PropCLR*No.leaves,na.rm=T)/sum(No.leaves,na.rm=T),iCLR=sum(iCLR*PropCLR*No.leaves,na.rm=T)/sum(PropCLR*No.leaves),PropWilt=sum(No.leaves*PropWilt,na.rm=T)/sum(No.leaves,na.rm=T),
              PropHerb=sum(No.leaves*PropHerb,na.rm=T)/sum(No.leaves,na.rm=T))
  #replace fruitset values > 1 with 1 and CBB, CBD and fruitset values with 0 if fruits are 0
  if(years[j]!=2014) combo2 <- combo2 %>% mutate(fruitset = replace(fruitset,fruitset>1,1), propCBD=replace(propCBD,Tot.fruits==0,0), propCBB = replace(propCBB,Tot.fruits==0,0), fruitset=replace(fruitset,Tot.fruits==0,0)) else combo2 <- combo2 %>% mutate(fruitset = replace(fruitset,fruitset>1,NA),fruitset=replace(fruitset,Tot.fruits==0,NA),propCBD=replace(propCBD,Tot.fruits==0,0), propCBB = replace(propCBB,Tot.fruits==0,0))
  
  final.s[[i]]<-combo2
  }  
  #write final files
  final.1<-do.call(rbind.data.frame,final)
  #remove final two colums
  final.1<-final.1[,1:(ncol(final.1)-2)]
  #write per branch disease measures to file
  write.csv(final.1,paste0(getwd(),"/Disease/Final.Disease.branch_",years[j],".csv"))
  
  final.s1<-do.call(rbind.data.frame,final.s)
  #write per shrub disease measures to file
  write.csv(final.s1,paste0(getwd(),"/Disease/Final.Disease.shrub_",years[j],".csv"))
  
}

