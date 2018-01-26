#creation of dataset for analysis of cocoa ES benefits/dis-benefits
library(corrplot)
library(gdata)
library(lubridate)
library(lattice)
library(tidyverse)

#detach("package:arm", unload=TRUE)
setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu")

#load plot names
ns<-read.csv("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu/plotnums.csv")

#load soil data
ES.soil<-read.csv(paste0(getwd(),"/Nutrients/Soils/Soil_variables.csv"))

#load yield data
ES.yield<-read.csv(paste0(getwd(),"/Yield/Yield.shrub.2015.csv"))

#load pollinator data
#ES.pollinos<-read.csv(paste0(getwd(),"/Pollination/Pollinator.nos.HC1415.csv"))
#ES.pollinabund<-read.csv(paste0(getwd(),"/Pollination/Pollinator.diversity.HC1415.csv"))
#ES.pollination

#load disease data
#ES.disease<-read.csv(paste0(getwd(),"/Disease/Seasonal_meanpestincidence.csv"))
#ES.disease<-ES.disease[ES.disease$season.1415==1,]
ES.branch.disease<-read.csv(paste0(getwd(),"/Yield/Final.disease.branch_2015.csv"))
#take average for shrub
ES.shrub.disease<-ddply(ES.branch.disease,.(Plot,Shrub.id),summarise,PropCLR=mean(PropCLR,na.rm=T),PropChl=mean(PropChl,na.rm=T),PropWilt=mean(PropWilt,na.rm=T),PropHerb=mean(PropHerb,na.rm=T),iCLR=mean(iCLR,na.rm=T),PropCBB=mean(PropCBB,na.rm=T),PropCBD=mean(PropCBD,na.rm=T))

#load fruitset/pollination data
ES.fruitset<-read.csv(paste0(getwd(),"/Yield/Final.fruitset.branch_2015.csv"))
#take shrub average
ES.shrub.fruitset<-ddply(ES.fruitset,.(plot,shrub.id),summarise,flower.set=mean(flower.set,na.rm=T),fruit.set=mean(fruit.set,na.rm=T),fruit.drop=mean(fruit.drop,na.rm=T))

#load diversity data
ES.vegetation<-read.csv(paste0(getwd(),"/CarbonStock/Tree_plotdata.csv"))
ES.veg1<-data.frame(cbind(as.character(ES.vegetation[,2]),ES.vegetation$Shannon.i1,ES.vegetation$BA1.legume,ES.vegetation$BA1.deciduous,ES.vegetation$BA1.shade,ES.vegetation$BA1.npioneer,ES.vegetation$BA1.pioneer,ES.vegetation$meanDBH1),stringsAsFactors = F)
colnames(ES.veg1)<-c("Plot","Veg.ShannonIndex","Veg.BAlegume","Veg.BAdeciduous","Veg.BAshade","Veg.BAnpioneer","Veg.BApioneer")

#load micro-climate data
ES.metdata<-read.csv(paste0(getwd(),"/MetData/MonthlyStress_estimates.csv"))

#get vegetation stress data for flowering
ES.metdata1<-ES.metdata[as.Date(ES.metdata$month)<"2015-04-01",]
ES.flower<-ddply(ES.metdata1,.(Plot),summarise,maxVPD=mean(maxVPD,na.rm=T),stress.mm=mean(stress,na.rm=T),stress.mm1=mean(stress1,na.rm=T),stress.days=max(stress.daysinarow,na.rm=T),stress.days1=max(stress.daysinarow1,na.rm=T))
colnames(ES.flower)<-c(paste0("flower.",colnames(ES.flower)))
#get vegetation stress data for fruiting
ES.metdata1<-ES.metdata[as.Date(ES.metdata$month)>"2015-03-01"&as.Date(ES.metdata$month)<"2015-10-01",]
ES.fruit<-ddply(ES.metdata1,.(Plot),summarise,maxVPD=mean(maxVPD,na.rm=T),stress.mm=mean(stress,na.rm=T),stress.mm1=mean(stress1,na.rm=T),stress.days=max(stress.daysinarow,na.rm=T),stress.days1=max(stress.daysinarow1,na.rm=T))
colnames(ES.fruit)<-c(paste0("fruit.",colnames(ES.fruit)))
#remove H1 and H9
ES.fruit<-ES.fruit[ES.fruit$fruit.Plot!="H1"&ES.fruit$fruit.Plot!="H9",]

#load household data
ES.manage<-read.xls(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Data/Yayu/HouseholdSurvey/Household_Data.xlsx"),sheet=1)
ES.management<-data.frame(cbind(as.character(ES.manage$plotcode),ES.manage$Coffeearea.ha.,ES.manage$Compost.input..kg.ha.,ES.manage$Total.labour.intensity..day.ha.yr.))
colnames(ES.management)<-c("Plot","Coffee.area","Compost","Labour")

#add canopy measure
ES.management$GapWet<-ns[match(ES.management$Plot,ns$name),"GapJuly_15"]
ES.management$GapDry<-ns[match(ES.management$Plot,ns$name),"Gap_Nov14"]
ES.management$Elevation<-ns[match(ES.management$Plot,ns$name),"elevation"]
ES.management$PatchSize<-ns[match(ES.management$Plot,ns$name),"PatchSize"]

#add seasonal yield measures
d.F<-data.frame(ES.yield)
d.F[,(ncol(d.F)+1):(ncol(d.F)+ncol(ES.management)-1)]<-ES.management[match(ES.yield$Plot,ES.management$Plot),2:ncol(ES.management)]
#add soil data
d.F[,(ncol(d.F)+1):(ncol(d.F)+ncol(ES.soil)-3)]<-ES.soil[match(d.F$Plot,ES.soil$Plot),3:(ncol(ES.soil)-3)]
#add vegetation diversity data
d.F[,(ncol(d.F)+1):(ncol(d.F)+6)]<-ES.veg1[match(d.F$Plot,ES.veg1$Plot),2:7]

#add fruit set
d.F[,(ncol(d.F)+1):(ncol(d.F)+ncol(ES.shrub.fruitset)-2)]<-ES.shrub.fruitset[match(interaction(d.F$Plot,d.F$Shrub.id),interaction(ES.shrub.fruitset$plot,ES.shrub.fruitset$shrub.id)),3:ncol(ES.shrub.fruitset)]

#add disease data
d.F[,(ncol(d.F)+1):(ncol(d.F)+7)]<-ES.shrub.disease[match(interaction(d.F$Plot,d.F$Shrub.id),interaction(ES.shrub.disease$Plot,ES.shrub.disease$Shrub.id)),3:ncol(ES.shrub.disease)]

#add metdata
d.F[,(ncol(d.F)+1):(ncol(d.F)+ncol(ES.flower)-1)]<-ES.flower[match(d.F$Plot,ES.flower$flower.Plot),2:ncol(ES.flower)]
d.F[,(ncol(d.F)+1):(ncol(d.F)+ncol(ES.fruit)-1)]<-ES.fruit[match(d.F$Plot,ES.fruit$fruit.Plot),2:ncol(ES.fruit)]

#do correlation matrices to remove correlated variables
d.C<-cbind(d.F[,8:19],d.F[,21:ncol(d.F)])
d.C<-data.frame(lapply(d.C,as.numeric))
s<-cor(d.C,use="complete.obs")
s[is.na(s)]<-0

pdf(paste0(getwd(),"/Analysis/ES/Corrplot_monthly_2015.pdf"))
corrplot(s, method = "circle",tl.cex = .7)
dev.off()

#d.F<-cbind(d.F[,2:8],d.F[,10:ncol(d.F)])
#d.F$DBH<-as.numeric(as.character(d.F$DBH))
#add kebele as random variable
d.F$kebele<-ns[match(d.F$Plot,ns$name),"Kebele"]

#write dataset for reference
write.csv(d.F,paste0(getwd(),"/Analysis/ES/ES_analysis_dataset.csv"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#rescale values to z-scores
d.F.z<-d.F[,1:6]
d.F.z[,7:(ncol(d.F)-2)]<-scale(d.F[,7:(ncol(d.F)-2)])
colnames(d.F.z)<-c(colnames(d.F.z[,1:6]),paste0("z.",colnames(d.F[,7:(ncol(d.F)-2)])))
#do correlation matrices to remove correlated variables
d.C<-d.F.z[,7:ncol(d.F.z)]
s<-cor(d.C,use="complete.obs")
s[is.na(s)]<-0

pdf(paste0(getwd(),"/Analysis/ES/Corrplot_z.HC1415.v2.pdf"))
corrplot(s, method = "circle",tl.cex = .7)
dev.off()

#save re-scaled dataset
write.csv(d.F.z,paste0(getwd(),"/Analysis/ES/ES_analysis_z.dataset.csv"))

