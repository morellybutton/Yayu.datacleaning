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
ns<-data_frame(Plot=as.character(ns$name),elevation=ns$elevation,patcharea=ns$PatchArea,GapWet=ns$GapJuly_15,GapDry=ns$Gap_Nov14,
               plotsize=ns$plotsize,buffer=ns$Buffer..1.yes.,kebele=ns$Kebele)

#load soil data
ES.soil<-read.csv(paste0(getwd(),"/Nutrients/Soils/Soil_nutrient_data.csv"))
ES.soil <- data_frame(Plot=as.character(ES.soil$Plot.Code),N.pct=ES.soil$N.pct,C.pct=ES.soil$C.pct,CN.ratio=ES.soil$CN.ratio,pH=ES.soil$pH,Tot.P.ppm=ES.soil$Avail.P.ppm,Ca.meq=ES.soil$Ca.meq,
                      K.meq=ES.soil$K.meq,Mg.meq=ES.soil$Mg.meq,Fe.ppm=ES.soil$Fe.ppm,eCEC.mol.kg=ES.soil$eCEC.mol.kg)

#load shrub yield data for years 2014-2016
years<-c("2014","2015","2016")
tmp<-list()
for(j in 1:length(years)){
  tmp.1<-read.csv(paste0(getwd(),"/Yield/Shoot.ha.yields_",years[j],".csv"))
  #add year
  tmp.1$year<-years[j]
  tmp[[j]]<-tmp.1
}
ES.yield<-do.call(rbind.data.frame,tmp)
ES.yield<-data_frame(Plot=as.character(ES.yield$Plot),Shrub.id=as.character(ES.yield$Shrub.id),Shrub.kg=ES.yield$wt/1000,year=as.numeric(ES.yield$year))
#remove duplicate rows for WE2-?
ES.yield<-ES.yield %>% filter(Shrub.id!="WE2-?")

#get plot level variability (std) and mean yield, in case not same shrub throughout monitoring period
x.yield <- ES.yield %>% group_by(Plot) %>% summarise(avg.kg=mean(Shrub.kg, na.rm=T),std.kg=sd(Shrub.kg,na.rm=T))

#add to yearly yield measures
ES.yield <- left_join(ES.yield,x.yield, by="Plot")

#spread data out by yield per year to calculate variability in yield (mean and SD)
#x.yield<-ES.yield %>% spread(key="year",value="Shrub.kg")
#colnames(x.yield)<-c("Plot","Shrub.id",paste0("kg.",colnames(x.yield[,3:5])))
#get plot level variability (std) and mean yield, in case not same shrub throughout monitoring period
#y.yield <- x.yield %>% group_by(Plot,Shrub.id) %>% mutate(avg=mean("kg.2014","kg.2015","kg.2016",na.rm=T),std=sd("kg.2014","kg.2015","kg.2016",na.rm=T))

#load pollinator data
#ES.pollinos<-read.csv(paste0(getwd(),"/Pollination/Pollinator.nos.HC1415.csv"))
#ES.pollinabund<-read.csv(paste0(getwd(),"/Pollination/Pollinator.diversity.HC1415.csv"))
#ES.pollination

#load disease and fruitset data
#ES.disease<-read.csv(paste0(getwd(),"/Disease/Seasonal_meanpestincidence.csv"))
#ES.disease<-ES.disease[ES.disease$season.1415==1,]

tmp<-list()
for(j in 1:length(years)){
  tmp.1<-read.csv(paste0(getwd(),"/Disease/Final.Disease.shrub_",years[j],".csv"))
  #add year
  tmp.1$year<-years[j]
  tmp[[j]]<-tmp.1
}

ES.shrub.disease<-do.call(rbind.data.frame,tmp)

ES.disease <- data_frame(Plot=as.character(ES.shrub.disease$Plot),Shrub.id=as.character(ES.shrub.disease$Shrub.id),Tot.fruits=ES.shrub.disease$Tot.fruits,fruitset=ES.shrub.disease$fruitset,
                         propCBB=ES.shrub.disease$propCBB,propCBD=ES.shrub.disease$propCBD,fruit.drop=ES.shrub.disease$fruit.drop,Tot.leaves=ES.shrub.disease$Tot.leaves,leaf.drop=ES.shrub.disease$leaf.drop,
                         prop.ldrop=ES.shrub.disease$prop.ldrop,propLM=ES.shrub.disease$PropLM,propCLR=ES.shrub.disease$PropCLR,iCLR=ES.shrub.disease$iCLR, propWilt=ES.shrub.disease$PropWilt,propHerb=ES.shrub.disease$PropHerb,
                         year=as.numeric(ES.shrub.disease$year))
  
#correct NA values if leaves 0 all leaf disease measures = 0
ES.disease <- ES.disease %>% mutate(leaf.drop=replace(leaf.drop,Tot.leaves==0,0), prop.ldrop=replace(prop.ldrop,Tot.leaves==0,0),propLM=replace(propLM,Tot.leaves==0,0),propCLR=replace(propCLR,Tot.leaves==0,0),iCLR=replace(iCLR,Tot.leaves==0,0),
                                    propWilt=replace(propWilt,Tot.leaves==0,0),propHerb=replace(propHerb,Tot.leaves==0,0),iCLR=replace(iCLR,propCLR==0,0))

#load diversity data
ES.vegetation<-read.csv(paste0(getwd(),"/CarbonStock/Tree_plotdata.csv"))

ES.veg1 <- data_frame(Plot=ES.vegetation$Plot,Shannon.i=ES.vegetation$Shannon.i1,BA.legume=ES.vegetation$BA1.legume,BA.deciduous=ES.vegetation$BA1.deciduous,
                            BA.shade=ES.vegetation$BA1.shade,BA.npioneer=ES.vegetation$BA1.npioneer,BA.pioneer=ES.vegetation$BA1.pioneer)
ES.veg2 <- data_frame(Plot=ES.vegetation$Plot,Shannon.i=ES.vegetation$Shannon.i2,BA.legume=ES.vegetation$BA2.legume,BA.deciduous=ES.vegetation$BA2.deciduous,
                      BA.shade=ES.vegetation$BA2.shade,BA.npioneer=ES.vegetation$BA2.npioneer,BA.pioneer=ES.vegetation$BA2.pioneer)

#load coffee density
coffee.density<-read.csv(paste0(getwd(),"/Yield/Coffee.density_2015_cleaned.csv"))
coffee.density<-data_frame(Plot=coffee.density$Plot,SPlotNo=coffee.density$SPlotNo,TotShoots=coffee.density$TotShoots)

cdensity <- coffee.density %>% group_by(Plot,SPlotNo) %>% summarise(density=sum(TotShoots)/0.2/0.2)
cdensity <- cdensity %>% group_by(Plot) %>% summarise(density=mean(density,na.rm=T))

#load micro-climate data
ES.metdata<-read.csv(paste0(getwd(),"/MetData/Monthly_metdata_withcanopygap.csv"))
#add year
ES.metdata$year<-year(ES.metdata$month)
ES.metdata$month <- month(ES.metdata$month)

ES.metdata<-data_frame(Plot=ES.metdata$Plot,month=ES.metdata$month,year=ES.metdata$year,ah=ES.metdata$ah,tavg=ES.metdata$tavg,tmax=ES.metdata$tmax,vpd=ES.metdata$vpd)
#remove -Inf
ES.metdata <- ES.metdata %>% mutate(vpd=replace(vpd,vpd==-Inf,NA))

#get vegetation stress data for flowering
met.flower <- ES.metdata %>% filter(month>=1&month<4) %>% group_by(Plot,year) %>% summarise(vpd.flower=mean(vpd,na.rm=T),ah.flower=mean(ah,na.rm=T),tavg.flower=mean(tavg,na.rm=T),tmax.flower=mean(tmax,na.rm=T))
met.fruit <- ES.metdata %>% filter(month>=4&month<10&year<2017) %>% group_by(Plot,year) %>% summarise(vpd.fruit=mean(vpd,na.rm=T),ah.fruit=mean(ah,na.rm=T),tavg.fruit=mean(tavg,na.rm=T),tmax.fruit=mean(tmax,na.rm=T))

#load household data
ES.manage<-read.xls(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Data/Yayu/HouseholdSurvey/Household_Data.xlsx"),sheet=1)
ES.management<-data_frame(Plot=as.character(ES.manage$plotcode),coffee.area.ha=ES.manage$Coffeearea.ha.,compost=ES.manage$Compost.input..kg.ha.,labour=ES.manage$Total.labour.intensity..day.ha.yr.)

#add canopy measure
ES.management <- left_join(ES.management,ns,by="Plot")

#start with yield measures
d.F<-ES.yield

#add management data
d.F<-left_join(d.F,ES.management, by="Plot")

#add coffee density
d.F<-left_join(d.F,cdensity,by="Plot")

#add soil data
d.F<-left_join(d.F,ES.soil,by="Plot")

#add vegetation diversity data
ES.veg1$year<-2014
ES.veg2$year<-2015
ES.veg<-bind_rows(ES.veg1,ES.veg2)
ES.veg2$year<-2016
ES.veg<-bind_rows(ES.veg,ES.veg2)

d.F<-left_join(d.F,ES.veg,by=c("Plot","year"))

#add fruit set & disease data
d.F<-left_join(d.F,ES.disease,by=c("Plot","Shrub.id","year"))

#add metdata
d.F<-left_join(d.F,met.flower,by=c("Plot","year"))
d.F<-left_join(d.F,met.fruit,by=c("Plot","year"))

######restart here!!!#######
#do correlation matrices to remove correlated variables
d.C<- d.F %>% select(-Plot,-Shrub.id,-kebele)
s<-cor(d.C,use="complete.obs")
s[is.na(s)]<-0

pdf(paste0(getwd(),"/Analysis/ES/Corrplot_monthly_2014_2016.pdf"))
corrplot(s, method = "circle",tl.cex = .7)
dev.off()

#d.F<-cbind(d.F[,2:8],d.F[,10:ncol(d.F)])
#d.F$DBH<-as.numeric(as.character(d.F$DBH))
#add kebele as random variable
#d.F$kebele<-ns[match(d.F$Plot,ns$name),"Kebele"]
#save correlation matrix
write.csv(s,paste0(getwd(),"/Analysis/ES/ES_correlation_analysis.csv"))

#write dataset for reference
write.csv(d.F,paste0(getwd(),"/Analysis/ES/ES_analysis_dataset.csv"))
