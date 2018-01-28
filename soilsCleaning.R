#averages of soil nutrients

library(gdata)
library(plyr)
library(stringr)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu/Nutrients/Soils")

#load plot values
plts<-data.frame(read.csv("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu/plotnums.csv"),stringsAsFactors = F)
#remove Met station
plts<-plts[grep("Met_",plts$name,invert=T),]
#plts<-plts[grep("FC",plts$name,invert=T),]

#npp<-npp[grep("FP",npp,invert=T)]
p<-as.character(plts$name)

#load household data
#h.hold<-read.xls(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Data/Yayu/HouseholdSurvey/Household_Data.xlsx"),sheet=1)
#remove blank columns
#h.hold<-h.hold[,colSums(is.na(h.hold))<nrow(h.hold)]

#load soil nutrient values
d.F<-read.xls(paste0(getwd(),"/Combined_soildata.xlsx"),sheet=1)
#reorder depth values
d.F$Depth<-factor(d.F$Depth,levels=rev(levels(d.F$Depth)))

#add elevation, patch and gap values
#d.F$elevation<-plts[match(d.F$Plot.Code,plts$name),"elevation"]
#d.F$patchsize<-plts[match(d.F$Plot.Code,plts$name),"PatchSize"]
#d.F$GapWet<-plts[match(d.F$Plot.Code,plts$name),"GapJuly_15"]
#d.F$GapDry<-plts[match(d.F$Plot.Code,plts$name),"Gap_Nov14"]

lod<-read.xls(paste0(getwd(),"/Combined_soildata.xlsx"),sheet=2)

#take average of all measures per plot
d.F$Total.P.mg.kg<-as.numeric(as.character(d.F$Total.P.mg.kg))
d.F[is.na(d.F$Total.P.mg.kg),"Total.P.mg.kg"]<-lod[nrow(lod),"X"]
d.F$weight<-NA
d.F[d.F$Depth=="0-10","weight"]<-0.1
d.F[d.F$Depth=="10-30","weight"]<-0.2

d.F.2<-ddply(d.F[!is.na(d.F$weight),],.(Plot.Code),summarise,N.pct=sum(N.*weight,na.rm=T)/sum(weight,na.rm=T),C.pct=sum(C.*weight,na.rm=T)/sum(weight,na.rm=T),pH=sum(pH..H2O.*weight,na.rm=T)/sum(weight,na.rm=T),Avail.P.ppm=sum(Total.P.mg.kg*weight,na.rm=T)/sum(weight,na.rm=T),Ca.meq=sum(Ca2..*weight,na.rm=T)/sum(weight,na.rm=T),K.meq=sum(K..*weight,na.rm=T)/sum(weight,na.rm=T),Mg.meq=mean(Mg.2.*weight/sum(weight),na.rm=T),Fe.ppm=mean(Fe..mg.kg.*weight/sum(weight),na.rm=T))

#add in C:N ratio
d.F.2$CN.ratio<-d.F.2$C.pct/d.F.2$N.pct

write.csv(d.F.2,paste0(getwd(),"/Soil_nutrient_data.csv"))
