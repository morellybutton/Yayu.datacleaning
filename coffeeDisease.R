#code for combining time series disease data and fruitset data for Yayu plots
library(tidyverse)
#library(stringr)
#library(plyr)
#library(ggplot2)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/")

site="Yayu"
#dates<-"nov14"
year<-"2016"

#corners1<-c("0,0","0,20","20,20","20,0")
#corners2<-c("20,0","20,20","40,20","40,0")
#corners3<-c("SW","NW","NE","SE")
#corners4<-c("0,20","20,20","20,40","0,40")
#corners<-cbind(corners1,corners2,corners3,corners4)

plts<-read.csv(paste0(getwd(),"/",site,"/plotnums.csv"))
#remove Met station and Forest plots
plts<-plts[grep("Met_",plts$name,invert=T),]
plts<-plts[grep("FC",plts$name,invert=T),]

#npp<-npp[grep("FP",npp,invert=T)]
p<-as.character(plts$name)
Fset<-data.frame(read.csv(paste0(getwd(),"/",site,"/Yield/Disease.Fruitset_",year,"_cleaned.csv")), stringsAsFactors=F)
Fset<-Fset[!is.na(Fset$Date),]

final<-list()
fs.3<-list()
for(i in 1:length(p)){
  #if(snames[i]=="B2") next
  #if(snames[i]=="B3") next
  #if(snames[i]=="B4") next
  if(p[i]=="H7") next
  if(p[i]=="H11") next
  if(year==2014&p[i]=="H1") next
  
  d.f<-Fset[Fset$Plot==p[i],]
  #pull out shrubs
  shr<-as.character(unique(d.f$ShrubId))
  final.2<-list()
  fs.2<-list()
  for(j in 1:length(shr)){
    fs<-data.frame(d.f[d.f$ShrubId==shr[j],],stringsAsFactors=F)
    #identify branches
    br<-unique(fs$Branch.cm)
    final.1<-list()
    fs.1<-list()
    for(k in 1:length(br)){
      fs1<-data.frame(fs[fs$Branch.cm==br[k],],stringsAsFactors=F)
      #fs1<-data.frame(lapply(fs1, as.character), stringsAsFactors=FALSE)
      #check if measurements have been on same shrub/branch continuously
      if(cumsum(fs1$Same)[nrow(fs1)]!=nrow(fs1)) next
      No.leaves<-data.frame(cbind(p[i],shr[j],br[k],as.numeric(as.character(fs1$No.of.leaves)),as.numeric(as.character(fs1$NoCLR))/as.numeric(as.character(fs1$No.of.leaves)),as.numeric(as.character(fs1$NoChl))/as.numeric(as.character(fs1$No.of.leaves)),as.numeric(as.character(fs1$NoWilt))/as.numeric(as.character(fs1$No.of.leaves)),as.numeric(as.character(fs1$NoHerb))/as.numeric(as.character(fs1$No.of.leaves))), stringsAsFactors = F)
      colnames(No.leaves)<-c("Plot","Shrub.id","Branch.cm","Tot.leaves","PropCLR","PropChl","PropWilt","PropHerb")
      #iCLR<-data.frame(str_split_fixed(as.character(fs1$iCLR),",",max(as.numeric(as.character(fs1$NoCLR)))),stringsAsFactors=F)
      #iCLR[iCLR==""]<-NA
      #iCLR[1,]<-as.numeric(iCLR[1:nrow(iCLR),1])
      #For CLR, the intensity scored using Zeru’s et. al. scale 1= no pustule, 2 = 1 pustule, 3 = 2 pustules and 4 = > 3 pustules
      one<-str_count(fs1$iCLR, "1")-str_count(fs1$iCLR, "10")
      two<-str_count(fs1$iCLR, "2")
      three<-str_count(fs1$iCLR, "3")
      four<-str_count(fs1$iCLR, "4")
      for(n in 5:10){
        four<-rbind(four,str_count(fs1$iCLR, paste(n)))
        #four<-sum(four,str_count(test,paste(n)))
      }
      #if(str_count(fs1$iCLR,"all")==1) four[str_count(fs1$iCLR,"all")] = as.character(fs1$NoCLR[str_count(fs1$iCLR,"all")])
      four<-colSums(four)
      No.leaves$iCLR<-(1*one+2*two+3*three+4*four)/as.numeric(as.character(fs1$NoCLR))
      No.leaves$Date<-fs1$Date
      rm(one,two,three,four)
      #find fruitset
      b1<-fs1[!is.na(fs1$No.of.buds),]
      buds<-max(as.numeric(as.character(b1$No.of.buds)))
      flowers<-sum(as.numeric(as.character(b1$No.of.flower)),na.rm=T)
      berry<-max(as.numeric(as.character(b1$No.of.eberry)),na.rm=T)-as.numeric(as.character(b1[1,"No.of.eberry"]))
      
      fl.set<-flowers/buds
      fr.set<-berry/flowers
      if(is.na(fr.set)) fr.set<-berry/buds
      
      c1<-fs1[!is.na(fs1$No.of.fruits),]
      fruits<-min(as.numeric(as.character(c1$No.of.fruits)),na.rm=T)
      if(fr.set==0|is.na(fr.set)) fr.set<-fruits/flowers*fl.set
      eberry<-max(as.numeric(as.character(b1$No.of.eberry)),na.rm=T)
      if(eberry>0) fr.drop<-(eberry-fruits)/eberry else fr.drop<-(max(as.numeric(as.character(c1$No.of.fruits)),na.rm=T)-fruits)/max(as.numeric(as.character(c1$No.of.fruits)),na.rm=T)
      if(fr.drop<0|is.na(fr.drop)) fr.drop=0
      
      Fruit.set<-data.frame(cbind(p[i],shr[j],br[k],buds,flowers,eberry,fl.set,fr.set,fruits,fr.drop))
      colnames(Fruit.set)<-c("plot","shrub.id","branch","buds","flowers","eberry","flower.set","fruit.set","fruits","fruit.drop")
      fs.1[[k]]<-Fruit.set
      No.fruits<-data.frame(cbind(as.numeric(as.character(c1$No.of.fruits)),as.numeric(as.character(c1$NoCBB))/as.numeric(as.character(c1$No.of.fruits)),as.numeric(as.character(c1$NoCBD))/as.numeric(as.character(c1$No.of.fruits))), stringsAsFactors = F)
      colnames(No.fruits)<-c("Tot.fruits","PropCBB","PropCBD")
      No.fruits$Date<-c1$Date

      branch<-cbind(No.leaves,No.fruits[match(No.leaves$Date,No.fruits$Date),1:3])
      rownames(branch)<-1:nrow(branch)
      
      final.1[[k]]<-branch
    }
    fs.2[[j]]<-do.call(rbind.data.frame,fs.1)
    final.2[[j]]<-do.call(rbind.data.frame,final.1) 
  }
  fs.3[[i]]<-do.call(rbind.data.frame,fs.2)
  final[[i]]<-do.call(rbind.data.frame,final.2)
}

Final<-do.call(rbind.data.frame,final)
#correct NAs
Final[Final$PropCLR==0|is.na(Final$PropCLR),"iCLR"]<-0
Final[Final$Tot.fruits==0|is.na(Final$Tot.fruits),"PropCBB"]<-0
Final[Final$Tot.fruits==0|is.na(Final$Tot.fruits),"PropCBD"]<-0

FS<-do.call(rbind.data.frame,fs.3)  
FS<-data.frame(lapply(FS,as.character),stringsAsFactors = F)
FS[FS$buds==-Inf,"buds"]<-NA
FS[FS$eberry==-Inf,"eberry"]<-NA

write.csv(Final,paste0(getwd(),"/",site,"/Yield/Final.disease.branch_",year,".csv"))
write.csv(FS,paste0(getwd(),"/",site,"/Yield/Final.fruitset.branch_",year,".csv"))

#take plot averages of disease measures for each month
Final[,4:8]<-data.frame(lapply(Final[,4:8],as.numeric),stringsAsFactors = F)
Final.1<-ddply(Final,.(Plot,Date),summarise,PropCLR=mean(PropCLR,na.rm=T),PropChl=mean(PropChl,na.rm=T),PropWilt=mean(PropWilt,na.rm=T),PropHerb=mean(PropHerb,na.rm=T),iCLR=mean(iCLR,na.rm=T),PropCBB=mean(PropCBB,na.rm=T),PropCBD=mean(PropCBD,na.rm=T))
write.csv(Final.1,paste0(getwd(),"/",site,"/Yield/Final.disease.plot_",year,".csv"))

FS[,4:10]<-data.frame(lapply(FS[,4:10],as.numeric),stringsAsFactors = F)
FS[FS=="Inf"|is.na(FS)]<-NA
FS.1<-ddply(FS,.(plot),summarise,flower.set=mean(flower.set,na.rm=T),fruit.set=mean(fruit.set,na.rm=T),fruit.drop=mean(fruit.drop,na.rm=T))
write.csv(FS.1,paste0(getwd(),"/",site,"/Yield/Final.fruitset.plot_",year,".csv"))
