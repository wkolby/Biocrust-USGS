setwd("/Users/wksmith/Documents/GitHub/Biocrust-USGS")
github_dir <- "/Users/wksmith/Documents/GitHub/Biocrust-USGS"
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggpubr)
library(RColorBrewer)

#####ASD############################################################################################################
#open data files
asd <- read.csv2(paste(github_dir,'/data/Level1/',"Chlorophyll_Indices_Full.csv",sep=''),sep=',',header=T)
#Indices
asd$NDVI<-as.numeric(asd$NDVI)
asd$CI1<-as.numeric(asd$CI1)
asd$CI2<-as.numeric(asd$CI2)

ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=reorder(Type,NDVI), y=NDVI, fill=Type)) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_wrap(~Treat)+
  theme_bw()
ggsave(paste(github_dir,'/figures/',"Box_ASD_NDVI.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=reorder(Type,CI2), y=CI2, fill=Type)) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_wrap(~Treat)+
  theme_bw()
ggsave(paste(github_dir,'/figures/',"Box_ASD_CI2.png",sep=""),dpi=300,width=180,height=120,units='mm')

######Micasense###########################################################################################################
#open data files
mica <- read.csv2(paste(github_dir,'/data/Level1/',"Micasense.csv",sep=''),sep=',',header=T)
#Indices
mica$Val<-as.numeric(mica$Val)
mica<-subset(mica, Wavelength %in% c('Blue','Green','Red','Red Edge','NIR'))

ggplot(mica, aes(x=reorder(Wavelength,Val), y=Val, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  labs(y='Reflectance',x='')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Box_MicaSense_Reflectance_Treat.png",sep=""),dpi=300,width=180,height=120,units='mm')


######WorldView3###########################################################################################################
#open data files
wv3 <- read.csv2(paste(github_dir,'/data/Level1/',"WorldView3.csv",sep=''),sep=',',header=T)
#Indices
wv3$Val<-as.numeric(wv3$Val)
wv3<-subset(wv3, Wavelength %in% c('Blue','Green','Red','Red Edge','NIR1'))

ggplot(wv3, aes(x=reorder(Wavelength,Val), y=Val, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  labs(y='Reflectance',x='')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Box_WorldView3_Reflectance_Treat.png",sep=""),dpi=300,width=180,height=120,units='mm')

######Scatterplot#################################
merge <- data.frame()
merge <-cbind(mica$Wavelength,mica$Treat,mica$Val,wv3$Val)
colnames(merge) <- c("Wavelength","Treatment","Micasense","WorldView3")
merge=transform(merge,Micasense = as.numeric(Micasense))
merge=transform(merge,WorldView3 = as.numeric(WorldView3))

ggplot(merge, aes(x=Micasense, y=WorldView3,color=Treatment)) +
  scale_color_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  geom_point(size=3)+
  geom_smooth(method=lm,aes(group = Treatment))+
  stat_cor(aes(group = Treatment,label = after_stat(rr.label)),geom = "label")+
  labs(y='WorldView3',x='Micasense')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_Micasense_WorldView3_Treatment.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(merge, aes(x=Micasense, y=WorldView3,color=Wavelength)) +
  scale_color_manual(values=c("#3288BD","#99D594","#5E4FA2","#D53E4F","#9E0142"))+
  geom_point(size=3)+
  geom_smooth(method=lm,aes(group = Wavelength))+
  stat_cor(aes(group = Wavelength,label = after_stat(rr.label)),geom = "label")+
  labs(y='WorldView3',x='Micasense')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_Micasense_WorldView3_Wavelength.png",sep=""),dpi=300,width=180,height=120,units='mm')

