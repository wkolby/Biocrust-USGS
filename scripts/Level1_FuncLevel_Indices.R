setwd("/Users/wksmith/Documents/GitHub/Biocrust-USGS")
github_dir <- "/Users/wksmith/Documents/GitHub/Biocrust-USGS"
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggpubr)
library(RColorBrewer)

#####ASD############################################################################################################
#open data files
asd <- read.csv2(paste(github_dir,'/data/Level1/',"ASD_Chlorophyll_Indices_Full.csv",sep=''),sep=',',header=T)
#Indices
asd$NDVI<-as.numeric(asd$NDVI)
asd$CI1<-as.numeric(asd$CI1)
asd$CI2<-as.numeric(asd$CI2)

ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=reorder(Type,NDVI), y=NDVI, fill=Type)) + 
  geom_boxplot() +
  scale_fill_manual(values=c('red3','green3','cyan2','purple'))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  #facet_wrap(~Treat)+
  labs(y='Normalized Chlorophyll Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_FieldSpec_Chlorophyll_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')

ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=reorder(Type,NDVI), y=NDVI, fill=Type)) + 
  geom_boxplot() +
  scale_fill_manual(values=c('red3','green3','cyan2','purple'))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_wrap(~Treat)+
  labs(y='Normalized Chlorophyll Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_FieldSpec_Chlorophyll_byFunc_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')

ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=reorder(Treat,NDVI), y=NDVI, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_wrap(~Type)+
  labs(y='Normalized Chlorophyll Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_FieldSpec_Chlorophyll_byTreat_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')

#####Headwall############################################################################################################
#open data files
head <- read.csv2(paste(github_dir,'/data/Level1/',"Headwall_Chlorophyll_Indices_Full.csv",sep=''),sep=',',header=T)

#Indices
head$NDVI<-as.numeric(head$NDVI)
head$CI1<-as.numeric(head$CI1)
head$CI2<-as.numeric(head$CI2)

ggplot(subset(head, Type %in% c('LCY','DCY','MSS')), aes(x=reorder(Type,NDVI), y=NDVI, fill=Type)) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  scale_fill_manual(values=c('red3','cyan2','purple'))+
  #facet_wrap(~Treat)+
  labs(y='Normalized Chlorophyll Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_Headwall_Chlorophyll_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')

headB2<-subset(head, Type %in% c('LCY','DCY','MSS'))

ggplot(headB2, aes(x=reorder(Type,NDVI), y=NDVI, fill=Type)) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  scale_fill_manual(values=c('red3','cyan2','purple'))+
  facet_wrap(~Treat)+
  labs(y='Normalized Chlorophyll Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Box_Headwall_B2_Chlorophyll_byFunc_FuncLevel.png",sep=""),dpi=300,width=270,height=180,units='mm')

asdB2<-subset(asd, Plot %in% c('B2'))
asdB2<-subset(asdB2, Treat %in% c('CC','LW'))
asdB2<-subset(asdB2, Type %in% c('LCY','DCY','MSS'))

ggplot(asdB2, aes(x=reorder(Type,NDVI), y=NDVI, fill=Type)) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  scale_fill_manual(values=c('red3','cyan2','purple'))+
  facet_wrap(~Treat)+
  labs(y='Normalized Chlorophyll Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Box_FieldSpec_B2_Chlorophyll_byFunc_FuncLevel.png",sep=""),dpi=300,width=270,height=180,units='mm')

#####Resonon############################################################################################################
#open data files
resn <- read.csv2(paste(github_dir,'/data/Level1/',"Resonon_Chlorophyll_Indices_Full.csv",sep=''),sep=',',header=T)

#Indices
resn$NDVI<-as.numeric(resn$NDVI)
resn$CI1<-as.numeric(resn$CI1)
resn$CI2<-as.numeric(resn$CI2)

ggplot(subset(resn, Type %in% c('LCY','DCY','MSS')), aes(x=reorder(Type,NDVI), y=NDVI, fill=Type)) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  scale_fill_manual(values=c('red3','cyan2','purple'))+
  #facet_wrap(~Treat)+
  labs(y='Normalized Chlorophyll Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_Resonon_Chlorophyll_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')

resnB2<-subset(resn, Type %in% c('LCY','DCY','MSS'))

ggplot(resnB2, aes(x=reorder(Type,NDVI), y=NDVI, fill=Type)) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  scale_fill_manual(values=c('red3','cyan2','purple'))+
  facet_wrap(~Treat)+
  labs(y='Normalized Chlorophyll Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Box_Resonon_B2_Chlorophyll_byFunc_FuncLevel.png",sep=""),dpi=300,width=270,height=180,units='mm')

#########Scatterplot#############################################
merge <- data.frame()
merge <-cbind(headB2$Type,headB2$Treat,asdB2$NDVI,headB2$NDVI)
colnames(merge) <- c("Type","Treatment","asd","headwall")
merge=transform(merge,asd = as.numeric(asd))
merge=transform(merge,headwall = as.numeric(headwall))

ggplot(merge, aes(x=asd, y=headwall,color=Type)) +
  scale_color_manual(values=c('red3','cyan2','purple'))+
  geom_point(size=3)+
  facet_wrap(~Treatment)+
  geom_smooth(method=lm,aes(group = Treatment))+
  stat_cor(aes(group = Treatment,label = after_stat(rr.label)),geom = "label")+
  labs(y='Headwall',x='ASD')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_FieldSpec_Headwall_B2_byFunc_FuncLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

