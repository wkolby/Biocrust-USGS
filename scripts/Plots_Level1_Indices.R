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
  theme_bw()
ggsave(paste(github_dir,'/figures/',"Box_ASD_NDVI.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=reorder(Type,NDVI), y=NDVI, fill=Type)) + 
  geom_boxplot() +
  scale_fill_manual(values=c('red3','green3','cyan2','purple'))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_wrap(~Treat)+
  theme_bw()
ggsave(paste(github_dir,'/figures/',"Box_ASD_NDVI_byTreatment.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=reorder(Treat,NDVI), y=NDVI, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_wrap(~Type)+
  theme_bw()
ggsave(paste(github_dir,'/figures/',"Box_ASD_NDVI_byType.png",sep=""),dpi=300,width=180,height=120,units='mm')

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
  theme_bw()
ggsave(paste(github_dir,'/figures/',"Box_Headwall_NDVI.png",sep=""),dpi=300,width=180,height=120,units='mm')

headB2<-subset(head, Type %in% c('LCY','DCY','MSS'))

ggplot(headB2, aes(x=reorder(Type,NDVI), y=NDVI, fill=Type)) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  scale_fill_manual(values=c('red3','cyan2','purple'))+
  facet_wrap(~Treat)+
  theme_bw()
ggsave(paste(github_dir,'/figures/',"Box_Headwall_B2_NDVI_byTreatment.png",sep=""),dpi=300,width=180,height=120,units='mm')

asdB2<-subset(asd, Plot %in% c('B2'))
asdB2<-subset(asdB2, Treat %in% c('CC','LW'))
asdB2<-subset(asdB2, Type %in% c('LCY','DCY','MSS'))

ggplot(asdB2, aes(x=reorder(Type,NDVI), y=NDVI, fill=Type)) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  scale_fill_manual(values=c('red3','cyan2','purple'))+
  facet_wrap(~Treat)+
  theme_bw()
ggsave(paste(github_dir,'/figures/',"Box_ASD_B2_NDVI_byTreatment.png",sep=""),dpi=300,width=180,height=120,units='mm')

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
ggsave(paste(github_dir,'/figures/',"Scatterplot_ASD_Headwall_Treatment.png",sep=""),dpi=300,width=180,height=120,units='mm')

#####ASD############################################################################################################
#open data files
asd_plot <- read.csv2(paste(github_dir,'/data/Level1/',"ASD_Bands_Indices_Plot.csv",sep=''),sep=',',header=T)
asd_plot_longer <- asd_plot %>% pivot_longer(cols = 4:13,
                                       names_to = "Index", 
                                       values_to = "Val")
#Indices
asd_plot_longer$Val<-as.numeric(asd_plot_longer$Val)
asd_plot_longer<-subset(asd_plot_longer, Index %in% c('BLUE','GREEN','RED','REDEDGE','NIR'))

ggplot(asd_plot_longer, aes(x=reorder(Index,Val), y=Val, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  labs(y='Reflectance',x='')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Box_ASD_Reflectance_Treat.png",sep=""),dpi=300,width=180,height=120,units='mm')


#####Resonon############################################################################################################
#open data files
reson <- read.csv2(paste(github_dir,'/data/Level1/',"RESONON_Bands_Indices_Plot.csv",sep=''),sep=',',header=T)
reson_longer <- reson %>% pivot_longer(cols = 4:13,
               names_to = "Index", 
               values_to = "Val")
#Indices
reson_longer$Val<-as.numeric(reson_longer$Val)
reson_longer<-subset(reson_longer, Index %in% c('BLUE','GREEN','RED','REDEDGE','NIR'))

ggplot(reson_longer, aes(x=reorder(Index,Val), y=Val, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  labs(y='Reflectance',x='')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Box_Resonon_Reflectance_Treat.png",sep=""),dpi=300,width=180,height=120,units='mm')

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

