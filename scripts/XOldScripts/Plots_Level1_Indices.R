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
  scale_y_continuous("Chlorophyll Index",expand = c(0, 0), limits = c(0.1, 0.4),breaks=c(.1,.2,.3,.4)) +
  scale_x_discrete(labels=c('L. Cyano','D. Cyano','Lichen','Moss'),guide = guide_axis(angle = 45)) +
  theme_bw() +
  labs(fill='',x='',y='',title='') +
  theme(legend.position = c(2,2),legend.text=element_text(size=20),legend.title=element_text(size=24)) +
  theme(legend.background = element_rect(linetype="solid", colour ="black"))+
  theme(text = element_text(size = 24))+
  stat_summary(fun.y=mean, geom="point", fill='black',colour='black',shape=18, size=5)
ggsave(paste(github_dir,'/figures/',"Box_ASD_NDVI.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=reorder(Type,NDVI), y=NDVI, fill=Type)) + 
  geom_boxplot() +
  facet_wrap(~Treat)+
  scale_fill_manual(values=c('red3','green3','cyan2','purple'))+
  scale_y_continuous("Chlorophyll Index",expand = c(0, 0), limits = c(0.1, 0.4),breaks=c(.1,.2,.3,.4)) +
  scale_x_discrete(labels=c('L. Cyano','D. Cyano','Lichen','Moss'),guide = guide_axis(angle = 45)) +
  theme_bw() +
  labs(fill='',x='',y='',title='') +
  theme(legend.position = c(2,2),legend.text=element_text(size=20),legend.title=element_text(size=24)) +
  theme(legend.background = element_rect(linetype="solid", colour ="black"))+
  theme(text = element_text(size = 24))+
  stat_summary(fun.y=mean, geom="point", fill='black',colour='black',shape=18, size=5)
ggsave(paste(github_dir,'/figures/',"Box_ASD_NDVI_byTreatment.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(subset(asd, Treat %in% c('CC','CW','LC','LW')), aes(x=reorder(Treat,NDVI), y=NDVI, fill=Treat)) + 
  geom_boxplot() +
  facet_wrap(~Type)+
  scale_fill_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  scale_y_continuous("Chlorophyll Index",expand = c(0, 0), limits = c(0.1, 0.4),breaks=c(.1,.2,.3,.4)) +
  scale_x_discrete(labels=c('Control','Alt. Precip.','Warming','Warming + \nAlt. Precip.'),guide = guide_axis(angle = 45)) +
  theme_bw() +
  labs(fill='',x='',y='',title='') +
  theme(legend.position = c(2,2),legend.text=element_text(size=20),legend.title=element_text(size=24)) +
  theme(legend.background = element_rect(linetype="solid", colour ="black"))+
  theme(text = element_text(size = 24))+
  stat_summary(fun.y=mean, geom="point", fill='black',colour='black',shape=18, size=5)
  stat_summary(fun.y=mean, geom="point", shape=5, size=4)
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

#######################################PLOT LEVEL###################################################################
########################################MERGED######################################################################
#open data files
Merged <- read.csv2(paste(github_dir,'/data/Level1/',"Merged_Indices_Plot.csv",sep=''),sep=',',header=T)
Merged_long <- Merged %>% pivot_longer(cols = 5:14,
                                             names_to = "Index", 
                                             values_to = "Val")
#Indices
Merged_long$Val<-as.numeric(Merged_long$Val)
Merged_long<-subset(Merged_long, Index %in% c('BLUE','GREEN','RED','REDEDGE','NIR'))

ggplot(Merged_long, aes(x=reorder(Index,Val), y=Val, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  facet_wrap(~Sensor, labeller = labeller(cyl = c("A","B","C","D")))+
  labs(y='Reflectance',x='')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Box_Merged_SpecBands_Treat.png",sep=""),dpi=300,width=180,height=120,units='mm')

######Scatterplot#################################
Merged_wide <- Merged_long %>% pivot_wider(names_from = Sensor, values_from = Val)
colnames(Merged_wide)<-c('ID','Plot','Treat','Index','Resonon','FieldSpec','MicaSense','WorldView3')

ggplot(Merged_wide, aes(x=FieldSpec, y=Resonon,color=Treat)) +
  scale_color_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  geom_point(size=3)+
  geom_smooth(method=lm,aes(group = Treat))+
  stat_cor(aes(group = Treat,label = after_stat(rr.label)),geom = "label")+
  labs(y='Resonon',x='FieldSpec')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_FieldSpec_Resonon_Treatment.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(Merged_wide, aes(x=FieldSpec, y=Resonon,color=Index)) +
  scale_color_manual(values=c("#3288BD","#99D594","#5E4FA2","#D53E4F","#9E0142"))+
  geom_point(size=3)+
  geom_smooth(method=lm,aes(group = Index))+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),geom = "label")+
  labs(y='Resonon',x='FieldSpec')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_FieldSpec_Resonon_SpecBand.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(Merged_wide, aes(x=FieldSpec, y=MicaSense,color=Treat)) +
  scale_color_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  geom_point(size=3)+
  geom_smooth(method=lm,aes(group = Treat))+
  stat_cor(aes(group = Treat,label = after_stat(rr.label)),geom = "label")+
  labs(y='MicaSense',x='FieldSpec')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_FieldSpec_MicaSense_Treatment.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(Merged_wide, aes(x=FieldSpec, y=MicaSense,color=Index)) +
  scale_color_manual(values=c("#3288BD","#99D594","#5E4FA2","#D53E4F","#9E0142"))+
  geom_point(size=3)+
  geom_smooth(method=lm,aes(group = Index))+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),geom = "label")+
  labs(y='MicaSense',x='FieldSpec')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_FieldSpec_MicaSense_SpecBand.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(Merged_wide, aes(x=FieldSpec, y=WorldView3,color=Treat)) +
  scale_color_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  geom_point(size=3)+
  geom_smooth(method=lm,aes(group = Treat))+
  stat_cor(aes(group = Treat,label = after_stat(rr.label)),geom = "label")+
  labs(y='WorldView3',x='FieldSpec')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_FieldSpec_WorldView3_Treatment.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(Merged_wide, aes(x=FieldSpec, y=WorldView3,color=Index)) +
  scale_color_manual(values=c("#3288BD","#99D594","#5E4FA2","#D53E4F","#9E0142"))+
  geom_point(size=3)+
  geom_smooth(method=lm,aes(group = Index))+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),geom = "label")+
  labs(y='WorldView3',x='FieldSpec')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_FieldSpec_WorldView3_SpecBand.png",sep=""),dpi=300,width=180,height=120,units='mm')

