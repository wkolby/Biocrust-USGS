setwd("/Users/wksmith/Documents/GitHub/Biocrust-USGS")
github_dir <- "/Users/wksmith/Documents/GitHub/Biocrust-USGS"
library(tidyverse)
#library(tidyr)
#library(dplyr)
library(ggpubr)
library(RColorBrewer)

#####ASD############################################################################################################
#open data files
asd <- read.csv2(paste(github_dir,'/data/Level1/',"ASD_Indices_Full.csv",sep=''),sep=',',header=T)
#Indices
asd$NDVI<-as.numeric(asd$NDVI)
asd$BI<-as.numeric(asd$BI)
asd$NDWI<-as.numeric(asd$NDWI)
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

trt_names=c('CC' = 'Control','CW' = 'AltP','LC' = 'Warmed','LW' = 'AltP + Warmed')
asd$Type <- factor(asd$Type,levels=c("LCY","DCY","LCN","MSS"))
ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=Type, y=NDVI, fill=Type)) + 
  geom_boxplot() +
  scale_fill_manual(name='Func Type', values=c('red3','green3','cyan2','purple'),labels=c("LtCy","DkCy","Lichen","Moss"))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_wrap(~Treat, labeller = as_labeller(trt_names), ncol=2)+
  scale_x_discrete(labels=c('LtCy','DkCy','Lichen','Moss'))+
  labs(title='A. FieldSpec Chlorophyll',y='Chlorophyll Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_FieldSpec_Chlorophyll_byFunc_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')

trt_names=c('CC' = 'Control','CW' = 'AltP','LC' = 'Warmed','LW' = 'AltP + Warmed')
asd$Type <- factor(asd$Type,levels=c("LCY","DCY","LCN","MSS"))
ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=Type, y=BI, fill=Type)) + 
  geom_boxplot() +
  scale_fill_manual(name='Func Type', values=c('red3','green3','cyan2','purple'),labels=c("LtCy","DkCy","Lichen","Moss"))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_wrap(~Treat, labeller = as_labeller(trt_names), ncol=2)+
  scale_x_discrete(labels=c('LtCy','DkCy','Lichen','Moss'))+
  labs(title='B. FieldSpec Brightness',y='Brightness Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_FieldSpec_Brightness_byFunc_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')

trt_names=c('CC' = 'Control','CW' = 'AltP','LC' = 'Warmed','LW' = 'AltP + Warmed')
asd$Type <- factor(asd$Type,levels=c("LCY","DCY","LCN","MSS"))
ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=Type, y=NDWI, fill=Type)) + 
  geom_boxplot() +
  scale_fill_manual(name='Func Type', values=c('red3','green3','cyan2','purple'),labels=c("LtCy","DkCy","Lichen","Moss"))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_wrap(~Treat, labeller = as_labeller(trt_names), ncol=2)+
  scale_x_discrete(labels=c('LtCy','DkCy','Lichen','Moss'))+
  labs(title='C. FieldSpec Moisture',y='Moisture Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_FieldSpec_Water_byFunc_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')

typ_names=c('LCY' = 'LtCy','DCY' = 'DkCy','LCN' = 'Lichen','MSS' = 'Moss')
asd$Treat <- factor(asd$Treat,levels=c("CC","CW","LC","LW"))
ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=Treat, y=NDVI, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_wrap(~Type, labeller = as_labeller(typ_names), ncol=2)+
  scale_x_discrete(labels=c('Control','AltP','Warmed','AltP + \nWarmed'))+
  labs(title='A. FieldSpec Chlorophyll',y='Chlorophyll Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_FieldSpec_Chlorophyll_byTreat_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')

typ_names=c('LCY' = 'LtCy','DCY' = 'DkCy','LCN' = 'Lichen','MSS' = 'Moss')
asd$Treat <- factor(asd$Treat,levels=c("CC","CW","LC","LW"))
ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=Treat, y=BI, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_wrap(~Type, labeller = as_labeller(typ_names), ncol=2)+
  scale_x_discrete(labels=c('Control','AltP','Warmed','AltP + \nWarmed'))+
  labs(title='B. FieldSpec Brightness',y='Brightness Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_FieldSpec_Brightness_byTreat_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')

typ_names=c('LCY' = 'LtCy','DCY' = 'DkCy','LCN' = 'Lichen','MSS' = 'Moss')
asd$Treat <- factor(asd$Treat,levels=c("CC","CW","LC","LW"))
ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=Treat, y=NDWI, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_wrap(~Type, labeller = as_labeller(typ_names), ncol=2)+
  scale_x_discrete(labels=c('Control','AltP','Warmed','AltP + \nWarmed'))+
  labs(title='C. FieldSpec Moisture',y='Moisture Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_FieldSpec_Water_byTreat_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')

#####UAS############################################################################################################
#open data files
uas <- read.csv2(paste(github_dir,'/data/Level1/',"UAS_Indices_Full.csv",sep=''),sep=',',header=T)
#Indices
uas$NDVI<-as.numeric(uas$NDVI)
uas$BI<-as.numeric(uas$BI)
uas$NTI_3<-as.numeric(uas$NTI_3)

trt_names=c('CC' = 'Control','CW' = 'AltP','LC' = 'Warmed','LW' = 'AltP + Warmed')
uas$Type <- factor(uas$Type,levels=c('Light','Dark'))
ggplot(subset(uas, Type %in% c('Light','Dark')), aes(x=Type, y=NDVI, fill=Type)) + 
  geom_boxplot() +
  scale_fill_manual(name='Func Type', values=c('lightgrey','darkgrey'),labels=c('Light','Dark'))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_wrap(~Treat, labeller = as_labeller(trt_names), ncol=2)+
  scale_x_discrete(labels=c('Light','Dark'))+
  labs(title='D. UAS Chlorophyll',y='Chlorophyll Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_Multispec_Chlorophyll_byFunc_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')

trt_names=c('CC' = 'Control','CW' = 'AltP','LC' = 'Warmed','LW' = 'AltP + Warmed')
uas$Type <- factor(uas$Type,levels=c('Light','Dark'))
ggplot(subset(uas, Type %in% c('Light','Dark')), aes(x=Type, y=BI, fill=Type)) + 
  geom_boxplot() +
  scale_fill_manual(name='Func Type', values=c('lightgrey','darkgrey'),labels=c('Light','Dark'))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_wrap(~Treat, labeller = as_labeller(trt_names), ncol=2)+
  scale_x_discrete(labels=c('Light','Dark'))+
  labs(title='E. UAS Brightness',y='Brightness Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_Multispec_Brightness_byFunc_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')

trt_names=c('CC' = 'Control','CW' = 'AltP','LC' = 'Warmed','LW' = 'AltP + Warmed')
uas$Type <- factor(uas$Type,levels=c('Light','Dark'))
ggplot(subset(uas, Type %in% c('Light','Dark')), aes(x=Type, y=NTI_3, fill=Type)) + 
  geom_boxplot() +
  scale_fill_manual(name='Func Type', values=c('lightgrey','darkgrey'),labels=c('Light','Dark'))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_wrap(~Treat, labeller = as_labeller(trt_names), ncol=2)+
  scale_x_discrete(labels=c('Light','Dark'))+
  labs(title='F. UAS Surface Temperature',y='Surface Temperature Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_Multispec_Temperature_byFunc_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')

typ_names=c('Light' = 'Light','Dark' = 'Dark')
uas$Treat <- factor(uas$Treat,levels=c("CC","CW","LC","LW"))
ggplot(subset(uas, Type %in% c('Light','Dark')), aes(x=Treat, y=NDVI, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_wrap(~Type, labeller = as_labeller(typ_names), ncol=2)+
  scale_x_discrete(labels=c('Control','AltP','Warmed','AltP + \nWarmed'))+
  labs(title='D. UAS Chlorophyll',y='Chlorophyll Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_Multispec_Chlorophyll_byTreat_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')

typ_names=c('Light' = 'Light','Dark' = 'Dark')
uas$Treat <- factor(uas$Treat,levels=c("CC","CW","LC","LW"))
ggplot(subset(uas, Type %in% c('Light','Dark')), aes(x=Treat, y=BI, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_wrap(~Type, labeller = as_labeller(typ_names), ncol=2)+
  scale_x_discrete(labels=c('Control','AltP','Warmed','AltP + \nWarmed'))+
  labs(title='E. UAS Brightness',y='Brightness Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_Multispec_Brightness_byTreat_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')

typ_names=c('Light' = 'Light','Dark' = 'Dark')
uas$Treat <- factor(uas$Treat,levels=c("CC","CW","LC","LW"))
ggplot(subset(uas, Type %in% c('Light','Dark')), aes(x=Treat, y=NTI_3, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_wrap(~Type, labeller = as_labeller(typ_names), ncol=2)+
  scale_x_discrete(labels=c('Control','AltP','Warmed','AltP + \nWarmed'))+
  labs(title='F. UAS Surface Temperature',y='Surface Temperature Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_Multispec_Termperature_byTreat_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')

