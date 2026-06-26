rm(list = ls()) # clear the environment
setwd("/Users/wksmith/Documents/GitHub/CastleValley_Campaign_Biocrust_Analysis")
github_dir <- "/Users/wksmith/Documents/GitHub/CastleValley_Campaign_Biocrust_Analysis"

library(tidyverse)
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

my_comparisons <- list(c("LCY","DCY"), c("LCY","LCN"), c("LCY","MSS"),c("DCY","LCN"),c("DCY","MSS"),c("LCN","MSS"))
ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=reorder(Type,NDVI), y=NDVI, fill=Type)) + 
  geom_boxplot() +
  scale_fill_manual(values=c('red3','green3','cyan2','purple'))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  #stat_compare_means(label = "p.signif", method = "t.test", ref.group = "LCY", label.y = .4, color = 'red', size = 10)+ # Pairwise comparison against LCY
  #stat_compare_means(label.y = .4)+ # Add global p-value
  labs(y='Normalized Chlorophyll Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_FieldSpec_Chlorophyll_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')
#stats
compare_means(NDVI ~ Type,  method = "t.test", ref.group = "LCY", data = subset(asd, Type %in% c('LCY','DCY','LCN','MSS')))

trt_names=c('CC' = 'Control','CW' = 'AltP','LC' = 'Warmed','LW' = 'AltP + Warmed')
asd$Type <- factor(asd$Type,levels=c("LCY","DCY","LCN","MSS"))
ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=Type, y=NDVI, fill=Type)) + 
  geom_boxplot() +
  scale_fill_manual(name='Func Type', values=c('red3','green3','cyan2','purple'),labels=c("LtCy","DkCy","Lichen","Moss"))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  #stat_compare_means(label = "p.signif", method = "t.test", ref.group = "LCY", label.y = .35, color = 'red', size = 10)+ # Pairwise comparison against LCY
  #stat_compare_means(label.y = .4)+ # Add global p-value
  facet_wrap(~Treat, labeller = as_labeller(trt_names), ncol=2)+
  scale_x_discrete(labels=c('LtCy','DkCy','Lichen','Moss'))+
  labs(title='A. FieldSpec Chlorophyll',y='Chlorophyll Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_FieldSpec_Chlorophyll_byFunc_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')
#stats
asd_subset<-subset(asd, Type %in% c('LCY','DCY','LCN','MSS'))
asd_subset %>% group_by(Type) %>% summarise(median = median(NDVI), iqr = IQR(NDVI))
asd_subset %>% group_by(Type,Treat) %>% summarise(median = median(NDVI), iqr = IQR(NDVI))
compare_means(NDVI ~ Type,  data = asd_subset)
compare_means(NDVI ~ Type,  data = subset(asd_subset, Treat %in% 'CC'))
compare_means(NDVI ~ Type,  data = subset(asd_subset, Treat %in% 'CW'))
compare_means(NDVI ~ Type,  data = subset(asd_subset, Treat %in% 'LC'))
compare_means(NDVI ~ Type,  data = subset(asd_subset, Treat %in% 'LW'))

trt_names=c('CC' = 'Control','CW' = 'AltP','LC' = 'Warmed','LW' = 'AltP + Warmed')
asd$Type <- factor(asd$Type,levels=c("LCY","DCY","LCN","MSS"))
ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=Type, y=BI, fill=Type)) + 
  geom_boxplot() +
  scale_fill_manual(name='Func Type', values=c('red3','green3','cyan2','purple'),labels=c("LtCy","DkCy","Lichen","Moss"))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  #stat_compare_means(label = "p.signif", method = "t.test", ref.group = "LCY",label.y = .16, color = 'red', size = 10)+ # Pairwise comparison against LCY
  #stat_compare_means(label.y = .4)+ # Add global p-value
  facet_wrap(~Treat, labeller = as_labeller(trt_names), ncol=2)+
  scale_x_discrete(labels=c('LtCy','DkCy','Lichen','Moss'))+
  labs(title='B. FieldSpec Brightness',y='Brightness Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_FieldSpec_Brightness_byFunc_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')
#stats
asd_subset<-subset(asd, Type %in% c('LCY','DCY','LCN','MSS'))
asd_subset %>% group_by(Type) %>% summarise(median = median(BI), iqr = IQR(BI))
asd_subset %>% group_by(Type,Treat) %>% summarise(median = median(BI), iqr = IQR(BI))
compare_means(BI ~ Type,  data = asd_subset)
compare_means(BI ~ Type,  data = subset(asd_subset, Treat %in% 'CC'))
compare_means(BI ~ Type,  data = subset(asd_subset, Treat %in% 'CW'))
compare_means(BI ~ Type,  data = subset(asd_subset, Treat %in% 'LC'))
compare_means(BI ~ Type,  data = subset(asd_subset, Treat %in% 'LW'))

trt_names=c('CC' = 'Control','CW' = 'AltP','LC' = 'Warmed','LW' = 'AltP + Warmed')
asd$Type <- factor(asd$Type,levels=c("LCY","DCY","LCN","MSS"))
ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=Type, y=NDWI, fill=Type)) + 
  geom_boxplot() +
  scale_fill_manual(name='Func Type', values=c('red3','green3','cyan2','purple'),labels=c("LtCy","DkCy","Lichen","Moss"))+
  #stat_compare_means(label = "p.signif", method = "t.test", ref.group = "LCY",label.y = .27, color = 'red', size = 10)+ # Pairwise comparison against LCY
  #stat_compare_means(label.y = .4)+ # Add global p-value
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  facet_wrap(~Treat, labeller = as_labeller(trt_names), ncol=2)+
  scale_x_discrete(labels=c('LtCy','DkCy','Lichen','Moss'))+
  labs(title='C. FieldSpec Moisture',y='Moisture Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_FieldSpec_Water_byFunc_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')
#stats
asd_subset<-subset(asd, Type %in% c('LCY','DCY','LCN','MSS'))
asd_subset %>% group_by(Type) %>% summarise(median = median(NDWI), iqr = IQR(NDWI))
asd_subset %>% group_by(Type,Treat) %>% summarise(median = median(NDWI), iqr = IQR(NDWI))
compare_means(NDWI ~ Type,  data = asd_subset)
compare_means(NDWI ~ Type,  data = subset(asd_subset, Treat %in% 'CC'))
compare_means(NDWI ~ Type,  data = subset(asd_subset, Treat %in% 'CW'))
compare_means(NDWI ~ Type,  data = subset(asd_subset, Treat %in% 'LC'))
compare_means(NDWI ~ Type,  data = subset(asd_subset, Treat %in% 'LW'))

typ_names=c('LCY' = 'LtCy','DCY' = 'DkCy','LCN' = 'Lichen','MSS' = 'Moss')
asd$Treat <- factor(asd$Treat,levels=c("CC","CW","LC","LW"))
ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=Treat, y=NDVI, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  #stat_compare_means(label = "p.signif", method = "t.test", ref.group = "CC",label.y = .37, color = 'red', size = 10)+ # Pairwise comparison against LCY
  #stat_compare_means(label.y = .4)+ # Add global p-value
  facet_wrap(~Type, labeller = as_labeller(typ_names), ncol=2)+
  scale_x_discrete(labels=c('Control','AltP','Warmed','AltP + \nWarmed'))+
  labs(title='A. FieldSpec Chlorophyll',y='Chlorophyll Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_FieldSpec_Chlorophyll_byTreat_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')
#stats
asd_subset<-subset(asd, Type %in% c('LCY','DCY','LCN','MSS'))
subset(asd_subset, Type %in% 'LCY') %>% group_by(Treat) %>% summarise(median = median(NDVI), iqr = IQR(NDVI))
subset(asd_subset, Type %in% 'DCY') %>% group_by(Treat) %>% summarise(median = median(NDVI), iqr = IQR(NDVI))
subset(asd_subset, Type %in% 'LCN') %>% group_by(Treat) %>% summarise(median = median(NDVI), iqr = IQR(NDVI))
subset(asd_subset, Type %in% 'MSS') %>% group_by(Treat) %>% summarise(median = median(NDVI), iqr = IQR(NDVI))
asd_subset %>% group_by(Type,Treat) %>% summarise(median = median(NDVI), iqr = IQR(NDVI))
compare_means(NDVI ~ Treat,  data = asd_subset)
compare_means(NDVI ~ Treat,  data = subset(asd_subset, Type %in% 'LCY'))
compare_means(NDVI ~ Treat,  data = subset(asd_subset, Type %in% 'DCY'))
compare_means(NDVI ~ Treat,  data = subset(asd_subset, Type %in% 'LCN'))
compare_means(NDVI ~ Treat,  data = subset(asd_subset, Type %in% 'MSS'))

typ_names=c('LCY' = 'LtCy','DCY' = 'DkCy','LCN' = 'Lichen','MSS' = 'Moss')
asd$Treat <- factor(asd$Treat,levels=c("CC","CW","LC","LW"))
ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=Treat, y=BI, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  #stat_compare_means(label = "p.signif", method = "t.test", ref.group = "CC",label.y = .18, color = 'red', size = 10)+ # Pairwise comparison against LCY
  #stat_compare_means(label.y = .4)+ # Add global p-value
  facet_wrap(~Type, labeller = as_labeller(typ_names), ncol=2)+
  scale_x_discrete(labels=c('Control','AltP','Warmed','AltP + \nWarmed'))+
  scale_y_continuous(limits=c(0.06,0.20))+
  labs(title='B. FieldSpec Brightness',y='Brightness Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_FieldSpec_Brightness_byTreat_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')
#stats
asd_subset<-subset(asd, Type %in% c('LCY','DCY','LCN','MSS'))
subset(asd_subset, Type %in% 'LCY') %>% group_by(Treat) %>% summarise(median = median(BI), iqr = IQR(BI))
subset(asd_subset, Type %in% 'DCY') %>% group_by(Treat) %>% summarise(median = median(BI), iqr = IQR(BI))
subset(asd_subset, Type %in% 'LCN') %>% group_by(Treat) %>% summarise(median = median(BI), iqr = IQR(BI))
subset(asd_subset, Type %in% 'MSS') %>% group_by(Treat) %>% summarise(median = median(BI), iqr = IQR(BI))
asd_subset %>% group_by(Treat) %>% summarise(median = median(BI), iqr = IQR(BI))
asd_subset %>% group_by(Type,Treat) %>% summarise(median = median(BI), iqr = IQR(BI))
compare_means(BI ~ Treat,  data = asd_subset)
compare_means(BI ~ Treat,  data = subset(asd_subset, Type %in% 'LCY'))
compare_means(BI ~ Treat,  data = subset(asd_subset, Type %in% 'DCY'))
compare_means(BI ~ Treat,  data = subset(asd_subset, Type %in% 'LCN'))
compare_means(BI ~ Treat,  data = subset(asd_subset, Type %in% 'MSS'))

typ_names=c('LCY' = 'LtCy','DCY' = 'DkCy','LCN' = 'Lichen','MSS' = 'Moss')
asd$Treat <- factor(asd$Treat,levels=c("CC","CW","LC","LW"))
ggplot(subset(asd, Type %in% c('LCY','DCY','LCN','MSS')), aes(x=Treat, y=NDWI, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  #stat_compare_means(label = "p.signif", method = "t.test", ref.group = "CC",label.y = .26, color = 'red', size = 10)+ # Pairwise comparison against LCY
  #stat_compare_means(label.y = .4)+ # Add global p-value
  facet_wrap(~Type, labeller = as_labeller(typ_names), ncol=2)+
  scale_x_discrete(labels=c('Control','AltP','Warmed','AltP + \nWarmed'))+
  labs(title='C. FieldSpec Moisture',y='Moisture Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_FieldSpec_Water_byTreat_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')
#stats
asd_subset<-subset(asd, Type %in% c('LCY','DCY','LCN','MSS'))
subset(asd_subset, Type %in% 'LCY') %>% group_by(Treat) %>% summarise(median = median(NDWI), iqr = IQR(NDWI))
subset(asd_subset, Type %in% 'DCY') %>% group_by(Treat) %>% summarise(median = median(NDWI), iqr = IQR(NDWI))
subset(asd_subset, Type %in% 'LCN') %>% group_by(Treat) %>% summarise(median = median(NDWI), iqr = IQR(NDWI))
subset(asd_subset, Type %in% 'MSS') %>% group_by(Treat) %>% summarise(median = median(NDWI), iqr = IQR(NDWI))
asd_subset %>% group_by(Treat) %>% summarise(median = median(BI), iqr = IQR(BI))
asd_subset %>% group_by(Type,Treat) %>% summarise(median = median(BI), iqr = IQR(BI))
compare_means(NDWI ~ Treat,  data = subset(asd_subset, Type %in% 'LCY'))
compare_means(NDWI ~ Treat,  data = subset(asd_subset, Type %in% 'DCY'))
compare_means(NDWI ~ Treat,  data = subset(asd_subset, Type %in% 'LCN'))
compare_means(NDWI ~ Treat,  data = subset(asd_subset, Type %in% 'MSS'))

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
  scale_fill_manual(name='Func Type', values=c('white','darkgrey'),labels=c('Early','Late'))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  #stat_compare_means(label = "p.signif", method = "t.test", ref.group = "Light",label.y = .31, color = 'red', size = 10)+ # Pairwise comparison against LCY
  #stat_compare_means(label.y = .4)+ # Add global p-value
  facet_wrap(~Treat, labeller = as_labeller(trt_names), ncol=2)+
  scale_x_discrete(labels=c('Early','Late'))+
  labs(title='D. UAS Chlorophyll',y='Chlorophyll Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_Multispec_Chlorophyll_byFunc_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')
#stats
uas_subset<-subset(uas, Type %in% c('Light','Dark'))
uas_subset %>% group_by(Type,Treat) %>% summarise(median = median(NDVI), iqr = IQR(NDVI))
uas_subset %>% group_by(Type) %>% summarise(median = median(NDVI), iqr = IQR(NDVI))
compare_means(NDVI ~ Type,  data = uas_subset)
compare_means(NDVI ~ Type,  data = subset(uas_subset, Treat %in% 'CC'))
compare_means(NDVI ~ Type,  data = subset(uas_subset, Treat %in% 'CW'))
compare_means(NDVI ~ Type,  data = subset(uas_subset, Treat %in% 'LC'))
compare_means(NDVI ~ Type,  data = subset(uas_subset, Treat %in% 'LW'))

trt_names=c('CC' = 'Control','CW' = 'AltP','LC' = 'Warmed','LW' = 'AltP + Warmed')
uas$Type <- factor(uas$Type,levels=c('Light','Dark'))
ggplot(subset(uas, Type %in% c('Light','Dark')), aes(x=Type, y=BI, fill=Type)) + 
  geom_boxplot() +
  scale_fill_manual(name='Func Type', values=c('white','darkgrey'),labels=c('Early','Late'))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  #stat_compare_means(label = "p.signif", method = "t.test", ref.group = "Light",label.y = .15, color = 'red', size = 10)+ # Pairwise comparison against LCY
  #stat_compare_means(label.y = .4)+ # Add global p-value
  facet_wrap(~Treat, labeller = as_labeller(trt_names), ncol=2)+
  scale_x_discrete(labels=c('Early','Late'))+
  labs(title='E. UAS Brightness',y='Brightness Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_Multispec_Brightness_byFunc_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')
#stats
uas_subset<-subset(uas, Type %in% c('Light','Dark'))
uas_subset %>% group_by(Type) %>% summarise(median = median(BI), iqr = IQR(BI))
compare_means(BI ~ Type,  data = uas_subset)
compare_means(BI ~ Type,  data = subset(uas_subset, Treat %in% 'CC'))
compare_means(BI ~ Type,  data = subset(uas_subset, Treat %in% 'CW'))
compare_means(BI ~ Type,  data = subset(uas_subset, Treat %in% 'LC'))
compare_means(BI ~ Type,  data = subset(uas_subset, Treat %in% 'LW'))

trt_names=c('CC' = 'Control','CW' = 'AltP','LC' = 'Warmed','LW' = 'AltP + Warmed')
uas$Type <- factor(uas$Type,levels=c('Light','Dark'))
ggplot(subset(uas, Type %in% c('Light','Dark')), aes(x=Type, y=NTI_3, fill=Type)) + 
  geom_boxplot() +
  scale_fill_manual(name='Func Type', values=c('white','darkgrey'),labels=c('Early','Late'))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  #stat_compare_means(label = "p.signif", method = "t.test", ref.group = "Light",label.y = .67, color = 'red', size = 10)+ # Pairwise comparison against LCY
  #stat_compare_means(label.y = .4)+ # Add global p-value
  facet_wrap(~Treat, labeller = as_labeller(trt_names), ncol=2)+
  scale_x_discrete(labels=c('Early','Late'))+
  labs(title='F. UAS Surface Temperature',y='Surface Temperature Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_Multispec_Temperature_byFunc_FuncLevel.png",sep=""),dpi=300,width=180,height=180,units='mm')
#stats
uas_subset<-subset(uas, Type %in% c('Light','Dark'))
uas_subset %>% group_by(Type) %>% summarise(median = median(NTI_3), iqr = IQR(NTI_3))
compare_means(NTI_3 ~ Type,  data = uas_subset)
compare_means(NTI_3 ~ Type,  data = subset(uas_subset, Treat %in% 'CC'))
compare_means(NTI_3 ~ Type,  data = subset(uas_subset, Treat %in% 'CW'))
compare_means(NTI_3 ~ Type,  data = subset(uas_subset, Treat %in% 'LC'))
compare_means(NTI_3 ~ Type,  data = subset(uas_subset, Treat %in% 'LW'))

typ_names=c('Light' = 'Early','Dark' = 'Late')
uas$Treat <- factor(uas$Treat,levels=c("CC","CW","LC","LW"))
ggplot(subset(uas, Type %in% c('Light','Dark')), aes(x=Treat, y=NDVI, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  #stat_compare_means(label = "p.signif", method = "t.test", ref.group = "CC",label.y = .33, color = 'red', size = 10)+ # Pairwise comparison against LCY
  #stat_compare_means(label.y = .4)+ # Add global p-value
  facet_wrap(~Type, labeller = as_labeller(typ_names), ncol=2)+
  scale_x_discrete(labels=c('Control','AltP','Warmed','AltP + \nWarmed'))+
  labs(title='D. UAS Chlorophyll',y='Chlorophyll Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_Multispec_Chlorophyll_byTreat_FuncLevel.png",sep=""),dpi=300,width=180,height=115,units='mm')
#stats
uas_subset<-subset(uas, Type %in% c('Light','Dark'))
subset(uas_subset, Type %in% 'Light') %>% group_by(Treat) %>% summarise(median = median(NDVI), iqr = IQR(NDVI))
subset(uas_subset, Type %in% 'Dark') %>% group_by(Treat) %>% summarise(median = median(NDVI), iqr = IQR(NDVI))
compare_means(NDVI ~ Treat,  data = uas_subset)
compare_means(NDVI ~ Treat,  data = subset(uas_subset, Type %in% 'Light'))
compare_means(NDVI ~ Treat,  data = subset(uas_subset, Type %in% 'Dark'))

typ_names=c('Light' = 'Early','Dark' = 'Late')
uas$Treat <- factor(uas$Treat,levels=c("CC","CW","LC","LW"))
ggplot(subset(uas, Type %in% c('Light','Dark')), aes(x=Treat, y=BI, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  #stat_compare_means(label = "p.signif", method = "t.test", ref.group = "CC",label.y = .16, color = 'red', size = 10)+ # Pairwise comparison against LCY
  #stat_compare_means(label.y = .4)+ # Add global p-value
  facet_wrap(~Type, labeller = as_labeller(typ_names), ncol=2)+
  scale_x_discrete(labels=c('Control','AltP','Warmed','AltP + \nWarmed'))+
  labs(title='E. UAS Brightness',y='Brightness Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_Multispec_Brightness_byTreat_FuncLevel.png",sep=""),dpi=300,width=180,height=115,units='mm')
#stats
uas_subset<-subset(uas, Type %in% c('Light','Dark'))
subset(uas_subset, Type %in% 'Light') %>% group_by(Treat) %>% summarise(median = median(BI), iqr = IQR(BI))
subset(uas_subset, Type %in% 'Dark') %>% group_by(Treat) %>% summarise(median = median(BI), iqr = IQR(BI))
compare_means(BI ~ Treat,  data = uas_subset)
compare_means(BI ~ Treat,  data = subset(uas_subset, Type %in% 'Light'))
compare_means(BI ~ Treat,  data = subset(uas_subset, Type %in% 'Dark'))

typ_names=c('Light' = 'Early','Dark' = 'Late')
uas$Treat <- factor(uas$Treat,levels=c("CC","CW","LC","LW"))
ggplot(subset(uas, Type %in% c('Light','Dark')), aes(x=Treat, y=NTI_3, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  #stat_compare_means(label = "p.signif", method = "t.test", ref.group = "CC",label.y = .7, color = 'red', size = 10)+ # Pairwise comparison against LCY
  #stat_compare_means(label.y = .4)+ # Add global p-value
  facet_wrap(~Type, labeller = as_labeller(typ_names), ncol=2)+
  scale_x_discrete(labels=c('Control','AltP','Warmed','AltP + \nWarmed'))+
  labs(title='F. UAS Surface Temperature',y='Surface Temperature Index',x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_Multispec_Temperature_byTreat_FuncLevel.png",sep=""),dpi=300,width=180,height=115,units='mm')
#stats
uas_subset<-subset(uas, Type %in% c('Light','Dark'))
subset(uas_subset, Type %in% 'Light') %>% group_by(Treat) %>% summarise(median = median(NTI_3), iqr = IQR(NTI_3))
subset(uas_subset, Type %in% 'Dark') %>% group_by(Treat) %>% summarise(median = median(NTI_3), iqr = IQR(NTI_3))
compare_means(NTI_3 ~ Treat,  data = uas_subset)
compare_means(NTI_3 ~ Treat,  data = subset(uas_subset, Type %in% 'Light'))
compare_means(NTI_3 ~ Treat,  data = subset(uas_subset, Type %in% 'Dark'))

