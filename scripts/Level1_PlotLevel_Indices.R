library(tidyverse)
library(tidyr)
library(dplyr)
library(ggpubr)
library(RColorBrewer)

setwd("/Users/wksmith/Documents/GitHub/Biocrust-USGS")
github_dir <- "/Users/wksmith/Documents/GitHub/Biocrust-USGS"

#######################################PLOT LEVEL###################################################################
########################################BioCrust Cover##############################################################
#open data files
Cover_long <- read.csv2(paste(github_dir,'/data/Level1/',"BioCrust_Cover_Plot.csv",sep=''),sep=',',header=T)
Cover_long$Val<-as.numeric(Cover_long$Val)
Cover_long<-subset(Cover_long, Type %in% c('DCY','LCN','LCY','MSS'))
Cover_long<-subset(Cover_long, Type %in% c('DCY','LCN','LCY','MSS'))
Cover_long$Type2 <- factor(Cover_long$Type,levels=c("LCY","DCY","MSS","LCN"))
Cover_long$Treat2 <- factor(Cover_long$Treat,levels=c("CC","CW","LC","LW"))
Cover_Stats = Cover_long %>% group_by(Treat2,Type2) %>% 
  summarise(N = n(), Cover_Mean = mean(Val,na.rm=T),Cover_Sd = sd(Val,na.rm=T))

# #Distribution
# ggplot(Cover_long, aes(x=Treat2, y=Val, fill=Treat2)) + 
#   geom_boxplot() +
#   scale_fill_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
#   facet_wrap(~Type2)+
#   labs(y='Cover',x='')+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
#   theme(text = element_text(size = 14))
# ggsave(paste(github_dir,'/figures/',"Box_Cover_Treat_PlotLevel.png",sep=""),dpi=300,width=240,height=120,units='mm')

#Stacked Barplot
Cover_Stats$Type2 <- factor(Cover_Stats$Type2,levels=c("LCY","DCY","LCN","MSS"))
ggplot(Cover_Stats, aes(x = Treat2, y = Cover_Mean, fill=Type2)) +
  geom_bar(stat="identity",position='stack',color='black') +
  scale_fill_manual(values=c('red3','green3','cyan2','purple'),labels=c('LtCy','DkCy','Lichen','Moss'))+
  scale_y_continuous("Percent Cover",expand = c(0, 0), limits = c(0, 60),breaks=c(0,20,40,60,80,100)) +
  scale_x_discrete("",labels=c('Control', 'AltP', 'Warmed', 'AltP + \nWarmed'),guide = guide_axis(angle = 45))+
  labs(title="",fill = "Func Type")+
  theme_bw() +
  theme(legend.position = 'right',legend.text=element_text(size=16),legend.title=element_text(size=18)) +
  theme(legend.background = element_rect(colour = NA))+
  theme(text = element_text(size = 24))
ggsave(paste(github_dir,'/figures/',"StackedBar_Cover_Treat_PlotLevel.png",sep=""),dpi=300,width=150,height=120,units='mm')


########################################MERGED######################################################################
#open data files
Merged <- read.csv2(paste(github_dir,'/data/Level1/',"Merged_Indices_Plot_V2.csv",sep=''),sep=',',header=T)
Merged_long <- Merged %>% pivot_longer(cols = 5:15,
                                       names_to = "Index", 
                                       values_to = "Val")
#Indices
Merged_long$Val<-as.numeric(Merged_long$Val)
Merged_long<-subset(Merged_long, Index %in% c('BLUE','GREEN','RED','REDEDGE','NIR'))

ggplot(Merged_long, aes(x=reorder(Index,Val), y=Val, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  facet_wrap(~Sensor)+
  labs(y='Reflectance',x='')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 14))
ggsave(paste(github_dir,'/figures/',"Box_Merged_SpecBands_Treat_PlotLevel.png",sep=""),dpi=300,width=240,height=120,units='mm')

ggplot(subset(Merged_long, Sensor %in% c("A. FieldSpec")), aes(x=reorder(Index,Val), y=Val, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  labs(y='Reflectance',x='')+
  theme_bw()+
  theme(legend.position = c(0.2, 0.8),legend.title = element_blank(),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 14))
ggsave(paste(github_dir,'/figures/',"Box_FieldSpec_SpecBands_Treat_PlotLevel.png",sep=""),dpi=300,width=240,height=120,units='mm')

ggplot(subset(Merged_long, Sensor %in% c("B. Resonon")), aes(x=reorder(Index,Val), y=Val, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  labs(y='Reflectance',x='')+
  theme_bw()+
  theme(legend.position = c(0.2, 0.8),legend.title = element_blank(),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 14))
ggsave(paste(github_dir,'/figures/',"Box_Resonon_SpecBands_Treat_PlotLevel.png",sep=""),dpi=300,width=240,height=120,units='mm')

ggplot(subset(Merged_long, Sensor %in% c("C. MicaSense")), aes(x=reorder(Index,Val), y=Val, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  labs(y='Reflectance',x='')+
  theme_bw()+
  theme(legend.position = c(0.2, 0.8),legend.title = element_blank(),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 14))
ggsave(paste(github_dir,'/figures/',"Box_MicaSence_SpecBands_Treat_PlotLevel.png",sep=""),dpi=300,width=240,height=120,units='mm')

ggplot(subset(Merged_long, Sensor %in% c("D. WorldView3")), aes(x=reorder(Index,Val), y=Val, fill=Treat)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  labs(y='Reflectance',x='')+
  theme_bw()+
  theme(legend.position = c(0.2, 0.8),legend.title = element_blank(),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 14))
ggsave(paste(github_dir,'/figures/',"Box_WorldView3_SpecBands_Treat_PlotLevel.png",sep=""),dpi=300,width=240,height=120,units='mm')


######Scatterplot#################################
Merged_wide <- Merged_long %>% pivot_wider(names_from = Sensor, values_from = Val)
colnames(Merged_wide)<-c('ID','Plot','Treat','Index','Resonon','FieldSpec','MicaSense','WorldView3')

ggplot(Merged_wide, aes(x=FieldSpec, y=Resonon,color=Index)) +
  scale_color_manual(values=c("#3288BD","#99D594","#5E4FA2","#D53E4F","#9E0142"))+
  geom_point(size=3)+
  geom_smooth(method=lm,aes(group = Index))+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),geom = "label",show.legend = FALSE)+
  labs(y='Resonon',x='FieldSpec')+
  theme_bw()+
  theme(legend.position = c(0.8, 0.2),legend.title = element_blank(),legend.text=element_text(size=16)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_FieldSpec_Resonon_SpecBand_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(Merged_wide, aes(x=FieldSpec, y=MicaSense,color=Index)) +
  scale_color_manual(values=c("#3288BD","#99D594","#5E4FA2","#D53E4F","#9E0142"))+
  geom_point(size=3)+
  geom_smooth(method=lm,aes(group = Index))+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),geom = "label",show.legend = FALSE)+
  labs(y='MicaSense',x='FieldSpec')+
  theme_bw()+
  theme(legend.position = c(0.8, 0.2),legend.title = element_blank(),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_FieldSpec_MicaSense_SpecBand_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(Merged_wide, aes(x=FieldSpec, y=WorldView3,color=Index)) +
  scale_color_manual(values=c("#3288BD","#99D594","#5E4FA2","#D53E4F","#9E0142"))+
  geom_point(size=3)+
  geom_smooth(method=lm,aes(group = Index))+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),geom = "label",show.legend = FALSE)+
  labs(y='WorldView3',x='FieldSpec')+
  theme_bw()+
  theme(legend.position = c(0.8, 0.2),legend.title = element_blank(),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_FieldSpec_WorldView3_SpecBand_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(Merged_wide, aes(x=Resonon, y=WorldView3,color=Index)) +
  scale_color_manual(values=c("#3288BD","#99D594","#5E4FA2","#D53E4F","#9E0142"))+
  geom_point(size=3)+
  geom_smooth(method=lm,aes(group = Index))+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),geom = "label",show.legend = FALSE)+
  labs(y='WorldView3',x='Resonon')+
  theme_bw()+
  theme(legend.position = c(0.8, 0.2),legend.title = element_blank(),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_Resonon_WorldView3_SpecBand_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(Merged_wide, aes(x=MicaSense, y=WorldView3,color=Index)) +
  scale_color_manual(values=c("#3288BD","#99D594","#5E4FA2","#D53E4F","#9E0142"))+
  geom_point(size=3)+
  geom_smooth(method=lm,aes(group = Index))+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),geom = "label",show.legend = FALSE)+
  labs(y='WorldView3',x='MicaSense')+
  theme_bw()+
  theme(legend.position = c(0.8, 0.2),legend.title = element_blank(),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_MicaSense_WorldView3_SpecBand_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

######Cover Scatterplot#################################
Cover_long <- read.csv2(paste(github_dir,'/data/Level1/',"BioCrust_Cover_Plot.csv",sep=''),sep=',',header=T)
Cover_long$Val<-as.numeric(Cover_long$Val)
Cover_long<-Cover_long[order(Cover_long$Plot, Cover_long$Treat),]
LCY<-subset(Cover_long, Type %in% c('LCY'))
DCY<-subset(Cover_long, Type %in% c('DCY'))
LCN<-subset(Cover_long, Type %in% c('LCN'))
MSS<-subset(Cover_long, Type %in% c('MSS'))
PLT<-subset(Cover_long, Type %in% c('Plant'))
RED<-subset(Merged_wide, Index %in% c('RED'))
NIR<-subset(Merged_wide, Index %in% c('NIR'))
NDVI<-(NIR[,5:8]-RED[,5:8])/(NIR[,5:8]+RED[,5:8])
data_RED<-cbind(RED,LCY$Val,DCY$Val,LCN$Val,MSS$Val,PLT$Val)
data_NIR<-cbind(NIR,LCY$Val,DCY$Val,LCN$Val,MSS$Val,PLT$Val)
data_NDVI<-cbind(RED[1:4],NDVI,LCY$Val,DCY$Val,LCN$Val,MSS$Val,PLT$Val)

#open data files
xt2_plot <- read.csv2(paste(github_dir,'/data/Level1/',"XT2_Temp_Plot.csv",sep=''),sep=',',header=T)
xt2_plot$LST<-as.numeric(xt2_plot$LSTindex)
ggplot(data_NDVI, aes(x=xt2_plot$LST, y=LCY$Val)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=c(2,3,2,1,1,2,4,4,3,1,3,1,1,1,1,1,2,3,2,1)*2.5)+
  geom_smooth(method=lm,aes(group = Index),color='black',fill='grey')+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),label.x=0,label.y=65,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_x_continuous('Surface Temperature Index',limits = c(-.025,1.025), breaks = seq(-.25,1.25,.25)) +
  scale_y_continuous('LtCy Percent Cover',limits = c(-5,70), breaks = seq(-10,70,10)) +
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.825,.23),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_LCY_LST_XT2_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#open data files
asd_plot <- read.csv2(paste(github_dir,'/data/Level1/',"ASD_Bands_Indices_Plot.csv",sep=''),sep=',',header=T)
asd_plot$NDVI<-as.numeric(asd_plot$NDVI)
asd_plot$NDWI<-as.numeric(asd_plot$NDWI)
ggplot(data_NDVI, aes(x=asd_plot$NDVI, y=LCY$Val)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=c(2,3,2,1,1,2,4,4,3,1,3,1,1,1,1,1,2,3,2,1)*2.5)+
  geom_smooth(method=lm,aes(group = Index),color='black',fill='grey')+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),label.x=.14,label.y=-4,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_x_continuous('Normalized Chlorophyll Index',limits = c(0.125,0.25), breaks = seq(0.125,0.25,.025)) +
  scale_y_continuous('LtCy Percent Cover',limits = c(-5,70), breaks = seq(-10,70,10)) +
  theme_bw()+
  theme(legend.position = c(.83,.78),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_LCY_NDVI_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(data_NDVI, aes(x=asd_plot$NDWI, y=LCY$Val)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=c(2,3,2,1,1,2,4,4,3,1,3,1,1,1,1,1,2,3,2,1)*2.5)+
  geom_smooth(method=lm,aes(group = Index),color='black',fill='grey')+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),label.x=.14,label.y=-4,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_x_continuous('Normalized Water Index',limits = c(0.075,0.25), breaks = seq(0.075,0.25,.025)) +
  scale_y_continuous('LtCy Percent Cover',limits = c(-5,70), breaks = seq(-10,70,10)) +
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.83,.78),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_LCY_NDWI_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

# ggplot(data_NDVI, aes(x=FieldSpec, y=LCY$Val)) +
#   geom_point(aes(color=Treat),size=log(DCY$Val+LCN$Val+MSS$Val+PLT$Val)*2)+
#   geom_smooth(method=lm,aes(group = Index),color='black',fill='grey')+
#   stat_cor(aes(group = Index,label = after_stat(rr.label)),label.x=.14,label.y=-4,size=8)+
#   scale_color_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
#   labs(y='LtCy Percent Cover',x='Normalized Chlorophyll Index')+
#   theme_bw()+
#   theme(legend.position = c(.83,.78),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
#   theme(text = element_text(size = 20))
# ggsave(paste(github_dir,'/figures/',"Scatterplot_LCY_NDVI_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(data_NDVI, aes(x=MicaSense, y=LCY$Val)) +
  geom_point(aes(color=Treat),size=log(DCY$Val+LCN$Val+MSS$Val+PLT$Val)*2)+
  geom_smooth(method=lm,aes(group = Index),color='black',fill='grey')+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),label.x=.14,label.y=-4,size=8)+
  scale_color_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_x_continuous('Normalized Chlorophyll Index',limits = c(0.125,0.25), breaks = seq(0.125,0.25,.025)) +
  scale_y_continuous('LtCy Percent Cover',limits = c(-5,70), breaks = seq(-10,70,10)) +
  theme_bw()+
  theme(legend.position = c(.83,.78),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_LCY_NDVI_MicaSense_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(data_NDVI, aes(x=Resonon, y=LCY$Val)) +
  geom_point(aes(color=Treat),size=log(DCY$Val+LCN$Val+MSS$Val+PLT$Val)*2)+
  geom_smooth(method=lm,aes(group = Index),color='black',fill='grey')+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),label.x=.14,label.y=-4,size=8)+
  scale_color_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_x_continuous('Normalized Chlorophyll Index',limits = c(0.125,0.25), breaks = seq(0.125,0.25,.025)) +
  scale_y_continuous('LtCy Percent Cover',limits = c(-5,70), breaks = seq(-10,70,10)) +
  theme_bw()+
  theme(legend.position = c(.83,.78),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_LCY_NDVI_Resonon_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(data_NDVI, aes(x=WorldView3, y=LCY$Val)) +
  geom_point(aes(color=Treat),size=log(DCY$Val+LCN$Val+MSS$Val+PLT$Val)*2)+
  geom_smooth(method=lm,aes(group = Index),color='black',fill='grey')+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),label.x=.14,label.y=-4,size=8)+
  scale_color_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_x_continuous('Normalized Chlorophyll Index',limits = c(0.125,0.25), breaks = seq(0.125,0.25,.025)) +
  scale_y_continuous('LtCy Percent Cover',limits = c(-5,70), breaks = seq(-10,70,10)) +
  theme_bw()+
  theme(legend.position = c(.83,.78),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_LCY_NDVI_WorldView3_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#############################################################################################################################################
ggplot(data_RED, aes(x=FieldSpec, y=LCY$Val)) +
  geom_point(aes(color=Treat),size=log(DCY$Val+LCN$Val+MSS$Val+PLT$Val)*2)+
  geom_smooth(method=lm,aes(group = Index))+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),geom = "label",show.legend = FALSE)+
  labs(y='LtCy Percent Cover',x='RED Reflectance')+
  theme_bw()+
  theme(legend.position = c(0.8, 0.2),legend.title = element_blank(),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_LCY_RED_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(data_RED, aes(x=WorldView3, y=LCY$Val)) +
  geom_point(aes(color=Treat),size=log(DCY$Val+LCN$Val+MSS$Val+PLT$Val)*2)+
  geom_smooth(method=lm,aes(group = Index))+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),geom = "label",show.legend = FALSE)+
  labs(y='LtCy Percent Cover',x='RED Reflectance')+
  theme_bw()+
  theme(legend.position = c(0.8, 0.2),legend.title = element_blank(),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_LCY_RED_WorldView3_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(data_NIR, aes(x=FieldSpec, y=LCY$Val)) +
  scale_color_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  geom_point(aes(color=Treat),size=log(DCY$Val+LCN$Val+MSS$Val+PLT$Val)*2)+
  geom_smooth(method=lm,aes(group = Index))+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),geom = "label",show.legend = FALSE)+
  labs(y='LCY Cover (%)',x='NIR Reflectance')+
  theme_bw()+
  #theme(legend.position = c(0.8, 0.2),legend.title = element_blank(),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_LCY_NIR_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(data_NIR, aes(x=MicaSense, y=LCY$Val)) +
  scale_color_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  geom_point(aes(color=Treat),size=log(DCY$Val+LCN$Val+MSS$Val+PLT$Val)*2)+
  geom_smooth(method=lm,aes(group = Index))+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),geom = "label",show.legend = FALSE)+
  labs(y='LCY Cover (%)',x='NIR Reflectance')+
  theme_bw()+
  #theme(legend.position = c(0.8, 0.2),legend.title = element_blank(),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_LCY_NIR_Mica_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(data_NIR, aes(x=Resonon, y=LCY$Val)) +
  scale_color_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  geom_point(aes(color=Treat),size=log(MSS$Val)*2)+
  geom_smooth(method=lm,aes(group = Index))+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),geom = "label",show.legend = FALSE)+
  labs(y='LCY Cover (%)',x='NIR Reflectance')+
  theme_bw()+
  #theme(legend.position = c(0.8, 0.2),legend.title = element_blank(),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_LCY_NIR_Resonon_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(data_NIR, aes(x=WorldView3, y=LCY$Val)) +
  scale_color_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  geom_point(aes(color=Treat),size=log(DCY$Val+LCN$Val+MSS$Val+PLT$Val)*2)+
  geom_smooth(method=lm,aes(group = Index))+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),geom = "label",show.legend = FALSE)+
  labs(y='LCY Cover (%)',x='NIR Reflectance')+
  theme_bw()+
  #theme(legend.position = c(0.8, 0.2),legend.title = element_blank(),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_LCY_NIR_VW3_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')
