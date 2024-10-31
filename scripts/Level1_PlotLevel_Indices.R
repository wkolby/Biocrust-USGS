library(tidyverse)
#library(tidyr)
#library(dplyr)
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

######Cover Scatterplot#################################
Cover_long <- read.csv2(paste(github_dir,'/data/Level1/',"BioCrust_Cover_Plot.csv",sep=''),sep=',',header=T)
Cover_long$Val<-as.numeric(Cover_long$Val)
Cover_long<-Cover_long[order(Cover_long$Plot, Cover_long$Treat),]
LCY<-subset(Cover_long, Type %in% c('LCY'))
DCY<-subset(Cover_long, Type %in% c('DCY'))
LCN<-subset(Cover_long, Type %in% c('LCN'))
MSS<-subset(Cover_long, Type %in% c('MSS'))
PLT<-subset(Cover_long, Type %in% c('Plant'))

#UAS Chlorophyll
uas_ndvi <- read.csv2(paste(github_dir,'/data/Level1/',"BPlots_Micasense_NDVI_Full.csv",sep=''),sep=',',header=T)
uas_ndvi$Value<-as.numeric(uas_ndvi$Value)
data<-cbind(uas_ndvi,LCY$Val,DCY$Val,LCN$Val,MSS$Val,PLT$Val)
colnames(data)<-c('Plot','Treat','NDVI','LCY','DCY','LCN','MSS','PLT')
ggplot(data, aes(x=NDVI, y=LCY)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=c(2,3,2,1,1,2,4,4,3,1,3,1,1,1,1,1,2,3,2,1)*2.5)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=0.16,label.y=0,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_x_continuous('Chlorophyll Index',limits = c(.16,.24), breaks = seq(.16,.24,.02))+
  scale_y_continuous('LtCy Percent Cover',limits = c(-5,70), breaks = seq(-10,70,10)) +
  labs(title="D. UAS Chlorophyll")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.825,.23),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_LCY_NDVI_UAS_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#UAS Brightness
uas_bi <- read.csv2(paste(github_dir,'/data/Level1/',"BPlots_Micasense_BI_Full.csv",sep=''),sep=',',header=T)
uas_bi$Value<-as.numeric(uas_bi$Value)
data<-cbind(uas_bi,LCY$Val,DCY$Val,LCN$Val,MSS$Val,PLT$Val)
colnames(data)<-c('Plot','Treat','BI','LCY','DCY','LCN','MSS','PLT')
ggplot(data, aes(x=BI, y=LCY)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=c(2,3,2,1,1,2,4,4,3,1,3,1,1,1,1,1,2,3,2,1)*2.5)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=0.065,label.y=65,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_x_continuous('Brightness Index',limits = c(.065,.12), breaks = seq(.065,.12,.01))+
  scale_y_continuous('LtCy Percent Cover',limits = c(-5,70), breaks = seq(-10,70,10)) +
  labs(title="E. UAS Brightness")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.825,.23),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_LCY_BI_UAS_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#UAS Surface Temperature
uas_sti <- read.csv2(paste(github_dir,'/data/Level1/',"BPlots_Thermal_Flight1_Full.csv",sep=''),sep=',',header=T)
uas_sti$Value<-as.numeric(uas_sti$Value)
data<-cbind(uas_sti,LCY$Val,DCY$Val,LCN$Val,MSS$Val,PLT$Val)
colnames(data)<-c('Plot','Treat','Temp','LCY','DCY','LCN','MSS','PLT')
ggplot(data, aes(x=Temp, y=LCY)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=c(2,3,2,1,1,2,4,4,3,1,3,1,1,1,1,1,2,3,2,1)*2.5)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=0.22,label.y=65,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_x_continuous('Surface Temperature Index',limits = c(.22,.32), breaks = seq(.22,.32,.01)) +
  scale_y_continuous('LtCy Percent Cover',limits = c(-5,70), breaks = seq(-10,70,10)) +
  labs(title="F. UAS Surface Temperature")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.825,.23),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_LCY_STI_UAS_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

###############ASD#####################################
asd_plot <- read.csv2(paste(github_dir,'/data/Level1/',"ASD_Bands_Indices_Plot.csv",sep=''),sep=',',header=T)
asd_plot$NIR<-as.numeric(asd_plot$NIR)
asd_plot$NDVI<-as.numeric(asd_plot$NDVI)
asd_plot$BI<-as.numeric(asd_plot$BI)
asd_plot$NDWI<-as.numeric(asd_plot$NDWI)
data_asd<-cbind(data$Plot,data$Treat,asd_plot[c(8,9,10,11)],LCY$Val,DCY$Val,LCN$Val,MSS$Val,PLT$Val)
colnames(data_asd)<-c('Plot','Treat','NIR','NDVI','BI','NDWI','LCY','DCY','LCN','MSS','PLT')

#ASD Chlorophyll
ggplot(data_asd, aes(x=NDVI, y=LCY)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=c(2,3,2,1,1,2,4,4,3,1,3,1,1,1,1,1,2,3,2,1)*2.5)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=.125,label.y=0,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_x_continuous('Chlorophyll Index',limits = c(0.125,0.25), breaks = seq(0.125,0.25,.025)) +
  scale_y_continuous('LtCy Percent Cover',limits = c(-5,70), breaks = seq(-10,70,10)) +
  labs(title="A. FieldSpec Chlorophyll")+
  theme_bw()+
  theme(legend.position = c(.83,.78),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_LCY_Chlorophyll_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#ASD Brightness
ggplot(data_asd, aes(x=BI, y=LCY)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=c(2,3,2,1,1,2,4,4,3,1,3,1,1,1,1,1,2,3,2,1)*2.5)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=.055,label.y=65,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_x_continuous('Brightness Index',limits = c(0.055,0.125), breaks = seq(0.055,0.125,.01)) +
  scale_y_continuous('LtCy Percent Cover',limits = c(-5,70), breaks = seq(-10,70,10)) +
  labs(title="B. FieldSpec Brightness")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.83,.78),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_LCY_Brightness_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#ASD Moisture
ggplot(data_asd, aes(x=NDWI, y=LCY)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=c(2,3,2,1,1,2,4,4,3,1,3,1,1,1,1,1,2,3,2,1)*2.5)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=.075,label.y=0,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_x_continuous('Moisture Index',limits = c(0.075,0.25), breaks = seq(0.075,0.25,.025)) +
  scale_y_continuous('LtCy Percent Cover',limits = c(-5,70), breaks = seq(-10,70,10)) +
  labs(title="C. FieldSpec Moisture")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.83,.78),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_LCY_Moisture_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#ASD NIR
ggplot(data_asd, aes(x=NIR, y=LCY)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=c(2,3,2,1,1,2,4,4,3,1,3,1,1,1,1,1,2,3,2,1)*2.5)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=.14,label.y=60,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_x_continuous('Normalized Near Infrared Reflectance Index',limits = c(0.125,0.3), breaks = seq(0.125,0.3,.025)) +
  scale_y_continuous('LtCy Percent Cover',limits = c(-5,70), breaks = seq(-10,70,10)) +
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.83,.78),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_LCY_NIR_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')
