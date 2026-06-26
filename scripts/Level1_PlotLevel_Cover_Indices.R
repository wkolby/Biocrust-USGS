rm(list = ls()) # clear the environment
setwd("/Users/wksmith/Documents/GitHub/CastleValley_Campaign_Biocrust_Analysis")
github_dir <- "/Users/wksmith/Documents/GitHub/CastleValley_Campaign_Biocrust_Analysis"

library(tidyverse)
library(ggpubr)
library(RColorBrewer)

#######################################PLOT LEVEL###################################################################
########################################BioCrust Cover 100pt##############################################################
Cover<-read.csv2(paste(github_dir,'/data/Level1/',"2022_DOE_Cover_Data.csv",sep=''),sep=',',header=T)
Cover$PctCover<-as.numeric(Cover$PctCover)
Cover_Stats = Cover %>% group_by(Plot,Block,Treatment,FG) %>% 
  summarise(N = n(), TotCover = mean(PctCover,na.rm=T),Cover_Sd = sd(PctCover,na.rm=T))
Cover_Stats<-subset(Cover_Stats, Plot %in% 'B')
Cover_Biocrust<-subset(Cover_Stats, FG %in% c('LtCy','DkCy','Lichen','Moss'))
Cover_Biocrust$Treatment <- factor(Cover_Biocrust$Treatment,levels=c("C","W","L","LW"))
Cover_Biocrust<-Cover_Biocrust[order(Cover_Biocrust$Block,Cover_Biocrust$Treatment),]
LtCy<-subset(Cover_Biocrust, FG %in% c('LtCy'))
DkCy<-subset(Cover_Biocrust, FG %in% c('DkCy'))
Lichen<-subset(Cover_Biocrust, FG %in% c('Lichen'))
Moss<-subset(Cover_Biocrust, FG %in% c('Moss'))

Cover_Plant<-subset(Cover_Stats, FG %in% c('Annual_Forb','Annual_Grass','Perennial_Grass','Shrub','Cactus'))
Cover_Plant = Cover_Plant %>% group_by(Plot,Block,Treatment) %>% 
  summarise(TotCover = sum(TotCover,na.rm=T))
Cover_Plant$Treatment <- factor(Cover_Plant$Treatment,levels=c("C","W","L","LW"))
Cover_Plant<-Cover_Plant[order(Cover_Plant$Treatment),]
Cover_Plant<-Cover_Plant[order(Cover_Plant$Block),]

#Stacked Barplot
Cover_Biocrust$FG <- factor(Cover_Biocrust$FG,levels=c('LtCy','DkCy','Lichen','Moss'))
ggplot(Cover_Biocrust, aes(x = Treatment, y = TotCover, fill=FG)) +
  geom_bar(stat="identity",position='stack',color='black') +
  scale_fill_manual(values=c('red3','green3','cyan2','purple'),labels=c('LtCy','DkCy','Lichen','Moss'))+
  scale_y_continuous("Percent Cover",expand = c(0, 0), limits = c(0, 60),breaks=c(0,20,40,60,80,100)) +
  scale_x_discrete("",labels=c('Control', 'AltP', 'Warmed', 'AltP + \nWarmed'),guide = guide_axis(angle = 45))+
  facet_wrap(~Block)+
  labs(title="",fill = "Func Type")+
  theme_bw() +
  theme(legend.position = 'right',legend.text=element_text(size=16),legend.title=element_text(size=18)) +
  theme(legend.background = element_rect(colour = NA))+
  theme(text = element_text(size = 24))
ggsave(paste(github_dir,'/figures/',"StackedBar_Cover_Treat_PointFrame_PlotLevel_ByPlot.png",sep=""),dpi=300,width=200,height=120,units='mm')

#Stacked Barplot
Cover_Biocrust = Cover_Biocrust %>% group_by(Plot,Treatment,FG) %>% 
  summarise(TotCover = mean(TotCover,na.rm=T))
Cover_Biocrust$FG <- factor(Cover_Biocrust$FG,levels=c('LtCy','DkCy','Lichen','Moss'))
ggplot(Cover_Biocrust, aes(x = Treatment, y = TotCover, fill=FG)) +
  geom_bar(stat="identity",position='stack',color='black') +
  scale_fill_manual(values=c('red3','green3','cyan2','purple'),labels=c('LtCy','DkCy','Lichen','Moss'))+
  scale_y_continuous("Percent Cover",expand = c(0, 0), limits = c(0, 35),breaks=c(0,5,10,15,20,25,30,35)) +
  scale_x_discrete("",labels=c('Control', 'AltP', 'Warmed', 'AltP + \nWarmed'),guide = guide_axis(angle = 45))+
  labs(title="",fill = "Func Type")+
  theme_bw() +
  theme(legend.position = 'right',legend.text=element_text(size=16),legend.title=element_text(size=18)) +
  theme(legend.background = element_rect(colour = NA))+
  theme(text = element_text(size = 24))
ggsave(paste(github_dir,'/figures/',"StackedBar_Cover_Treat_PointFrame_PlotLevel.png",sep=""),dpi=300,width=200,height=120,units='mm')

######Scatterplots#################################
#UAS Chlorophyll
uas_ndvi <- read.csv2(paste(github_dir,'/data/Level1/',"BPlots_Micasense_NDVI_Full.csv",sep=''),sep=',',header=T)
uas_ndvi$Value<-as.numeric(uas_ndvi$Value)
data<-cbind(uas_ndvi,LtCy$TotCover,DkCy$TotCover,Lichen$TotCover,Moss$TotCover,Cover_Plant$TotCover,LtCy$TotCover/(LtCy$TotCover+DkCy$TotCover+Lichen$TotCover+Moss$TotCover))
colnames(data)<-c('Plot','Treat','NDVI','LCY','DCY','LCN','MSS','PLT','RATIO')
fit=lm(data$NDVI~data$LCY)
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
PDlt=round((Dlt/0.85623)*.1,1)
ggplot(data, aes(x=LCY, y=NDVI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=6)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=0.0,label.y=.16,size=8)+
  annotate("text",x=0,y=0.145,label=(paste0("Delta = ",Dlt,"%")),adj=0,size = 8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Chlorophyll Index',limits = c(.14,.24), breaks = seq(.14,.24,.02))+
  scale_x_continuous('LtCy Percent Cover',limits = c(0,70), breaks = seq(-10,70,10)) +
  labs(title="D. UAS Chlorophyll")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.825,.23),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_NDVI_LCY_UAS_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

fit=lm(data$NDVI~data$RATIO)
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
PDlt=round((Dlt/0.85623)*.1,1)
ggplot(data, aes(x=RATIO, y=NDVI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=.1,label.y=.175,size=8)+
  annotate("text",x=.1,y=0.165,label=(paste0("Delta = ",-PDlt,"%")),adj=0,size = 8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Chlorophyll Index',limits = c(.16,.24), breaks = seq(.14,.24,.02))+
  scale_x_continuous('LtCy Fractional Cover',limits = c(0.05,1), breaks = seq(0,1,.1)) +
  labs(title="D. UAS Chlorophyll")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.825,.23),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_NDVI_RATIO_UAS_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#UAS Brightness
uas_bi <- read.csv2(paste(github_dir,'/data/Level1/',"BPlots_Micasense_BI_Full.csv",sep=''),sep=',',header=T)
uas_bi$Value<-as.numeric(uas_bi$Value)
data<-cbind(uas_bi,LtCy$TotCover,DkCy$TotCover,Lichen$TotCover,Moss$TotCover,Cover_Plant$TotCover,LtCy$TotCover/(LtCy$TotCover+DkCy$TotCover+Lichen$TotCover+Moss$TotCover))
colnames(data)<-c('Plot','Treat','BI','LCY','DCY','LCN','MSS','PLT','RATIO')
fit=lm(data$BI~data$LCY)
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
PDlt=round((Dlt/0.85623)*.1,1)
ggplot(data, aes(x=LCY, y=BI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=70,label.y=.08,adj=1,size=8)+
  annotate("text",x=70,y=0.07,label=(paste0("Delta = ",Dlt,"%")),adj=1,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Brightness Index',limits = c(.065,.125), breaks = seq(.065,.12,.01))+
  scale_x_continuous('LtCy Percent Cover',limits = c(0,70), breaks = seq(0,70,10)) +
  labs(title="E. UAS Brightness")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.825,.23),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_BI_LCY_UAS_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

fit=lm(data$BI~data$RATIO)
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
PDlt=round((Dlt/0.85623)*.1,1)
ggplot(data, aes(x=RATIO, y=BI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=1,label.y=.07,adj=1,size=8)+
  annotate("text",x=1,y=0.0625,label=(paste0("Delta = ",PDlt,"%")),adj=1,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Brightness Index',limits = c(.06,.12), breaks = seq(.06,.12,.01))+
  scale_x_continuous('LtCy Fractional Cover',limits = c(.05,1), breaks = seq(.1,1,.1)) +
  labs(title="E. UAS Brightness")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.825,.23),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_BI_RATIO_UAS_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')


#UAS Surface Temperature
uas_sti <- read.csv2(paste(github_dir,'/data/Level1/',"BPlots_Thermal_Flight1_Full.csv",sep=''),sep=',',header=T)
uas_sti$Value<-as.numeric(uas_sti$Value)
data<-cbind(uas_sti,LtCy$TotCover,DkCy$TotCover,Lichen$TotCover,Moss$TotCover,Cover_Plant$TotCover,LtCy$TotCover/(LtCy$TotCover+DkCy$TotCover+Lichen$TotCover+Moss$TotCover))
colnames(data)<-c('Plot','Treat','Temp','LCY','DCY','LCN','MSS','PLT','RATIO')
fit=lm(data$Temp~data$LCY)
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
PDlt=round((Dlt/0.85623)*.1,1)
ggplot(data, aes(x=LCY, y=Temp)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=70,label.y=0.24,adj=1,size=8)+
  annotate("text",x=70,y=0.225,label=(paste0("Delta = ",Dlt,"%")),adj=1,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Surface Temperature Index',limits = c(.22,.33), breaks = seq(.22,.34,.02)) +
  scale_x_continuous('LtCy Percent Cover',limits = c(0,70), breaks = seq(-0,70,10)) +
  labs(title="F. UAS Surface Temperature")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.825,.23),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_ST_LCY_UAS_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

fit=lm(data$Temp~data$RATIO)
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
PDlt=round((Dlt/0.85623)*.1,1)
ggplot(data, aes(x=RATIO, y=Temp)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=1,label.y=0.23,adj=1,size=8)+
  annotate("text",x=1,y=0.215,label=(paste0("Delta = ",PDlt,"%")),adj=1,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Surface Temperature Index',limits = c(.21,.32), breaks = seq(.21,.32,.02)) +
  scale_x_continuous('LtCy Fractional Cover',limits = c(.05,1), breaks = seq(.1,1,.1)) +
  labs(title="F. UAS Surface Temperature")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.825,.23),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_ST_RATIO_UAS_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

###############ASD#####################################
asd_plot <- read.csv2(paste(github_dir,'/data/Level1/',"ASD_Bands_Indices_Plot.csv",sep=''),sep=',',header=T)
asd_plot$NIR<-as.numeric(asd_plot$NIR)
asd_plot$NDVI<-as.numeric(asd_plot$NDVI)
asd_plot$BI<-as.numeric(asd_plot$BI)
asd_plot$NDWI<-as.numeric(asd_plot$NDWI)
data_asd<-cbind(data$Plot,data$Treat,asd_plot[c(8,9,10,11)],LtCy$TotCover,DkCy$TotCover,Lichen$TotCover,Moss$TotCover,Cover_Plant$TotCover,LtCy$TotCover/(LtCy$TotCover+DkCy$TotCover+Lichen$TotCover+Moss$TotCover))
colnames(data_asd)<-c('Plot','Treat','NIR','NDVI','BI','NDWI','LCY','DCY','LCN','MSS','PLT','RATIO')

#ASD Chlorophyll
fit=lm(data_asd$NDVI~data_asd$LCY)
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
PDlt=round((Dlt/0.85623)*.1,1)
ggplot(data_asd, aes(x=LCY, y=NDVI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.y=.15,label.x=0,size=8)+
  annotate("text",x=0,y=0.125,label=(paste0("Delta = ",Dlt,"%")),adj=0,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Chlorophyll Index',limits = c(0.11,0.25), breaks = seq(0.11,0.25,.02)) +
  scale_x_continuous('LtCy Percent Cover',limits = c(0,70), breaks = seq(0,70,10)) +
  labs(title="A. FieldSpec Chlorophyll")+
  theme_bw()+
  theme(legend.position = c(.825,.775),legend.background = element_blank(),legend.text=element_text(size=16)) + #legend.box.background = element_rect(color = 'black')
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_NDVI_LCY_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#ASD Chlorophyll
fit=lm(data_asd$NDVI~data_asd$RATIO)
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
PDlt=round((Dlt/0.85623)*.1,1)
ggplot(data_asd, aes(x=RATIO, y=NDVI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.y=.15,label.x=.1,size=8)+
  annotate("text",x=.1,y=0.135,label=(paste0("Delta = ",-PDlt,"%")),adj=0,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Chlorophyll Index',limits = c(0.13,0.255), breaks = seq(0.13,0.27,.02)) +
  scale_x_continuous('LtCy Fractional Cover',limits = c(.05,1), breaks = seq(.1,1,.1)) +
  labs(title="A. FieldSpec Chlorophyll")+
  theme_bw()+
  theme(legend.position = c(.825,.775),legend.background = element_blank(),legend.text=element_text(size=16)) + #legend.box.background = element_rect(color = 'black')
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_NDVI_RATIO_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#ASD Brightness
fit=lm(data_asd$BI~data_asd$LCY)
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
PDlt=round((Dlt/0.85623)*.1,1)
ggplot(data_asd, aes(x=LCY, y=BI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.y=.0725,label.x=70,adj=1,size=8)+
  annotate("text",x=70,y=0.06,label=(paste0("Delta = ",Dlt,"%")),adj=1,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Brightness Index',limits = c(0.055,0.145), breaks = seq(0.055,0.145,.01)) +
  scale_x_continuous('LtCy Percent Cover',limits = c(0,70), breaks = seq(0,70,10)) +
  labs(title="B. FieldSpec Brightness")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.83,.78),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_BI_LCY_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#ASD Brightness
fit=lm(data_asd$BI~data_asd$RATIO)
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
PDlt=round((Dlt/0.85623)*.1,1)
ggplot(data_asd, aes(x=RATIO, y=BI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.y=.0675,label.x=1,adj=1,size=8)+
  annotate("text",x=1,y=0.055,label=(paste0("Delta = ",PDlt,"%")),adj=1,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Brightness Index',limits = c(0.04,0.15), breaks = seq(0.04,0.15,.01)) +
  scale_x_continuous('LtCy Fractional Cover',limits = c(.05,1), breaks = seq(.1,1,.1)) +
  labs(title="B. FieldSpec Brightness")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.83,.78),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_BI_RATIO_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#ASD Moisture
fit=lm(data_asd$NDWI~data_asd$LCY)
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
PDlt=round((Dlt/0.85623)*.1,1)
ggplot(data_asd, aes(x=LCY, y=NDWI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.y=.085,label.x=0,size=8)+
  annotate("text",x=0,y=0.06,label=(paste0("Delta = ",Dlt,"%")),adj=0,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Moisture Index',limits = c(0.05,0.25), breaks = seq(0.05,0.25,.02)) +
  scale_x_continuous('LtCy Percent Cover',limits = c(0,70), breaks = seq(0,70,10)) +
  labs(title="C. FieldSpec Moisture")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.83,.78),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_NDWI_RATIO_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#ASD Moisture
fit=lm(data_asd$NDWI~data_asd$RATIO)
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
PDlt=round((Dlt/0.85623)*.1,1)
ggplot(data_asd, aes(x=RATIO, y=NDWI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.y=.095,label.x=.1,size=8)+
  annotate("text",x=.1,y=0.075,label=(paste0("Delta = ",-PDlt,"%")),adj=0,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Moisture Index',limits = c(0.07,0.24), breaks = seq(0.07,0.24,.02)) +
  scale_x_continuous('LtCy Fractional Cover',limits = c(.05,1), breaks = seq(.1,1,.1)) +
  labs(title="C. FieldSpec Moisture")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.83,.78),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_NDWI_RATIO_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

###############WV3#####################################
wv3_plot <- read.csv2(paste(github_dir,'/data/Level1/',"WV3_Indices_Plot.csv",sep=''),sep=',',header=T)
wv3_plot$NDVI<-as.numeric(wv3_plot$NDVI)
wv3_plot$BI<-as.numeric(wv3_plot$BI)
data_wv3<-cbind(wv3_plot$Plot,wv3_plot$Treat,wv3_plot[c(5,6)],LtCy$TotCover,DkCy$TotCover,Lichen$TotCover,Moss$TotCover,Cover_Plant$TotCover,LtCy$TotCover/(LtCy$TotCover+DkCy$TotCover+Lichen$TotCover+Moss$TotCover))
colnames(data_wv3)<-c('Plot','Treat','NDVI','BI','LCY','DCY','LCN','MSS','PLT','RATIO')

#WV3 Chlorophyll
fit=lm(data_wv3$NDVI~data_wv3$LCY)
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
PDlt=round((Dlt/0.85623)*.1,1)
ggplot(data_wv3, aes(x=LCY, y=NDVI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.y=.15,label.x=0,size=8)+
  annotate("text",x=0,y=0.125,label=(paste0("Delta = ",Dlt,"%")),adj=0,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Chlorophyll Index',limits = c(0.11,0.25), breaks = seq(0.11,0.25,.02)) +
  scale_x_continuous('LtCy Percent Cover',limits = c(0,70), breaks = seq(0,70,10)) +
  labs(title="A. WorldView-3 Chlorophyll")+
  theme_bw()+
  theme(legend.position = c(.825,.775),legend.background = element_blank(),legend.text=element_text(size=16)) + #legend.box.background = element_rect(color = 'black')
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_NDVI_LCY_WV3_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#WV3 Chlorophyll
fit=lm(data_wv3$NDVI~data_wv3$RATIO)
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
PDlt=round((Dlt/0.85623)*.1,1)
ggplot(data_wv3, aes(x=RATIO, y=NDVI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.y=.145,label.x=.05,size=8)+
  annotate("text",x=.05,y=0.135,label=(paste0("Delta = ",-PDlt,"%")),adj=0,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Chlorophyll Index',limits = c(0.13,0.23), breaks = seq(0.13,0.23,.02)) +
  scale_x_continuous('LtCy Fractional Cover',limits = c(.05,1), breaks = seq(.1,1,.1)) +
  labs(title="A. WorldView-3 Chlorophyll")+
  theme_bw()+
  theme(legend.position = c(.825,.775),legend.background = element_blank(),legend.text=element_text(size=16)) + #legend.box.background = element_rect(color = 'black')
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_NDVI_RATIO_WV3_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#WV3 Brightness
fit=lm(data_wv3$BI~data_wv3$LCY)
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
PDlt=round((Dlt/0.85623)*.1,1)
ggplot(data_wv3, aes(x=LCY, y=BI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.y=.0725,label.x=70,adj=1,size=8)+
  annotate("text",x=70,y=0.06,label=(paste0("Delta = ",Dlt,"%")),adj=1,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Brightness Index',limits = c(0.055,0.145), breaks = seq(0.055,0.145,.01)) +
  scale_x_continuous('LtCy Percent Cover',limits = c(0,70), breaks = seq(0,70,10)) +
  labs(title="B. WorldView-3 Brightness")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.83,.78),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_BI_LCY_WV3_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#WV3 Brightness
fit=lm(data_wv3$BI~data_wv3$RATIO)
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
PDlt=round((Dlt/0.85623)*.1,1)
ggplot(data_wv3, aes(x=RATIO, y=BI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.y=.0675,label.x=1,adj=1,size=8)+
  annotate("text",x=1,y=0.0625,label=(paste0("Delta = ",PDlt,"%")),adj=1,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Brightness Index',limits = c(0.06,0.10), breaks = seq(0.06,0.10,.01)) +
  scale_x_continuous('LtCy Fractional Cover',limits = c(.05,1), breaks = seq(.1,1,.1)) +
  labs(title="B. WorldView-3 Brightness")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.83,.78),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_BI_RATIO_WV3_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

