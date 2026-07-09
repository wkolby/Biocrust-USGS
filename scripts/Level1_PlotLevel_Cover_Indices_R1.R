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
data<-cbind(uas_ndvi,LtCy$TotCover,DkCy$TotCover,Lichen$TotCover,Moss$TotCover,Cover_Plant$TotCover/100,LtCy$TotCover/(LtCy$TotCover+DkCy$TotCover+Lichen$TotCover+Moss$TotCover))
colnames(data)<-c('Plot','Treat','NDVI','LCY','DCY','LCN','MSS','PLT','RATIO')

#By LCY
#filter poor data quality / outliers
data$NDVI[which(data$Plot=="B4" & data$Treat=="CC")]=NA
#Stats
fit=lm(data$NDVI~data$RATIO)
#calculate percent change
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
#Normalize to 10% RATIO change
PDlt=round((Dlt/(max(data$RATIO)-min(data$RATIO)))*.1,1)
ggplot(data, aes(x=RATIO, y=NDVI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=.1,label.y=.1725,size=8)+
  annotate("text",x=.1,y=0.1625,label=(paste0("Delta = ",-PDlt,"%")),adj=0,size = 8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Chlorophyll Index',limits = c(.16,.24), breaks = seq(.14,.24,.02))+
  scale_x_continuous('LtCy Fractional Cover',limits = c(0.1,1), breaks = seq(0,1,.1)) +
  labs(title="D. UAS Chlorophyll")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.825,.23),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_NDVI_RATIO_UAS_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#By PLT
fit=lm(data$NDVI~data$PLT)
ggplot(data, aes(x=PLT, y=NDVI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=.11,label.y=.235,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Chlorophyll Index',limits = c(.16,.24), breaks = seq(.16,.24,.02))+
  scale_x_continuous('Plant Fractional Cover',limits = c(0.11,.21), breaks = seq(0,1,.01)) +
  labs(title="D. UAS Chlorophyll")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.825,.23),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_NDVI_PLANT_UAS_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')


#UAS Brightness
uas_bi <- read.csv2(paste(github_dir,'/data/Level1/',"BPlots_Micasense_BI_Full.csv",sep=''),sep=',',header=T)
uas_bi$Value<-as.numeric(uas_bi$Value)
data<-cbind(uas_bi,LtCy$TotCover,DkCy$TotCover,Lichen$TotCover,Moss$TotCover,Cover_Plant$TotCover/100,LtCy$TotCover/(LtCy$TotCover+DkCy$TotCover+Lichen$TotCover+Moss$TotCover))
colnames(data)<-c('Plot','Treat','BI','LCY','DCY','LCN','MSS','PLT','RATIO')

#By LCY
#filter poor data quality / outliers
data$BI[which(data$Plot=="B4" & data$Treat=="CC")]=NA
#Stats
fit=lm(data$BI~data$RATIO)
#calculate percent change
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
#Normalize to 10% RATIO change
PDlt=round((Dlt/(max(data$RATIO)-min(data$RATIO)))*.1,1)
ggplot(data, aes(x=RATIO, y=BI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=1,label.y=.07,adj=1,size=8)+
  annotate("text",x=1,y=0.0625,label=(paste0("Delta = ",PDlt,"%")),adj=1,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Brightness Index',limits = c(.06,.12), breaks = seq(.06,.12,.01))+
  scale_x_continuous('LtCy Fractional Cover',limits = c(.1,1), breaks = seq(.1,1,.1)) +
  labs(title="E. UAS Brightness")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.825,.23),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_BI_RATIO_UAS_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#By PLT
fit=lm(data$BI~data$PLT)
ggplot(data, aes(x=PLT, y=BI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=.11,label.y=.0625,adj=0,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Brightness Index',limits = c(.06,.12), breaks = seq(.06,.12,.01))+
  scale_x_continuous('Plant Fractional Cover',limits = c(.11,.21), breaks = seq(.11,.21,.01)) +
  labs(title="E. UAS Brightness")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.825,.23),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_BI_PLANT_UAS_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')


#UAS Surface Temperature
uas_sti <- read.csv2(paste(github_dir,'/data/Level1/',"BPlots_Thermal_Flight3_Full.csv",sep=''),sep=',',header=T)
uas_sti$Value<-as.numeric(uas_sti$Value)
data<-cbind(uas_sti,LtCy$TotCover,DkCy$TotCover,Lichen$TotCover,Moss$TotCover,Cover_Plant$TotCover/100,LtCy$TotCover/(LtCy$TotCover+DkCy$TotCover+Lichen$TotCover+Moss$TotCover))
colnames(data)<-c('Plot','Treat','Temp','LCY','DCY','LCN','MSS','PLT','RATIO')
data$AbsTemp=(data$Temp*37) + 1.5

#By LCY
#filter poor data quality / outliers
data$Temp[which(data$Plot=="B4" & data$Treat=="CC")]=NA
#Stats
fit=lm(data$Temp~data$RATIO)
#calculate percent change
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
#Normalize to 10% RATIO change
PDlt=round((Dlt/(max(data$RATIO)-min(data$RATIO)))*.1,1)
ggplot(data, aes(x=RATIO, y=Temp)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=1,label.y=0.503,adj=1,size=8)+
  annotate("text",x=1,y=0.485,label=(paste0("Delta = ",PDlt,"%")),adj=1,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Surface Temperature Index',limits = c(.48,.62), breaks = seq(.48,.62,.02)) +
  scale_x_continuous('LtCy Fractional Cover',limits = c(.1,1), breaks = seq(.1,1,.1)) +
  labs(title="F. UAS Surface Temperature")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.825,.23),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_ST_RATIO_UAS_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#By LCY (Absolute Temp)
#filter poor data quality / outliers
data$AbsTemp[which(data$Plot=="B4" & data$Treat=="CC")]=NA
#Stats
fit=lm(data$AbsTemp~data$RATIO)
summary(fit)$coefficients
#calculate percent change
Abs=round(max(fit$fitted.values)-min(fit$fitted.values),1)
#Normalize to 10% RATIO change
PDlt=round((Abs/(max(data$RATIO)-min(data$RATIO)))*.1,1)
ggplot(data, aes(x=RATIO, y=AbsTemp)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=.1,label.y=24.75,adj=0,size=8)+
  annotate("text",x=.1,y=24,label=(paste0("Delta = ",PDlt," C")),adj=0,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Surface Temperature Index',limits = c(19,25), breaks = seq(19,25,.5)) +
  scale_x_continuous('LtCy Fractional Cover',limits = c(.1,1), breaks = seq(.1,1,.1)) +
  labs(title="UAS Absolute Surface Temperature")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.825,.23),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_STabs_RATIO_UAS_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')


#By PLT
fit=lm(data$Temp~data$PLT)
ggplot(data, aes(x=PLT, y=Temp)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=.11,label.y=0.49,adj=0,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Surface Temperature Index',limits = c(.48,.62), breaks = seq(.48,.62,.02)) +
  scale_x_continuous('Plant Fractional Cover',limits = c(.11,.21), breaks = seq(.11,.21,.01)) +
  labs(title="F. UAS Surface Temperature")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.825,.23),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_ST_PLANT_UAS_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')


###############ASD#####################################
asd_plot <- read.csv2(paste(github_dir,'/data/Level1/',"ASD_Bands_Indices_Plot.csv",sep=''),sep=',',header=T)
asd_plot$NIR<-as.numeric(asd_plot$NIR)
asd_plot$NDVI<-as.numeric(asd_plot$NDVI)
asd_plot$BI<-as.numeric(asd_plot$BI)
asd_plot$NDWI<-as.numeric(asd_plot$NDWI)
data_asd<-cbind(data$Plot,data$Treat,asd_plot[c(8,9,10,11)],LtCy$TotCover,DkCy$TotCover,Lichen$TotCover,Moss$TotCover,Cover_Plant$TotCover/100,LtCy$TotCover/(LtCy$TotCover+DkCy$TotCover+Lichen$TotCover+Moss$TotCover))
colnames(data_asd)<-c('Plot','Treat','NIR','NDVI','BI','NDWI','LCY','DCY','LCN','MSS','PLT','RATIO')

#LCY Chlorophyll
#filter poor data quality / outliers
data_asd$NDVI[which(data_asd$Plot=="B4" & data_asd$Treat=="CC")]=NA
#Stats
fit=lm(data_asd$NDVI~data_asd$RATIO)
#calculate percent change
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
#Normalize to 10% RATIO change
PDlt=round((Dlt/(max(data$RATIO)-min(data$RATIO)))*.1,1)
ggplot(data_asd, aes(x=RATIO, y=NDVI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.y=.15,label.x=.1,size=8)+
  annotate("text",x=.1,y=0.135,label=(paste0("Delta = ",-PDlt,"%")),adj=0,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Chlorophyll Index',limits = c(0.13,0.255), breaks = seq(0.13,0.27,.02)) +
  scale_x_continuous('LtCy Fractional Cover',limits = c(.1,1), breaks = seq(.1,1,.1)) +
  labs(title="A. FieldSpec Chlorophyll")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.825,.775),legend.background = element_blank(),legend.text=element_text(size=16)) + #legend.box.background = element_rect(color = 'black')
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_NDVI_RATIO_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#PLT Chlorophyll
#Stats
fit=lm(data_asd$NDVI~data_asd$PLT)
ggplot(data_asd, aes(x=PLT, y=NDVI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.y=.245,label.x=.11,size=8)+
  #annotate("text",x=.1,y=0.135,label=(paste0("Delta = ",-PDlt,"%")),adj=0,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Chlorophyll Index',limits = c(0.13,0.25), breaks = seq(0.13,0.25,.02)) +
  scale_x_continuous('Plant Fractional Cover',limits = c(.11,.21), breaks = seq(.11,.21,.01)) +
  labs(title="A. FieldSpec Chlorophyll")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.825,.1),legend.background = element_blank(),legend.text=element_text(size=16)) + #legend.box.background = element_rect(color = 'black')
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_NDVI_PLANT_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#LCY Brightness
#filter poor data quality / outliers
data_asd$BI[which(data_asd$Plot=="B4" & data_asd$Treat=="CC")]=NA
#Stats
fit=lm(data_asd$BI~data_asd$RATIO)
#calculate percent change
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
#Normalize to 10% RATIO change
PDlt=round((Dlt/(max(data$RATIO)-min(data$RATIO)))*.1,1)
ggplot(data_asd, aes(x=RATIO, y=BI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.y=.06,label.x=1,adj=1,size=8)+
  annotate("text",x=1,y=0.045,label=(paste0("Delta = ",PDlt,"%")),adj=1,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Brightness Index',limits = c(0.04,0.16), breaks = seq(0.04,0.16,.02)) +
  scale_x_continuous('LtCy Fractional Cover',limits = c(.1,1), breaks = seq(.1,1,.1)) +
  labs(title="B. FieldSpec Brightness")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.83,.78),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_BI_RATIO_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#PLT Brightness
#Stats
fit=lm(data_asd$BI~data_asd$PLT)
ggplot(data_asd, aes(x=PLT, y=BI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.y=.055,label.x=.11,adj=0,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Brightness Index',limits = c(0.05,0.15), breaks = seq(0.05,0.15,.02)) +
  scale_x_continuous('Plant Fractional Cover',limits = c(.11,.21), breaks = seq(.11,.21,.01)) +
  labs(title="B. FieldSpec Brightness")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.83,.78),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_BI_PLANT_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#LCY Moisture
#filter poor data quality / outliers
data_asd$NDWI[which(data_asd$Plot=="B4" & data_asd$Treat=="CC")]=NA
#Stats
fit=lm(data_asd$NDWI~data_asd$RATIO)
#calculate percent change
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
#Normalize to 10% RATIO change
PDlt=round((Dlt/(max(data$RATIO)-min(data$RATIO)))*.1,1)
ggplot(data_asd, aes(x=RATIO, y=NDWI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.y=.095,label.x=.1,size=8)+
  annotate("text",x=.1,y=0.075,label=(paste0("Delta = ",-PDlt,"%")),adj=0,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Moisture Index',limits = c(0.07,0.24), breaks = seq(0.07,0.24,.02)) +
  scale_x_continuous('LtCy Fractional Cover',limits = c(.1,1), breaks = seq(.1,1,.1)) +
  labs(title="C. FieldSpec Moisture")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.83,.78),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_NDWI_RATIO_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#PLT Moisture
#Stats
fit=lm(data_asd$NDWI~data_asd$PLT)
ggplot(data_asd, aes(x=PLT, y=NDWI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.y=.23,label.x=.11,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Moisture Index',limits = c(0.07,0.24), breaks = seq(0.07,0.24,.02)) +
  scale_x_continuous('Plant Fractional Cover',limits = c(.11,.21), breaks = seq(.11,.21,.01)) +
  labs(title="C. FieldSpec Moisture")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.83,.78),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_NDWI_PLANT_ASD_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

###############WV3#####################################
wv3_plot <- read.csv2(paste(github_dir,'/data/Level1/',"WV3_Indices_Plot.csv",sep=''),sep=',',header=T)
wv3_plot$NDVI<-as.numeric(wv3_plot$NDVI)
wv3_plot$BI<-as.numeric(wv3_plot$BI)
data_wv3<-cbind(wv3_plot$Plot,wv3_plot$Treat,wv3_plot[c(5,6)],LtCy$TotCover,DkCy$TotCover,Lichen$TotCover,Moss$TotCover,Cover_Plant$TotCover,LtCy$TotCover/(LtCy$TotCover+DkCy$TotCover+Lichen$TotCover+Moss$TotCover))
colnames(data_wv3)<-c('Plot','Treat','NDVI','BI','LCY','DCY','LCN','MSS','PLT','RATIO')

#WV3 Chlorophyll
#filter poor data quality / outliers
data_wv3$NDVI[which(data_wv3$Plot=="B4" & data_wv3$Treat=="CC")]=NA
#Stats
fit=lm(data_wv3$NDVI~data_wv3$RATIO)
#calculate percent change
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
#Normalize to 10% RATIO change
PDlt=round((Dlt/(max(data$RATIO)-min(data$RATIO)))*.1,1)
ggplot(data_wv3, aes(x=RATIO, y=NDVI)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.y=.1575,label.x=.05,size=8)+
  annotate("text",x=.05,y=0.145,label=(paste0("Delta = ",-PDlt,"%")),adj=0,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Chlorophyll Index',limits = c(0.139,0.24), breaks = seq(0.14,0.24,.02)) +
  scale_x_continuous('LtCy Fractional Cover',limits = c(.05,1), breaks = seq(.1,1,.1)) +
  labs(title="A. WorldView-3 Chlorophyll")+
  theme_bw()+
  theme(legend.position = c(.825,.775),legend.background = element_blank(),legend.text=element_text(size=16)) + #legend.box.background = element_rect(color = 'black')
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_NDVI_RATIO_WV3_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

#WV3 Brightness
#filter poor data quality / outliers
data_wv3$BI[which(data_wv3$Plot=="B4" & data_wv3$Treat=="CC")]=NA
#Stats
fit=lm(data_wv3$BI~data_wv3$RATIO)
#calculate percent change
Dlt=round(((max(fit$fitted.values)-min(fit$fitted.values))/min(fit$fitted.values))*100,1)
#Normalize to 10% RATIO change
PDlt=round((Dlt/(max(data$RATIO)-min(data$RATIO)))*.1,1)
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

########################################MERGED######################################################################
#open data files
Merged <- read.csv2(paste(github_dir,'/data/Level1/',"Merged_SpecBands_Plot.csv",sep=''),sep=',',header=T)
Merged_long <- Merged %>% pivot_longer(cols = 5:11,
                                       names_to = "Index", 
                                       values_to = "Val")
#Indices
Merged_long$Val<-as.numeric(Merged_long$Val)
Merged_wide <- Merged_long %>% pivot_wider(names_from = Sensor, values_from = Val)
Merged_wide_uas<-subset(Merged_wide, Index %in% c("CBLUE","BLUE","GREEN","RED","REDEDGE","NIR"))

Merged_wide_uas$Index <- factor(Merged_wide_uas$Index,levels=c("CBLUE","BLUE","GREEN","RED","REDEDGE","NIR"))
ggplot(Merged_wide_uas, aes(x=FieldSpec, y=UAS,color=Index)) +
  scale_color_manual(values=c("cyan2","blue","green2","red1","red4","purple3"))+
  geom_point(size=3)+
  geom_smooth(method=lm,aes(group = Index))+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),label.y=c(.30,.275,.250,.225,.30,.275,.250,.225),label.x=c(.05,.05,.05,.05,.1,.1,.1,.1),geom = "label",show.legend = FALSE)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  scale_y_continuous('UAS',limits = c(0.05,0.3), breaks = seq(0,0.3,.05)) +
  scale_x_continuous('FieldSpec',limits = c(0.05,0.3), breaks = seq(0,0.3,.05)) +
  theme_bw()+
  theme(legend.title = element_blank(),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_FieldSpec_UAS_SpecBand_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

Merged_wide$Index <- factor(Merged_wide$Index,levels=c("CBLUE","BLUE","GREEN","YELLOW","RED","REDEDGE","NIR"))
ggplot(Merged_wide, aes(x=FieldSpec, y=Satellite,color=Index)) +
  scale_color_manual(values=c("cyan2","blue","green2","yellow4","red1","red4","purple3"))+
  geom_point(size=3)+
  geom_smooth(method=lm,aes(group = Index))+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),label.y=c(.30,.275,.250,.225,.30,.275,.250,.225),label.x=c(.05,.05,.05,.05,.1,.1,.1,.1),geom = "label",show.legend = FALSE)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  scale_y_continuous('WorldView-3',limits = c(0.05,0.3), breaks = seq(0,0.3,.05)) +
  scale_x_continuous('FieldSpec',limits = c(0.05,0.3), breaks = seq(0,0.3,.05)) +
  theme_bw()+
  theme(legend.title = element_blank(),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_FieldSpec_WV3_SpecBand_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(Merged_wide_uas, aes(x=UAS, y=Satellite,color=Index)) +
  scale_color_manual(values=c("cyan3","blue","green2","red1","red4","purple3"))+
  geom_point(size=3)+
  geom_smooth(method=lm,aes(group = Index))+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),label.y=c(.30,.275,.250,.225,.30,.275,.250,.225),label.x=c(.05,.05,.05,.05,.1,.1,.1,.1),geom = "label",show.legend = FALSE)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  scale_y_continuous('WorldView-3',limits = c(0.05,0.3), breaks = seq(0,0.3,.05)) +
  scale_x_continuous('UAS',limits = c(0.05,0.3), breaks = seq(0,0.3,.05)) +
  theme_bw()+
  theme(legend.title = element_blank(),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_UAS_WV3_SpecBand_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

##############################ABSLOUTE TEMPERATURE CALCULATION############################################
#open data files
Altum <- read.csv2(paste(github_dir,'/data/Level1/',"BPlots_Thermal_Altum_Full.csv",sep=''),sep=',',header=T)
Altum$Value<-(as.numeric(Altum$Value)/100)-273.15
XT2 <- read.csv2(paste(github_dir,'/data/Level1/',"BPlots_Thermal_Flight3_Full.csv",sep=''),sep=',',header=T)
XT2$Value<-as.numeric(XT2$Value)
XT2$Value2<-Altum$Value
#Long format
AbsT_long <- XT2 %>% pivot_longer(cols = 3:4,
                                       names_to = "Index", 
                                       values_to = "Val")

ggplot(XT2, aes(x=Value, y=Value2)) +
  geom_point(aes(fill=Treat),color = "black",shape = 21,size=8)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=.48,label.y=24.1,size=8)+
  stat_regline_equation(label.x =.48,label.y=24.9,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_x_continuous('Surface Temperature Index',limits = c(.48,.62), breaks = seq(.48,.62,.02)) +
  scale_y_continuous('Surface Temperature (C)',limits = c(19,25), breaks = seq(19,25,.5)) +
  labs(title="")+
  theme_bw()+
  theme(legend.position = "none") + 
  #theme(legend.position = c(.825,.23),legend.background = element_blank(), legend.box.background = element_rect(color = 'black'),legend.text=element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_STabs_STindex_UAS_PlotLevel.png",sep=""),dpi=300,width=180,height=120,units='mm')

