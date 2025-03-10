rm(list = ls()) # clear the environment
setwd("/Users/wksmith/Documents/GitHub/Biocrust-USGS")
github_dir <- "/Users/wksmith/Documents/GitHub/Biocrust-USGS"
library(asdreader)
library(reshape2)
library(tidyverse)
library(stringr)

#############MOPRO########################
data <- read.csv2(paste(github_dir,'/data/MoPro/bplot_025_soil_moisture_hourly.csv',sep=''),sep=',',header=T)
subset_stats=subset(data, type=="MOPRO")
subset_stats$temp<-as.numeric(subset_stats$temp)
subset_stats$sm.corr<-as.numeric(subset_stats$sm.corr)

out <- subset_stats %>% group_by(block,tx) %>% summarise(mean_t = mean(temp), mean_sm = mean(sm.corr))
#write csv file
write.csv(out,paste0(github_dir,'/data/level1/BPlots_MOPRO_Full.csv'),row.names=FALSE,col.names=TRUE)

#############Box Plots###################
subset_stats$tx <- factor(subset_stats$tx,levels=c("C","W","L","LW"))
tx_names=c('lc' = 'LtCy','dc'="DkCy",'moss'='Moss')
ggplot(subset_stats, aes(x=tx, y=temp, fill=tx)) + 
  geom_boxplot() +
  scale_fill_manual(name='Treatment', values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c('Control','AltP','Warmed','AltP+Warmed'))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "C",label.y = 31)+ # Pairwise comparison against Control
  stat_compare_means(label.y = -10)+ # Add global p-value
  #facet_wrap(~cover,labeller = as_labeller(tx_names))+ 
  scale_x_discrete(labels=c('Control','AltP','Warmed','AltP+Warmed'))+
  labs(title='A. Surface Temperature',y=expression(paste("Surface Temperature ( ", degree, "C)")),x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_MOPRO_SurfaceTemp_byTreat.png",sep=""),dpi=300,width=180,height=180,units='mm')
#stats
compare_means(temp ~ tx,  data = subset(subset_stats))

#############Box Plots###################
subset_stats$cover <- factor(subset_stats$cover,levels=c("lc","dc","moss"))
tx_names=c('lc' = 'LtCy','dc'="DkCy",'moss'='Moss')
ggplot(subset_stats, aes(x=cover, y=temp, fill=cover)) + 
  geom_boxplot() +
  scale_fill_manual(name='Func Type', values=c('red3','green3','purple'),labels=c('LtCy','DkCy','Moss'))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "lc",label.y = 30)+ # Pairwise comparison against Control
  stat_compare_means(label.y = -10)+ # Add global p-value
  #facet_wrap(~tx)+ #labeller = as_labeller(tx_names) 
  scale_x_discrete(labels=c('LtCy','DkCy','Moss'))+
  labs(title='C. Surface Temperature',y=expression(paste("Surface Temperature ( ", degree, "C)")),x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_MOPRO_SurfaceTemp_byFunc.png",sep=""),dpi=300,width=180,height=180,units='mm')
#stats
compare_means(temp ~ cover,  data = subset(subset_stats))

#############Box Plots###################
subset_stats$tx <- factor(subset_stats$tx,levels=c("C","W","L","LW"))
tx_names=c('lc' = 'LtCy','dc'="DkCy",'moss'='Moss')
ggplot(subset_stats, aes(x=tx, y=sm.corr, fill=tx)) + 
  geom_boxplot() +
  scale_fill_manual(name='Treatment', values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c('Control','AltP','Warmed','AltP+Warmed'))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "C",label.y = 0.00175)+ # Pairwise comparison against Control
  stat_compare_means(label.y = -.000175)+ # Add global p-value
  #facet_wrap(~cover,labeller = as_labeller(tx_names))+ 
  scale_x_discrete(labels=c('Control','AltP','Warmed','AltP+Warmed'))+
  labs(title='B. Soil Moisture',y=expression(paste("Gravimetric Water Content")),x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_MOPRO_SoilMoisture_byTreat.png",sep=""),dpi=300,width=180,height=180,units='mm')
#stats
compare_means(sm.corr ~ tx,  data = subset(subset_stats))

#############Box Plots###################
subset_stats$cover <- factor(subset_stats$cover,levels=c("lc","dc","moss"))
tx_names=c('lc' = 'LtCy','dc'="DkCy",'moss'='Moss')
ggplot(subset_stats, aes(x=cover, y=sm.corr, fill=cover)) + 
  geom_boxplot() +
  scale_fill_manual(name='Func Type', values=c('red3','green3','purple'),labels=c('LtCy','DkCy','Moss'))+
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "lc",label.y = 0.00175)+ # Pairwise comparison against Control
  stat_compare_means(label.y = -.000175)+ # Add global p-value
  #facet_wrap(~tx)+ #labeller = as_labeller(tx_names) 
  scale_x_discrete(labels=c('LtCy','DkCy','Moss'))+
  labs(title='D. Soil Moisture',y=expression(paste("Gravimetric Water Content")),x='')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/',"Box_MOPRO_SoilMoisture_byFunc.png",sep=""),dpi=300,width=180,height=180,units='mm')

#stats
compare_means(sm.corr ~ cover,  data = subset(subset_stats))

