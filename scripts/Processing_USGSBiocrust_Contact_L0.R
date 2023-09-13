setwd("/Users/wksmith/Documents/GitHub/Biocrust-USGS")
github_dir <- "/Users/wksmith/Documents/GitHub/Biocrust-USGS"
library(asdreader)
library(reshape2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)

#####################################
master <- read.csv2(paste(github_dir,'/data/ASD/Contact_Datasheet_021422_021522.csv',sep=''),sep=',',header=T)
master$File<-str_pad(master$File, 5, pad = "0")
dataset <- data.frame()

for(i in 1:length(master$File)){
  file <- list.files(paste(github_dir,'/data/ASD/',master$Dir[i],sep=''), full.names = T, pattern = paste(master$File[i],".asd",sep=''))
  md<-get_metadata(file)
  temp_data<-get_spectra(file)
  data<-as.numeric(temp_data)
  wvl=as.integer(colnames(temp_data))
  c1=data[which(wvl==1000)]-data[which(wvl==1001)]
  c2=data[which(wvl==1800)]-data[which(wvl==1801)]
  data_c=c(data[1:which(wvl==1000)],data[which(wvl==1001):which(wvl==1800)]+c1,data[which(wvl==1801):which(wvl==2500)]+c1+c2)
  data_s=data_c/sqrt(sum(data_c^2))
  plot=rep(master$Plot[i],length(wvl))
  tmp=rep(master$Tmp[i],length(wvl))
  wtr=rep(master$Wtr[i],length(wvl))
  treat=rep(master$Treat[i],length(wvl))
  quad=rep(master$Quad[i],length(wvl))
  type=rep(master$Biocrust[i],length(wvl))
  rep=rep(master$Rep[i],length(wvl))
  scan=rep(sub(".*/", "", file),length(wvl))
  
  ds=cbind(wvl,data_c,plot,tmp,wtr,treat,quad,type,rep,scan)
  dataset <- rbind(dataset,ds)
}
#Name the columns
names(dataset) <- c("wavelength","reflectance","plot","tmp","wtr","treat","quad","type","rep","scan")
dataset=transform(dataset,wavelength = as.numeric(wavelength))
dataset=transform(dataset,reflectance = as.numeric(reflectance))


#####ASD Reflectance Plots#################
dataset<-subset(dataset, type %in% c('DCY','LCY','LCN','MSS'))
dataset_mean_std <- dataset %>%
 group_by(wavelength,treat,type) %>%
 summarise_at(vars(reflectance), list(mean=mean, sd=sd)) %>%
 as.data.frame()

ggplot(subset(dataset_mean_std, treat %in% c('LW','LC')), aes(x=wavelength,y=mean,group=type,color=type)) +
  geom_line(show.legend = T,linewidth=.5,linetype="solid") +
  scale_color_manual(values=c('red3','green3','cyan2','purple'))+
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd, fill = type), alpha = .2) +
  scale_fill_manual(values=c('red3','green3','cyan2','purple'))+
  facet_wrap(~treat)+
  scale_y_continuous("Reflectance") +
  scale_x_continuous("Wavelength (nm)",limits = c(400,2400), breaks = seq(400,2400,200)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 40)) +
  theme_bw()
ggsave(paste(github_dir,'/figures/Hyperspectral_Reflectance_FULL_ASD.png',sep=''),dpi=300,width=180,height=120,units='mm')

ggplot(subset(dataset_mean_std, treat %in% c('LW','LC')), aes(x=wavelength,y=mean,group=type,color=type)) +
  geom_line(show.legend = T,linewidth=.5,linetype="solid") +
  scale_color_manual(values=c('red3','green3','cyan2','purple'))+
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd, fill = type), alpha = .2) +
  scale_fill_manual(values=c('red3','green3','cyan2','purple'))+
  facet_wrap(~treat)+
  scale_y_continuous("Reflectance") +
  scale_x_continuous("Wavelength (nm)",limits = c(450,900), breaks = seq(450,900,50)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 40)) +
  theme_bw()
ggsave(paste(github_dir,'/figures/Hyperspectral_Reflectance_VNIR_ASD.png',sep=''),dpi=300,width=180,height=120,units='mm')


#####################################
master <- read.csv2(paste(github_dir,'/data/Headwall/Headwall_Datasheet_021422_021522.csv',sep=''),sep=',',header=T)
dataset <- data.frame()
for(i in 1:length(master$File)){
  temp_data<-read.table(paste(github_dir,'/data/Headwall/',master$Dir[i],'/',master$File[i],sep=''),skip=3)
  data_c<-as.numeric(temp_data[,2])
  wvl=as.numeric(temp_data[,1])
  data_s=data_c/sqrt(sum(data_c^2))
  plot=rep(master$Plot[i],length(wvl))
  tmp=rep(master$Tmp[i],length(wvl))
  wtr=rep(master$Wtr[i],length(wvl))
  treat=rep(master$Treat[i],length(wvl))
  quad=rep(master$Quad[i],length(wvl))
  type=rep(master$Biocrust[i],length(wvl))
  rep=rep(master$Rep[i],length(wvl))
  scan=rep(master$File[i],length(wvl))
  
  ds=cbind(wvl,data_c,plot,tmp,wtr,treat,quad,type,rep,scan)
  dataset <- rbind(dataset,ds)
}
#Name the columns
names(dataset) <- c("wavelength","reflectance","plot","tmp","wtr","treat","quad","type","rep","scan")
dataset=transform(dataset,wavelength = as.numeric(wavelength))
dataset=transform(dataset,reflectance = as.numeric(reflectance))

#####Headwall Reflectance Plots#################
dataset<-subset(dataset, type %in% c('DCY','LCY','MSS'))
dataset_mean_std <- dataset %>%
  group_by(wavelength,treat,type) %>%
  summarise_at(vars(reflectance), list(mean=mean, sd=sd)) %>%
  as.data.frame()

ggplot(dataset_mean_std, aes(x=wavelength,y=mean,group=type,color=type)) +
  geom_line(show.legend = T,linewidth=.5,linetype="solid") +
  scale_color_manual(values=c('red3','cyan2','purple'))+
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd, fill = type), alpha = .2) +
  scale_fill_manual(values=c('red3','cyan2','purple'))+
  facet_wrap(~treat)+
  scale_y_continuous("Reflectance") +
  scale_x_continuous("Wavelength (nm)",limits = c(450,900), breaks = seq(400,2400,200)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 40)) +
  theme_bw()
ggsave(paste(github_dir,'/figures/Hyperspectral_Reflectance_Headwall.png',sep=''),dpi=300,width=180,height=120,units='mm')

  