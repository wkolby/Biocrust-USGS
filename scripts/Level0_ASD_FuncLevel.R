setwd("/Users/wksmith/Documents/GitHub/Biocrust-USGS")
github_dir <- "/Users/wksmith/Documents/GitHub/Biocrust-USGS"
library(asdreader)
library(reshape2)
library(tidyverse)
#library(tidyr)
#library(dplyr)
library(stringr)

###FUNCTIONS##################################################################################
#Calculates vegetation simple ratio (a:b) / (c:d)
calc_SR <- function(df, a, b, c, d){
  b1_1 <- which.min(abs(a - as.numeric(names(df), options(warn=-1))))   #Identify first band and start range
  b1_2 <- which.min(abs(b - as.numeric(names(df), options(warn=-1))))   #Identify first band and end range
  b2_1 <- which.min(abs(c - as.numeric(names(df), options(warn=-1))))   #Identify second band and start range
  b2_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify second band and end range
  (rowMeans(df_wide[b1_1:b1_2])/(rowMeans(df_wide[b2_1:b2_2])))
}
#Calculates brightness index sqrt(G^2+R^2+NIR^2) / 3
calc_BI <- function(df, Gmin, Gmax, Rmin, Rmax, Nmin, Nmax){
  G_1 <- which.min(abs(Gmin - as.numeric(names(df), options(warn=-1))))   #Identify first band and start range
  G_2 <- which.min(abs(Gmax - as.numeric(names(df), options(warn=-1))))   #Identify first band and end range
  R_1 <- which.min(abs(Rmin - as.numeric(names(df), options(warn=-1))))   #Identify second band and start range
  R_2 <- which.min(abs(Rmax - as.numeric(names(df), options(warn=-1))))   #Identify second band and end range
  N_1 <- which.min(abs(Nmin - as.numeric(names(df), options(warn=-1))))   #Identify second band and start range
  N_2 <- which.min(abs(Nmax - as.numeric(names(df), options(warn=-1))))   #Identify second band and end range
  sqrt(rowMeans(df_wide[G_1:G_2])^2+rowMeans(df_wide[R_1:R_2])^2+rowMeans(df_wide[N_1:N_2])^2) / 3
}
#Calculates normalized index using (a:b - c:d) / (a:b + c:d)
calc_VI <- function(df, a, b, c, d){
  b1_1 <- which.min(abs(a - as.numeric(names(df), options(warn=-1))))   #Identify first band and start range
  b1_2 <- which.min(abs(b - as.numeric(names(df), options(warn=-1))))   #Identify first band and end range
  b2_1 <- which.min(abs(c - as.numeric(names(df), options(warn=-1))))   #Identify second band and start range
  b2_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify second band and end range
  ((rowMeans(df_wide[b1_1:b1_2]) - rowMeans(df_wide[b2_1:b2_2])) /      #vegetation index equation
      (rowMeans(df_wide[b1_1:b1_2]) + rowMeans(df_wide[b2_1:b2_2])))
}

#####################################
master <- read.csv2(paste(github_dir,'/data/ASD/ASD_Contact_Datasheet_021422_021522.csv',sep=''),sep=',',header=T)
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
  full=paste(plot,'_',treat,'_',type,'_',rep,sep='')
  scan=rep(sub(".*/", "", file),length(wvl))
  
  ds=cbind(wvl,data_c,plot,tmp,wtr,treat,quad,type,rep,full,scan)
  dataset <- rbind(dataset,ds)
}
#Name the columns
colnames(dataset) <- c("wavelength","reflectance","plot","tmp","wtr","treat","quad","type","rep","full","scan")
dataset=transform(dataset,wavelength = as.numeric(wavelength))
dataset=transform(dataset,reflectance = as.numeric(reflectance))

#Reduce by taking the mean across reps
dataset_mean <- dataset %>%
  group_by(wavelength,full) %>%
  summarise_at(vars(reflectance), list(mean=mean)) %>%
  as.data.frame()

#Convert to wide format for export
dataset_mean_wide <- dataset_mean %>% select(wavelength, mean, full) %>%
  pivot_wider(names_from = wavelength, values_from = mean)

#write csv file# NEED TO FIX
write.csv(dataset_mean_wide,paste(github_dir,'/data/Level1/ASD_All_Spectra_ContactProbe.csv',sep=''),row.names=FALSE,col.names=FALSE)


#####ASD Reflectance Plots#################
master<-subset(master, Biocrust %in% c('DCY','LCY','LCN','MSS'))
###Isolate soil signature
soil<-subset(dataset, type %in% c('LCY','SOIL'))
soil_mean_std <- soil %>%
  group_by(wavelength,treat,type) %>%
  summarise_at(vars(reflectance), list(mean=mean, sd=sd)) %>%
  as.data.frame()
###Isolate biocrust signatures
dataset<-subset(dataset, type %in% c('DCY','LCY','LCN','MSS'))
dataset_mean_std <- dataset %>%
 group_by(wavelength,treat,type) %>%
 summarise_at(vars(reflectance), list(mean=mean, sd=sd)) %>%
 as.data.frame()

ggplot(subset(soil_mean_std, treat %in% c('CC','CW','LW','LC')), aes(x=wavelength,y=mean,group=type,color=type)) +
  geom_line(show.legend = T,linewidth=.5,linetype="solid") +
  scale_color_manual(values=c('red3','green3','cyan2','purple'))+
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd, fill = type), alpha = .2) +
  scale_fill_manual(values=c('red3','green3','cyan2','purple'))+
  facet_wrap(~treat)+
  scale_y_continuous("Reflectance") +
  scale_x_continuous("Wavelength (nm)",limits = c(400,2400), breaks = seq(400,2400,200)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/Line_FieldSpec_Hyperspectra_Full_Soil_FuncLevel.png',sep=''),dpi=300,width=180,height=120,units='mm')

trt_names=c('CC' = 'Control','CW' = 'AltP','LC' = 'Warmed','LW' = 'AltP + Warmed')
dataset_mean_std$type <- factor(dataset_mean_std$type,levels=c("LCY","DCY","LCN","MSS"))
ggplot(subset(dataset_mean_std, treat %in% c('CC','CW','LW','LC')), aes(x=wavelength,y=mean,group=type,color=type)) +
  geom_line(show.legend = T,linewidth=.5,linetype="solid") +
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd, fill = type), alpha = .2) +
  facet_wrap(~treat, labeller = as_labeller(trt_names), ncol=2)+
  scale_y_continuous("Reflectance") +
  scale_x_continuous("Wavelength (nm)",limits = c(400,2400), breaks = seq(400,2400,200)) +
  scale_color_manual(name='Func Type', values=c('red3','green3','cyan2','purple'),labels=c("LtCy","DkCy","Lichen","Moss"))+
  scale_fill_manual(name='Func Type', values=c('red3','green3','cyan2','purple'),labels=c("LtCy","DkCy","Lichen","Moss"))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/Line_FieldSpec_Hyperspectra_Full_ByFunc_FuncLevel.png',sep=''),dpi=300,width=240,height=120,units='mm')

ggplot(subset(dataset_mean_std, treat %in% c('CC','CW','LW','LC')), aes(x=wavelength,y=mean,group=treat,color=treat)) +
  geom_line(show.legend = T,linewidth=.5,linetype="solid") +
  scale_color_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  #geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd, fill = type), alpha = .2) +
  #scale_fill_manual(values=c("#3288BD","#99D594","#9E0142","#5E4FA2"))+
  facet_wrap(~type)+
  scale_y_continuous("Reflectance") +
  scale_x_continuous("Wavelength (nm)",limits = c(400,2400), breaks = seq(400,2400,200)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/Line_FieldSpec_Hyperspectra_Full_ByTreat_FuncLevel.png',sep=''),dpi=300,width=180,height=120,units='mm')


datasetB2<-subset(dataset, plot %in% c('B2'))
datasetB2_mean_std <- datasetB2 %>%
  group_by(wavelength,treat,type) %>%
  summarise_at(vars(reflectance), list(mean=mean, sd=sd)) %>%
  as.data.frame()

ggplot(subset(datasetB2_mean_std, treat %in% c('CC','LW')), aes(x=wavelength,y=mean,group=type,color=type)) +
  geom_line(show.legend = T,linewidth=.5,linetype="solid") +
  scale_color_manual(values=c('red3','green3','cyan2','purple'))+
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd, fill = type), alpha = .2) +
  scale_fill_manual(values=c('red3','green3','cyan2','purple'))+
  facet_wrap(~treat)+
  scale_y_continuous("Reflectance") +
  scale_x_continuous("Wavelength (nm)",limits = c(450,900), breaks = seq(450,900,50)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/Line_FieldSpec_Hyperspectra_B2_VNIR_ByFunc_FuncLevel.png',sep=''),dpi=300,width=180,height=120,units='mm')

datasetB3<-subset(dataset, plot %in% c('B3'))
datasetB3_mean_std <- datasetB3 %>%
  group_by(wavelength,treat,type) %>%
  summarise_at(vars(reflectance), list(mean=mean, sd=sd)) %>%
  as.data.frame()

ggplot(subset(datasetB3_mean_std, treat %in% c('CC','LW','LC','CW')), aes(x=wavelength,y=mean,group=type,color=type)) +
  geom_line(show.legend = T,linewidth=.5,linetype="solid") +
  scale_color_manual(values=c('red3','green3','cyan2','purple'))+
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd, fill = type), alpha = .2) +
  scale_fill_manual(values=c('red3','green3','cyan2','purple'))+
  facet_wrap(~treat)+
  scale_y_continuous("Reflectance") +
  scale_x_continuous("Wavelength (nm)",limits = c(450,900), breaks = seq(450,900,50)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/Line_FieldSpec_Hyperspectra_B3_VNIR_ByFunc_FuncLevel.png',sep=''),dpi=300,width=180,height=120,units='mm')

####################################################################################################
#VIs - Convert to wide format
df_wide <- dataset %>% select(wavelength, reflectance, full) %>%
  pivot_wider(names_from = wavelength, values_from = reflectance, id_cols = full, values_fn = mean)

#Chlorophyll
df_wide$NDVI <- calc_VI(df_wide, 850, 850, 650, 650)
df_wide$BI <- calc_BI(df_wide, 560, 560, 650, 650, 850, 850)
df_wide$NDWI <- calc_VI(df_wide, 1850, 1850, 1925, 1925)
df_wide$CI1 <- calc_VI(df_wide, 750, 750, 550, 550)
df_wide$CI2 <- calc_VI(df_wide, 750, 750, 710, 710)

#write csv file
out<-cbind(df_wide$full,master$Plot,master$Treat,master$Biocrust,master$Rep,df_wide$NDVI,df_wide$BI,df_wide$NDWI,df_wide$CI1,df_wide$CI2)
colnames(out)<-c('ID','Plot','Treat','Type','Rep','NDVI','BI','NDWI','CI1','CI2')
write.csv(out,paste(github_dir,'/data/Level1/ASD_Indices_Full.csv',sep=''),row.names=FALSE,col.names=TRUE)