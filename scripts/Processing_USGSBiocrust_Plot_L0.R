setwd("/Users/wksmith/Documents/GitHub/Biocrust-USGS")
github_dir <- "/Users/wksmith/Documents/GitHub/Biocrust-USGS"
library(asdreader)
library(reshape2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)

###FUNCTIONS##################################################################################
#Calculates sum over band range
calc_Band <- function(df, a, b){
  b1 <- which.min(abs(a - as.numeric(names(df), options(warn=-1))))   #Identify first band and start range
  b2 <- which.min(abs(b - as.numeric(names(df), options(warn=-1))))   #Identify first band and end range
  (rowMeans(df_wide[b1:b2]))
}
#Calculates simple ratio (a:b) / (c:d)
calc_SR <- function(df, a, b, c, d){
  b1_1 <- which.min(abs(a - as.numeric(names(df), options(warn=-1))))   #Identify first band and start range
  b1_2 <- which.min(abs(b - as.numeric(names(df), options(warn=-1))))   #Identify first band and end range
  b2_1 <- which.min(abs(c - as.numeric(names(df), options(warn=-1))))   #Identify second band and start range
  b2_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify second band and end range
  (rowMeans(df_wide[b1_1:b1_2])/(rowMeans(df_wide[b2_1:b2_2])))
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

#############ASD########################
master <- read.csv2(paste(github_dir,'/data/ASD/ASD_Plot_Datasheet_021422_021522.csv',sep=''),sep=',',header=T)
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
  loc=rep(master$Loc[i],length(wvl))
  rep=rep(master$Rep[i],length(wvl))
  full=paste(plot,'_',treat,'_',loc,'_',rep,sep='')
  scan=rep(sub(".*/", "", file),length(wvl))
  
  ds=cbind(wvl,data_c,plot,tmp,wtr,treat,loc,rep,full,scan)
  dataset <- rbind(dataset,ds)
}

#Name the columns
colnames(dataset) <- c("wavelength","reflectance","plot","tmp","wtr","treat","location","rep","full","scan")
dataset=transform(dataset,wavelength = as.numeric(wavelength))
dataset=transform(dataset,reflectance = as.numeric(reflectance))

#####ASD Reflectance Plots#################
dataset_mean_std <- dataset %>%
 group_by(wavelength,plot,treat) %>%
 summarise_at(vars(reflectance), list(mean=mean, sd=sd)) %>%
 as.data.frame()

ggplot(dataset_mean_std, aes(x=wavelength,y=mean,group=treat,color=treat)) +
  geom_line(show.legend = T,linewidth=.5,linetype="solid") +
  scale_color_manual(values=c('cyan2','green3','red3','purple'))+
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd, fill = treat), alpha = .2) +
  scale_fill_manual(values=c('cyan2','green3','red3','purple'))+
  facet_wrap(~plot)+
  scale_y_continuous("Reflectance") +
  scale_x_continuous("Wavelength (nm)",limits = c(400,2400), breaks = seq(400,2400,200)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/Hyperspectral_Reflectance_FULL_ASD_PLOT.png',sep=''),dpi=300,width=180,height=120,units='mm')

ggplot(dataset_mean_std, aes(x=wavelength,y=mean,group=treat,color=treat)) +
  geom_line(show.legend = T,linewidth=.5,linetype="solid") +
  scale_color_manual(values=c('cyan2','green3','red3','purple'))+
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd, fill = treat), alpha = .2) +
  scale_fill_manual(values=c('cyan2','green3','red3','purple'))+
  facet_wrap(~plot)+
  scale_y_continuous("Reflectance") +
  scale_x_continuous("Wavelength (nm)",limits = c(400,900), breaks = seq(400,2400,200)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/Hyperspectral_Reflectance_PLOT_VNIR_ASD_PLOT.png',sep=''),dpi=300,width=180,height=120,units='mm')

####################################################################################################
#VIs - Convert to wide format
df_wide <- dataset %>% select(wavelength, reflectance, full) %>%
  pivot_wider(names_from = wavelength, values_from = reflectance, id_cols = full, values_fn = mean)

#Bands & Chlorophyll
df_wide$CBLUE <- calc_Band(df_wide,400,450)
df_wide$BLUE <- calc_Band(df_wide,450,510)
df_wide$GRN <- calc_Band(df_wide,510,580)
df_wide$YLW <- calc_Band(df_wide,584,624)
df_wide$RED <- calc_Band(df_wide,630,690)
df_wide$REDE <- calc_Band(df_wide,705,745)
df_wide$NIR <- calc_Band(df_wide,770,895)
df_wide$NDVI <- calc_VI(df_wide, 850, 850, 650, 650)
df_wide$CI1 <- calc_VI(df_wide, 750, 750, 550, 550)
df_wide$CI2 <- calc_VI(df_wide, 750, 750, 710, 710)

#write csv file
out<-cbind(df_wide$full,master$Plot,master$Treat,df_wide$CBLUE,df_wide$BLUE,df_wide$GRN,df_wide$YLW,df_wide$RED,df_wide$REDE,df_wide$NIR,df_wide$NDVI,df_wide$CI1,df_wide$CI2)
colnames(out)<-c('ID','Plot','Treat','CBLUE','BLUE','GREEN','YELLOW','RED','REDEDGE','NIR','NDVI','CI1','CI2')
write.csv(out,paste(github_dir,'/data/Level1/ASD_Bands_Indices_Plot.csv',sep=''),row.names=FALSE,col.names=TRUE)



#######UAS RESONON##############################
master <- read.csv2(paste(github_dir,'/data/UAS/Entire_Plot/Resonon_Datasheet_EntirePlot.csv',sep=''),sep=',',header=T)
dataset <- data.frame()
for(i in 1:length(master$File)){
  temp_data<-read.table(paste(github_dir,'/data/UAS/',master$Dir[i],'/',master$File[i],sep=''),skip=7)
  data_c<-as.numeric(temp_data[,4])
  sd_c<-as.numeric(temp_data[,4])-as.numeric(temp_data[,3])
  wvl=as.numeric(temp_data[,1])
  data_s=data_c/sqrt(sum(data_c^2))
  plot=rep(master$Plot[i],length(wvl))
  tmp=rep(master$Tmp[i],length(wvl))
  wtr=rep(master$Wtr[i],length(wvl))
  treat=rep(master$Treat[i],length(wvl))
  full=paste(plot,'_',treat,sep='')
  scan=rep(master$File[i],length(wvl))
  
  ds=cbind(wvl,data_c,sd_c,plot,tmp,wtr,treat,full,scan)
  dataset <- rbind(dataset,ds)
}
#Name the columns
names(dataset) <- c("wavelength","reflectance",'stdv',"plot","tmp","wtr","treat","full","scan")
dataset=transform(dataset,wavelength = as.numeric(wavelength))
dataset=transform(dataset,reflectance = as.numeric(reflectance))
dataset=transform(dataset,stdv = as.numeric(stdv))

#####Headwall Reflectance Plots#################
ggplot(dataset, aes(x=wavelength,y=reflectance,group=treat,color=treat)) +
  geom_line(show.legend = T,linewidth=.5,linetype="solid") +
  scale_color_manual(values=c('cyan2','green3','red3','purple'))+
  geom_ribbon(aes(y = reflectance, ymin = reflectance - stdv, ymax = reflectance + stdv, fill = treat), alpha = .2) +
  scale_fill_manual(values=c('cyan2','green3','red3','purple'))+
  facet_wrap(~plot)+
  scale_y_continuous("Reflectance") +
  scale_x_continuous("Wavelength (nm)",limits = c(400,900), breaks = seq(400,2400,200)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 18))
ggsave(paste(github_dir,'/figures/Hyperspectral_Reflectance_PLOT_VNIR_RESONON_PLOT.png',sep=''),dpi=300,width=180,height=120,units='mm')

####################################################################################################
#VIs - Convert to wide format
df_wide <- dataset %>% select(wavelength, reflectance, full) %>%
  pivot_wider(names_from = wavelength, values_from = reflectance, id_cols = full, values_fn = mean)

#Bands & Chlorophyll
df_wide$CBLUE <- calc_Band(df_wide,399.47,450.64)
df_wide$BLUE <- calc_Band(df_wide,450.64,510.67)
df_wide$GRN <- calc_Band(df_wide,510.67,579.85)
df_wide$YLW <- calc_Band(df_wide,584.07,624.34)
df_wide$RED <- calc_Band(df_wide,630.73,690.72)
df_wide$REDE <- calc_Band(df_wide,705.82,744.84)
df_wide$NIR <- calc_Band(df_wide,771,894.71)
df_wide$NDVI <- calc_VI(df_wide, 850.23, 850.23, 649.94, 649.94)
df_wide$CI1 <- calc_VI(df_wide, 749.19, 749.19, 550.39, 550.39)
df_wide$CI2 <- calc_VI(df_wide, 749.19, 749.19, 710.14, 710.14)

#write csv file
out<-cbind(df_wide$full,master$Plot,master$Treat,df_wide$CBLUE,df_wide$BLUE,df_wide$GRN,df_wide$YLW,df_wide$RED,df_wide$REDE,df_wide$NIR,df_wide$NDVI,df_wide$CI1,df_wide$CI2)
colnames(out)<-c('ID','Plot','Treat','CBLUE','BLUE','GREEN','YELLOW','RED','REDEDGE','NIR','NDVI','CI1','CI2')
write.csv(out,paste(github_dir,'/data/Level1/RESONON_Bands_Indices_Plot.csv',sep=''),row.names=FALSE,col.names=TRUE)

