rm(list = ls()) # clear the environment
setwd("/Users/wksmith/Documents/GitHub/CastleValley_Campaign_Biocrust_Analysis")
github_dir <- "/Users/wksmith/Documents/GitHub/CastleValley_Campaign_Biocrust_Analysis"
library(asdreader)
library(reshape2)
library(tidyverse)
library(stringr)
#library(tidyr)
#library(dplyr)

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
#Calculates brightness index sqrt(G^2+R^2+NIR^2) / 3
calc_BI <- function(df, Gmin, Gmax, Rmin, Rmax, Nmin, Nmax){
  G_1 <- which.min(abs(Gmin - as.numeric(names(df), options(warn=-1))))   #Identify first band and start range
  G_2 <- which.min(abs(Gmax - as.numeric(names(df), options(warn=-1))))   #Identify first band and end range
  R_1 <- which.min(abs(Rmin - as.numeric(names(df), options(warn=-1))))   #Identify second band and start range
  R_2 <- which.min(abs(Rmax - as.numeric(names(df), options(warn=-1))))   #Identify second band and end range
  N_1 <- which.min(abs(Nmin - as.numeric(names(df), options(warn=-1))))   #Identify second band and start range
  N_2 <- which.min(abs(Nmax - as.numeric(names(df), options(warn=-1))))   #Identify second band and end range
  sqrt(rowMeans(df[G_1:G_2])^2+rowMeans(df[R_1:R_2])^2+rowMeans(df[N_1:N_2])^2) / 3
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
  full=paste(plot,'_',treat,'_',loc,sep='')
  scan=rep(sub(".*/", "", file),length(wvl))
  
  ds=cbind(wvl,data_c,plot,tmp,wtr,treat,loc,rep,full,scan)
  dataset <- rbind(dataset,ds)
}

#Name the columns
colnames(dataset) <- c("wavelength","reflectance","plot","tmp","wtr","treat","location","rep","full","scan")
dataset=transform(dataset,wavelength = as.numeric(wavelength))
dataset=transform(dataset,reflectance = as.numeric(reflectance))

#Reduce by taking the mean across reps
dataset_mean <- dataset %>%
  group_by(wavelength, full, location, plot, treat) %>%
  summarise_at(vars(reflectance), list(mean=mean)) %>%
  as.data.frame()

#Convert to wide format for export
dataset_mean_wide <- dataset_mean %>% select(wavelength, mean, full, location, plot, treat) %>%
  pivot_wider(names_from = wavelength, values_from = mean)

#Calculate BI
dataset_mean_wide$BI <- calc_BI(dataset_mean_wide, 560, 560, 650, 650, 850, 850)

#Subset dataset
BI_bySpectra <- dataset_mean_wide[,c(1,2,3,4,2156)]
BI_bySpectra <- BI_bySpectra[order(BI_bySpectra$location), ]
BI_bySpectra <- BI_bySpectra[order(BI_bySpectra$plot), ]
BI_bySpectra <- BI_bySpectra[order(BI_bySpectra$treat), ]
BI_bySpectra$location <- paste0('S',BI_bySpectra$location)
BI_bySpectra$KEY <-paste0(BI_bySpectra$plot,BI_bySpectra$treat,BI_bySpectra$location)

#write csv file#
write.csv(BI_bySpectra,paste(github_dir,'/data/Level1/ASD_Brightness_BySpectra_2021.csv',sep=''),row.names=FALSE,col.names=FALSE)

####Plot Check###
plots=c("B1","B2","B3","B4","B5")
treats=c("CC","LC","CW","LW")
c_order=c("S1","S2","S3","S4","S5","S6","S7","S8","S9","S10","S11","S12","S13","S14","S15")
for(i in 1:length(treats)){
  for(j in 1:length(plots)){
    BIsub = subset(BI_bySpectra, plot %in% plots[j] & treat %in% treats[i])
    BIsub$location = factor(BIsub$location, levels = c_order, ordered = TRUE)
    BIsub<-BIsub[order(BIsub$location), ]
    ptitle = paste0(BIsub$plot[1],BIsub$treat[1],"_BI")
    bi_matrix <- matrix(BIsub$BI[1:12], nrow = 3, ncol = 4)
    df_long <- melt(bi_matrix)
    df_long=transform(df_long,value = as.numeric(value))
    ggplot(data = df_long, aes(x = Var1, y = Var2[12:1], fill = value)) +
      geom_tile() +
      labs(title = ptitle,
           x = "Columns",
           y = "Rows",
           fill = "Value") +
      scale_fill_gradient(limits = c(.01, .19),low = "white", high = "orange")
    ggsave(paste(github_dir,'/cover_figures/BI_',ptitle,'.png',sep=''),dpi=300,width=125,height=125,units='mm')
  }
}


###############Comparison with fractional cover data##########
FracCover<-read.csv2(paste0(github_dir,'/data/Level1/FractionaCover_BySpectra_2021.csv'),sep=',',header=T)
FracCover$Treatment <- str_replace_all(string = FracCover$Treatment, pattern = "C", replacement = "CC")
FracCover$Treatment <- str_replace_all(string = FracCover$Treatment, pattern = "L", replacement = "LC")
FracCover$Treatment <- str_replace_all(string = FracCover$Treatment, pattern = "W", replacement = "CW")
FracCover$Treatment <- str_replace_all(string = FracCover$Treatment, pattern = "LCCW", replacement = "LW")
FracCover$KEY <- paste0(FracCover$Plot,FracCover$Block,FracCover$Treatment,FracCover$Spectra)

C_data <- merge(BI_bySpectra,FracCover[,c(12,13)],by = "KEY")
C_data=transform(C_data,Light = as.numeric(Light))
C_data=transform(C_data,BI = as.numeric(BI))

C_data$location = factor(C_data$location, levels = c_order, ordered = TRUE)
ggplot(C_data, aes(x=Light, y=BI)) +
  geom_point(aes(fill=treat),color = "black",shape = 21)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=0.7,label.y=.025,size=8)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Brightness Index',limits = c(0,.2), breaks = seq(0,.2,.05))+
  scale_x_continuous('Light Cyano',limits = c(0,1), breaks = seq(0,1,.1)) +
  theme_bw()+
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_BI_LCY_BySpectra.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(C_data, aes(x=Light, y=BI)) +
  geom_point(aes(fill=treat),color = "black",shape = 21)+
  geom_smooth(method=lm,color='black',fill='grey')+
  stat_cor(aes(label = after_stat(rr.label)),label.x=0.5,label.y=.025,size=4)+
  scale_fill_manual(name='Treatment',values=c("#3288BD","#99D594","#9E0142","#5E4FA2"),labels=c("Control","AltP","Warmed","AltP + Warmed"))+
  scale_y_continuous('Brightness Index',limits = c(0,.2), breaks = seq(0,.2,.05))+
  scale_x_continuous('Light Cyano',limits = c(0,1), breaks = seq(0,1,.1)) +
  facet_wrap(~location,nrow = 4,ncol = 3) +
  theme_bw()+
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_BI_LCY_BySpectra_Multiplot.png",sep=""),dpi=300,width=150,height=250,units='mm')
