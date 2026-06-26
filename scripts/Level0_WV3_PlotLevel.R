rm(list = ls()) # clear the environment
setwd("/Users/wksmith/Documents/GitHub/CastleValley_Campaign_Biocrust_Analysis")
github_dir <- "/Users/wksmith/Documents/GitHub/CastleValley_Campaign_Biocrust_Analysis"
library(tidyverse)
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

#############ASD########################
#open data files
wv3 <- read.csv2(paste(github_dir,'/data/WorldView3/',"WorldView3_Bands_Plot.csv",sep=''),sep=',',header=T)
#Indices
wv3$Val<-as.numeric(wv3$Val)

####################################################################################################
#VIs - Convert to wide format
df_wide <- wv3 %>% select(Val, mean, full) %>%
  pivot_wider(names_from = wavelength, values_from = mean, id_cols = full, values_fn = mean)

#Bands & Chlorophyll
df_wide$CBLUE <- calc_Band(df_wide,400,450)
df_wide$BLUE <- calc_Band(df_wide,450,510)
df_wide$GRN <- calc_Band(df_wide,510,580)
df_wide$YLW <- calc_Band(df_wide,584,624)
df_wide$RED <- calc_Band(df_wide,630,690)
df_wide$REDE <- calc_Band(df_wide,705,745)
df_wide$NIR <- calc_Band(df_wide,770,895)
df_wide$NDVI <- calc_VI(df_wide, 850, 850, 650, 650)
df_wide$BI <- calc_BI(df_wide, 560, 560, 650, 650, 850, 850)
df_wide$NDWI <- calc_VI(df_wide, 1850, 1850, 1925, 1925)
df_wide$CI1 <- calc_VI(df_wide, 750, 750, 550, 550)
df_wide$CI2 <- calc_VI(df_wide, 750, 750, 710, 710)

#write csv file# NEED TO FIX
out<-cbind(df_wide$full,df_wide$CBLUE,df_wide$BLUE,df_wide$GRN,df_wide$YLW,df_wide$RED,df_wide$REDE,df_wide$NIR,df_wide$NDVI,df_wide$BI,df_wide$NDWI,df_wide$CI1,df_wide$CI2)
colnames(out)<-c('ID','CBLUE','BLUE','GREEN','YELLOW','RED','REDEDGE','NIR','NDVI','BI','NDWI','CI1','CI2')
write.csv(out,paste(github_dir,'/data/Level1/ASD_Bands_Indices_Plot.csv',sep=''),row.names=FALSE,col.names=TRUE)
