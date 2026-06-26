rm(list = ls()) # clear the environment
setwd("/Users/wksmith/Documents/GitHub/CastleValley_Campaign_Biocrust_Analysis")
github_dir <- "/Users/wksmith/Documents/GitHub/CastleValley_Campaign_Biocrust_Analysis"
data_dir='/Users/wksmith/Data/USGS_Biocrust_S22/'

library(tidyverse)
library(raster)
library(sf)
library(dplyr)

###Plots########################################################################
PlotLevel_Box<-function(D1,D2,D3,D4,clrs,filename) {
  png(file=filename,width=4500,height=4500,res=600)
  par(oma=c(1,8,1,1))
  
  ###STATS###
  #plot 1
  mins<-c(min(D1,na.rm=T),min(D2,na.rm=T),min(D3,na.rm=T),min(D4,na.rm=T))
  maxs<-c(max(D1,na.rm=T),max(D2,na.rm=T),max(D3,na.rm=T),max(D4,na.rm=T))
  means<-c(mean(D1,na.rm=T),mean(D2,na.rm=T),mean(D3,na.rm=T),mean(D4,na.rm=T))
  stdvs<-c(sd(D1,na.rm=T),sd(D2,na.rm=T),sd(D3,na.rm=T),sd(D4,na.rm=T))
  
  #####Plot 1
  par(mar=c(2,0,1,0))
  boxplot(D1,D2,D3,D4,col=clrs,ylim = c(0,1),varwidth=F,outline=FALSE,axes=FALSE,xaxs="i",yaxs="i")
  points(x=c(1,2,3,4),y=means,type="p",cex=2.5,pch=7,lwd=3,col='black')
  text(x=c(1,2,3,4),y=c(means[1]+.15,means[2]+.15,means[3]+.15,means[4]+.15),paste(formatC(means,format='f',digits=2)),adj=0.5,font=2,cex=1.25,col='black')
  text(x=c(1,2,3,4),y=c(means[1]+.1,means[2]+.1,means[3]+.1,means[4]+.1),paste("+/-",formatC(stdvs,format='f',digits=2)),adj=0.5,font=2,cex=1.25,col='black')
  
  #EXTRA
  #mtext(2, text=expression(bold(paste('Aridity Index (mm mm'^{-1},')'))),line=5,cex=3,font=2.2,col='black')
  #legend(1.1,.825,fill=clrs,c('Global','Western US'),bty="n",cex=2.8,text.font=2)
  axis(2, at = seq(0,1,.1), cex.axis=2.8, font.axis=2, tck=0.015, las=2)
  axis(3, at = c(0,5), tck=0.015, labels=FALSE)
  axis(1, at = c(0,5), tck=0.015, labels=FALSE)
  axis(4, at = seq(0,1,.1), tck=0.015, labels=FALSE)
  
  dev.off()
  
  return(means)
}

PlotLevel_GGBox<-function(D1,D2,D3,D4,clrs,filename) {
  ###STATS###
  #plot 1
  mins<-c(min(D1,na.rm=T),min(D2,na.rm=T),min(D3,na.rm=T),min(D4,na.rm=T))
  maxs<-c(max(D1,na.rm=T),max(D2,na.rm=T),max(D3,na.rm=T),max(D4,na.rm=T))
  means<-c(mean(D1,na.rm=T),mean(D2,na.rm=T),mean(D3,na.rm=T),mean(D4,na.rm=T))
  stdvs<-c(sd(D1,na.rm=T),sd(D2,na.rm=T),sd(D3,na.rm=T),sd(D4,na.rm=T))
  
  D1names<-rep('Control',each=length(D1))
  D2names<-rep('AltP',each=length(D2))
  D3names<-rep('Warmed',each=length(D3))
  D4names<-rep('AltP + Warmed',each=length(D4))
  df<-data.frame(c(D1names,D2names,D3names,D4names), c(D1,D2,D3,D4))
  colnames(df)<-c('Treat','Val')
  
  df$Treat <- factor(df$Treat,levels=c("Control","AltP","Warmed","AltP + Warmed"))
  ggplot(df, aes(x=Treat, y=Val, fill=Treat)) + 
    geom_boxplot(outlier.shape = NA,coef=0,notch=FALSE) +
    scale_fill_manual(name='Treatment', values=clrs,labels=c("Control","AltP","Warmed","AltP + Warmed"))+
    stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
    labs(y='Normalized Temperature Index',x='')+
    scale_y_continuous(limits=c(.1,.3))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    theme(text = element_text(size = 14))
  ggsave(filename,dpi=300,width=250,height=200,units='mm')
  
  return(means)
}

################################################################################
##############################################Chlorophyll#######################
#Open and process data
Multispec <- raster(paste0(data_dir,'/MicaSense_Dual_Tarp/moab_micasense_ortho_utm83_ndvi_clipped.tif'))
B1_C <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B1_Control.shp'))
B2_C <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B2_Control.shp'))
B3_C <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B3_Control.shp'))
B4_C <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B4_Control.shp'))
B5_C <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B5_Control.shp'))

B1_A <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B1_AlteredP.shp'))
B2_A <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B2_AlteredP.shp'))
B3_A <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B3_AlteredP.shp'))
B4_A <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B4_AlteredP.shp'))
B5_A <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B5_AlteredP.shp'))

B1_WA <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B1_WarmAltP.shp'))
B2_WA <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B2_WarmAltP.shp'))
B3_WA <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B3_WarmAltP.shp'))
B4_WA <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B4_WarmAltP.shp'))
B5_WA <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B5_WarmAltP.shp'))

B1_W <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B1_Warmed.shp'))
B2_W <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B2_Warmed.shp'))
B3_W <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B3_Warmed.shp'))
B4_W <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B4_Warmed.shp'))
B5_W <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B5_Warmed.shp'))

Multispec.B1_C <- crop(raster::mask(Multispec, B1_C),B1_C)
Multispec.B2_C <- crop(raster::mask(Multispec, B2_C),B2_C)
Multispec.B3_C <- crop(raster::mask(Multispec, B3_C),B3_C)
Multispec.B4_C <- crop(raster::mask(Multispec, B4_C),B4_C)
Multispec.B5_C <- crop(raster::mask(Multispec, B5_C),B5_C)

Multispec.B1_A <- crop(raster::mask(Multispec, B1_A),B1_A)
Multispec.B2_A <- crop(raster::mask(Multispec, B2_A),B2_A)
Multispec.B3_A <- crop(raster::mask(Multispec, B3_A),B3_A)
Multispec.B4_A <- crop(raster::mask(Multispec, B4_A),B4_A)
Multispec.B5_A <- crop(raster::mask(Multispec, B5_A),B5_A)

Multispec.B1_WA <- crop(raster::mask(Multispec, B1_WA),B1_WA)
Multispec.B2_WA <- crop(raster::mask(Multispec, B2_WA),B2_WA)
Multispec.B3_WA <- crop(raster::mask(Multispec, B3_WA),B3_WA)
Multispec.B4_WA <- crop(raster::mask(Multispec, B4_WA),B4_WA)
Multispec.B5_WA <- crop(raster::mask(Multispec, B5_WA),B5_WA)

Multispec.B1_W <- crop(raster::mask(Multispec, B1_W),B1_W)
Multispec.B2_W <- crop(raster::mask(Multispec, B2_W),B2_W)
Multispec.B3_W <- crop(raster::mask(Multispec, B3_W),B3_W)
Multispec.B4_W <- crop(raster::mask(Multispec, B4_W),B4_W)
Multispec.B5_W <- crop(raster::mask(Multispec, B5_W),B5_W)

# #Check data
# plot(Multispec.B1_C,col = heat.colors(50))
# plot(Multispec.B2_C,col = heat.colors(50))
# plot(Multispec.B3_C,col = heat.colors(50))
# plot(Multispec.B4_C,col = heat.colors(50))
# plot(Multispec.B5_C,col = heat.colors(50))
# 
# plot(Multispec.B1_A,col = heat.colors(50))
# plot(Multispec.B2_A,col = heat.colors(50))
# plot(Multispec.B3_A,col = heat.colors(50))
# plot(Multispec.B4_A,col = heat.colors(50))
# plot(Multispec.B5_A,col = heat.colors(50))
# 
# plot(Multispec.B1_WA,col = heat.colors(50))
# plot(Multispec.B2_WA,col = heat.colors(50))
# plot(Multispec.B3_WA,col = heat.colors(50))
# plot(Multispec.B4_WA,col = heat.colors(50))
# plot(Multispec.B5_WA,col = heat.colors(50))
# 
# plot(Multispec.B1_W,col = heat.colors(50))
# plot(Multispec.B2_W,col = heat.colors(50))
# plot(Multispec.B3_W,col = heat.colors(50))
# plot(Multispec.B4_W,col = heat.colors(50))
# plot(Multispec.B5_W,col = heat.colors(50))

##########BOX PLOT##############################################################
#Plots
B1_means<-PlotLevel_Box(as.vector(as.matrix(Multispec.B1_C)),as.vector(as.matrix(Multispec.B1_A)),as.vector(as.matrix(Multispec.B1_W)),as.vector(as.matrix(Multispec.B1_WA)),c('white','cyan2','red3','magenta3'),paste0(github_dir,'figures/B1_Chlorophyll_Box.png'))
B2_means<-PlotLevel_Box(as.vector(as.matrix(Multispec.B2_C)),as.vector(as.matrix(Multispec.B2_A)),as.vector(as.matrix(Multispec.B2_W)),as.vector(as.matrix(Multispec.B2_WA)),c('white','cyan2','red3','magenta3'),paste0(github_dir,'figures/B2_Chlorophyll_Box.png'))
B3_means<-PlotLevel_Box(as.vector(as.matrix(Multispec.B3_C)),as.vector(as.matrix(Multispec.B3_A)),as.vector(as.matrix(Multispec.B3_W)),as.vector(as.matrix(Multispec.B3_WA)),c('white','cyan2','red3','magenta3'),paste0(github_dir,'figures/B3_Chlorophyll_Box.png'))
B4_means<-PlotLevel_Box(as.vector(as.matrix(Multispec.B4_C)),as.vector(as.matrix(Multispec.B4_A)),as.vector(as.matrix(Multispec.B4_W)),as.vector(as.matrix(Multispec.B4_WA)),c('white','cyan2','red3','magenta3'),paste0(github_dir,'figures/B4_Chlorophyll_Box.png'))
B5_means<-PlotLevel_Box(as.vector(as.matrix(Multispec.B5_C)),as.vector(as.matrix(Multispec.B5_A)),as.vector(as.matrix(Multispec.B5_W)),as.vector(as.matrix(Multispec.B5_WA)),c('white','cyan2','red3','magenta3'),paste0(github_dir,'figures/B5_Chlorophyll_Box.png'))
C<-c(as.vector(as.matrix(Multispec.B1_C)),as.vector(as.matrix(Multispec.B2_C)),as.vector(as.matrix(Multispec.B3_C)),as.vector(as.matrix(Multispec.B4_C)),as.vector(as.matrix(Multispec.B5_C)))
A<-c(as.vector(as.matrix(Multispec.B1_A)),as.vector(as.matrix(Multispec.B2_A)),as.vector(as.matrix(Multispec.B3_A)),as.vector(as.matrix(Multispec.B4_A)),as.vector(as.matrix(Multispec.B5_A)))
WA<-c(as.vector(as.matrix(Multispec.B1_WA)),as.vector(as.matrix(Multispec.B2_WA)),as.vector(as.matrix(Multispec.B3_WA)),as.vector(as.matrix(Multispec.B4_WA)),as.vector(as.matrix(Multispec.B5_WA)))
W<-c(as.vector(as.matrix(Multispec.B1_W)),as.vector(as.matrix(Multispec.B2_W)),as.vector(as.matrix(Multispec.B3_W)),as.vector(as.matrix(Multispec.B4_W)),as.vector(as.matrix(Multispec.B5_W)))
All_means<-PlotLevel_Box(C,A,W,WA,c(c('white','cyan2','red3','magenta3')),paste0(github_dir,'figures/All_BPlots_Chlorophyll_Box.png'))

B1_means<-PlotLevel_GGBox(as.vector(as.matrix(Multispec.B1_C)),as.vector(as.matrix(Multispec.B1_A)),as.vector(as.matrix(Multispec.B1_W)),as.vector(as.matrix(Multispec.B1_WA)),c('white','cyan2','red3','magenta3'),paste0(github_dir,'figures/B1_Chlorophyll_GGBox.png'))
B2_means<-PlotLevel_GGBox(as.vector(as.matrix(Multispec.B2_C)),as.vector(as.matrix(Multispec.B2_A)),as.vector(as.matrix(Multispec.B2_W)),as.vector(as.matrix(Multispec.B2_WA)),c('white','cyan2','red3','magenta3'),paste0(github_dir,'figures/B2_Chlorophyll_GGBox.png'))
B3_means<-PlotLevel_GGBox(as.vector(as.matrix(Multispec.B3_C)),as.vector(as.matrix(Multispec.B3_A)),as.vector(as.matrix(Multispec.B3_W)),as.vector(as.matrix(Multispec.B3_WA)),c('white','cyan2','red3','magenta3'),paste0(github_dir,'figures/B3_Chlorophyll_GGBox.png'))
B4_means<-PlotLevel_GGBox(as.vector(as.matrix(Multispec.B4_C)),as.vector(as.matrix(Multispec.B4_A)),as.vector(as.matrix(Multispec.B4_W)),as.vector(as.matrix(Multispec.B4_WA)),c('white','cyan2','red3','magenta3'),paste0(github_dir,'figures/B4_Chlorophyll_GGBox.png'))
B5_means<-PlotLevel_GGBox(as.vector(as.matrix(Multispec.B5_C)),as.vector(as.matrix(Multispec.B5_A)),as.vector(as.matrix(Multispec.B5_W)),as.vector(as.matrix(Multispec.B5_WA)),c('white','cyan2','red3','magenta3'),paste0(github_dir,'figures/B5_Chlorophyll_GGBox.png'))
C<-c(as.vector(as.matrix(Multispec.B1_C)),as.vector(as.matrix(Multispec.B2_C)),as.vector(as.matrix(Multispec.B3_C)),as.vector(as.matrix(Multispec.B4_C)),as.vector(as.matrix(Multispec.B5_C)))
A<-c(as.vector(as.matrix(Multispec.B1_A)),as.vector(as.matrix(Multispec.B2_A)),as.vector(as.matrix(Multispec.B3_A)),as.vector(as.matrix(Multispec.B4_A)),as.vector(as.matrix(Multispec.B5_A)))
WA<-c(as.vector(as.matrix(Multispec.B1_WA)),as.vector(as.matrix(Multispec.B2_WA)),as.vector(as.matrix(Multispec.B3_WA)),as.vector(as.matrix(Multispec.B4_WA)),as.vector(as.matrix(Multispec.B5_WA)))
W<-c(as.vector(as.matrix(Multispec.B1_W)),as.vector(as.matrix(Multispec.B2_W)),as.vector(as.matrix(Multispec.B3_W)),as.vector(as.matrix(Multispec.B4_W)),as.vector(as.matrix(Multispec.B5_W)))
All_means<-PlotLevel_GGBox(C,A,W,WA,c(c('white','cyan2','red3','magenta3')),paste0(github_dir,'figures/All_BPlots_Chlorophyll_GGBox.png'))

#write csv file
out<-cbind(rep(c("B1","B2","B3","B4","B5"),each=4),rep(c("CC","CW","LC","LW"),times=5),c(B1_means,B2_means,B3_means,B4_means,B5_means))
colnames(out)<-c('Plot','Treat','Value')
write.csv(out,paste0(github_dir,'data/level1/BPlots_Micasense_NDVI_Full.csv'),row.names=FALSE,col.names=TRUE)

################################################################################
##############################################Brightness########################
#Open and process data
Multispec <- raster(paste0(data_dir,'/MicaSense_Dual_Tarp/moab_micasense_ortho_utm83_bi_clipped.tif'))
B1_C <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B1_Control.shp'))
B2_C <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B2_Control.shp'))
B3_C <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B3_Control.shp'))
B4_C <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B4_Control.shp'))
B5_C <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B5_Control.shp'))

B1_A <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B1_AlteredP.shp'))
B2_A <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B2_AlteredP.shp'))
B3_A <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B3_AlteredP.shp'))
B4_A <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B4_AlteredP.shp'))
B5_A <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B5_AlteredP.shp'))

B1_WA <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B1_WarmAltP.shp'))
B2_WA <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B2_WarmAltP.shp'))
B3_WA <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B3_WarmAltP.shp'))
B4_WA <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B4_WarmAltP.shp'))
B5_WA <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B5_WarmAltP.shp'))

B1_W <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B1_Warmed.shp'))
B2_W <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B2_Warmed.shp'))
B3_W <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B3_Warmed.shp'))
B4_W <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B4_Warmed.shp'))
B5_W <- st_read(paste0('/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/BPlots_B5_Warmed.shp'))

Multispec.B1_C <- crop(raster::mask(Multispec, B1_C),B1_C)
Multispec.B2_C <- crop(raster::mask(Multispec, B2_C),B2_C)
Multispec.B3_C <- crop(raster::mask(Multispec, B3_C),B3_C)
Multispec.B4_C <- crop(raster::mask(Multispec, B4_C),B4_C)
Multispec.B5_C <- crop(raster::mask(Multispec, B5_C),B5_C)

Multispec.B1_A <- crop(raster::mask(Multispec, B1_A),B1_A)
Multispec.B2_A <- crop(raster::mask(Multispec, B2_A),B2_A)
Multispec.B3_A <- crop(raster::mask(Multispec, B3_A),B3_A)
Multispec.B4_A <- crop(raster::mask(Multispec, B4_A),B4_A)
Multispec.B5_A <- crop(raster::mask(Multispec, B5_A),B5_A)

Multispec.B1_WA <- crop(raster::mask(Multispec, B1_WA),B1_WA)
Multispec.B2_WA <- crop(raster::mask(Multispec, B2_WA),B2_WA)
Multispec.B3_WA <- crop(raster::mask(Multispec, B3_WA),B3_WA)
Multispec.B4_WA <- crop(raster::mask(Multispec, B4_WA),B4_WA)
Multispec.B5_WA <- crop(raster::mask(Multispec, B5_WA),B5_WA)

Multispec.B1_W <- crop(raster::mask(Multispec, B1_W),B1_W)
Multispec.B2_W <- crop(raster::mask(Multispec, B2_W),B2_W)
Multispec.B3_W <- crop(raster::mask(Multispec, B3_W),B3_W)
Multispec.B4_W <- crop(raster::mask(Multispec, B4_W),B4_W)
Multispec.B5_W <- crop(raster::mask(Multispec, B5_W),B5_W)

# #Check data
# plot(Multispec.B1_C,col = heat.colors(50))
# plot(Multispec.B2_C,col = heat.colors(50))
# plot(Multispec.B3_C,col = heat.colors(50))
# plot(Multispec.B4_C,col = heat.colors(50))
# plot(Multispec.B5_C,col = heat.colors(50))
# 
# plot(Multispec.B1_A,col = heat.colors(50))
# plot(Multispec.B2_A,col = heat.colors(50))
# plot(Multispec.B3_A,col = heat.colors(50))
# plot(Multispec.B4_A,col = heat.colors(50))
# plot(Multispec.B5_A,col = heat.colors(50))
# 
# plot(Multispec.B1_WA,col = heat.colors(50))
# plot(Multispec.B2_WA,col = heat.colors(50))
# plot(Multispec.B3_WA,col = heat.colors(50))
# plot(Multispec.B4_WA,col = heat.colors(50))
# plot(Multispec.B5_WA,col = heat.colors(50))
# 
# plot(Multispec.B1_W,col = heat.colors(50))
# plot(Multispec.B2_W,col = heat.colors(50))
# plot(Multispec.B3_W,col = heat.colors(50))
# plot(Multispec.B4_W,col = heat.colors(50))
# plot(Multispec.B5_W,col = heat.colors(50))

##########BOX PLOT##############################################################
#Plots
B1_means<-PlotLevel_Box(as.vector(as.matrix(Multispec.B1_C)),as.vector(as.matrix(Multispec.B1_A)),as.vector(as.matrix(Multispec.B1_W)),as.vector(as.matrix(Multispec.B1_WA)),c('white','cyan2','red3','magenta3'),paste0(github_dir,'figures/B1_Brightness_Box.png'))
B2_means<-PlotLevel_Box(as.vector(as.matrix(Multispec.B2_C)),as.vector(as.matrix(Multispec.B2_A)),as.vector(as.matrix(Multispec.B2_W)),as.vector(as.matrix(Multispec.B2_WA)),c('white','cyan2','red3','magenta3'),paste0(github_dir,'figures/B2_Brightness_Box.png'))
B3_means<-PlotLevel_Box(as.vector(as.matrix(Multispec.B3_C)),as.vector(as.matrix(Multispec.B3_A)),as.vector(as.matrix(Multispec.B3_W)),as.vector(as.matrix(Multispec.B3_WA)),c('white','cyan2','red3','magenta3'),paste0(github_dir,'figures/B3_Brightness_Box.png'))
B4_means<-PlotLevel_Box(as.vector(as.matrix(Multispec.B4_C)),as.vector(as.matrix(Multispec.B4_A)),as.vector(as.matrix(Multispec.B4_W)),as.vector(as.matrix(Multispec.B4_WA)),c('white','cyan2','red3','magenta3'),paste0(github_dir,'figures/B4_Brightness_Box.png'))
B5_means<-PlotLevel_Box(as.vector(as.matrix(Multispec.B5_C)),as.vector(as.matrix(Multispec.B5_A)),as.vector(as.matrix(Multispec.B5_W)),as.vector(as.matrix(Multispec.B5_WA)),c('white','cyan2','red3','magenta3'),paste0(github_dir,'figures/B5_Brightness_Box.png'))
C<-c(as.vector(as.matrix(Multispec.B1_C)),as.vector(as.matrix(Multispec.B2_C)),as.vector(as.matrix(Multispec.B3_C)),as.vector(as.matrix(Multispec.B4_C)),as.vector(as.matrix(Multispec.B5_C)))
A<-c(as.vector(as.matrix(Multispec.B1_A)),as.vector(as.matrix(Multispec.B2_A)),as.vector(as.matrix(Multispec.B3_A)),as.vector(as.matrix(Multispec.B4_A)),as.vector(as.matrix(Multispec.B5_A)))
WA<-c(as.vector(as.matrix(Multispec.B1_WA)),as.vector(as.matrix(Multispec.B2_WA)),as.vector(as.matrix(Multispec.B3_WA)),as.vector(as.matrix(Multispec.B4_WA)),as.vector(as.matrix(Multispec.B5_WA)))
W<-c(as.vector(as.matrix(Multispec.B1_W)),as.vector(as.matrix(Multispec.B2_W)),as.vector(as.matrix(Multispec.B3_W)),as.vector(as.matrix(Multispec.B4_W)),as.vector(as.matrix(Multispec.B5_W)))
All_means<-PlotLevel_Box(C,A,W,WA,c(c('white','cyan2','red3','magenta3')),paste0(github_dir,'figures/All_BPlots_Brightness_Box.png'))

B1_means<-PlotLevel_GGBox(as.vector(as.matrix(Multispec.B1_C)),as.vector(as.matrix(Multispec.B1_A)),as.vector(as.matrix(Multispec.B1_W)),as.vector(as.matrix(Multispec.B1_WA)),c('white','cyan2','red3','magenta3'),paste0(github_dir,'figures/B1_Brightness_GGBox.png'))
B2_means<-PlotLevel_GGBox(as.vector(as.matrix(Multispec.B2_C)),as.vector(as.matrix(Multispec.B2_A)),as.vector(as.matrix(Multispec.B2_W)),as.vector(as.matrix(Multispec.B2_WA)),c('white','cyan2','red3','magenta3'),paste0(github_dir,'figures/B2_Brightness_GGBox.png'))
B3_means<-PlotLevel_GGBox(as.vector(as.matrix(Multispec.B3_C)),as.vector(as.matrix(Multispec.B3_A)),as.vector(as.matrix(Multispec.B3_W)),as.vector(as.matrix(Multispec.B3_WA)),c('white','cyan2','red3','magenta3'),paste0(github_dir,'figures/B3_Brightness_GGBox.png'))
B4_means<-PlotLevel_GGBox(as.vector(as.matrix(Multispec.B4_C)),as.vector(as.matrix(Multispec.B4_A)),as.vector(as.matrix(Multispec.B4_W)),as.vector(as.matrix(Multispec.B4_WA)),c('white','cyan2','red3','magenta3'),paste0(github_dir,'figures/B4_Brightness_GGBox.png'))
B5_means<-PlotLevel_GGBox(as.vector(as.matrix(Multispec.B5_C)),as.vector(as.matrix(Multispec.B5_A)),as.vector(as.matrix(Multispec.B5_W)),as.vector(as.matrix(Multispec.B5_WA)),c('white','cyan2','red3','magenta3'),paste0(github_dir,'figures/B5_Brightness_GGBox.png'))
C<-c(as.vector(as.matrix(Multispec.B1_C)),as.vector(as.matrix(Multispec.B2_C)),as.vector(as.matrix(Multispec.B3_C)),as.vector(as.matrix(Multispec.B4_C)),as.vector(as.matrix(Multispec.B5_C)))
A<-c(as.vector(as.matrix(Multispec.B1_A)),as.vector(as.matrix(Multispec.B2_A)),as.vector(as.matrix(Multispec.B3_A)),as.vector(as.matrix(Multispec.B4_A)),as.vector(as.matrix(Multispec.B5_A)))
WA<-c(as.vector(as.matrix(Multispec.B1_WA)),as.vector(as.matrix(Multispec.B2_WA)),as.vector(as.matrix(Multispec.B3_WA)),as.vector(as.matrix(Multispec.B4_WA)),as.vector(as.matrix(Multispec.B5_WA)))
W<-c(as.vector(as.matrix(Multispec.B1_W)),as.vector(as.matrix(Multispec.B2_W)),as.vector(as.matrix(Multispec.B3_W)),as.vector(as.matrix(Multispec.B4_W)),as.vector(as.matrix(Multispec.B5_W)))
All_means<-PlotLevel_GGBox(C,A,W,WA,c(c('white','cyan2','red3','magenta3')),paste0(github_dir,'figures/All_BPlots_Brightness_GGBox.png'))

#write csv file
out<-cbind(rep(c("B1","B2","B3","B4","B5"),each=4),rep(c("CC","CW","LC","LW"),times=5),c(B1_means,B2_means,B3_means,B4_means,B5_means))
colnames(out)<-c('Plot','Treat','Value')
write.csv(out,paste0(github_dir,'data/level1/BPlots_Micasense_BI_Full.csv'),row.names=FALSE,col.names=TRUE)

