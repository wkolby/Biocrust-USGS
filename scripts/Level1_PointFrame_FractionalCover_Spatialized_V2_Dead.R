rm(list = ls()) # clear the environment
setwd("/Users/wksmith/Documents/GitHub/Biocrust-USGS")
github_dir <- "/Users/wksmith/Documents/GitHub/Biocrust-USGS"

library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(reshape2)

########################################BioCrust Cover 100pt##############################################################
Cover<-read.csv2(paste(github_dir,'/data/Cover/Plot/',"2021_DOE_Raw_Veg_Data_for_Bill_20250211_v2.csv",sep=''),sep=',',header=T)
blks=c(1,2,3,4,5)
treats=c("C","L","LW","W")
ctypes=
S1=c()
for(i in 1:length(treats)){
  for(j in 1:length(blks)){
    Cover_S1=subset(Cover, Block %in% blks[j] & Treatment %in% treats[i] & Quad %in% 1 & Row %in% c(4,5,6,7,8,14,15,16,17,18,24,25,26,27,28,34,35,36,37,38,44,45,46,47,48))
    tmp=c("S1","B",blks[j],treats[i],colSums(Cover_S1[,c(8,9,10,12,13,42,43,44,45)])/25)
    S1=rbind(S1,tmp)
  }
}

S2=c()
for(i in 1:length(treats)){
  for(j in 1:length(blks)){
    Cover_S2a=subset(Cover, Block %in% blks[j] & Treatment %in% treats[i] & Quad %in% 1 & Row %in% c(9,10,19,20,29,30,39,40,49,50))
    Cover_S2b=subset(Cover, Block %in% blks[j] & Treatment %in% treats[i] & Quad %in% 2 & Row %in% c(1,2,11,12,21,22,31,32,41,42))
    Cover_S2=rbind(Cover_S2a,Cover_S2b)
    tmp=c("S2","B",blks[j],treats[i],colSums(Cover_S2[,c(8,9,10,12,13,42,43,44,45)])/20)
    S2=rbind(S2,tmp)
  }
}

S3=c()
for(i in 1:length(treats)){
  for(j in 1:length(blks)){
    Cover_S3=subset(Cover, Block %in% blks[j] & Treatment %in% treats[i] & Quad %in% 2 & Row %in% c(3,4,5,6,7,13,14,15,16,17,23,24,25,26,27,33,34,35,36,37,43,44,45,46,47))
    tmp=c("S3","B",blks[j],treats[i],colSums(Cover_S3[,c(8,9,10,12,13,42,43,44,45)])/25)
    S3=rbind(S3,tmp)
  }
}

S4=c()
for(i in 1:length(treats)){
  for(j in 1:length(blks)){
    Cover_S4=subset(Cover, Block %in% blks[j] & Treatment %in% treats[i] & Quad %in% 1 & Row %in% c(44,45,46,47,48,54,55,56,57,58,64,65,66,67,68,74,75,76,77,78,84,85,86,87,88))
    tmp=c("S4","B",blks[j],treats[i],colSums(Cover_S4[,c(8,9,10,12,13,42,43,44,45)])/25)
    S4=rbind(S4,tmp)
  }
}

S5=c()
for(i in 1:length(treats)){
  for(j in 1:length(blks)){
    Cover_S5a=subset(Cover, Block %in% blks[j] & Treatment %in% treats[i] & Quad %in% 1 & Row %in% c(49,50,59,60,69,70,79,80,89,90))
    Cover_S5b=subset(Cover, Block %in% blks[j] & Treatment %in% treats[i] & Quad %in% 2 & Row %in% c(41,42,51,52,61,62,71,72,81,82))
    Cover_S5=rbind(Cover_S5a,Cover_S5b)
    tmp=c("S5","B",blks[j],treats[i],colSums(Cover_S5[,c(8,9,10,12,13,42,43,44,45)])/20)
    S5=rbind(S5,tmp)
  }
}

S6=c()
for(i in 1:length(treats)){
  for(j in 1:length(blks)){
    Cover_S6=subset(Cover, Block %in% blks[j] & Treatment %in% treats[i] & Quad %in% 2 & Row %in% c(43,44,45,46,47,53,54,55,56,57,63,64,65,66,67,73,74,75,76,77,83,84,85,86,87))
    tmp=c("S6","B",blks[j],treats[i],colSums(Cover_S6[,c(8,9,10,12,13,42,43,44,45)])/25)
    S6=rbind(S6,tmp)
  }
}

S7=c()
for(i in 1:length(treats)){
  for(j in 1:length(blks)){
    Cover_S7a=subset(Cover, Block %in% blks[j] & Treatment %in% treats[i] & Quad %in% 1 & Row %in% c(84,85,86,87,88,94,95,96,97,98))
    Cover_S7b=subset(Cover, Block %in% blks[j] & Treatment %in% treats[i] & Quad %in% 3 & Row %in% c(4,5,6,7,8,14,15,16,17,18,24,25,26,27,28))
    Cover_S7=rbind(Cover_S7a,Cover_S7b)
    tmp=c("S7","B",blks[j],treats[i],colSums(Cover_S7[,c(8,9,10,12,13,42,43,44,45)])/25)
    S7=rbind(S7,tmp)
  }
}

S8=c()
for(i in 1:length(treats)){
  for(j in 1:length(blks)){
    Cover_S8a=subset(Cover, Block %in% blks[j] & Treatment %in% treats[i] & Quad %in% 1 & Row %in% c(89,90,99,100))
    Cover_S8b=subset(Cover, Block %in% blks[j] & Treatment %in% treats[i] & Quad %in% 2 & Row %in% c(81,82,91,92))
    Cover_S8c=subset(Cover, Block %in% blks[j] & Treatment %in% treats[i] & Quad %in% 3 & Row %in% c(9,10,19,20,29,30))
    Cover_S8d=subset(Cover, Block %in% blks[j] & Treatment %in% treats[i] & Quad %in% 4 & Row %in% c(1,2,11,12,21,22))
    Cover_S8=rbind(Cover_S8a,Cover_S8b,Cover_S8c,Cover_S8d)
    tmp=c("S8","B",blks[j],treats[i],colSums(Cover_S8[,c(8,9,10,12,13,42,43,44,45)])/20)
    S8=rbind(S8,tmp)
  }
}

S9=c()
for(i in 1:length(treats)){
  for(j in 1:length(blks)){
    Cover_S9a=subset(Cover, Block %in% blks[j] & Treatment %in% treats[i] & Quad %in% 2 & Row %in% c(83,84,85,86,87,93,94,95,96,97))
    Cover_S9b=subset(Cover, Block %in% blks[j] & Treatment %in% treats[i] & Quad %in% 4 & Row %in% c(3,4,5,6,7,13,14,15,16,17,23,24,25,26,27))
    Cover_S9=rbind(Cover_S9a,Cover_S9b)
    tmp=c("S9","B",blks[j],treats[i],colSums(Cover_S9[,c(8,9,10,12,13,42,43,44,45)])/25)
    S9=rbind(S9,tmp)
  }
}

S10=c()
for(i in 1:length(treats)){
  for(j in 1:length(blks)){
    Cover_S10=subset(Cover, Block %in% blks[j] & Treatment %in% treats[i] & Quad %in% 3 & Row %in% c(34,35,36,37,38,44,45,46,47,48,54,55,56,57,58,64,65,66,67,68,74,75,76,77,78))
    tmp=c("S10","B",blks[j],treats[i],colSums(Cover_S10[,c(8,9,10,12,13,42,43,44,45)])/25)
    S10=rbind(S10,tmp)
  }
}

S11=c()
for(i in 1:length(treats)){
  for(j in 1:length(blks)){
    Cover_S11a=subset(Cover, Block %in% blks[j] & Treatment %in% treats[i] & Quad %in% 3 & Row %in% c(39,40,49,50,59,60,69,70,79,80))
    Cover_S11b=subset(Cover, Block %in% blks[j] & Treatment %in% treats[i] & Quad %in% 4 & Row %in% c(31,32,41,42,51,52,61,62,71,72))
    Cover_S11=rbind(Cover_S11a,Cover_S11b)
    tmp=c("S11","B",blks[j],treats[i],colSums(Cover_S11[,c(8,9,10,12,13,42,43,44,45)])/20)
    S11=rbind(S11,tmp)
  }
}

S12=c()
for(i in 1:length(treats)){
  for(j in 1:length(blks)){
    Cover_S12=subset(Cover, Block %in% blks[j] & Treatment %in% treats[i] & Quad %in% 4 & Row %in% c(33,34,35,36,37,43,44,45,46,47,53,54,55,56,57,63,64,65,66,67,73,74,75,76,77))
    tmp=c("S12","B",blks[j],treats[i],colSums(Cover_S12[,c(8,9,10,12,13,42,43,44,45)])/25)
    S12=rbind(S12,tmp)
  }
}

Cover_resampled = rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12)
Cover_resampled <- as.data.frame(Cover_resampled)

colnames(Cover_resampled) <- c("Spectra", "Plot", "Block", "Treatment", "Litter", "Rock", "Bare", "Live","Dead", "Lichen", "Moss", "Dark", "Light")
rownames(Cover_resampled) <- c(1:240)

#write csv file#
write.csv(Cover_resampled,paste(github_dir,'/data/Level1/FractionaCover_BySpectra_2021_V2.csv',sep=''),row.names = FALSE)

####Plot Check###
blks=c(1,2,3,4,5)
treats=c("C","L","LW","W")
for(i in 1:length(treats)){
  for(j in 1:length(blks)){
    pcover = subset(Cover_resampled, Block %in% blks[j] & Treatment %in% treats[i])
    ptitle = paste0(pcover$Plot[1],pcover$Block[1],pcover$Treatment[1],"_LightCyano")
    cover_matrix <- matrix(pcover$Light, nrow = 3, ncol = 4)
    df_long <- melt(cover_matrix)
    df_long=transform(df_long,value = as.numeric(value))
    ggplot(data = df_long, aes(x = Var1, y = Var2[12:1], fill = value)) +
      geom_tile() +
      labs(title = ptitle,
           x = "Columns",
           y = "Rows",
           fill = "Value") +
      scale_fill_gradient(limits = c(0, 1),low = "white", high = "red") # Use white for low, red for high
    ggsave(paste(github_dir,'/cover_figures/FractionalCover_',ptitle,'.png',sep=''),dpi=300,width=125,height=125,units='mm')
  }
}
    
####Plot Check###
blks=c(1,2,3,4,5)
treats=c("C","L","LW","W")
for(i in 1:length(treats)){
  for(j in 1:length(blks)){
    pcover = subset(Cover_resampled, Block %in% blks[j] & Treatment %in% treats[i])
    ptitle = paste0(pcover$Plot[1],pcover$Block[1],pcover$Treatment[1],"_Live")
    cover_matrix <- matrix(pcover$Live, nrow = 3, ncol = 4)
    df_long <- melt(cover_matrix)
    df_long=transform(df_long,value = as.numeric(value))
    ggplot(data = df_long, aes(x = Var1, y = Var2[12:1], fill = value)) +
      geom_tile() +
      labs(title = ptitle,
           x = "Columns",
           y = "Rows",
           fill = "Value") +
      scale_fill_gradient(low = "white", high = "green3") # Use white for low, red for high
    ggsave(paste(github_dir,'/cover_figures/FractionalCover_',ptitle,'.png',sep=''),dpi=300,width=125,height=125,units='mm')
  }
}

####Plot Check###
blks=c(1,2,3,4,5)
treats=c("C","L","LW","W")
for(i in 1:length(treats)){
  for(j in 1:length(blks)){
    pcover = subset(Cover_resampled, Block %in% blks[j] & Treatment %in% treats[i])
    ptitle = paste0(pcover$Plot[1],pcover$Block[1],pcover$Treatment[1],"_Dead")
    cover_matrix <- matrix(pcover$Dead, nrow = 3, ncol = 4)
    df_long <- melt(cover_matrix)
    df_long=transform(df_long,value = as.numeric(value))
    ggplot(data = df_long, aes(x = Var1, y = Var2[12:1], fill = value)) +
      geom_tile() +
      labs(title = ptitle,
           x = "Columns",
           y = "Rows",
           fill = "Value") +
      scale_fill_gradient(low = "white", high = "brown4") # Use white for low, red for high
    ggsave(paste(github_dir,'/cover_figures/FractionalCover_',ptitle,'.png',sep=''),dpi=300,width=125,height=125,units='mm')
  }
}


####Plot Check###
blks=c(1,2,3,4,5)
treats=c("C","L","LW","W")
for(i in 1:length(treats)){
  for(j in 1:length(blks)){
    pcover = subset(Cover_resampled, Block %in% blks[j] & Treatment %in% treats[i])
    ptitle = paste0(pcover$Plot[1],pcover$Block[1],pcover$Treatment[1],"_Moss")
    cover_matrix <- matrix(pcover$Moss, nrow = 3, ncol = 4)
    df_long <- melt(cover_matrix)
    df_long=transform(df_long,value = as.numeric(value))
    ggplot(data = df_long, aes(x = Var1, y = Var2[12:1], fill = value)) +
      geom_tile() +
      labs(title = ptitle,
           x = "Columns",
           y = "Rows",
           fill = "Value") +
      scale_fill_gradient(low = "white", high = "purple3") # Use white for low, red for high
    ggsave(paste(github_dir,'/cover_figures/FractionalCover_',ptitle,'.png',sep=''),dpi=300,width=125,height=125,units='mm')
  }
}
