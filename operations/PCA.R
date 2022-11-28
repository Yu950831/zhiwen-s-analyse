####load all the files and combine them into one file  
## load some package 
install.packages("factoextra")
library(factoextra)
library(tidyverse)
library(fs)
library(ggplot2)
library(readxl)

T1 <- read_xlsx('orgin.data/data/20220104/3FLAG-TPP1(1).xlsx')

T1 <- T1 %>%filter(`Protein FDR Confidence` == 'High') %>% 
  select(1,2,5:11) %>% 
  mutate(sample='a')


T2 <- read_xlsx('orgin.data/data/20220104/3FLAG-TPP1(2).xlsx')

T2 <- T2 %>%filter(`Protein FDR Confidence` == 'High') %>% 
  select(1,2,5:11) %>% 
  mutate(sample='b')

T3 <- read_xlsx('orgin.data/data/20220104/3FLAG-TPP1(3).xlsx')

T3 <- T3 %>%filter(`Protein FDR Confidence` == 'High') %>% 
  select(1,2,5:11) %>% 
  mutate(sample='c')

# make all the samples into one dataframe
T_all <- rbind(T1,T2,T3)

# make a PCA
T_all.pca <-prcomp(T_all[3:9])
summary(T_all.pca)
T_all.pca <-as.data.frame(T_all.pca)
#data visualization
fviz_eig(T_all.pca,addlabels = T,xlab = 'PC',main = 'TPP1的主成分碎石图',
         ylab = '百分率')
pca <-as.data.frame(T_all.pca$x)
pca$Accession <- T_all$Accession
pca$sample <- T_all$sample
pca
ggplot(pca,aes(x=PC1,y=PC2,col = pca$sample,labes= pca$Accession))+geom_point()
dim(T_all.pca$x)
