### we found it is meaningless to compare the same proteins in different groups
### so we decide to make the different groups of each unique protein into one group 
### and use them to do the clustering.

### initial settings 
library(factoextra)
library(tidyverse)
library(fs)
library(ggplot2)
library(readxl)

### load the data 
### as we supposed to editing the different kinds of the proteins and bind them 
### so my solution is to load them respectively

## firstly we set the path of the 2 files to make it easier to read the files
savepleace <- 'analyse.R/'
path1 <- 'orgin.data/data/20220104'
path2 <- 'orgin.data/data/20220530'

## get all the file names in to list respectively
TPP1day1 <- list.files(path1,pattern = '^3FLAG-TPP1')
TPP1day2 <- list.files(path2,pattern = '^3FLAG-TPP1')
USP7day1 <- list.files(path1,pattern = '^3FLAG-USP7')
USP7day2 <- list.files(path1,pattern = '^3FLAG-USP7')
BLANkday1 <- list.files(path1,pattern = '^FLAGblank')
BLANkday2 <- list.files(path1,pattern = '^FLAGblank')

## editing the list to loop
dirTPP1a <- paste(path1,TPP1day1,sep = '/')
dirTPP1b <- paste(path2,TPP1day2,sep = '/')
dirUSP7a <- paste(path1,USP7day1,sep = '/')
dirUSP7b <- paste(path2,USP7day2,sep = '/')
dirBLANka <- paste(path1,BLANkday1,sep = '/')
dirBLANkb <- paste(path2,BLANkday1,sep = '/')

## set up the temp files to read the first of each protein files and make them 
## as the samples to paste the rest of files after them
Temp1 <- read_xlsx(dirTPP1a[1])
Temp2 <- read_xlsx(dirTPP1b[1])
Temp3 <- read_xlsx(dirUSP7a[1])
Temp4 <- read_xlsx(dirUSP7b[1])
Temp5 <- read_xlsx(dirBLANka[1])
Temp6 <- read_xlsx(dirBLANkb[1])

### now read all the TPP1 protein files and combine them into one file 
for (filenames in 2:3) {
  new.data <- read_xlsx(dirTPP1a[filenames])
  Temp1 <- rbind(Temp1,new.data)
  }

for (filenames in 2:3) {
  new.data <- read_xlsx(dirTPP1b[filenames])
  Temp2 <- rbind(Temp2,new.data)
}
TPP1_all <- rbind(Temp1,Temp2)

### next step to editing the USP7 files  
for (filenames in 2:3) {
  new.data <- read_xlsx(dirUSP7a[filenames])
  Temp3 <- rbind(Temp3,new.data)
}

for (filenames in 2:3) {
  new.data <- read_xlsx(dirUSP7b[filenames])
  Temp4 <- rbind(Temp4,new.data)
}
USP7_all <- rbind(Temp3,Temp4)

### finally the blank samples

for (filenames in 2:3) {
  new.data <- read_xlsx(dirBLANka[filenames])
  Temp5 <- rbind(Temp5,new.data)
}

for (filenames in 2:3) {
  new.data <- read_xlsx(dirBLANkb[filenames])
  Temp6 <- rbind(Temp6,new.data)
}
Blank_all <- rbind(Temp5,Temp6)

## select the data we need and add some labels for the color of the later plot 
## firstly the TPP1 data 
TPP1_final <- TPP1_all %>% 
  select(1,2,5,7) %>% 
  mutate(protein= 'TPP1') %>% 
  filter(`Protein FDR Confidence` == 'High')

## then the USP7 data
USP7_final <- USP7_all %>% 
  select(1,2,5,7) %>% 
  mutate(protein = 'USP7') %>%
  filter(`Protein FDR Confidence` == 'High')

## finally the BLANK samples
Blank_final <- Blank_all %>% 
  select(1,2,5,7) %>% 
  mutate(protein = 'Blank') %>% 
  filter(`Protein FDR Confidence` == 'High')

##Final_data <- rbind(TPP1_final,USP7_final)
##Final_data <- rbind(Final_data,Blank_final) 

#save the data
write.table(TPP1_final,'TPP1_final')
write.table(USP7_final,'USP7_final')
write.table(Blank_final,'Blank_final')

## move the files to another folfer
file.copy('TPP1_final','analyse.R/')
file.copy('USP7_final','analyse.R/')
file.copy('Blank_final','analyse.R/')
unlink('TPP1_final')
unlink('USP7_final')
unlink('Blank_final')