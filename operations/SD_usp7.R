### after the TPp1 protein's data been soved then comes the USP7
### now we use the same code
### initial settings 
library(tidyverse)
library(ggplot2)
library(dplyr)

## we got the data yesterday in (inner_group.R)
names(USP7_final) <- c('confidence','Accession','coverage','PSMs','sample','protein')
sum_USP7 <- as.data.frame(table(USP7_final$Accession)) %>% 
  filter(sum_USP7$Freq == 6)

## we get the head of the data we want. 
USP7_6 <- USP7_final %>% filter(USP7_final$Accession =='B5ME19')
cov_mean <- c(USP7_6$coverage)
PSMs_mean <- c(USP7_6$PSMs)
USP7_6 <- USP7_6 %>% mutate(coverage_mean=mean(cov_mean))
USP7_6 <- USP7_6 %>% mutate(PSMs_mean=mean(PSMs_mean))

## calculate the center of each unique protein
for (proteins in sum_USP7$Var1[-1]) {
  USP7_6_R <- USP7_final %>% 
    filter(USP7_final$Accession == proteins)
  cov_mean_R <- c(USP7_6_R$coverage)
  PSMs_mean_R <- c(USP7_6_R$PSMs)
  USP7_6_R <- USP7_6_R %>% mutate(coverage_mean=mean(cov_mean_R))
  USP7_6_R <- USP7_6_R %>% mutate(PSMs_mean=mean(PSMs_mean_R))
  USP7_6 <- rbind(USP7_6,USP7_6_R)
}
## the distance of each point to it's center
USP7_6 <- USP7_6 %>% 
  mutate(USP7_6,distance= sqrt((coverage-coverage_mean)^2+(PSMs-PSMs_mean)^2))

## add the Var and SD calculation
## firstly we create a head for the file
USP7_6vs <- USP7_6[1:6,] %>% 
  mutate(var=var(distance)) %>%
  mutate(sd=sd(distance))

## the we use a for loop to calculate the proteins' Var and SD respectively  
for (proteins in sum_USP7$Var1[-1]) {
  USP7_6vs_R <- USP7_6 %>% 
    filter(USP7_6$Accession==proteins) %>% 
    mutate(var=var(USP7_6vs_R$distance)) %>%
    mutate(sd=sd(USP7_6vs_R$distance))
  USP7_6vs <- rbind(USP7_6vs,USP7_6vs_R)
}

## clean the final data
USP7_result <-USP7_6vs %>% select(2,7,8,10,11)
cleaned_result <- USP7_result[1,]
for (entry in 1:nrow(USP7_result)-5) {
  if (entry%%6==1) {cleaned_result_USP7 <- rbind(cleaned_result,USP7_result[entry,])}}


cleaned_result_USP7 <- cleaned_result_USP7[-1,]
cleaned_result_USP7