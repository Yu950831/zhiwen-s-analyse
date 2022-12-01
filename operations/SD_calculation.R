### now we have a different ideal.we calculate the center of each unique protein 
### that identified by the accession numbers. we compute all the centers and 
### the distance of each point to its' center.Finally we get the SD of the distance 
### for each protein. we set a standard to evaluate if this protein have some connection 
### to TPP1

### initial settings 
library(tidyverse)
library(ggplot2)

## we got the data yesterday in (inner_group.R)
sum_Tpp1 <- as.data.frame(table(TPP1_final$Accession)) %>% 
  filter(sum_Tpp1$Freq == 5)

## we get the head of the data we want. 
TPP1_5 <- TPP1_final %>% filter(TPP1_final$Accession =='O95831')
cov_mean <- c(TPP1_5$coverage)
PSMs_mean <- c(TPP1_5$PSMs)
TPP1_5 <- TPP1_5 %>% mutate(coverage_mean=mean(cov_mean))
TPP1_5 <- TPP1_5 %>% mutate(PSMs_mean=mean(PSMs_mean))

## calculate the center of each unique protein
for (proteins in sum_Tpp1$Var1[-1]) {
  TPP1_5_R <- TPP1_final %>% 
    filter(TPP1_final$Accession == proteins)
  cov_mean_R <- c(TPP1_5_R$coverage)
  PSMs_mean_R <- c(TPP1_5_R$PSMs)
  TPP1_5_R <- TPP1_5_R %>% mutate(coverage_mean=mean(cov_mean_R))
  TPP1_5_R <- TPP1_5_R %>% mutate(PSMs_mean=mean(PSMs_mean_R))
  TPP1_5 <- rbind(TPP1_5,TPP1_5_R)
}
## the distance of each point to it's center
TPP1_5 <- TPP1_5 %>% 
  mutate(TPP1_5,distance= sqrt((coverage-coverage_mean)^2+(PSMs-PSMs_mean)^2))

## add the Var and SD calculation
## firstly we create a head for the file
TPP1_5vs <- TPP1_5[1:5,] %>% 
  mutate(var=var(distance)) %>%
  mutate(sd=sd(distance))

## the we use a for loop to calculate the proteins' Var and SD respectively  
for (proteins in sum_Tpp1$Var1[-1]) {
   TPP1_5vs_R <- TPP1_5 %>% 
    filter(TPP1_5$Accession==proteins) %>% 
    mutate(var=var(TPP1_5vs_R$distance)) %>% 
    mutate(sd=sd(TPP1_5vs_R$distance))
    TPP1_5vs <- rbind(TPP1_5vs,TPP1_5vs_R)
}