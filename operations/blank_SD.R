## now analyse the blank data

library(tidyverse)
library(ggplot2)
library(dplyr)

## we got the data yesterday in (inner_group.R)
names(Blank_final) <- c('confidence','Accession','coverage','PSMs','sample','protein')
sum_Blank <- as.data.frame(table(Blank_final$Accession)) %>% 
  filter(sum_Blank$Freq == 6)

## we get the head of the data we want. 
Blank_6 <- Blank_final %>% filter(Blank_final$Accession =='	P04406')
cov_mean <- c(Blank_6$coverage)
PSMs_mean <- c(Blank_6$PSMs)
Blank_6 <- Blank_6 %>% mutate(coverage_mean=mean(cov_mean))
Blank_6 <- Blank_6 %>% mutate(PSMs_mean=mean(PSMs_mean))

## calculate the center of each unique protein
for (proteins in sum_Blank$Var1[-1]) {
  Blank_6_R <- Blank_final %>% 
    filter(Blank_final$Accession == proteins)
  cov_mean_R <- c(Blank_6_R$coverage)
  PSMs_mean_R <- c(Blank_6_R$PSMs)
  Blank_6_R <- Blank_6_R %>% mutate(coverage_mean=mean(cov_mean_R))
  Blank_6_R <- Blank_6_R %>% mutate(PSMs_mean=mean(PSMs_mean_R))
  Blank_6 <- rbind(Blank_6,Blank_6_R)
}
## the distance of each point to it's center
Blank_6 <- Blank_6 %>% 
  mutate(Blank_6,distance= sqrt((coverage-coverage_mean)^2+(PSMs-PSMs_mean)^2))

## add the Var and SD calculation
## firstly we create a head for the file
Blank_6vs <- Blank_6[1:6,] %>% 
  mutate(var=var(distance)) %>%
  mutate(sd=sd(distance))

## the we use a for loop to calculate the proteins' Var and SD respectively  
for (proteins in sum_Blank$Var1[-1]) {
  Blank_6vs_R <- Blank_6 %>% 
    filter(Blank_6$Accession==proteins) %>% 
    mutate(var=var(Blank_6vs_R$distance)) %>%
    mutate(sd=sd(Blank_6vs_R$distance))
  Blank_6vs <- rbind(Blank_6vs,Blank_6vs_R)
}

## clean the final data
Blank_result <-Blank_6vs %>% select(2,7,8,10,11)
cleaned_result <- Blank_result[1,]
for (entry in 1:nrow(Blank_result)-5) {
  if (entry%%6==1) {cleaned_result <- rbind(cleaned_result,USP7_result[entry,])}}


cleaned_result_Blank <- cleaned_result[-1,]
cleaned_result_Blank
sheets <- list('sheet1'=Blank_6vs,'sheet2' =cleaned_result_Blank)
write_xlsx(sheets,'Blank_results.xlsx')