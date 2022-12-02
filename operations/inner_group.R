### in this section we select the proteins that exist the most times.As we delete
### one sample in TPP1 so we decide that if the protein appears more them 3 times 
### we add it to an effective list.As for the others(USP7 and Blank),we set it to 4

### initial settings 
install.packages("ggplot2")
library(tidyverse)
library(fs)
library(ggplot2)
library(readxl)
library(dplyr)

## now count the times of each the proteins that exist
sum_Tpp1 <- as.data.frame(table(TPP1_final$Accession)) %>% 
  filter(sum_Tpp1$Freq == 5)

TPP1_5 <- TPP1_final %>% filter(TPP1_final$Accession =='O95831')
cov_mean <- c(TPP1_5$coverage)
PSMs_mean <- c(TPP1_5$PSMs)
TPP1_5 <- TPP1_5 %>% mutate(coverage_mean=mean(cov_mean))
TPP1_5 <- TPP1_5 %>% mutate(PSMs_mean=mean(PSMs_mean))

for (proteins in sum_Tpp1$Var1[-1]) {
  TPP1_5_R <- TPP1_final %>% 
     filter(TPP1_final$Accession == proteins)
     cov_mean_R <- c(TPP1_5_R$coverage)
     PSMs_mean_R <- c(TPP1_5_R$PSMs)
     TPP1_5_R <- TPP1_5_R %>% mutate(coverage_mean=mean(cov_mean_R))
    TPP1_5_R <- TPP1_5_R %>% mutate(PSMs_mean=mean(PSMs_mean_R))
    print(TPP1_5_R)
    TPP1_5 <- rbind(TPP1_5,TPP1_5_R)
}

## make the plot
names(TPP1_5) <- c('confidence','Accession','coverage','PSMs','sample','protein')
plot_TPP1 <- ggplot(data = TPP1_5,
                    mapping = aes(x=PSMs,
                                  y=coverage,
                                  col=sample,
                                  xlab='PSMs',
                                  ylab='coverage'))+geom_point()+
  geom_text(aes(16,13.1578947,label='Q93009'))+
  geom_text(aes(14,12.7051742,label='Q93009'))+
  geom_text(aes(12,9.9096189,label='Q93009'))+
  geom_text(aes(21,20.1451906,label='Q93009'))+
  geom_text(aes(8,6.9872958,label='Q93009'))
  
plot_TPP1
                
