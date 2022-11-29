### in this section we select the proteins that exist the most times.As we delete
### one sample in TPP1 so we decide that if the protein appears more them 3 times 
### we add it to an effective list.As for the others(USP7 and Blank),we set it to 4

### initial settings 
library(tidyverse)
library(fs)
library(ggplot2)
library(readxl)

## now count the times of each the proteins that exist
sum_Tpp1 <- as.data.frame(table(TPP1_final$Accession)) %>% 
  filter(sum_Tpp1$Freq >= 5)

TPP1_3 <- TPP1_final %>% filter(TPP1_final$Accession =='O14744')

for (proteins in sum_Tpp1$Var1[-1]) {
  TPP1_3_R <- TPP1_final %>% 
    filter(TPP1_final$Accession == proteins)
  TPP1_3 <- rbind(TPP1_3,TPP1_3_R)
}

## make the plot
names(TPP1_3) <- c('confidence','Accession','coverage','PSMs','sample','protein')
plot_TPP1 <- 
  ggplot(data = TPP1_3,
         mapping = aes(x=TPP1_3$PSMs[0:20],
                       y=TPP1_3$coverage[0:20],
                       colours=TPP1_3$sample
                       ))+ geom_point() 
         
