### now we have a different ideal.we calculate the center of each unique protein 
### that identified by the accession numbers. we compute all the centers and 
### the distance of each point to its' center.Finally we get the SD of the distance 
### for each protein. we set a standard to evaluate if this protein have some connection 
### to TPP1

### initial settings 
library(tidyverse)
library(ggplot2)

## we got the data yesterday in (inner_group.R)
names(TPP1_final) <- c('confidence','Accession','coverage','PSMs','sample','protein')
for (proteins in unique(sum_Tpp1$Var1)) {
  TPP1_SD <- TPP1_final %>% 
    filter(TPP1_final$Accession=proteins)
  print(TPP1_SD)
  }
  
#   %>% 
#     coverage_meanx=sum(TPP1_final$coverage)/5
#     mutate(coverage_mean=coverage_meanx)} 
# %>%
#     mutate(PSMs_mean=sum(TPP1_final)/5) %>% 
#     mutate(distance_center= sqrt((TPP1_final$coverage-sum(TPP1_final$coverage)/5)^2
#                                  +(TPP1_final-sum(TPP1_final)/5)^2))
# }
  