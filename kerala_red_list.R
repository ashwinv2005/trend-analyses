klsp = read.csv("Raw_Trends_Kerala.csv")

library(plyr)
library(tidyverse)
source('~/GitHub/trend-analyses/functions.R')

sp = klsp %>% distinct(Species)
sp = sp$Species

load("dataforanalyses.RData")

datat = data %>% filter(ST_NM == "KERALA")
period = "long"
scol = "#869B27"

trends = freqtrends(data=datat, species=sp[i], error=T, type="part", nsim=100)

trends = read.csv("KLtrends.csv")
l = length(unique(trends$species)) + 1

for (i in 2:length(sp))
{
  trends1 = freqtrends(data=datat, species=sp[i], error=T, type="part", nsim=100)
  trends = rbind(trends,trends1)
  write.csv(trends, "KLtrends.csv", row.names = F)
}

write.csv(trends, "KLtrends.csv", row.names = F)
