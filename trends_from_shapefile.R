library(plyr)
library(tidyverse)
require(data.table)
require(sp)
require(rgeos)
source('~/GitHub/trend-analyses/functions.R')

period = "short"
scol = "#869B27"

userpol = readOGR("shpExport","shpExport")
userpol@data$notes = 1
area = raster::area(userpol)/1000000

if (area < 100)
{
  type = "null"
}

if (area >= 100 & area < 10000)
{
  type = "hot"
}

if (area >= 10000 & area < 200000)
{
  type = "part"
}

if (area >= 100000)
{
  type = "full"
}

load("dataforanalyses.RData")

if (period == "short")
{
  data = data %>% filter(year >= 2015)
}

temp = data %>% group_by(group.id) %>% slice(1) # same group ID, same grid/district/state 

rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
proj4string(temp) = "+proj=longlat +datum=WGS84"
temp = over(temp,userpol) 
temp = data.frame(temp) # convert into data frame for left_join
temp$group.id = rownames(temp) # add column to join with the main data
data1 = left_join(temp,data)
data1 = data1 %>% filter(!is.na(notes))

species = "Common Myna"
trends = freqtrends(data=data1, species=species, error=T, type=type)

trends1 = trends
if (period == "short")
{
  trends1$freq[trends1$timegroups<2014] = NA
  trends1$se[trends1$timegroups<2014] = NA
}
trends1 = stdtrends(trends1)

temp = trends1

require(extrafont)
#loadfonts(device = "win")

temp$maxci = temp$nmfreqbyspec + temp$nmsebyspec*1.96
temp$minci = temp$nmfreqbyspec - temp$nmsebyspec*1.96
temp$minci[temp$minci<0] = 0

liml = min(temp$minci)
liml = round_any(liml,50,floor)

limu = max(temp$maxci)
limu = round_any(limu,50,ceiling)

if ((limu-liml) < 100 & liml < 0)
  liml = liml - 50
if ((limu-liml) < 100 & limu > 0)
  limu = limu + 50

ybreaks = seq(liml,limu,length.out=5)

if (any(ybreaks != 100))
{
  tmpx = sort((abs(ybreaks-100)))
  tmp = tmpx[1]
  tmp1 = ybreaks - tmp
  tmp2 = ybreaks + tmp
  if (any(tmp1 == 100) & min(tmp1) >= 0)
  {
    ybreaks = tmp1
    liml = round_any(ybreaks[1],50,floor)
  }
  if (min(tmp1) < 0 & any(tmp1 == 100))
  {
    ybreaks = ybreaks + tmpx[2]
    limu = round_any(ybreaks[5],50,ceiling)
    limu = limu + round(0.01*(limu-liml))
  }
  if (any(tmp2 == 100))
  {
    ybreaks = tmp2
    limu = round_any(ybreaks[5],50,ceiling)
    limu = limu + round(0.01*(limu-liml))
  }
  
  ybreaks = round_any(ybreaks,10,round)
}

ybreaksl = rep("",5)

for (j in 1:5)
{
  ybreaksl[j] = paste("+",(ybreaks[j]-100),"%",sep="")
  if (ybreaks[j] <= 100)
    ybreaksl[j] = paste((ybreaks[j]-100),"%",sep="")
}


ggp = ggplot(temp, aes(x=timegroups, y=nmfreqbyspec)) + 
  geom_point(size = 3, colour = scol) +
  geom_line(size = 1.5, colour = scol) +
  geom_hline(yintercept = ybreaks[1], linetype = "dotted", size = 0.7) +
  geom_hline(yintercept = ybreaks[2], linetype = "dotted", size = 0.7) +
  geom_hline(yintercept = ybreaks[3], linetype = "dotted", size = 0.7) +
  geom_hline(yintercept = ybreaks[4], linetype = "dotted", size = 0.7) +
  geom_hline(yintercept = ybreaks[5], linetype = "dotted", size = 0.7) +
  geom_ribbon(aes(x = timegroups, ymin = minci,
                  ymax = maxci), fill = scol, colour = NA, alpha = 0.3) +
  xlab("years") +
  ylab("change in abundance index")


if (period == "long")
{ 
  xbreaks1 = temp$timegroups[1:11]
  lbreaks1 = temp$timegroupsf[1:11]
  lbreaks1[1:3] = c(paste(sprintf('\u2190')," before 2000"),"2000-06","2007-10")
  lbreaks1[c(4,6,8,10)] = ""
}

if (period == "short")
{
  xbreaks1 = temp$timegroups[1:6]
  lbreaks1 = temp$timegroupsf[1:6]
}

ggpx = ggp +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 20, colour = "#56697B", vjust = -4, 
                                   margin = margin(0, 0, 0.8, 0, 'cm')),
        axis.title.y = element_text(size = 22, colour = "#56697B",margin = margin(0, 0.8, 0, 0, 'cm')), 
        axis.ticks.x = element_line(size = 0.7, colour = "#56697B"), 
        axis.ticks.length=unit(.4, "cm"),
        axis.text.y = element_text(size = 25, colour = "#56697B", vjust = -0.4, hjust = 1, 
                                   margin = margin(0, -0.8, 0, 0, 'cm')),
        axis.ticks.y = element_blank(), 
        axis.line.x = element_line(size = 0.7, colour = "#56697B")) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = xbreaks1, labels = lbreaks1) +
  scale_y_continuous(breaks = c(ybreaks[1],ybreaks[2],ybreaks[3],ybreaks[4],ybreaks[5]), 
                     limits = c(liml,limu),
                     labels = c(ybreaksl[1],ybreaksl[2],ybreaksl[3],
                                ybreaksl[4],ybreaksl[5])) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA))+
  theme(legend.position = "none")

ggpx1 = ggpx +
  {if(period == "long")labs(tag = paste(sprintf('\u25A0')," ",sprintf('\u25A0'),
                                        " ",sprintf('\u25A0')," ",sprintf('\u25A0')))}+
  {if(period == "long")theme(plot.tag.position = c(0.84, 0.072),
                             plot.tag = element_text(size = 30, colour = 'white', face = 'bold'))}

sps = as.character(unique(trends$species))
name = paste(sps,"_","trend.jpeg",sep="")

print(ggpx1)
ggsave(file=name, units="in", width=11, height=7, bg = "transparent")
dev.off()