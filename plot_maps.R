load("dataforanalyses.RData")
load("maps.RData")
load("clips.RData")

statemap = readOGR("in_states_2019","in_states_2019")

require(tidyverse)
require(rgdal)
require(sp)
require(sf)
require(rgeos)
require(extrafont)

require(viridis)

species = "Malabar Pied-Hornbill"

temp = data %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(g2clip) %>% mutate(lists = n_distinct(group.id))

tempr = temp %>%
  filter(lists <= 10)

temp = temp %>%
  filter(COMMON.NAME == species) %>%
  group_by(g2clip) %>% summarize(freq = n_distinct(group.id)/max(lists))

temp$freq[temp$g2clip %in% tempr$g2clip] = min(temp$freq)



fortified = fortify(g2clip, region = c("id"))
temp$g2clip = as.character(temp$g2clip)
plotg2 = na.omit(left_join(fortified,temp, by = c('id' = "g2clip"))) # SPDF to plot

require(mltools)

plotg2$freq1 = mltools::bin_data(plotg2$freq, bins=4, binType = "quantile")

sm = plotg2 %>%
  group_by(freq1) %>% summarize(min = round(min(freq),3),max = round(max(freq),3))

l = length(sm$freq1)
vals = c("#99CCFF","#6699CC","#336699","#003399")

plotg2map = ggplot() +
  #geom_polygon(data = pamap, aes(x=long, y=lat, group=group), colour = 'black', fill = NA)+  
  geom_polygon(data = plotg2, aes(x=long, y=lat, group=group,fill = freq1), colour = NA)+  
  geom_polygon(data = statemap, aes(x = long, y = lat, group = group), col = "black", fill = NA, size = 0.5) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  {if(l <= 2)scale_fill_manual(values = vals[1:l],
                               breaks = sm$freq1, labels = sm$freq1,
                               name = "frequency of\nreporting")} +
  {if(l == 3)scale_fill_manual(values = vals[1:l],
                               breaks = sm$freq1, 
                               labels = c(paste("<=",sm$max[1]),paste(sm$min[2],
                                                                      " - ",sm$max[2]), paste(">",sm$min[3])),
                               name = "frequency of\nreporting")} +
  {if(l > 3)scale_fill_manual(values = vals,breaks = sm$freq1, 
                              labels = c(paste("<=",sm$max[1]),
                                         paste(sm$min[2]," - ",sm$max[2]), paste(sm$min[3]," - ",sm$max[3]),
                                         paste(">",sm$min[4])),
                              name = "frequency of\nreporting")} +
  theme(text=element_text(family="Gill Sans MT"), legend.title = element_text(size = 12)) +
  #scale_fill_viridis(name = "frequency of\nreporting")+
  coord_map()

name = paste(species,"_map_india.jpeg",sep="")
print(plotg2map)
ggsave(file=name, units="in", width=10, height=7)
dev.off()


###################

statemap = readOGR("in_states_2019","in_states_2019")
state = "GOA"

smap = statemap[statemap@data$stname %in% state,]
proj4string(smap) = "+proj=longlat +datum=WGS84"

g1 = 6.6

bb = bbox(smap) # creates a box with extents from map
cs = c(g1*1000/111111,g1*1000/111111)  # cell size g1 km x g1 km
cc = bb[, 1] + (cs/2)  # cell offset
cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd) # create required grids
sp_grd = SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd))) # create spatial grid data frame
sp_grd_poly = as(sp_grd, "SpatialPolygonsDataFrame") # SGDF to SPDF
assign("gridmap6.6",sp_grd_poly,.GlobalEnv)

proj4string(gridmap6.6) = "+proj=longlat +datum=WGS84"

require(rgeos)
require(raster)
diff = gDifference(gridmap6.6,gBuffer(smap, byid=TRUE, width=0))
diff = gBuffer(diff, byid=TRUE, width=0)
cliptemp = gridmap6.6 - diff
crs(cliptemp) = NA

require(tidyverse)
require(data.table)
require(sp)
require(rgeos)

load("dataforanalyses.RData")
load("maps.RData")
load("clips.RData")

datat = data %>% filter(ST_NM == "GOA")

temp = datat %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,cliptemp)
temp = data.frame(temp)
temp$group.id = rownames(temp)
datat = left_join(temp,datat)
names(datat)[1] = "cliptemp"

require(tidyverse)
require(rgdal)
require(sp)
require(sf)
require(rgeos)
require(extrafont)

require(viridis)




species = "Malabar Pied-Hornbill"

temp = datat %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(cliptemp) %>% mutate(lists = n_distinct(group.id))

tempr = temp %>%
  filter(lists <= 10)

temp = temp %>%
  filter(COMMON.NAME == species) %>%
  group_by(cliptemp) %>% summarize(freq = n_distinct(group.id)/max(lists))

temp$freq[temp$cliptemp %in% tempr$cliptemp] = min(temp$freq)



fortified = fortify(cliptemp, region = c("id"))
temp$cliptemp = as.character(temp$cliptemp)
plottemp = na.omit(left_join(fortified,temp, by = c('id' = "cliptemp"))) # SPDF to plot

require(mltools)

plottemp$freq1 = mltools::bin_data(plottemp$freq, bins=4, binType = "quantile")

sm = plottemp %>%
  group_by(freq1) %>% summarize(min = round(min(freq),3),max = round(max(freq),3))

l = length(sm$freq1)
vals = c("#99CCFF","#6699CC","#336699","#003399")

plottempmap = ggplot() +
  #geom_polygon(data = pamap, aes(x=long, y=lat, group=group), colour = 'black', fill = NA)+  
  geom_polygon(data = plottemp, aes(x=long, y=lat, group=group,fill = freq1), colour = NA)+  
  geom_polygon(data = smap, aes(x = long, y = lat, group = group), col = "black", fill = NA, size = 0.5) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  theme(text=element_text(family="Gill Sans MT"), legend.title = element_text(size = 12)) +
  {if(l <= 2)scale_fill_manual(values = vals[1:l],
                                                 breaks = sm$freq1, labels = sm$freq1,
                               name = "frequency of\nreporting")} +
  {if(l == 3)scale_fill_manual(values = vals[1:l],
                                                 breaks = sm$freq1, 
                                                 labels = c(paste("<=",sm$max[1]),paste(sm$min[2],
                                                                " - ",sm$max[2]), paste(">",sm$min[3])),
                               name = "frequency of\nreporting")} +
  {if(l > 3)scale_fill_manual(values = vals,breaks = sm$freq1, 
                                                labels = c(paste("<=",sm$max[1]),
                                                paste(sm$min[2]," - ",sm$max[2]), paste(sm$min[3]," - ",sm$max[3]),
                                                paste(">",sm$min[4])),
                              name = "frequency of\nreporting")} +
  #scale_fill_viridis(name = "frequency of\nreporting")+
  coord_map()

name = paste(species,"_map_",state,".jpeg",sep="")
print(plottempmap)
ggsave(file=name, units="in", width=10, height=7)
dev.off()
