require(tidyverse)
require(raster)
require(rgdal)
require(rgeos)

smap = readOGR("in_states_2019","in_states_2019")

indiatif = brick("IndiaDEM-Colour.tif")

r2 = mask(indiatif, smap)
#r2 = trim(r2, values = NA)

indiatif = r2

indiatif = as.data.frame(indiatif, xy = TRUE)
indiatif$IndiaDEM.Colour.1 = indiatif$IndiaDEM.Colour.1/255
indiatif$IndiaDEM.Colour.2 = indiatif$IndiaDEM.Colour.2/255
indiatif$IndiaDEM.Colour.3 = indiatif$IndiaDEM.Colour.3/255
names(indiatif)[3:5] = c("r","g","b")
indiatif[is.na(indiatif)] = 0
indiatif$codes = rgb(indiatif$r,indiatif$g,indiatif$b)
indiatif = indiatif %>% mutate(codes = replace(codes, codes == "#000000", NA))

save(indiatif,smap,file = "latest_state_map.RData")
