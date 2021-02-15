require(tidyverse)
require(rgdal)
require(sp)
require(sf)
require(mapview)

map_2019 = readOGR("in_dists_2019","in_dist_2019")
state = "GUJARAT"
stp = map_2019[map_2019@data$stname %in% state,]

proj4string(stp) = "+proj=longlat +datum=WGS84"

a = mapView(stp, map.types = c("Esri.WorldImagery","OpenTopoMap"),
            alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
mapshot(a, "GJdistricts.html")
