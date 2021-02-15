require(rgeos)
require(raster)
diff = gDifference(gridmapg3,gBuffer(indiamap, byid=TRUE, width=0))
diff = gBuffer(diff, byid=TRUE, width=0)
g3clip = gridmapg3 - diff

save(g2clip,g3clip,file = "clips.RData")


