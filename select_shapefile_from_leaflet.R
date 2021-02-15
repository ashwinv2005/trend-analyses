#https://github.com/r-spatial/mapedit/commit/2de4bb702f41c3bd04cff0fb8cab126e0d3b89f9

require(shiny)
require(sp)
require(rgdal)
require(leaflet)
require(leaflet.extras)

map_2019 = readOGR("in_dists_2019","in_dist_2019")
smap_2019 = readOGR("in_states_2019","in_states_2019")

ui <- fluidPage(
  
  textOutput("text"),leafletOutput("mymap"),
  downloadButton('downloadData', 'Download Shp'))

server<- function(input, output, session) {
  
  require(sp)
  require(rgdal)
  require(leaflet)
  require(leaflet.extras)
  
  popup <- paste0("<strong>District: </strong>", 
                  map_2019@data$dtname)
  
  output$mymap <- renderLeaflet({
    
    leaflet("mymap") %>%
      addProviderTiles(providers$OpenTopoMap, #map type or map theme. -default($Stame.TonerLite)
                       options = providerTileOptions(noWrap = TRUE)) %>% 
      setView(78, 23,
              zoom = 3.9) %>% 
      addPolygons(data = smap_2019, 
                  fillColor= NA,
                  fillOpacity = 0, 
                  weight = 2, 
                  color = "black") %>%
      addPolygons(data = map_2019, 
                  fillColor= NA,
                  fillOpacity = 0, 
                  weight = 1, 
                  color = "black",
                  popup = popup) %>%
      addDrawToolbar(targetGroup = "drawnPoly", 
                     rectangleOptions = F, 
                     polylineOptions = F, 
                     markerOptions = F, 
                     editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()), 
                     circleOptions=F,
                     polygonOptions=drawPolygonOptions(showArea=TRUE, repeatMode=F  , shapeOptions=drawShapeOptions( fillColor="red",clickable = TRUE))) %>%
      
      addStyleEditor()
    
  })
  
  
  
  latlongs<-reactiveValues()   #temporary to hold coords
  latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0))
  
  #########
  #empty reactive spdf
  value<-reactiveValues()
  SpatialPolygonsDataFrame(SpatialPolygons(list()), data=data.frame (notes=character(0), stringsAsFactors = F))->value$drawnPoly
  
  #fix the polygon to start another
  
  observeEvent(input$mymap_draw_new_feature, {
    
    coor<-unlist(input$mymap_draw_new_feature$geometry$coordinates)
    
    Longitude<-coor[seq(1,length(coor), 2)] 
    
    Latitude<-coor[seq(2,length(coor), 2)]
    
    isolate(latlongs$df2<-rbind(latlongs$df2, cbind(Longitude, Latitude)))
    
    poly<-Polygon(cbind(latlongs$df2$Longitude, latlongs$df2$Latitude))
    polys<-Polygons(list(poly),    ID=input$mymap_draw_new_feature$properties$`_leaflet_id`)
    spPolys<-SpatialPolygons(list(polys))
    
    
    #
    value$drawnPoly<-rbind(value$drawnPoly,SpatialPolygonsDataFrame(spPolys, 
                                                                    data=data.frame(notes=NA, row.names=
                                                                                      row.names(spPolys))))
    
    ###plot upon ending draw
    observeEvent(input$mymap_draw_stop, {
      
      #replot it - take off the DrawToolbar to clear the features and add it back and use the values from the SPDF to plot the polygons
      leafletProxy('mymap') %>%  removeDrawToolbar(clearFeatures=TRUE) %>% removeShape('temp') %>% clearGroup('drawnPoly') %>% addPolygons(data=value$drawnPoly, popup="poly",   group='drawnPoly', color="blue", layerId=row.names(value$drawnPoly)) %>% 
        
        addDrawToolbar(targetGroup = "drawnPoly", 
                       rectangleOptions = F, 
                       polylineOptions = F, 
                       markerOptions = F, 
                       editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()), 
                       circleOptions=F,
                       polygonOptions=drawPolygonOptions(showArea=TRUE, repeatMode=F  , shapeOptions=drawShapeOptions( fillColor="red",clickable = TRUE)))
      
    })
    
    latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0))   #clear df
    
  })
  
  ########################
  ### edit polygons / delete polygons
  
  observeEvent(input$mymap_draw_edited_features, {
    
    f <- input$mymap_draw_edited_features
    
    coordy<-lapply(f$features, function(x){unlist(x$geometry$coordinates)})
    
    Longitudes<-lapply(coordy, function(coor) {coor[seq(1,length(coor), 2)] })
    
    Latitudes<-lapply(coordy, function(coor) { coor[seq(2,length(coor), 2)] })
    
    polys<-list()
    for (i in 1:length(Longitudes)){polys[[i]]<- Polygons(
      list(Polygon(cbind(Longitudes[[i]], Latitudes[[i]]))), ID=f$features[[i]]$properties$layerId
    )}
    
    spPolys<-SpatialPolygons(polys)
    
    
    SPDF<-SpatialPolygonsDataFrame(spPolys, 
                                   data=data.frame(notes=value$drawnPoly$notes[row.names(value$drawnPoly) %in% row.names(spPolys)], row.names=row.names(spPolys)))
    
    value$drawnPoly<-value$drawnPoly[!row.names(value$drawnPoly) %in% row.names(SPDF),]
    value$drawnPoly<-rbind(value$drawnPoly, SPDF)
    
  })
  
  observeEvent(input$mymap_draw_deleted_features, { 
    
    f <- input$mymap_draw_deleted_features
    
    ids<-lapply(f$features, function(x){unlist(x$properties$layerId)})
    
    
    value$drawnPoly<-value$drawnPoly[!row.names(value$drawnPoly) %in% ids ,]
    
  }) 
  
  
  
  #write the polys to .shp
  output$downloadData<-downloadHandler(
    
    filename = 'shpExport.zip',
    content = function(file) {
      if (length(Sys.glob("shpExport.*"))>0){
        file.remove(Sys.glob("shpExport.*"))
      }
      
      proj4string(value$drawnPoly)<-"+proj=longlat +datum=WGS84"
      writeOGR(value$drawnPoly, dsn="shpExport.shp", layer="shpExport", driver="ESRI Shapefile")
      zip(zipfile='shpExport.zip', files=Sys.glob("shpExport.*"))
      file.copy("shpExport.zip", file)
      if (length(Sys.glob("shpExport.*"))>0){
        file.remove(Sys.glob("shpExport.*"))
      }
    }
  )
  
}

shinyApp(ui=ui,server=server)
