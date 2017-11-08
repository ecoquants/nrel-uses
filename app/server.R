shinyServer(function(input, output, session) {
  
  # leaflet ----
  output$map <- renderLeaflet({
    
    #browser()
    r_depth = raster(s_constraints, 1)
    r_depth_gcs = projectRaster(r_depth, crs=leaflet:::epsg4326)
    bb = bbox(r_depth_gcs)
    
    leaflet(
      options = leafletOptions(attributionControl=F)) %>%
      addProviderTiles(providers$Stamen.TonerLite, group = 'B&W') %>%
      addProviderTiles(providers$Esri.OceanBasemap, group = 'Ocean') %>%
      addLayersControl(
        baseGroups = c('B&W','Ocean'),
        overlayGroups = c('Layer', 'Constraints'),
        position='topright') %>%
      addScaleBar('bottomleft') %>%
      fitBounds(bb[1,'min'], bb[2,'min'], bb[1,'max'], bb[2,'max'])
    
  })
  
  # leafletProxy ----
  observe({
    
    # layer
    if (input$select_layer=='score'){
      r = score
      r[r==0] = NA
      title = 'Competing Uses Score'
    } else {
      tif = input$select_layer
      lyr = str_replace(tif, '.*/(.*)\\.tif$', '\\1')
      r = s_layers[[lyr]]
      title = names(unlist(list_layers))[unlist(list_layers)==tif] %>%
        str_replace('^.*\\.(.*)','\\1')
    }
    
    # mask
    #r_depth = raster(s_constraints, sprintf('%s_depth', ter))
    #r_wind  = raster(s_constraints, sprintf('%s_90mwindspeed', ter))
    r_depth = raster(s_constraints, 1)
    r_wind  = raster(s_constraints, 2)
    r_mask = 
      score   > input$slider_score |
      r_depth < input$slider_depth[1] | 
      r_depth > input$slider_depth[2] |
      r_wind  > input$slider_wind[2] |
      r_wind < input$slider_wind[1]
    r_mask[r_mask == 0] = NA # plot(r_mask) #browser()
    
    # aggregate cells
    if (input$slider_cellsize > 2){
      r      = aggregate(r     , fact=input$slider_cellsize/2)
      r_mask = aggregate(r_mask, fact=input$slider_cellsize/2)
    }
    
    # palette
    if (is.na(minValue(r)) || minValue(r) == maxValue(r)){
      vals = c(0,1,NA) # unique(getValues(r)) showing 4 1's?
      pal = colorFactor(
        palette = 'Reds',
        domain  = vals, na.color="#00000000")
      vals = c(1,NA)
    } else {
      vals = getValues(r)
      pal = colorNumeric(
        palette = 'Spectral', reverse=T,
        domain  = vals, na.color="#00000000")
    }
    
    leafletProxy('map') %>%
      clearGroup('Layer') %>%
      clearGroup('Constraints') %>%
      clearControls() %>%
      addRasterImage(
        r, project=F, group='Layer',
        colors=pal, opacity=0.8) %>%
      addRasterImage(
        r_mask, project=F, group='Constraints',
        colors='black', opacity=0.7) %>%
      addLegend('bottomright', pal, vals, title=title, opacity=0.8) %>%
      addLayersControl(
        baseGroups = c('B&W','Ocean'),
        overlayGroups = c('Layer', 'Constraints'),
        position='topright')
    })
  
  # map_click ----
  observeEvent(input$map_click, {
    
    #browser()
    #msg(sprintf('pryr::mem_used(): %0.2f MB', pryr::mem_used() / (1000*1000)))
    #r_depth = raster(s_constraints, sprintf('%s_depth', ter))
    r_depth = raster(s_constraints, 1)
    
    xy = data_frame(lon=input$map_click$lng, lat=input$map_click$lat)
    coordinates(xy) = ~ lon + lat
    proj4string(xy) = leaflet:::epsg4326
    suppressWarnings({
      #xy = spTransform(xy, leaflet:::epsg3857)
      xy_r = extract(r_depth, xy, cellnumbers=T)
    })
    xy_idx   = xy_r[,1]
    xy_depth = xy_r[,2]

    leafletProxy('map') %>%
      clearGroup('click')
        
    if (!is.na(xy_depth)){
      txt = paste(c(
        sprintf('lon, lat: %0.4f, %0.4f', input$map_click$lng, lat=input$map_click$lat), 
        '<font size="4">Parameters</font>',
        sprintf('Competing Uses Score: %0.1f', score[xy_idx]),
        sprintf('%s: %0.1f', c('Depth (m)', 'Wind (m/s)'), s_constraints[xy_idx] %>% as.numeric()),
        '<font size="4">Competing Uses</font>',
        sprintf(
          '%s: %g', 
          names(unlist(list_uses)) %>% 
            str_replace('^.*\\.(.*)','\\1'), 
          s_uses[xy_idx] %>% 
            as.numeric())[!is.na(s_uses[xy_idx] %>% as.numeric())]),
        collapse='<br/>') # cat(txt)

      leafletProxy('map') %>%
        addPopups(lng=input$map_click$lng, lat=input$map_click$lat, popup=txt, group='click')
    }
    
    })
  
  
  
})