shinyServer(function(input, output, session) {
  
  # map ----
  output$map <- renderLeaflet({
    
    if (input$select_layer=='score'){
      r = score
      r[r==0] = NA
      title = 'Competing Use Score'
    } else {
      #browser()
      tif = input$select_layer
      lyr = str_replace(tif, '.*/(.*)\\.tif$', '\\1')
      r = s_layers[[lyr]]
      title = names(unlist(list_layers))[unlist(list_layers)==tif] %>%
        str_replace('^.*\\.(.*)','\\1')
    }
    
    #browser()
    if (minValue(r) == maxValue(r)){
      #pal = 'red'
      #browser() 
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
    
    leaflet() %>%
      addProviderTiles('Stamen.TonerLite', group = 'B&W') %>%
      addRasterImage(
        r, project=T,
        colors=pal, opacity=0.8) %>%
      addLegend('bottomright', pal, vals, title=title, opacity=0.8) %>%
      addScaleBar('bottomleft')
    
  })
})