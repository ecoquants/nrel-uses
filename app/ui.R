dashboardPage(
  title = 'Competing Uses with Marine Renewable Energy',
  dashboardHeader(
    title=tagList(icon('bolt'), 'Competing Uses with Marine Renewable Energy'),
    titleWidth = 500),
  dashboardSidebar(
    width = 250,
    
    # layer to display ----
    selectInput('select_layer', h4('Layer to display'), list_layers, selected='score'),
    
    # side menu ----
    sidebarMenu(
      id = 'menu',
      
      menuItem(
        tagList(h3(icon('gears'), 'Constraints')), tabName = 'constraints', selected=T, startExpanded=F,
        sliderInput(
          'slider_score', label=h4('Competing Uses Score'),
          min=0, max=ceiling(max_score), step=0.5, value=round(max_score/2)),
        sliderInput(
          'slider_depth', label=h4('Depth (m)'),
          min=0, max=1000, step=20, value=c(0,1000)),
        sliderInput(
          'slider_wind', label=h4('Wind (m/s)'),
          min=0, max=12, step=1, value=c(0,12)),
        sliderInput(
          'slider_cellsize', 'Cell width (km) [→ faster]', 
          min=2, max=10, step=2, value=2)),
      
      menuItem(
        tagList(h3(icon('flag'), 'Competing Uses')), tabName = 'uses',
        selectInput(
          'select_use', label = h4('Select Use'), 
          choices = list_uses,
          selected = 1),
        sliderInput(
          'slider_weight', label=h4('[TODO: Apply Weight]'),
          min=0, max=max_weight, step=0.1, value=max_weight)))),
  
  # body ----
  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    leafletOutput('map', height = 550)))
