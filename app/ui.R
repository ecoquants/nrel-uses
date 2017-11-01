dashboardPage(
  title = 'Ocean Uses for Marine Renewable Energy',
  dashboardHeader(
    title=tagList(icon('bolt'), 'Ocean Uses for Renewables'),
    titleWidth = 300),
  dashboardSidebar(
    width = 300,
    # side menu ----
    sidebarMenu(
      id = 'menu',
      menuItem(
        tagList(h3('Constraints', icon('gears'))), tabName = 'constraints', selected=T, startExpanded=T),
      menuItem(
        tagList(h3('Ocean Uses', icon('flag'))), tabName = 'uses')),
    
    # layer to display ----
    selectInput('select_layer', h4('Layer to display'), list_layers, selected='score'),
    
    # constraints conditional -----
    conditionalPanel(
      condition = "input.menu != 'uses'",
      sliderInput(
        'slider_depth', label=h4('Depth (m)'),
        min=0, max=1000, value=c(0,100)),
      sliderInput(
        'slider_wind', label=h4('Wind (m/s)'),
        min=0, max=12, value=c(7,12)),
      sliderInput(
        'slider_score', label=h4('Ocean Uses Score'),
        min=0, max=max_weight*n_uses, value=max_weight*n_uses/2)),
    
    # uses conditional ----
    conditionalPanel(
      condition = "input.menu == 'uses'",
      selectInput(
        'select_use', label = h4('Select Use'), 
        choices = list_uses,
        selected = 1),
      sliderInput(
        'slider_weight', label=h4('Apply Weight'),
        min=0, max=max_weight, value=max_weight))),
  
  # body ----
  dashboardBody(
    box(
      width=12,
      leafletOutput('map', height = 550))))
