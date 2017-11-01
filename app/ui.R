dashboardPage(
  title = 'Competing Uses with Marine Renewable Energy',
  dashboardHeader(
    title=tagList(icon('bolt'), 'Competing Uses with Marine Renewable Energy'),
    titleWidth = 450),
  dashboardSidebar(
    width = 250,
    # side menu ----
    sidebarMenu(
      id = 'menu',
      menuItem(
        tagList(h3(icon('gears'), 'Parameters')), tabName = 'constraints', selected=T, startExpanded=T),
      menuItem(
        tagList(h3(icon('flag'), 'Competing Uses')), tabName = 'uses')),
    
    # layer to display ----
    selectInput('select_layer', h4('Layer to display'), list_layers, selected='score'),
    
    # constraints conditional -----
    box(
      style = "background-color: #1e282c;", width=12,
      conditionalPanel(
        condition = "input.menu != 'uses'",
        'Parameters constraining viable renewable energy:',
        sliderInput(
          'slider_depth', label=h4('Depth (m)'),
          min=0, max=1000, value=c(0,100)),
        sliderInput(
          'slider_wind', label=h4('Wind (m/s)'),
          min=0, max=12, value=c(7,12)),
        sliderInput(
          'slider_score', label=h4('Competing Uses Score'),
          min=0, max=max_weight*n_uses, value=max_weight*n_uses/2)),
      
      # uses conditional ----
      conditionalPanel(
        condition = "input.menu == 'uses'",
        'Competing uses to weight and score:',
        selectInput(
          'select_use', label = h4('Select Use'), 
          choices = list_uses,
          selected = 1),
        sliderInput(
          'slider_weight', label=h4('Apply Weight'),
          min=0, max=max_weight, value=max_weight))
    )),
  
  # body ----
  dashboardBody(
    box(
      width=12,
      leafletOutput('map', height = 550))))
