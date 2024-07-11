server <- function(session, input, output) {
  
  # display all lgas with their type (urban/regional)
  output$all_lgas <- renderDataTable({
    DT::datatable(lga_type, selection = "none", 
                  options = list(lengthMenu = 
                                   list(c(15, 30, 50, -1), c("15", "30", "50", "All"))))
  })
  
  # gets the top n LGAs based on user's selection 
  getTopData <- reactive({
    total %>%
      filter(year == input$year1 & stat == input$stat_type1) %>%
      arrange(desc(value)) %>%
      select(LGA, value) %>%
      head(input$top_n)
  })
  
  # gets the bottom n LGAs based on user's selection
  getBotData <- reactive({
    total %>%
      filter(year == input$year1 & stat == input$stat_type1) %>%
      arrange(value) %>%
      select(LGA, value) %>%
      head(input$bot_n)
  })
  
  # gets the trends of a specific LGA and the stats type based on user's selection
  getLGATrendData <- reactive({
    total %>%
      filter(LGA == input$lga_trend & stat == input$stat_type_trend)
  })
  
  # gets the data used for choropleth map
  getChoroMapData <- reactive({
    # take dependency on show map button
    input$show_map
    
    # do not update when the user select inputs without clicking the "show map" button
    # isolate input$stat_type2 and input$year2
    res <- isolate(total %>%
      filter(stat == input$stat_type2 & year == input$year2))
    lgastats <- merge(viclga, res, by.x = "LGA_NAME", by.y = "LGA", duplicateGeoms=TRUE) # to merge a spatial df, add duplicateGeoms = TRUE
    lgastats
  })
  
  # display top n LGA results
  output$top <- renderDataTable(
    DT::datatable(getTopData(), options = list(paging = FALSE, searching = FALSE, ordering = FALSE),
                  selection = "none")
  )
  
  # display bottom n LGA results
  output$bottom <- renderDataTable(
    DT::datatable(getBotData(), options = list(paging = FALSE, searching = FALSE, ordering = FALSE),
                  selection = "none")
  )
  
  # display the trends for a specific LGA and stat type
  output$lga_trend_plot <- renderGirafe({
    
    p <- ggplot(getLGATrendData()) + 
      aes(x = year, y = value, group = LGA) + 
      geom_line() + 
      geom_point() +
      # scale_y_continuous(limits = c(min(a1$value), max(a1$value))) +
      theme(panel.background = element_blank(),
            panel.grid.major.y = element_line(color = '#e2e2e2'),
            axis.title = element_text(size = 12),
            plot.title = element_text(hjust = 0.5), # adjust title position
            axis.title.y = element_blank()) +
      ggtitle(paste(input$stat_type_trend, "in", input$lga_trend, "from 2015 to 2020")) + 
      geom_text_interactive(aes(label = value, hjust = 1.0, vjust = -0.7), size = 3) # display values on the graph
    
    girafe(ggobj = p, height_svg = 4)
  })
  
  # display the map title
  output$map_title <- renderText({
    paste(input$stat_type2, "in Victoria during", input$year2)
  })
  
  # display the choropleth map
  output$choropleth_map <- renderLeaflet({
    
    # default value for action button is 0, this condition prevents the app from rendering map for the first time 
    # without clicking the action button
    if (input$show_map == 0) return ()
    
    data <- getChoroMapData()
    
    # generates pop up for each LGA
    popup <- sprintf(
      "LGA: %s<br/>%s: %s",
      data$LGA_NAME, input$stat_type2, data$value
    ) %>% lapply(htmltools::HTML)
    
    # assign color palette
    palette <- colorBin(palette="PuBuGn", domain=data$value, 
                        na.color="transparent")
    
    
    # make map
    leaflet(data, options = leafletOptions(minZoom = 6)) %>%
      setView(145.4691, -38, zoom = 8) %>%
      addTiles(providers$CartoDB) %>%
      addPolygons(
        fillColor = ~palette(value),
        layerId = ~LGA_NAME, # can be the reference for observeEvent
        dashArray = 3,
        weight = 2,
        fillOpacity = 0.6,
        color = "white",
        highlightOptions = highlightOptions( # highlights options when the user hovers over an LGA
          weight = 4,
          color = "grey",
          dashArray = "",
          bringToFront = TRUE
        ),
        label = popup,
        labelOptions = labelOptions(
          textsize = "11px"
        )
      ) %>%
      addLegend(pal = palette, values = ~value, title = input$stat_type2,
                position = "topright")
    
  })
  
  # when the user clicks an LGA on the map, automatically directs the user to another tab
  # which presents the trend based on the clicked LGA and selected stat type
  observeEvent(input$choropleth_map_shape_click, {
    p <- input$choropleth_map_shape_click
    if (is.null(p$id)) return ()
    
    # update input to clicked LGA and selected stat type
    updateSelectInput(session, "lga_trend", selected = p$id)
    updateSelectInput(session, "stat_type_trend", selected = input$stat_type2)
    
    # switch tabs
    updateTabItems(session, "sidebar", selected = "trend")
    updateTabItems(session, "box4", selected = "by LGA")
  })
  
  # display the trend data by gif files based on user's selection
  output$vic_trends <- renderImage({
    filename <- normalizePath(file.path(paste0("animations/", input$lga_type_map, "_",
                                               saveAbbr(input$stat_type_map), ".gif")))
    list(src = filename)
  }, deleteFile = FALSE)
  
  
  
}