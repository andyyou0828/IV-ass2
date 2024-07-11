# runApp("D:/IV/assignment2")

dashboardPage(
  
  dashboardHeader(title = "Total Income in Victoria and Local Government Areas (LGA)", titleWidth = 600),
  
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("LGA", tabName = "lga", icon = icon("earth-oceania")),
                menuItem("Ranks", tabName = "ranks", icon = icon("ranking-star")),
                conditionalPanel("input.sidebar == 'ranks'", selectInput(inputId = "stat_type1", label = "select the stats type",
                            choices = stat_types)),
                conditionalPanel("input.sidebar == 'ranks'", selectInput(inputId = "year1", label = "select the year to compare",
                            choices = years)),
                menuItem("Choropleth Map", tabName = "map", icon = icon("map")),
                conditionalPanel("input.sidebar == 'map'", selectInput(inputId = "stat_type2", 
                                  label = "select the stats type", choices = stat_types)),
                conditionalPanel("input.sidebar == 'map'", selectInput(inputId = "year2", 
                                  label = "select the year", choices = years)),
                conditionalPanel("input.sidebar == 'map'", actionButton("show_map", "Show map")),
                menuItem("Trends", tabName = "trend", icon = icon("arrow-trend-up"))
                )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "lga",
              
              tabBox(id = "box1", width = 14,
                     tabPanel("Urban LGA", fluidRow(tags$div(align = "center", column(
                       width = 12, tags$img(src="urban.jpg",width = 850, height = 500),
                       tags$br(),
                       h2("test"),
                       tags$a("source:https://responsiblegambling.vic.gov.au/reducing-harm/gamblers-help-services/", align = "center")
                     )))),
                     tabPanel("Regional LGA", fluidRow(tags$div(align = "center", column(
                       width = 12, tags$img(src="rural.png",width = 850, height = 500),
                       tags$br(),
                       tags$a("source:https://www.audit.vic.gov.au/report/local-government-and-economic-development?section=", align = "center")
                     )))),
                     tabPanel("All LGAs", dataTableOutput("all_lgas"))
                     )
              ),
      
      tabItem(tabName = "ranks",
              
              tabBox(id = "box2", width = 12,
                     tabPanel("Top", 
                              fluidRow(
                                tags$div(
                                  align = "center",
                                  sliderInput(inputId = "top_n", label = "top n",
                                              min = 5, max = 30, value = 5),
                                  column(dataTableOutput("top"), width = 12)
                                )
                              )
                            ),
                     tabPanel("Bottom", 
                              fluidRow(
                                tags$div(
                                  align = "center",
                                  sliderInput(inputId = "bot_n", label = "bottom n",
                                              min = 5, max = 30, value = 5),
                                  column(dataTableOutput("bottom"), width = 12)
                                )
                              )
                          )
                     )
              ),
      
      tabItem(tabName = "map",
              box(width = 14, 
                  fluidRow(
                    tags$div(
                      align = "center",
                      column(textOutput("map_title"), width = 12)
                    ),
                    column(withSpinner(leafletOutput("choropleth_map"), color = "#6c3082"), width = 12),
                    tags$head(tags$style("#map_title{font-size: 20px;
                                         font-style: italic;
                                         padding-bottom: 10px;}"))
                  )
              )
        ),
      
      tabItem(tabName = "trend", 
              
              tabBox(id = "box4", width = 14,
                     tabPanel("by LGA", fluidRow(
                       column(selectInput(inputId = "lga_trend", label = "select LGA", choices = lgas), width = 6),
                       column(selectInput(inputId = "stat_type_trend", label = "select stat type", choices = stat_types), width = 6),
                       column(withSpinner(girafeOutput("lga_trend_plot", width = "100%", height = "400px")), width = 12)
                     )),
                     tabPanel("whole Victoria", fluidRow(
                       tags$div(
                         align = "center",
                         column(radioButtons(inputId = "lga_type_map", label = "select the LGA type",
                                     choices = c("urban", "regional"), inline = TRUE), width = 5),
                         column(selectInput(inputId = "stat_type_map", label = "select the stat type",
                                     choices = stat_types), width = 6),
                         column(withSpinner(imageOutput("vic_trends", width = "100%")), width = 12)
                       )
                     )))
    )
  )
  )
  
)
