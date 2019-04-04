library(leaflet)
library(tidyverse)
library(plotly)

navbarPage(title = div(
  div(
    id = "img-sparc",
    img(src = "sparc_logo_thin_grey.png", width = 170)
  ),
  div(
    id = "img-id",
    img(src = "coded_JF.png", width = 250)
  ),
  "_________________"
),
           id = "nav",
           tabPanel("Interactive map",
                    div(class = "outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js"),
                          includeScript("lazy_slider.js")  # Slider only react when releasing mouse - taken from https://stackoverflow.com/questions/29222603/shiny-slider-restrict-reaction-to-releasing-left-mouse-button
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width = "100%", height = "100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = 10, right = "auto", bottom = "auto",
                                      width = 400, height = "auto",
                                      
                                      # hr(),
                                      
                                      # img(src = "sparc_logo_thin.png", 
                                      #     width = 250),
                                          # style = "text-align: center;"),
                                      
                                      hr(),
                                      
                                      radioButtons("type_map", label = NA,
                                                   choices = c("PRIORITY", 
                                                               "CARBON OFFSET"),
                                                   inline = TRUE,
                                                   selected = "PRIORITY"),
                                      
                                      radioButtons("speed", "Speed of refresh (affects detail)",
                                                   choices = c("Slow", 
                                                               "Medium",
                                                               "Fast"),
                                                   inline = TRUE,
                                                   selected = "Fast"),
                                      
                                      sliderInput("PA_alpha", "Opacity of Protected Areas:",
                                                  min = 0, max = 1, step = 0.1,
                                                  value = 0.5),
                                      
                                      # hr(),
                                      
                                      conditionalPanel(condition = "input.type_map == 'PRIORITY'",
                                                       sliderInput("thr1", "Solution threshold:",
                                                                   min = 0, max = 100, step = 1,
                                                                   value = 17),
                                                       
                                                       # hr(),
                                                       
                                                       sliderInput("thr2", "Carbon offset",
                                                                   min = 0, max = 100, step = 1,
                                                                   value = tbl %>% 
                                                                     filter(abs(perc_pixels - 17) == min(abs(perc_pixels - 17))) %>% 
                                                                     pull(prop_carbon_strg)),
                                                       
                                                       # hr(),
                                                       
                                                       plotlyOutput("plotly", 
                                                                    width = "90%"),
                                                       
                                                       hr()
                                                       ),
                                      
                                      conditionalPanel(condition = "input.type_map == 'CARBON OFFSET'",
                                                       hr())
                                      
                        )#,
                        
                        # tags$div(id="cite",
                        #          'Coded by ', tags$em('Javier Fajardo'))
                    )
           ),

           tabPanel("About",
                    fluidRow(
                      column(3, 
                             h4("A description of the research and a link to the paper will appear here")
                             )
                      )
                    )
)
