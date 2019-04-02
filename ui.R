library(leaflet)
library(tidyverse)
library(plotly)

# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)


navbarPage(title = div(
  div(
    id = "img-id",
    img(src = "coded_JF.png", width = 180)
  ),
  "SPARC VISUALIZER"
),
           id = "nav",
           tabPanel("Interactive map",
                    div(class = "outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width = "100%", height = "100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = 10, right = "auto", bottom = "auto",
                                      width = 400, height = "auto",
                                      
                                      # hr(),
                                      
                                      img(src = "sparc_logo_thin.png", 
                                          width = 250),
                                          # style = "text-align: center;"),
                                      
                                      hr(),
                                      
                                      radioButtons("type_map", label = NA,
                                                   choices = c("PRIORITY", 
                                                               "CARBON OFFSET"),
                                                   inline = TRUE,
                                                   selected = "PRIORITY"),
                                      
                                      hr(),
                                      
                                      # checkboxInput("layA", label = "LAYER 1", value = T),
                                      
                                      # column(6, numericInput("thresholds_A", "Number Colors",
                                      #              min = 2, max = 3, value = 2)),
                                      # 
                                      # column(6, selectInput("color_A", "Palette",
                                      #                       c("Purple to yellow" = "viridis",
                                      #                         "Yellow to black" = "magma",
                                      #                         "Greens" = "Greens",
                                      #                         "Blue to purple" = "BuPu",
                                      #                         "Oranges" = "Oranges",
                                      #                         "Blues" = "Blues",
                                      #                         "Greys" = "Greys",
                                      #                         "Reds" = "Reds",
                                      #                         "Purples" = "Purples"),
                                      #                       selected = "Oranges",
                                      #                       width = 800)),
                                      
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
                                                       
                                                       radioButtons("speed", "Speed of refresh (affects detail)",
                                                                    choices = c("Slow", 
                                                                                "Medium",
                                                                                "Fast"),
                                                                    inline = TRUE,
                                                                    selected = "Slow"),
                                                       
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
