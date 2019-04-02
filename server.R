library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(plotly)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
# set.seed(100)

# leaflet(options = leafletOptions(control = "topright")) %>%
#   addTiles()

# Reactive values object storage
rvs <- reactiveValues()

function(input, output, session) {
  
  ## Interactive Map ###########################################
  output$map <- renderLeaflet({
    # leaflet(TAC_border) %>% 
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        htmlwidgets::onRender("function(el, x) {    
                                L.control.zoom({ position: 'topright' }).addTo(this)
                              }") %>% # This code just moves the zoom buttons to the right
      # addTiles() %>%                                   # Default Map - city names
      # addProviderTiles("CartoDB.PositronNoLabels") %>%   # Clean Map
      # addProviderTiles("CartoDB.Positron") %>%   # Clean Map alternative with few names
      addProviderTiles("Esri.WorldPhysical") %>%   # Physical Map
      # addProviderTiles(providers$Stamen.TonerLite,
      #                  options = providerTileOptions(noWrap = TRUE)) %>% 
      fitBounds(lng1 = (extent(solution)[1] + extent(solution)[1]),    # zoom to raster extent
                lat1 = extent(solution)[3],
                lng2 = extent(solution)[2],
                lat2 = extent(solution)[4]) #%>%
  })
  
  # Create map proxy to make further changes to existing map
  map <- leafletProxy("map") %>% 
    clearControls()  # Can't make the legend to disappear...
  
  
  observeEvent(input$type_map, {
    if(input$type_map == "CARBON OFFSET"){
      map %>% 
        clearControls() %>%    # Refreshes the legend
        clearGroup("solutions_A") %>%
        addRasterImage(carbon_stor,
                       colors = pal,
                       # opacity = input$opacity_A,
                       group = "Carbon") %>%
        addLegend(pal = pal,
                  values = values(carbon_stor),
                  title = "Carbon storage",
                  group = "Carbon",
                  # # layerId = "raster",
                  # # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                  position = "topright")
    }
    
    if(input$type_map == "PRIORITY"){
      # React to modification in Threshold 1
      observeEvent(input$thr1, {
        glue::glue("#> thr1 has changed - adjusting thr2") %>% print
        
        # Select posible values in thr2 corresponding to the change in thr1
        rvs$possibles_table_thr2 <- tbl %>%
          filter(abs(perc_pixels - input$thr1) == min(abs(perc_pixels - input$thr1)))
        rvs$possibles_values_thr2 <- rvs$possibles_table_thr2 %>% 
          pull(prop_carbon_strg)
        
        # Check in the current value in thr2 is among the possible values
        needs_change <- !(input$thr2 %in% rvs$possibles_values_thr2)
        
        # If thr2 has to change, trigger the change and selection of new values
        if(needs_change){
          # Check if there is only one possible value or there are more
          if(length(rvs$possibles_values_thr2) > 1){
            # If more than one, select the first
            rvs$possibles_values_thr2 <- rvs$possibles_values_thr2[1]
            rvs$possibles_table_thr2 <- rvs$possibles_table_thr2[1,]
          }
          
          # Update the slider
          updateSliderInput(session = session, inputId = "thr2",
                            value = rvs$possibles_values_thr2)
          
        }
        
        # save the new value to reclasify the raster
        rvs$new_thr <- rvs$possibles_table_thr2 %>% pull(cutoff_sol)
      })
      
      observeEvent(input$thr2, {
        glue::glue("#> thr2 has changed - adjusting thr1") %>% print
        
        # Select posible values in thr2 corresponding to the change in thr1
        rvs$possibles_table_thr1 <- tbl %>%
          filter(abs(prop_carbon_strg - input$thr2) == min(abs(prop_carbon_strg - input$thr2)))
        rvs$possibles_values_thr1 <- rvs$possibles_table_thr1 %>% 
          pull(perc_pixels)
        
        # Check in the current value in thr2 is among the possible values
        needs_change <- !(input$thr1 %in% rvs$possibles_values_thr1)
        
        # If thr2 has to change, trigger the change and selection of new values
        if(needs_change){
          # Check if there is only one possible value or there are more
          if(length(rvs$possibles_values_thr1) > 1){
            # If more than one, select the first
            rvs$possibles_values_thr1 <- rvs$possibles_values_thr1[1]
            rvs$possibles_table_thr1 <- rvs$possibles_table_thr1[1,]
          }
          
          # Update the slider
          updateSliderInput(session = session, inputId = "thr1",
                            value = rvs$possibles_values_thr1)
          
        }
        
        # save the new value to reclasify the raster
        rvs$new_thr <- rvs$possibles_table_thr1 %>% pull(cutoff_sol)
      })
      
      rvs$plot_solution <- solution
      
      observeEvent(input$speed, {
        if (input$speed == "Slow"){
          glue::glue("Now it is in slow") %>% print
          rvs$plot_solution <- solution
        } 
        if (input$speed == "Medium"){
          glue::glue("Now it is in medium") %>% print
          rvs$plot_solution <- solution_2
        } 
        if (input$speed == "Fast"){
          glue::glue("Now it is in fassssst") %>% print
          rvs$plot_solution <- solution_3
        }
      })  
      
      observeEvent(rvs$new_thr, {
        # Threshold and reclassify
        # sel_thr <- rvs$sel_row %>% pull(cutoff_sol)
        glue::glue("{rvs$new_thr}") %>% print
        
        sol_rcl <- rvs$plot_solution %>% 
          reclassify(tibble(from = c(0, rvs$new_thr),
                            to = c(rvs$new_thr, max(values(rvs$plot_solution), na.rm = T)),
                            becomes = c(NA, 1)))
        
        # Leaflet
        pal_A <- colorFactor(
          # palette = input$color_A,
          palette = "Oranges",
          levels = c(0.5, 1),
          na.color = "transparent",
          reverse = FALSE)
        
        map %>% 
          clearControls() %>%    # Refreshes the legend
          clearGroup("Carbon") %>%
          clearGroup("solutions_A") %>%
          addRasterImage(sol_rcl,
                         colors = pal_A,
                         # opacity = input$opacity_A,
                         group = "solutions_A") #%>%
        # addLegend(pal = pal_A,
        #           values = c(NA, 1),
        #           group = "solutions_A",
        #           layerId = "raster",
        #           labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
        #           position = "topright")
      })
      
      ### Plotly graph
      observeEvent(input$thr1, {
        plot1 <- tbl %>% 
          mutate(perc_pixels = perc_pixels,
                 prop_carbon_strg = prop_carbon_strg) %>% 
          ggplot +
          geom_area(aes(x = perc_pixels, y = prop_carbon_strg)) +
          geom_area(data = tbl %>% 
                      mutate(perc_pixels = perc_pixels,
                             prop_carbon_strg = prop_carbon_strg) %>% 
                      filter(perc_pixels <= input$thr1),
                    aes(x = perc_pixels, y = prop_carbon_strg),
                    fill = "#E45621") +
          xlab("Proportion of study area protected") +
          ylab("Proportion of carbon storage saved") +
          theme_minimal()
        
        output$plotly <- renderPlotly(
          plotly::ggplotly(plot1)
        )
      })
    }
  })

}