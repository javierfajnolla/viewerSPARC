library(tidyverse)
library(leaflet)
library(RColorBrewer)
# library(scales)
# library(lattice)
library(dplyr)
library(plotly)



# Reactive values object storage
rvs <- reactiveValues()
rvs$new_thr <- 0.8300012   # INITIAL VALUE CORRESPONDING TO 17%

function(input, output, session) {
  
  ## Interactive Map ###########################################
  output$map <- renderLeaflet({
    # leaflet(TAC_border) %>% 
    leaflet(PAs_shp, 
            options = leafletOptions(zoomControl = FALSE)) %>%
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
  map <- leafletProxy("map", data = PAs_shp) %>% 
    clearControls()  # Can't make the legend to disappear...
  
  
  ################ MAPS TO PLOT AND RESOLUTION
  rvs$plot_solution <- solution
  rvs$plot_carbon <- carbon_stor
  
  observeEvent(input$speed, {
    if (input$speed == "Slow"){
      glue::glue("Now it is in slow") %>% print
      rvs$plot_solution <- solution
      rvs$plot_carbon <- carbon_stor
    } 
    if (input$speed == "Medium"){
      glue::glue("Now it is in medium") %>% print
      rvs$plot_solution <- solution_2
      rvs$plot_carbon <- carbon_stor_2
    } 
    if (input$speed == "Fast"){
      glue::glue("Now it is in fassssst") %>% print
      rvs$plot_solution <- solution_3
      rvs$plot_carbon <- carbon_stor_3
    }
  })  
  ################
  
  observeEvent(
    c(input$type_map, input$PA_alpha), {
    if(input$type_map == "CARBON OFFSET"){
      map %>% 
        clearControls() %>%    # Refreshes the legend
        clearGroup("PAs") %>% 
        clearGroup("solutions_A") %>%
        addRasterImage(carbon_stor,
                       colors = pal,
                       group = "Carbon") %>%
        addLegend(pal = pal,
                  values = values(carbon_stor),
                  title = "Carbon storage",
                  group = "Carbon",
                  position = "topright") %>% 
        addPolygons(color = "#444444", fill = "#444444",
                    weight = 1, smoothFactor = 0.5,
                    opacity = input$PA_alpha, fillOpacity = input$PA_alpha,
                    group = "PAs")
    }
    
    if(input$type_map == "PRIORITY"){
      # React to modification in Threshold 1
      observeEvent(input$thr1, {
        glue::glue("#> thr1 has changed - adjusting thr2") %>% print
        glue::glue("#> thr1 is {input$thr1}") %>% print
        glue::glue("#> thr2 value to replace is {input$thr2}") %>% print
        
        # Select posible values in thr2 corresponding to the change in thr1
        rvs$possibles_table_thr2 <- tbl %>%
          filter(abs(perc_pixels - input$thr1) == min(abs(perc_pixels - input$thr1)))
        rvs$possibles_values_thr2 <- rvs$possibles_table_thr2 %>% 
          pull(prop_carbon_strg)
        glue::glue("#> Possible values thr2:") %>% print
        cat(rvs$possibles_values_thr2, '\n')
        
        # Check in the current value in thr2 is among the possible values
        needs_change <- !(input$thr2 %in% rvs$possibles_values_thr2)
        glue::glue("#> Is it old thr2 among possible values? {needs_change}") %>% print
        
        # If thr2 has to change, trigger the change and selection of new values
        if(needs_change){
          # Check if there is only one possible value or there are more
          if(length(rvs$possibles_values_thr2) > 1){
            # If more than one, select the first
            rvs$possibles_table_thr2 <- rvs$possibles_table_thr2[1,]
            rvs$possibles_values_thr2 <- rvs$possibles_table_thr2 %>% pull(prop_carbon_strg)
          }
          
          # Update the slider
          updateSliderInput(session = session, inputId = "thr2",
                            value = rvs$possibles_values_thr2)
          
          # save the new value to reclasify the raster
          rvs$new_thr <- rvs$possibles_table_thr2 %>% pull(cutoff_sol)
          glue::glue("THE NEW THRESHOLD IS") %>% print
          cat(rvs$new_thr, '\n')
          
        } #else {
        #   # Check if there is only one possible value or there are more
        #   if(length(rvs$possibles_values_thr2) > 1){
        #     # If more than one, leave the one already selected
        #     rvs$possibles_table_thr2 <- rvs$possibles_table_thr2 %>% 
        #       filter(perc_pixels == input$thr1)
        #     # rvs$possibles_values_thr2 <- rvs$possibles_table_thr2 %>% 
        #     #   pull(prop_carbon_strg)
        #   }
        #   
        #   # save the new value to reclasify the raster
        #   rvs$new_thr <- rvs$possibles_table_thr2 %>% pull(cutoff_sol)
        #   glue::glue("THE NEW THRESHOLD IS") %>% print
        #   cat(rvs$new_thr, '\n')
        #   
        # }

      })
      
      
      ##########
      observeEvent(input$thr2, {
        glue::glue("#> thr2 has changed - adjusting thr1") %>% print
        glue::glue("#> thr2 is {input$thr2}") %>% print
        glue::glue("#> thr1 value to replace is {input$thr1}") %>% print
        
        # Select posible values in thr2 corresponding to the change in thr1
        rvs$possibles_table_thr1 <- tbl %>%
          filter(abs(prop_carbon_strg - input$thr2) == min(abs(prop_carbon_strg - input$thr2)))
        rvs$possibles_values_thr1 <- rvs$possibles_table_thr1 %>% 
          pull(perc_pixels)
        glue::glue("#> Possible values thr1:") %>% print
        cat(rvs$possibles_values_thr1, '\n')
        
        # Check in the current value in thr2 is among the possible values
        needs_change <- !(input$thr1 %in% rvs$possibles_values_thr1)
        glue::glue("#> Is it old thr1 among possible values? {needs_change}") %>% print
        
        # If thr2 has to change, trigger the change and selection of new values
        if(needs_change){
          # Check if there is only one possible value or there are more
          if(length(rvs$possibles_values_thr1) > 1){
            # If more than one, select the first
            rvs$possibles_table_thr1 <- rvs$possibles_table_thr1[1,]
            rvs$possibles_values_thr1 <- rvs$possibles_table_thr1 %>% pull(perc_pixels)
          }
          
          # Update the slider
          updateSliderInput(session = session, inputId = "thr1",
                            value = rvs$possibles_values_thr1)
          
          # save the new value to reclasify the raster
          rvs$new_thr <- rvs$possibles_table_thr1 %>% pull(cutoff_sol)
          glue::glue("THE NEW THRESHOLD IS") %>% print
          cat(rvs$new_thr, '\n')
          
        } #else {
        #   # Check if there is only one possible value or there are more
        #   if(length(rvs$possibles_values_thr1) > 1){
        #     # If more than one, leave the one already selected
        #     rvs$possibles_table_thr1 <- rvs$possibles_table_thr1 %>% 
        #       filter(prop_carbon_strg == input$thr2)
        #     # rvs$possibles_values_thr1 <- rvs$possibles_table_thr1 %>% 
        #     #   pull(perc_pixels)
        #   }
        #   
        #   # save the new value to reclasify the raster
        #   rvs$new_thr <- rvs$possibles_table_thr1 %>% pull(cutoff_sol)
        #   glue::glue("THE NEW THRESHOLD IS") %>% print
        #   cat(rvs$new_thr, '\n')
        #   
        # }
        
      })
      
      
      #################
      
      observeEvent(
        c(rvs$new_thr, input$PA_alpha), {
        # Threshold and reclassify
        rcl_tbl <- tibble(from = c(0, rvs$new_thr),
                          to = c(rvs$new_thr, max(values(rvs$plot_solution), na.rm = T)),
                          becomes = c(NA, 1))
        sol_rcl <- rvs$plot_solution %>% 
          reclassify(rcl_tbl)
        cat('\n')
        print(rcl_tbl)
        cat('\n')
        
        # Leaflet
        pal_A <- colorFactor(
          # palette = input$color_A,
          palette = "Oranges",
          levels = c(0.5, 1),
          na.color = "transparent",
          reverse = FALSE)
        
        map %>% 
          clearControls() %>%    # Refreshes the legend
          clearGroup("PAs") %>% 
          clearGroup("Carbon") %>%
          clearGroup("solutions_A") %>%
          addRasterImage(sol_rcl,
                         colors = pal_A,
                         # opacity = input$opacity_A,
                         group = "solutions_A") %>%
          addPolygons(color = "#444444", fill = "#444444",
                      weight = 1, smoothFactor = 0.5,
                      opacity = input$PA_alpha, fillOpacity = input$PA_alpha,
                      group = "PAs")
          
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