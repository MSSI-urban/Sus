### GREEN ALLEY MODULE #########################################################

# UI ----------------------------------------------------------------------

alley_UI <- function(id) {
  tabItem(tabName = "alley",
          mapdeckOutput(NS(id, "map"), height = "92vh"),
          title_UI(NS(id, "title")),
          right_panel(id, 
                      compare_UI(NS(id, "alley"), var_list_alley),
                      # explore_UI(NS(id, "explore")),
                      dyk_UI(NS(id, "dyk"))),
          legend_bivar_UI(NS(id, "alley")))
}


# Server ------------------------------------------------------------------

alley_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Title bar
    title_server("title", "alley")
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(
        style = map_style, token = token_alley,
        zoom = map_zoom, location = map_location) %>%
        add_polygon(data = green_space,
                    stroke_width = 10, stroke_colour = "#FFFFFF", 
                    fill_colour = "#00FF00", 
                    update_view = FALSE, id = "ID", auto_highlight = TRUE,
                    highlight_colour = "#FFFFFF90")
    })
    
    # Zoom level
    observeEvent(input$map_view_change$zoom, {
      rv_alley$zoom <- case_when(input$map_view_change$zoom >= 14 ~ "DA_2",
                                 input$map_view_change$zoom >= 12 ~ "DA",
                                 input$map_view_change$zoom >= 10.5 ~ "CT",
                                 TRUE ~ "borough")
    })
    
    # Compare panel
    var_right_alley <- compare_server("alley", var_list_alley,
                                       reactive(rv_alley$zoom))
    
    # Data 
    # data_alley <- data_server("alley", reactive("canale_ind"), 
    #                            var_right_alley, reactive(rv_alley$zoom))
    data_alley <- reactive(green_space)
    
    # # Explore panel
    # explore_server("explore", data_canale, reactive("canale_ind"),
    #                var_right_canale, reactive(rv_canale$poly_selected),
    #                reactive(rv_canale$zoom), reactive("CanALE index"))
    
    # Did-you-know panel
    dyk_server("dyk", reactive("alley_ind"), var_right_alley)
    
    # # Left map
    # small_map_server("left", reactive(paste0(
    #   "left_", sub("_2", "", rv_canale$zoom), "_canale_ind")))
    
    # Bivariate legend
    legend_bivar_server("alley", var_right_alley)
    
    # # Update map in response to variable changes or zooming
    # observeEvent({
    #   var_right_canale()
    #   rv_canale$zoom}, {
    #     width <- switch(rv_canale$zoom, "borough" = 100, "CT" = 10, 2)
    #     mapdeck_update(map_id = NS(id, "map")) %>%
    #       add_polygon(
    #         data = data_canale(), stroke_width = width,
    #         stroke_colour = "#FFFFFF", fill_colour = "fill",
    #         update_view = FALSE, id = "ID", auto_highlight = TRUE,
    #         highlight_colour = "#FFFFFF90")
    #   })
    
    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      lst <- jsonlite::fromJSON(input$map_polygon_click)
      if (is.null(lst$object$properties$id)) {
        rv_alley$poly_selected <- NA
      } else rv_alley$poly_selected <- lst$object$properties$id
    })
    
    # # Clear poly_selected on zoom
    # observeEvent(rv_alley$zoom, {rv_alley$poly_selected <- NA},
    #              ignoreInit = TRUE)
    
    # Update map in response to poly_selected change
    observeEvent(rv_alley$poly_selected, {
      if (!is.na(rv_alley$poly_selected)) {
        # width <- switch(rv_canale$zoom, "borough" = 100, "CT" = 10, 2)
        data_to_add <-
          data_alley() %>%
          filter(ID == rv_alley$poly_selected)
        
        mapdeck_update(map_id = NS(id, "map")) %>%
          add_polygon(
            data = data_to_add, stroke_width = 10, stroke_colour = "#000000",
            fill_colour = "#000000", update_view = FALSE,
            layer_id = "poly_highlight", auto_highlight = TRUE,
            highlight_colour = "#FFFFFF90")
      } else {
        mapdeck_update(map_id = NS(id, "map")) %>%
          clear_polygon(layer_id = "poly_highlight")
      }
    })
    
    # # Clear click status if prompted
    # # (Namespacing hardwired to explore module; could make it return a reactive)
    # observeEvent(input$`explore-clear_selection`, {
    #   rv_alley$poly_selected <- NA})
    
  })
}
