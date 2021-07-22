### CRASH MODULE ###############################################################

# UI ----------------------------------------------------------------------

crash_UI <- function(id) {
  tabItem(tabName = "crash",
          mapdeckOutput(NS(id, "map"), height = "92vh"),
          title_UI(NS(id, "title"),
                   select_var_UI(NS(id, "left_1"), var_list_left_crash_1),
                   select_var_UI(NS(id, "left_2"), var_list_left_crash_2),
                   slider_UI(NS(id, "left"), crash_slider$min, crash_slider$max, 
                             crash_slider$interval, crash_slider$init)
                   ),
          right_panel(id, 
                      compare_UI(NS(id, "crash"), var_list_right_crash),
                      # explore_UI(NS(id, "explore")),
                      # dyk_UI(NS(id, "dyk"))
                      ),
          legend_bivar_UI(NS(id, "crash")))
}


# Server ------------------------------------------------------------------

crash_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Title bar
    title_server("title", "alley")
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(
        style = map_style, token = token_crash,
        zoom = map_zoom, location = map_location) %>%
        add_polygon(data = borough %>%
                      mutate(group = paste(crash_ped_2019_prop_area_q3, "- 1")) %>%
                      left_join(colour_borough, by = "group"),
                    stroke_width = 100, stroke_colour = "#FFFFFF", fill_colour = "fill", 
                    update_view = FALSE, id = "ID", auto_highlight = TRUE,
                    highlight_colour = "#FFFFFF90")
    })
    
    # Zoom level
    observeEvent(input$map_view_change$zoom, {
      rv_crash$zoom <- case_when(input$map_view_change$zoom >= 14 ~ "DA_2",
                                 input$map_view_change$zoom >= 12 ~ "DA",
                                 input$map_view_change$zoom >= 10.5 ~ "CT",
                                 TRUE ~ "borough")
    })
    
    # Compare panel
    var_right_crash <- compare_server("crash", var_list_right_crash,
                                      reactive(rv_crash$zoom))
    
    # Left variable servers
    var_left_crash_1 <- select_var_server("left_1", var_list_left_crash_1)
    var_left_crash_2 <- select_var_server("left_2", var_list_left_crash_2)
    
    # Get time from slider
    time <- slider_server("left")

    # Construct left variable string
    var_left_crash <- reactive(
      stringr::str_remove(paste(
        "crash", 
        var_left_crash_1(), 
        time(), 
        var_left_crash_2(), 
        sep = "_"), "_ $")
    )
    
    # Data 
    data_crash <- data_server("crash", var_left_crash, var_right_crash, 
                              reactive(rv_crash$zoom))

    # # Explore panel
    # explore_server("explore", data_canale, reactive("canale_ind"),
    #                var_right_canale, reactive(rv_canale$poly_selected),
    #                reactive(rv_canale$zoom), reactive("CanALE index"))
    
    # Did-you-know panel
    # dyk_server("dyk", reactive("alley_ind"), var_right_alley)
    
    # # Left map
    # small_map_server("left", reactive(paste0(
    #   "left_", sub("_2", "", rv_canale$zoom), "_canale_ind")))
    
    # Bivariate legend
    legend_bivar_server("crash", var_right_crash)
    
    # # Update map in response to variable changes or zooming
    observeEvent({
      var_left_crash()
      var_right_crash()
      rv_crash$zoom}, {
        width <- switch(rv_crash$zoom, "borough" = 100, "CT" = 10, 2)
        mapdeck_update(map_id = NS(id, "map")) %>%
          add_polygon(
            data = data_crash(), stroke_width = width,
            stroke_colour = "#FFFFFF", fill_colour = "fill",
            update_view = FALSE, id = "ID", auto_highlight = TRUE,
            highlight_colour = "#FFFFFF90")
      })
    
    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      lst <- jsonlite::fromJSON(input$map_polygon_click)
      if (is.null(lst$object$properties$id)) {
        rv_crash$poly_selected <- NA
      } else rv_crash$poly_selected <- lst$object$properties$id
    })
    
    # # Clear poly_selected on zoom
    observeEvent(rv_crash$zoom, {rv_crash$poly_selected <- NA},
                 ignoreInit = TRUE)
    
    # Update map in response to poly_selected change
    observeEvent(rv_crash$poly_selected, {
      if (!is.na(rv_crash$poly_selected)) {
        width <- switch(rv_crash$zoom, "borough" = 100, "CT" = 10, 2)
        data_to_add <-
          data_crash() %>%
          filter(ID == rv_crash$poly_selected)
        
        mapdeck_update(map_id = NS(id, "map")) %>%
          add_polygon(
            data = data_to_add, stroke_width = width, stroke_colour = "#000000",
            fill_colour = "fill", update_view = FALSE,
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
