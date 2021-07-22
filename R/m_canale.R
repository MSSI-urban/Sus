### CANALE MODULE ##############################################################

# UI ----------------------------------------------------------------------

canale_UI <- function(id) {
  tabItem(tabName = "canale",
          mapdeckOutput(NS(id, "map"), height = "92vh"),
          title_UI(NS(id, "title")),
          right_panel(id,
                      compare_UI(NS(id, "canale"), var_list_canale),
                      explore_UI(NS(id, "explore")),
                      dyk_UI(NS(id, "dyk"))
                      ),
          legend_bivar_UI(NS(id, "canale"))
          )
  }


# Server ------------------------------------------------------------------

canale_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Title bar
    title_server("title", "canale")
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(
        style = map_style, token = token_canale,
        zoom = map_zoom, location = map_location) %>%
        add_polygon(data = borough %>%
                      mutate(group = paste(canale_ind_q3, "- 1")) %>%
                      left_join(colour_borough, by = "group"),
          stroke_width = 100, stroke_colour = "#FFFFFF", fill_colour = "fill", 
          update_view = FALSE, id = "ID", auto_highlight = TRUE,
          highlight_colour = "#FFFFFF90")
      })
    
    # Zoom level
    observeEvent(input$map_view_change$zoom, {
      rv_canale$zoom <- case_when(input$map_view_change$zoom >= 14 ~ "DA_2",
                                  input$map_view_change$zoom >= 12 ~ "DA",
                                  input$map_view_change$zoom >= 10.5 ~ "CT",
                                  TRUE ~ "borough")
      })
    
    # Compare panel
    var_right_canale <- compare_server("canale", var_list_canale,
                                       reactive(rv_canale$zoom))

    # Data
    data_canale <- data_server(id = "canale",
                               var_left = reactive("canale_ind"),
                               var_right = var_right_canale,
                               df = reactive(rv_canale$zoom))
    
    # Explore panel
    explore_server("explore", data_canale, reactive("canale_ind"),
                   var_right_canale, reactive(rv_canale$poly_selected),
                   reactive(rv_canale$zoom), reactive("CanALE index"))

    # Did-you-know panel
    dyk_server("dyk", reactive("canale_ind"), var_right_canale)

    # Left map
    small_map_server("left", reactive(paste0(
      "left_", sub("_2", "", rv_canale$zoom), "_canale_ind")))
    
    # Bivariate legend
    legend_bivar_server("canale", var_right_canale)
    
    # Update map in response to variable changes or zooming
    observeEvent({
      var_right_canale()
      rv_canale$zoom}, {
        width <- switch(rv_canale$zoom, "borough" = 100, "CT" = 10, 2)
        mapdeck_update(map_id = NS(id, "map")) %>%
          add_polygon(
            data = data_canale(), stroke_width = width,
            stroke_colour = "#FFFFFF", fill_colour = "fill",
            update_view = FALSE, id = "ID", auto_highlight = TRUE,
            highlight_colour = "#FFFFFF90")
        })

    # Update poly_selected on click
    observeEvent(input$map_polygon_click, {
      lst <- jsonlite::fromJSON(input$map_polygon_click)
      if (is.null(lst$object$properties$id)) {
        rv_canale$poly_selected <- NA
      } else rv_canale$poly_selected <- lst$object$properties$id
      })

    # Clear poly_selected on zoom
    observeEvent(rv_canale$zoom, {rv_canale$poly_selected <- NA},
                 ignoreInit = TRUE)

    # Update map in response to poly_selected change
    observeEvent(rv_canale$poly_selected, {
      if (!is.na(rv_canale$poly_selected)) {
        width <- switch(rv_canale$zoom, "borough" = 100, "CT" = 10, 2)
        data_to_add <-
          data_canale() %>%
          filter(ID == rv_canale$poly_selected) %>%
          mutate(fill = substr(fill, 1, 7))

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

    # Clear click status if prompted
    # (Namespacing hardwired to explore module; could make it return a reactive)
    observeEvent(input$`explore-clear_selection`, {
      rv_canale$poly_selected <- NA})
    
  })
}
