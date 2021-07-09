### STREET MODULE ##############################################################
# TEST MODULE. DISPLAYS THE GLOBAL STREET VARIABLE ON THE MAP.

# UI ----------------------------------------------------------------------

street_UI <- function(id) {
  tabItem(tabName = "street",
          mapdeckOutput(NS(id, "map"), height = "92vh"),
          title_UI(NS(id, "title")))
}


# Server ------------------------------------------------------------------

street_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Title bar
    title_server("title", "street")
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(
        style = map_style, token = token_canale,
        zoom = map_zoom, location = map_location) %>%
        add_path(data = street,
                 stroke_width = 10, stroke_colour = "#A2A2A2",
                 update_view = FALSE, layer_id = "street", id = "id",
                 auto_highlight = TRUE, highlight_colour = "#FF000090")
    })
    
    # Update path_selected on click
    observeEvent(input$map_path_click, {
      lst <- jsonlite::fromJSON(input$map_path_click)
      if (is.null(lst$object$properties$id)) {
        rv_canale$path_selected <- NA
      } else rv_canale$path_selected <- lst$object$properties$id
    })
    
    # Update map in response to path_selected change
    observeEvent(rv_canale$path_selected, {
      if (!is.na(rv_canale$path_selected)) {
        data_to_add <-
          street %>%
          filter(id == rv_canale$path_selected)
        mapdeck_update(map_id = NS(id, "map")) %>%
          add_path(
            data = data_to_add, stroke_width = 10, stroke_colour = "#000000",
            update_view = FALSE, layer_id = "path_highlight",
            auto_highlight = TRUE, highlight_colour = "#FFFFFF90")
      } else {
        mapdeck_update(map_id = NS(id, "map")) %>%
          clear_polygon(layer_id = "path_highlight")
      }
    })
    
  })
}
