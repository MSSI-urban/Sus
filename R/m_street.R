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
    
    # Reactive
    data_street <- reactive(street %>% filter(significance == rv_street$zoom))
    
    # Map
    output$map <- renderMapdeck({
      mapdeck(
        style = map_style, token = token_street,
        zoom = map_zoom, location = map_location) %>%
        add_path(data = data_street(),
                 stroke_width = 5 * (5 - rv_street$zoom), stroke_colour = "#CCCCCC",
                 update_view = FALSE, layer_id = "street_sig1", id = "id",
                 auto_highlight = TRUE, highlight_colour = "#FF000090")
    })
    
    # Zoom level
    observeEvent(input$map_view_change$zoom, {
      rv_street$zoom <- case_when(input$map_view_change$zoom >= 14 ~ 4,
                                  input$map_view_change$zoom >= 12 ~ 3,
                                  input$map_view_change$zoom >= 10.5 ~ 2,
                                  TRUE ~ 1)
    })
    
    # Update street display on zoom
    observeEvent(rv_street$zoom, {
      
      # zoom in
      if(rv_street$zoom > rv_street$prev_zoom) {
        mapdeck_update(map_id = NS(id, "map")) %>%
          add_path(data = data_street(),
                   stroke_width = 5 * (5 - rv_street$zoom),
                   stroke_colour = "#CCCCCC", update_view = FALSE,
                   layer_id = paste0("street_sig", rv_street$zoom),
                   id = "id", auto_highlight = TRUE, highlight_colour = "#FF000090",
                   transitions = list(path = 0, stroke_colour = 0, stroke_width = 0)) # Experimental
      }
      
      # zoom out
      else {
        mapdeck_update(map_id = NS(id, "map")) %>%
          clear_path(layer_id = paste0("street_sig", rv_street$prev_zoom))
      }
      
      rv_street$prev_zoom <- rv_street$zoom
      
      if(!(is.na(rv_street$path_selected))) {
        selected <- 
          street %>%
          filter(id == rv_street$path_selected)
        if (selected$significance > rv_street$zoom)
          rv_street$path_selected <- NA
      }
    })
    
    # Update path_selected on click
    observeEvent(input$map_path_click, {
      lst <- jsonlite::fromJSON(input$map_path_click)
      if (is.null(lst$object$properties$id)) {
        rv_street$path_selected <- NA
      } else rv_street$path_selected <- lst$object$properties$id
    })
    
    # Update map in response to path_selected change
    observeEvent(rv_street$path_selected, {
      if (!is.na(rv_street$path_selected)) {
        data_to_add <-
          street %>%
          filter(id == rv_street$path_selected)
        mapdeck_update(map_id = NS(id, "map")) %>%
          add_path(
            data = data_to_add, stroke_width = 5 * (5 - rv_street$zoom),
            stroke_colour = "#000000",
            update_view = FALSE, layer_id = "path_highlight",
            auto_highlight = TRUE, highlight_colour = "#FFFFFF90")
      } else {
        mapdeck_update(map_id = NS(id, "map")) %>%
          clear_path(layer_id = "path_highlight")
      }
    })
    
  })
}
