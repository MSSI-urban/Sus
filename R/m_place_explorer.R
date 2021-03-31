### PLACE EXPORER ##############################################################


### Online Sources ###

## tmaptools and geocode_osm
# https://www.rdocumentation.org/packages/tmap/versions/1.6-1/topics/geocode_OSM 
# https://rdrr.io/cran/tmaptools/man/geocode_OSM.html

## mapdeck tricks
# https://symbolixau.github.io/mapdeck/articles/tips_tricks.html 

# UI ----------------------------------------------------------------------
place_explorer_UI <- function(id) {
  tabItem(tabName = "place_explorer",
          
          tags$head(
            tags$style(
              HTML(".shiny-notification {
      position: fixed;
      top: 10%;
      bottom: unset;
      left: 0;
      right: 0;
      margin-left: auto;
      margin-right: auto;
      width: 100%;
      max-width: 450px;
             }
             "
              )
            )
          ),
          
          mapdeckOutput(NS(id, "place_explorer"), height = "98vh"),
          
          absolutePanel(id = NS(id,"search_bar"),
                        class = "panel panel-default", 
                        style = "padding: 5px; margin: 0px 5px; border-width: 0px; z-index: 500",
                        draggable = FALSE, top = 70, #width = "350px", 
                        textInput(inputId = NS(id, "address"), label = "Enter street number and street name, or postal code"),
                        actionButton(inputId = NS(id, "search"), label = "Search"))
  )
  
}


# Server ------------------------------------------------------------------

place_explorer_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$place_explorer <- renderMapdeck({
        mapdeck(
            style = "mapbox://styles/dwachsmuth/ckh6cg4wg05nw19p5yrs9tib7",
            token = paste0(
                "pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
                "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ"),
            zoom = 10.1, location = c(-73.58, 45.53), pitch = 0)
        
    }) 

    
    observeEvent(input$search, {
    
    inputed_adress <- input$address
    
    inputed_adress <- glue::glue("{inputed_adress}, Montreal, QC, Canada")
    
    address <- suppressMessages(tmaptools::geocode_OSM(
      inputed_adress,
      return.first.only = TRUE,
      as.sf = TRUE))
    
    if (is.null(address)) {
      showNotification(glue::glue("No address found for {inputed_adress}"),type = "error")
    } else {
      mapdeck_update(map_id = NS(id, "place_explorer")) %>%
        add_scatterplot(data = address,
                        lat = "lat",
                        lon = "lon",
                        radius = 10)
    }
    })

  })
}