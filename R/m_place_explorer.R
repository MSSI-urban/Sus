### PLACE EXPORER ##############################################################

### Online Sources ###

## tmaptools and geocode_osm
# https://www.rdocumentation.org/packages/tmap/versions/1.6-1/topics/geocode_OSM 
# https://rdrr.io/cran/tmaptools/man/geocode_OSM.html

## mapdeck tricks
# https://symbolixau.github.io/mapdeck/articles/tips_tricks.html 


# To get an idea of the columns present in the tables
# colnames(borough) %>%
#   str_remove_all(pattern = "_.*") %>%
#   unique()



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
          
          fluidRow(
            column(4, 
                   # MAP
                   mapdeckOutput(NS(id, "place_explorer"), height = "33vh"),
                   
                   # SEARCH BAR
                   mainPanel(id = NS(id, "search_bar"),
                             # class = "panel panel-default",
                             strong("Street number and street name, or postal code"),
                             splitLayout(cellWidths = c("70%", "30%"),
                                         textInput(inputId = NS(id, "address"), label = NULL),
                                         actionButton(inputId = NS(id, "search"), label = "Search")),
                   ),
                   
                   # HIGHLIGHTS
                   mainPanel(id = NS(id, "highlights"),
                             class = "panel panel-default",
                             style = "padding: 5px; margin: 0px 5px; border-width: 0px; z-index: 500",
                             h1("Highlights"),
                   ),
            ),
            column(8,
                   # THEME AND GEOGRAPHY SCALE CHOICES
                   mainPanel(id = NS(id, "themes_and_geo"),
                             # class = "panel panel-default",
                             checkboxGroupInput(inputId = NS(id, "themes_checkbox"),
                                                label = "Themes to choose",
                                                choiceNames = c("Housing", "Income", "Immigration", "Transport", "CanALE"),
                                                choiceValues = (colnames(borough) %>% 
                                                                  stringr::str_remove_all(pattern = "_.*") %>% 
                                                                  unique())[5:9],
                                                selected = (colnames(borough) %>% 
                                                              stringr::str_remove_all(pattern = "_.*") %>% 
                                                              unique())[5:9],
                                                inline = T),
                             sliderTextInput(inputId = NS(id, "geo_scale"),
                                             label = "Geography scale",
                                             choices = c("Borough", "Census Tract", "Dissemination Area"),
                                             selected = "Borough",
                                             grid = TRUE)
                   ),
                   
                   # INFO OUTPUT
                   mainPanel(id = NS(id, "info"),
                             class = "panel panel-default",
                             style = "padding: 5px; margin: 0px 5px; border-width: 0px; z-index: 500",
                             h1("Info output"),
                             htmlOutput(outputId = NS(id, "info_address_scale")),
                             textOutput(outputId = NS(id, "boxes")))
            )
            
          )
          
  )
  
}


# Server ------------------------------------------------------------------

place_explorer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # MAP INITIAL OUTPUT    
    output$place_explorer <- renderMapdeck({
      mapdeck(
        style = "mapbox://styles/dwachsmuth/ckh6cg4wg05nw19p5yrs9tib7",
        token = paste0(
          "pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
          "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ"),
        zoom = 10.1, location = c(-73.58, 45.53), pitch = 0)
      
    }) 
    
    # PER DEFAULT ADDRESS
    inputed_address <- "McGill University, Montreal, QC, Canada"
    address <- suppressMessages(tmaptools::geocode_OSM(
      inputed_address,
      return.first.only = TRUE,
      as.sf = TRUE))
    
    # RESEARCH AND UPDATED MAP
    observeEvent(input$search, {
      
      inputed_address <- input$address
      
      inputed_address <- glue::glue("{inputed_address}, Montreal, QC, Canada")
      
      address <<- suppressMessages(tmaptools::geocode_OSM(
        inputed_address,
        return.first.only = TRUE,
        as.sf = TRUE))
      
      if (is.null(address)) {
        showNotification(glue::glue("No address found for {inputed_address}"),type = "error")
      } else {
        mapdeck_update(map_id = NS(id, "place_explorer")) %>%
          add_scatterplot(data = address,
                          lat = "lat",
                          lon = "lon",
                          radius = 10)
      }
    })
    
    # DEPENDING ON GEO, BOROUGH, CT, OR DA ID
    
    observe({
      if (input$geo_scale == "Borough") {
        geo_ID <<- 
          borough %>%
          st_filter(address) %>%
          pull(ID)
        geo_dt <<- borough
      } else if (input$geo_scale == "Census Tract") {
        geo_ID <<- 
          CT %>%
          st_filter(address) %>%
          pull(ID)
        geo_dt <<- CT
      } else if (input$geo_scale == "Dissemination Area") {
        geo_ID <<- 
          DA %>%
          st_filter(address) %>%
          pull(ID)
        geo_dt <<- DA
      }
    })
    
    # Just to observe the reactions of geo_scale and the previous observeEvent
    # observe({
    #   if(exists("geo_ID")){
    #     showNotification(glue::glue("{input$geo_scale}, {geo_ID}"),type = "warning")
    #   }
    # })
    
    # TELL THE USER WHAT IS THE ADDRESS CHOSEN AND SCALE
    toListen <- reactive({
      list(input$search,input$geo_scale)
    })
    observeEvent(toListen(), {
    output$info_address_scale <- renderUI({
      HTML(
        paste(
          glue::glue("Location: {address$query}"),
          glue::glue("Scale: {input$geo_scale}"),
          sep = "<br/>"
        ))
    })
    })
    
    # VARIABLES TO DISPLAY
    output$boxes <- renderText({
      geo_dt %>%
        select(starts_with(input$themes_checkbox)) %>% 
        colnames()
    })
    
  })
}