### PLACE EXPORER ##############################################################

# FUTUR GLOBAL
pc_regex <- c("(\\w\\d\\w\\d\\w\\d)|(\\w\\d\\w\\s\\d\\w\\d)")
postal_codes <- qread("data/postal_codes.qs")

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
                   mapdeckOutput(NS(id, "placeex"), height = "33vh"),
                   
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
                             htmlOutput(outputId = NS(id, "highest_p"))
                   ),
            ),
            column(8,
                   # THEME AND GEOGRAPHY SCALE CHOICES
                   mainPanel(id = NS(id, "themes_and_geo"),
                             # class = "panel panel-default",
                             checkboxGroupInput(inputId = NS(id, "themes_checkbox"),
                                                label = "Themes to choose",
                                                choiceNames = c("Housing", "Income", "Immigration", "Transport", "CanALE", "Climate"),
                                                choiceValues = (colnames(borough) %>% 
                                                                  stringr::str_remove_all(pattern = "_.*") %>% 
                                                                  unique())[5:10],
                                                selected = (colnames(borough) %>% 
                                                              stringr::str_remove_all(pattern = "_.*") %>% 
                                                              unique())[5:10],
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
    output$placeex <- renderMapdeck({
      mapdeck(
        style = "mapbox://styles/dwachsmuth/ckh6cg4wg05nw19p5yrs9tib7",
        token = paste0(
          "pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
          "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ"),
        zoom = 9, location = c(-73.654479, 45.515161), pitch = 0) %>% 
        add_polygon(data = borough, fill_colour = NULL, stroke_opacity = 1,
                    fill_opacity = 1,
                    update_view = FALSE, id = "ID")
      
    }) 

    # PER DEFAULT ADDRESS
    selected_point <- reactive({suppressMessages(tmaptools::geocode_OSM(
      "McGill University, Montreal, QC, Canada",
      return.first.only = TRUE,
      as.sf = TRUE))})
    
    # 1. UPDATE MAP ON ADDRESS SEARCH
    observeEvent(input$search, {
      
      if (stringr::str_detect(input$address, pc_regex)) {
        
        selected_point <- stringr::str_to_lower(input$address) %>% 
          stringr::str_remove_all(pattern = " ")
        
        selected_point <- postal_codes %>% 
            filter(postal_code == selected_point) %>% 
          st_transform(crs = st_crs(CT))
          
      } else {
        selected_point <- suppressMessages(tmaptools::geocode_OSM(
          input$address,
          return.first.only = FALSE, 
          as.data.frame = TRUE))
        
        if (nrow(selected_point) != 0) {
          selected_point <- selected_point %>% 
            st_as_sf(coords = c("lon", "lat"), crs = st_crs(CT)) %>%
            st_filter(DA)
        }
      }
      
      
      if (nrow(selected_point) == 0) {
        showNotification(glue::glue("No address found for this query"), type = "error")
      } else {
        mapdeck_update(map_id = NS(id, "placeex")) %>%
          add_scatterplot(data = selected_point,
                          lat = "lat",
                          lon = "lon",
                          fill_colour = "#6C83B5",
                          radius_min_pixels = 5,
                          radius_max_pixels = 20)
      }
      
      selected_point <<- reactive({selected_point})
      
    })
    
    # 2. UPDATE MAP ON A CLICK ON THE}) MAP
    observeEvent(input$placeex_polygon_click,{
      js <- input$placeex_polygon_click
      lst <- jsonlite::fromJSON(js)
      
      selected_point <- data.frame(lat = lst$lat, lon = lst$lon) %>%
        st_as_sf(coords = c("lon", "lat"), crs = st_crs(CT))
      
      mapdeck_update(map_id = NS(id, "placeex")) %>%
        add_scatterplot(data = selected_point,
                        lat = "lat",
                        lon = "lon",
                        fill_colour = "#6C83B5",
                        radius_min_pixels = 5,
                        radius_max_pixels = 20,
                        update_view = FALSE)
      
      selected_point <<- reactive({selected_point})
    })
    
    # DEPENDING ON GEO, BOROUGH, CT, OR DA ID
    geo_ID <- 
    reactive({
      if (input$geo_scale == "Borough") {
          borough %>%
          st_filter(selected_point()) %>%
          pull(ID)
      } else if (input$geo_scale == "Census Tract") {
          CT %>%
          st_filter(selected_point()) %>%
          pull(ID)
      } else if (input$geo_scale == "Dissemination Area") {
          DA %>%
          st_filter(selected_point()) %>%
          pull(ID)
      }
    })
    
    geo_dt <- 
      reactive({
        if (input$geo_scale == "Borough") borough
        else if (input$geo_scale == "Census Tract") CT
        else if (input$geo_scale == "Dissemination Area") DA
      })
    
    
    # SHOW THE 3 HIGHLIGHTS
    observe({
    output$highest_p <- renderUI({
      
      min_max <-
        geo_dt() %>%
        st_drop_geometry() %>%
        select(ID, population:heat_wave_ind, -ends_with("_q3")) %>%
        mutate(across(everything(), percent_rank, .names = "{.col}_perc")) %>%
        filter(ID == geo_ID()) %>%
        select(ends_with("_perc"), -ID_perc) %>%
        tidyr::pivot_longer(cols = everything()) %>%
        filter(value %in% c(max(value, na.rm = T), min(value, na.rm = T))) %>% 
        mutate(value = round(value*100, digits = 2))

      highlight_values <-
        geo_dt() %>%
        st_drop_geometry() %>%
        filter(ID == geo_ID()) %>%
        select(stringr::str_remove(min_max$name, "_perc"))
      
      highlight_ouputs <- list()

      for(i in 1:nrow(min_max)){
        percent_if_proportion <- 
          if (stringr::str_detect(names(highlight_values[,i]), "_prop")) scales::percent(pull(highlight_values[1,i]), accuracy =0.1)
          else round(highlight_values[1,i], digits = 2)
        
        highlight_ouputs[i] <- 
         glue::glue("The {stringr::str_to_lower(input$geo_scale)} selected is at the {min_max[i,2]} ",
                          "percentile in the {names(highlight_values)[i]} value ",
                          "at {percent_if_proportion}.")
      }
      
      HTML(paste0(highlight_ouputs, sep = "<br/>"))
      
    })
    })
    

    
    # TELL THE USER WHAT IS THE ADDRESS CHOSEN AND SCALE
    toListen <- reactive({
      list(input$search,input$geo_scale, geo_ID(), selected_point())
    })
    observeEvent(toListen(), {
    output$info_address_scale <- renderUI({
      HTML(
        paste(
          glue::glue("Location: {selected_point()$query}"),
          glue::glue("Scale: {input$geo_scale}"),
          glue::glue("Geo ID: {geo_ID()}"),
          sep = "<br/>"
        ))
    })
    })
    
    # VARIABLES TO DISPLAY
    output$boxes <- renderText({
      # if (is.null(input$themes_checkbox)) "No box is ticked"
      #else 
      geo_dt() %>%
        select(population, households, starts_with(input$themes_checkbox), -ends_with("_q3")) %>% 
        colnames()
    })
    
  })
}