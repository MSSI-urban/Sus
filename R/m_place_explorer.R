### PLACE EXPORER ##############################################################


#' The place explorer, as for now, has 4 parts. 

#' 1. The map. Regarding the map, the user can enter an address. OSM will retrieve
#' all addresses corresponding to what the user enter, and we add an operation
#' where we filter only the addresses that are in Montreal's CMA. If there are multiple results,
#' multiple points are going to appear on the map. The user can then select the the location wanted.
#' The user can also search by postal codes. The regex used to detect if a postal code is entered is
#' in the R/global_place_explorer.R file, and the centroid of the postal code becomes the selected
#' location. The user can also simply click a location on the map. 

#' 2. The second part is the highlights. Whatever the themes choose, the highlights does not change.
#' It lets the user see what are the most extreme 4 variables for this location, when compared
#' to all the other polygons in the CMA.

#' 3. Third part is to select the themes that the user is interested in, and the geography scale.
#' At the moment, the three are either Borough, Census tract, or Dessimination Area.

#' 4. Fourth part is how the data are displayed. I think it should be one plot, on which all
#' the variables from all the selected themes are regrouped. The user can hover or click
#' on the variable he's interested in, and a tooltip appears with information regarding
#' the geography and the value of the variable, as well as a plot design for this kind
#' of value. If it's a proportion it can be represented in a way, if it's $ in another, etc.

# UI ----------------------------------------------------------------------
place_explorer_UI <- function(id) {
  tabItem(tabName = "place_explorer",
          
          # This CSS style is only used for when one address does not have entry in OSM. Then
          # a red popup will appear in the middle of the page, stating there's no 
          # result for the query.
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
                             strong("Street number and street name, or postal code"),
                             # put the address bar next to the search button. Splitting the space 70/30.
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
                             checkboxGroupInput(inputId = NS(id, "themes_checkbox"),
                                                label = "Themes to choose",
                                                choiceNames = c("Housing", "Income", "Immigration", "Transport", "CanALE", "Climate"),
                                                # retrieve the categories available right now in our dfs. This will have to get updated
                                                # manually whenever new categories emmerged.
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
                             plotly::plotlyOutput(NS(id, "output_plot")))
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
    
    # 1. UPDATE MAP ON ADDRESS SEARCH
    observeEvent(input$search, {
      
      # If a postal code is entered, with the regex in the global.
      if (stringr::str_detect(input$address, pc_regex)) {
        
        address_searched <- stringr::str_to_lower(input$address) %>% 
          stringr::str_remove_all(pattern = " ")
        
        address_searched <- postal_codes %>% 
            filter(postal_code == address_searched) %>% 
          st_transform(crs = st_crs(CT))
          
      } else { # If it's not a postal code, then must be an address to retrieve from OSM.
        address_searched <- suppressMessages(tmaptools::geocode_OSM(
          input$address,
          return.first.only = FALSE, 
          as.data.frame = TRUE))
        
        if (nrow(address_searched) != 0) {
          address_searched <- address_searched %>% 
            st_as_sf(coords = c("lon", "lat"), crs = st_crs(CT)) %>%
            # This last st_filter is to filter in only the points inside the Montreal's CMA.
            # Because the OSM finds all these address worlwide.
            st_filter(CT)
        }
      }
      
      
      if (nrow(address_searched) == 0) {
        # If there's no result found, send an error popup.
        showNotification(glue::glue("No address found for this query"), type = "error")
      } else {
        mapdeck_update(map_id = NS(id, "placeex")) %>%
          add_scatterplot(data = address_searched,
                          lat = "lat",
                          lon = "lon",
                          fill_colour = "#6C83B5",
                          radius_min_pixels = 5,
                          radius_max_pixels = 20)
        
        rv_placeex$selected_point <- address_searched
      }
      
    })
    
    # 2. UPDATE MAP ON A CLICK ON THE}) MAP
    observeEvent(input$placeex_polygon_click,{
      js <- input$placeex_polygon_click
      lst <- jsonlite::fromJSON(js)
      
      clicked_location <- data.frame(lat = lst$lat, lon = lst$lon) %>%
        st_as_sf(coords = c("lon", "lat"), crs = st_crs(CT))
      
      mapdeck_update(map_id = NS(id, "placeex")) %>%
        add_scatterplot(data = clicked_location,
                        lat = "lat",
                        lon = "lon",
                        fill_colour = "#6C83B5",
                        radius_min_pixels = 5,
                        radius_max_pixels = 20,
                        update_view = FALSE)
      
      rv_placeex$selected_point <<- clicked_location
    })
    
    # DEPENDING ON GEO, BOROUGH, CT, OR DA ID
    geo_ID <- 
    reactive({
      if (input$geo_scale == "Borough") {
          borough %>%
          st_filter(rv_placeex$selected_point) %>%
          pull(ID)
      } else if (input$geo_scale == "Census Tract") {
          CT %>%
          st_filter(rv_placeex$selected_point) %>%
          pull(ID)
      } else if (input$geo_scale == "Dissemination Area") {
          DA %>%
          st_filter(rv_placeex$selected_point) %>%
          pull(ID)
      }
    })
    
    geo_dt <- 
      reactive({
        if (input$geo_scale == "Borough") borough
        else if (input$geo_scale == "Census Tract") CT
        else if (input$geo_scale == "Dissemination Area") DA
      })
    
    
    # SHOW THE 4 HIGHLIGHTS
    observe({
    output$highest_p <- renderUI({
      
      highlights_4 <-
        geo_dt() %>%
        # borough %>% 
        st_drop_geometry() %>%
        select(ID, population:heat_wave_ind, -ends_with("_q3")) %>%
        mutate(across(everything(), percent_rank, .names = "{.col}_perc")) %>%
        filter(ID == geo_ID()) %>%
        # filter(ID == "2466023_3") %>%
        select(ends_with("_perc"), -ID_perc) %>%
        tidyr::pivot_longer(cols = everything()) %>%
        mutate(dist_to_50 = abs(0.5-value)) %>% 
        arrange(-dist_to_50) %>% 
        slice(1:4) %>% 
        mutate(value = round(value*100, digits = 2))

      highlight_values <-
        geo_dt() %>%
        st_drop_geometry() %>%
        filter(ID == geo_ID()) %>%
        select(stringr::str_remove(highlights_4$name, "_perc"))
      
      highlight_ouputs <- list()

      for(i in 1:nrow(highlights_4)){
        percent_if_proportion <- 
          if (stringr::str_detect(names(highlight_values[,i]), "_prop")) scales::percent(pull(highlight_values[1,i]), accuracy =0.1)
          else round(highlight_values[1,i], digits = 2)
        
        highlight_ouputs[i] <- 
         glue::glue("The {stringr::str_to_lower(input$geo_scale)} selected is at the {highlights_4[i,2]} ",
                          "percentile in the {names(highlight_values)[i]} value ",
                          "at {percent_if_proportion}.")
      }
      
      HTML(paste0(highlight_ouputs, sep = "<br/><br/>"))
      
    })
    })
    

    # PLOT WITH HOVER
    output$output_plot <- plotly::renderPlotly({
      
      percentiles <<- 
        # borough %>%
        geo_dt() %>%
        st_drop_geometry() %>%
        select(ID, population, households, starts_with(input$themes_checkbox), -ends_with("_q3")) %>%
        # select(ID, everything(), -ends_with("_q3")) %>%
        mutate(across(everything(), percent_rank, .names = "{.col}_perc")) %>%
        filter(ID == geo_ID()) %>%
        # filter(ID == "2466072") %>%
        select(ends_with("_perc"), -ID_perc) %>%
        tidyr::pivot_longer(cols = everything()) %>%
        mutate(value = round(value*100, digits = 2),
               category = stringr::str_remove_all(name, pattern = "_.*"),
               name = stringr::str_remove(name, "_perc"))
      
      info_plot <- percentiles %>% 
        ggplot()+ 
        geom_col(aes(x= value, y = name, fill = category, 
                     text = paste0(
                       "Variable: ", name, "<br>",
                       "Percentile: ", value, "<br>",
                       "Category: ", category
                     )), position= "identity", color = "white")+
        theme_minimal()+
        xlab(glue::glue("Value percentile for the {stringr::str_to_lower(input$geo_scale)}"))+
        ylab("Variable")
      
      plotly::ggplotly(info_plot, tooltip = "text")
      
    })
  })
}