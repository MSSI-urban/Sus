Mode_switch_module_UI <- function(id ) {
  ns <- NS(id)
  tabItem(
  tags$head(tags$style(HTML('
          #title_bar_commute {border-width: 10px; border-color: rgb(255, 255, 255);}
          #commute_right_bar {border-width: 10px;
          border-color: rgba(255,255,255,1);}'))),

  # Main map
  mapdeckOutput(outputId = ns("qzmyMap"), height = "91vh")
  ,

  # Title bar
  absolutePanel(
    id = "title_bar_commute", class = "panel panel-default", style = "padding:5px;",
    draggable = FALSE, top = 70, left = 270, width = "40%",
    h2(i18n$t("Shifting car trips to cycling")),
    p(i18n$t(title_text %>%
        filter(tab == "commute", type == "main") %>%
        pull(text))),
    actionLink(ns("commute_more_info"), i18n$t("Learn more")),
    conditionalPanel(
      condition = "output.commute_more_info_status == 1", ns = ns,
      uiOutput(outputId = ns("commute_extra_html")))),
      # HTML(title_text %>%
      #        filter(tab == "commute", type == "extra") %>%
      #        pull(text)))),

  # Explore panel
  absolutePanel(
    id = "commute_right_bar", style = "z-index:500; max-height: 88vh; overflow-y: auto; overflow-x:hidden; padding:5px;",
    class = "panel panel-default", top = 70, right = 50, width = 300,
    conditionalPanel(
      condition = "output.zoom_level == 'OUT'",ns = ns ,
      h4(i18n$t("Explore")),
      selectInput(
        ns("commute_variable"), label = NULL,
        choices = list("Share of trips taken by car" = 2,
                       "Average commuting distance" = 3,
                       "Access to cycling infrastructure" = 1),
        selected = 2),
      sliderInput(
        inputId = ns("commute_explore_slider"),
        label = i18n$t("% of trips taken by car, by census tract"),
        min = 0,
        max = 100,
        value = c(0, 100)),
      plotOutput(ns("commute_histogram"), height = 200)
    ),

    # Simulate panel
    conditionalPanel(
      condition = "output.zoom_level == 'IN'",ns = ns ,

      h4("Simulate"),

      radioGroupButtons(
        ns("radio1"),
        label = i18n$t("Modal shift scenarios"),
        choices = list("Baseline" = 3,
                       "Distance" = 1,
                       "Elevation/time" = 2),
        selected = 3),

      conditionalPanel(
        condition = "output.zoom_level == 'IN' && input.radio1 <3",ns = ns ,
        materialSwitch(inputId = ns("baseline_switch"),
                       label = i18n$t("Show baseline"),
                       status = "primary", value = TRUE)
      ),

      sliderTextInput(
        inputId = ns("slider1"),
        label =i18n$t( "Cycling distance (km):"),
        choices = seq(from = 1,
                      to = 10,
                      by = 0.1),
        grid = TRUE),

      sliderTextInput(
        inputId = ns("slider2"),
        label = i18n$t("Elevation gain (m):"),
        choices = seq(from = 10,
                      to = 55,
                      by = 5),
        grid = TRUE
      ),

      sliderTextInput(
        inputId = ns("slider3"),
        label = i18n$t("Time ratio:"),
        choices = seq(from = 1.0,
                      to = 3.0,
                      by = 0.2),
        grid = TRUE
      ),

      hr(),

      conditionalPanel(
        condition = "output.zoom_level == 'IN' && input.radio1 <3",ns = ns ,
        h5(strong(i18n$t("VKT Reduction"))),
        DT::DTOutput(ns("table"))),

      hr(),
      materialSwitch(inputId = ns("switch2"),
                     label = i18n$t("Cycling network"),
                     status = "primary", value = FALSE)
    )
   )
  )
  }





Mode_switch_module_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {

                 # loadRData <- function(fileName){
                 #   #loads an RData file, and returns it
                 #   load(fileName)
                 #   get(ls()[ls() != "fileName"])
                 # }
                 # 
                 # cycling1 <- loadRData("data/car_1_finals.Rdata")
                 # cycling2 <- loadRData("data/car_3_finals.Rdata")
                 # cycling_network <- loadRData("data/reseau_cyclable.Rdata")
                 # car_share <- loadRData("data/Car_Share.Rdata")
                 # cycling_access <- loadRData("data/Cycling_Access.Rdata")
                 # trip_distance <- loadRData("data/Trip_Distance.Rdata")
                 # 
                 # load("data/cycling_total_final.Rdata")



                 # Commute extra html translation ------------------------------------------
                 output$commute_extra_html <- renderUI(HTML(sus_translate(title_text %>%
                                                                           filter(tab == "commute", type == "extra") %>%
                                                                           pull(text))))

                 # Drop down list for variable selection -----------------------------------
                 # List reactive translation
                 observe({
                   updateSelectInput(session = session,
                                     inputId = "commute_variable",
                                     choices = sus_translate(list("Share of trips taken by car" = 2,
                                                                  "Average commuting distance" = 3,
                                                                  "Access to cycling infrastructure" = 1)))
                 })

                 observe({
                   updateRadioGroupButtons(session = session,
                                     inputId = "radio1",
                                     choices = sus_translate(list("Baseline" = 3,
                                                                  "Distance" = 1,
                                                                  "Elevation/time" = 2)))
                 })

                 # Commute mode change globals ---------------------------------------------

                 # Set access token
                 set_token('pk.eyJ1IjoidHR1ZmYiLCJhIjoiY2pvbTV2OTk3MGkxcTN2bzkwZm1hOXEzdiJ9.KurIg4udRE3PiJqY1p2pdQ')

                 scenario1 <- tibble(c("Criteria: Cycling Distance (km)",
                                       "Potential Cyclable Trips (per day)",
                                       "VMT Savings (per day)"),
                                     c(4.4, 60460, 102862))

                 scenario2 <- tibble(c("Criteria: Cycling Distance (km)",
                                       "Criteria: Elevation Gain (m)",
                                       "Criteria: Time Ratio","Potential Cyclable Trips (per day)",
                                       "VMT Savings (per day)"),
                                     c(4.4, 45, 2.4, 44205, 72992))

                 # Legend
                 df_pal1 <- data.frame(
                   color = c(1,2,3,4,5),
                   color_value.x = c('#ECF4CD','#C6DE68','#B2D235','#8AA324','#5C6D18'),
                   stringsAsFactors = F
                 )

                 cycling_access <- left_join(cycling_access, df_pal1, by = "color")

                 legend_po1 <- legend_element(
                   variables = c("0 - 0.87","0.88 - 1.91","1.92 - 3.08","3.09 - 4.61","4.62 - 16.8"),
                   colours = c('#ECF4CD','#C6DE68','#B2D235','#8AA324','#5C6D18'),
                   colour_type = "fill",
                   variable_type = "category",
                   title = "Access to Cycling Infrastructure (km/sq.km)"
                 )
                 legend1 <- mapdeck_legend(legend_po1)

                 df_pal2 <- data.frame(
                   color = c(1,2,3,4,5),
                   color_value.x = c('#CAF0F8','#90E0EF','#00B4D8','#0077B6','#005D7C'),
                   stringsAsFactors = F
                 )

                 car_share <- left_join(car_share, df_pal2, by = "color")

                 legend_po2 <- legend_element(
                   variables = c("4% - 21%","22% - 33%","34% - 47%","48% - 61%","62% - 91%"),
                   colours = c('#CAF0F8','#90E0EF','#00B4D8','#0077B6','#005D7C'),
                   colour_type = "fill",
                   variable_type = "category",
                   title = "Share of Car Trips by Origin (%)"
                 )
                 legend2 <- mapdeck_legend(legend_po2)

                 df_pal3 <- data.frame(
                   color = c(1,2,3,4,5),
                   color_value.x = c('#004BC9','#0071C9','#0096C9','#BE9735','#C95C34'),
                   stringsAsFactors = F
                 )

                 trip_distance <- left_join(trip_distance, df_pal3, by = "color")

                 legend_po3 <- legend_element(
                   variables = c("2.3 - 6.4","6.5 - 7.8","7.9 - 8.9","9.0 - 10.4","10.5 - 22.6"),
                   colours = c('#004BC9','#0071C9','#0096C9','#BE9735','#C95C34'),
                   colour_type = "fill",
                   variable_type = "category",
                   title = "Average Commuting Distance (km)"
                 )

                 legend3 <- mapdeck_legend(legend_po3)




                 qz <- reactiveValues(zoom_level = 'OUT')

                 ns <- NS(id)

                 ## Draw map ------------------------------------------------------------------

                 output$qzmyMap <- renderMapdeck({

                   mapdeck(style = "mapbox://styles/dwachsmuth/ckh6cg4wg05nw19p5yrs9tib7",
                           token = paste0("pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
                                          "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ"),
                           zoom = 10, location = c(-73.611, 45.526))

                 })


                 ## Draw histogram ------------------------------------------------------------
                 #car_share <- reactive(car_share)
                 #cycling_access <- reactive(cycling_access)
                #trip_distance <- reactive(trip_distance)
                # return(car_share)
                 #return(cycling_access)
                 #return(trip_distance)


                histogram_function <- function(cycling_access, car_share, trip_distance, inputer){
                  print(inputer)

                  if (inputer == 1) {
                    data <- rename(cycling_access, var = cycling_ac)
                  } else if (inputer == 2) {
                    data <-  rename(car_share, var = Car_per)
                  } else if (inputer == 3) {
                    data <- rename(trip_distance, var = avg_dist)
                  }

                  print(data)
                  # d <-  d %>% st_drop_geometry() %>% mutate(color = as.character(color))
                  #selection <- 1
                  selection <- as.numeric(inputer)

                  x_var_name <- c(sus_translate("Access to cycling inf. (km/sq.km)"),
                                  sus_translate("Share of trips taken by car (%)"),
                                  sus_translate("Average commuting distance (km)"))[selection]

                  print(x_var_name)
                  ggplot(data ) +
                    geom_histogram(aes( var, fill = color), bins = 25) +
                    scale_fill_manual(values = tibble::deframe(distinct(
                      dplyr::select(data, color, color_value.x)))) +
                    labs(x = x_var_name, y = NULL) +
                    theme_minimal() +
                    theme(legend.position = "none",
                          panel.grid.minor.x = element_blank(),
                          panel.grid.major.x = element_blank(),
                          panel.grid.minor.y = element_blank())
                }



                 output$commute_histogram <- renderPlot({

                   histogram_function(cycling_access, car_share , trip_distance, inputer = input$commute_variable)

                 })


                 ## Set zoom level ------------------------------------------------------------

                 observeEvent(input$qzmyMap_view_change$zoom, {

                   if (input$qzmyMap_view_change$zoom > 10.2) {
                     qz$zoom_level <- 'IN'} else {
                       qz$zoom_level <- 'OUT'}
                   print(qz$zoom_level)

                 })

                 output$zoom_level <- reactive(qz$zoom_level)
                 outputOptions(output, "zoom_level", suspendWhenHidden = FALSE)


                 ## Show/hide more info panel in title bar ------------------------------------

                 # More info
                 output$commute_more_info_status <- reactive(input$commute_more_info %% 2 == 1)
                 outputOptions(output, "commute_more_info_status", suspendWhenHidden = FALSE)

                 observe({

                   if (input$commute_more_info %% 2 == 1) txt <- sus_translate("Hide") else txt <- sus_translate("Learn more")
                   updateActionButton(session, "commute_more_info", label = txt)

                 })


                 ## Control the scenario sliders ----------------------------------------------

                 observeEvent(input$radio1, {
                   print("radio input")
                   if (input$radio1 == 1) {
                     updateSliderTextInput(session = session,
                                           inputId = "slider1",
                                           selected = 4.4)
                     updateSliderTextInput(session = session,
                                           inputId = "slider2",
                                           selected = 55)
                     updateSliderTextInput(session = session,
                                           inputId = "slider3",
                                           selected = 3)

                   } else if (input$radio1 == 2) {
                     updateSliderTextInput(session = session,
                                           inputId = "slider1",
                                           selected = 4.4)
                     updateSliderTextInput(session = session,
                                           inputId = "slider2",
                                           selected = 45)
                     updateSliderTextInput(session = session,
                                           inputId = "slider3",
                                           selected = 2.4)

                   } else if (input$radio1 == 3) {
                     updateSliderTextInput(session = session,
                                           inputId = "slider1",
                                           selected = 1)
                     updateSliderTextInput(session = session,
                                           inputId = "slider2",
                                           selected = 10)
                     updateSliderTextInput(session = session,
                                           inputId = "slider3",
                                           selected = 1)
                   }
                 })

                 #cycling_network <- reactive(cycling_network)

                 observeEvent(input$switch2, {
                   if (input$switch2 == TRUE) {
                     mapdeck_update(map_id = ns("qzmyMap")) %>%
                       add_path(data = cycling_network,
                                stroke_colour = "#EA3546",
                                stroke_width = 50,
                                layer_id = "network",
                                update_view = FALSE)
                   } else {
                     mapdeck_update(map_id = ns("qzmyMap")) %>%
                       clear_path(layer_id = "network")
                   }
                 })


                 ## Change Explore sliders when menu selection changes ------------------------

                 observe({

                   if (input$commute_variable == 1) {

                     updateSliderInput(session = session,
                                       inputId = "commute_explore_slider",
                                       label = sus_translate("Cycling infrastructure (km/sq.km) by census tract:"),
                                       min = 0,
                                       max = 17,
                                       value = c(0, 17))

                   } else if (input$commute_variable == 2) {

                     updateSliderInput(session = session,
                                       inputId = "commute_explore_slider",
                                       label = sus_translate("% of trips taken by car, by census tract"),
                                       min = 0,
                                       max = 100,
                                       value = c(0, 100))

                   } else if (input$commute_variable == 3) {

                     updateSliderInput(session = session,
                                       inputId = "commute_explore_slider",
                                       label = sus_translate("Length of the average commute (km), by census tract:"),
                                       min = 0,
                                       max = 23,
                                       value = c(0, 100))
                   }

                 })
                 #
                 #
                 ## Change map in reponse to inputs -------------------------------------------

                 observe({
                   input$tabs

                   if (qz$zoom_level == "OUT") {

                     updateMaterialSwitch(session = session,
                                          inputId = "switch2",
                                          value = FALSE)

                     if (input$commute_variable == 1) {

                       mapdeck_update(map_id = ns("qzmyMap"))  %>%
                         clear_path(layer_id = "cyclable") %>%
                         clear_path(layer_id = "baseline") %>%
                         add_polygon(data = {
                           cycling_access %>%
                             filter(cycling_ac >= input$commute_explore_slider[1],
                                    cycling_ac <= input$commute_explore_slider[2])},
                           fill_opacity = 150,
                           fill_colour = "color_value.x",
                           stroke_colour = "#868683",
                           stroke_width = 50,
                           layer_id = "choropleth",
                           legend = FALSE,
                           highlight_colour  =  "#AAFFFFFF",
                           auto_highlight = TRUE,
                           update_view = FALSE)

                     } else if (input$commute_variable == 2) {

                       mapdeck_update(map_id = ns("qzmyMap")) %>%
                         clear_path(layer_id = "cyclable") %>%
                         clear_path(layer_id = "baseline") %>%
                         add_polygon(data = {
                           car_share %>%
                             filter(Car_per >= input$commute_explore_slider[1],
                                    Car_per <= input$commute_explore_slider[2])},
                           fill_colour = "color_value.x",
                           stroke_colour = "#CCD1D1",
                           stroke_width = 50,
                           layer_id = "choropleth",
                           legend = FALSE,
                           highlight_colour = "#AAFFFFFF",
                           auto_highlight = TRUE,
                           update_view = FALSE)

                     } else {

                       mapdeck_update(map_id = ns("qzmyMap"))  %>%
                         clear_path(layer_id = "cyclable") %>%
                         clear_path(layer_id = "baseline") %>%
                         add_polygon(data = {
                           trip_distance %>%
                             filter(avg_dist >= input$commute_explore_slider[1],
                                    avg_dist <= input$commute_explore_slider[2])},
                           fill_colour = "color_value.x",
                           stroke_colour = "#CCD1D1",
                           stroke_width = 50,
                           layer_id = "choropleth",
                           legend = FALSE,
                           highlight_colour  =  "#AAFFFFFF",
                           auto_highlight = TRUE,
                           update_view = FALSE)
                     }

                   }

                   # Scenario maps
                   if (qz$zoom_level == "IN") {

                     if (input$radio1 == 3) updateMaterialSwitch(session, "baseline_switch",
                                                                 value = TRUE)

                     # Baseline scenario
                     if (input$baseline_switch) {
                       mapdeck_update(map_id = ns("qzmyMap"))  %>%
                         clear_polygon(layer_id = "choropleth") %>%
                         add_path(data = cycling_final,
                                  stroke_width  = "total_cycling",
                                  stroke_colour = "#722AEE80",
                                  layer_id = "baseline",
                                  width_scale = 0.2,
                                  update_view = FALSE)

                     } else if (!input$baseline_switch) {
                       mapdeck_update(map_id = ns("qzmyMap"))  %>%
                         clear_polygon(layer_id = "choropleth") %>%
                         clear_path("baseline")
                     }

                     # Distance scenario
                     if (input$radio1 == 1) {

                       mapdeck_update(map_id = ns("qzmyMap"))  %>%
                         clear_polygon(layer_id = "choropleth") %>%
                         add_path(data = cycling1,
                                  stroke_width  = "total_car",
                                  stroke_colour = "#0061FF80",
                                  layer_id = "cyclable",
                                  width_scale = 0.2,
                                  update_view = FALSE)

                       output$table <- renderDT({

                         DT::datatable(scenario1,
                                       rownames = FALSE, colnames = c("",""), filter = "none",
                                       style = "bootstrap",
                                       options = list(dom = 'b', ordering = FALSE))

                       })

                       # Elevation/time scenario
                     } else if (input$radio1 == 2) {

                       mapdeck_update(map_id = ns("qzmyMap"))  %>%
                         clear_polygon(layer_id = "choropleth") %>%
                         add_path(data = cycling2,
                                  stroke_width  = "total_car",
                                  stroke_colour = "#0061FF80",
                                  layer_id = "cyclable",
                                  width_scale = 0.2,
                                  update_view = FALSE)

                       output$table <- renderDT({

                         DT::datatable(scenario2,
                                       rownames = FALSE, colnames = c("",""), filter = "none",
                                       style = "bootstrap",
                                       options = list(dom = 'b', ordering = FALSE))

                       })

                     } else if (input$radio1 == 3) {

                       mapdeck_update(map_id = ns("qzmyMap"))  %>%
                         clear_polygon(layer_id = "choropleth") %>%
                         clear_path(layer_id = "cyclable")
                     }
                   }
                  })





               }
  )}


