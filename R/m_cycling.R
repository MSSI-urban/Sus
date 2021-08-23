### CYCLING NETWORK MODULE ###############################################################
# UI ----------------------------------------------------------------------
z_score <- function(x){
    (x - mean(x))/sd(x)
}

cycling_UI <- function(id) {
    ns <- NS(id)
    tabItem(tabName = "cycling",
            
            #Main Map
            mapdeckOutput(ns("mymap"), height = "92vh"),
            
            title_UI(NS(id, "title")),
            
            #Legend
            absolutePanel(
                id = "legend",
                style = paste0("z-index:500; background-color: rgba(0,0,255,0); ",
                               "border-width: 0px; margin:0px"),
                bottom = 25, fixed = TRUE,
                conditionalPanel(
                    condition = "output.bttn > 1",ns = ns,
                    img(src = "bivariate_legend_2.png", width = 200, height = 177)
                )),
                     
            #Right Panel
            absolutePanel(
                id = "right_bar", style = paste0("z-index:500; max-height: 91vh; overflow-y: auto; ",
                                                 "overflow-x:hidden; padding: 5px; border-width: 0px;"),
                class = "panel panel-default", top = 70, right = 20, width = "17%",bottom = "auto",
                h5(strong("Bicycle Network")),
                radioGroupButtons(inputId = ns("radio1"),
                                  label = "",
                                  checkIcon = list(
                                      yes = icon("ok",lib = "glyphicon")),
                                  choices = var_list_year,
                                  selected = 2020,
                                  status = "primary"),
                hr(),
                pickerInput(
                    inputId = ns("variable1"),
                    label = "Relationship between", 
                    choices = var_list_dep,
                    selected = "cyc_dent"
                ),
                pickerInput(
                    inputId = ns("variable2"),
                    label = "and", 
                    choices = var_list_ind,
                    selected = "pop_den"
                ),
                actionBttn(
                    inputId = ns("bttn1"),
                    label = "Investigate",
                    style = "unite", 
                    size = "sm",
                    color = "primary"
                ),
                conditionalPanel(
                    condition = "output.bttn > 1",ns = ns,
                    plotOutput(ns("line_plot"), height = 200)
                ),
                hr(),
                conditionalPanel(
                    condition = "output.da != null",ns = ns,
                    DT::DTOutput(ns("table1")))
                
                ) 
                
            )
    
}

# Server ------------------------------------------------------------------

cycling_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id)
        
        # Title bar
        title_server("title", "cycling")
        
        #legend-------- 
        legend_po <- legend_element(
            variables = c("High-comfort Bikeway","Medium-comfort Bikeway","Low-comfort Bikeway"),
            colours = c("#1E8449","#3383FF","#FFAC33"),
            colour_type = "stroke",
            variable_type = "category"
        )
        legend1 <- mapdeck_legend(legend_po)
        
        # Map-------- 
        output$mymap <- renderMapdeck({
            mapdeck(style = map_style, token = token_cycling, zoom = map_zoom_covid, 
                    location = map_location) %>%
                add_polygon(data = DA_qiao[which(DA_qiao$year == 2020),],
                            fill_opacity = 0.5,
                            fill_colour = "#F8F8F8",
                            stroke_colour = "#BEBEBE",
                            stroke_opacity = 0.2,
                            stroke_width = 40,
                            layer_id = "DA",
                            id = "DAUID",
                            highlight_colour  =  "#00FFFFFF",
                            auto_highlight = TRUE,
                            update_view = FALSE) %>%
                add_path(data = cycling_network[which(cycling_network$year == 2020),],
                         stroke_colour = "color",
                         stroke_width = 4,
                         width_units = "pixels",
                         layer_id = "network",
                         legend = legend1,
                         update_view = FALSE)
        }) 
        
        #Cycling network in different year-------- 
        observeEvent(input$radio1, {
            network <-cycling_network[which(cycling_network$year == input$radio1),]
            DA_year <- DA_qiao[which(DA_qiao$year == input$radio1),]
            mapdeck_update(map_id = ns("mymap"))  %>%
                clear_polygon(layer_id = "DA") %>%
                clear_polygon(layer_id = "bivar") %>%
                clear_path(layer_id = "network") %>%
                add_polygon(data = DA_year,
                            fill_opacity = 0.5,
                            fill_colour = "#F8F8F8",
                            stroke_colour = "#BEBEBE",
                            stroke_opacity = 0.2,
                            stroke_width = 40,
                            layer_id = "DA",
                            id = "DAUID",
                            highlight_colour  =  "#00FFFFFF",
                            auto_highlight = TRUE,
                            update_view = FALSE) %>%
                add_path(data = network,
                         stroke_colour = "color",
                         stroke_width = 4,
                         width_units = "pixels",
                         layer_id = "network",
                         legend = legend1,
                         update_view = FALSE)
        })
        
        #Chart-------- 
        button <- reactiveValues(n = 1)
        
        observeEvent({
            input$bttn1
            },{
            button$n <- button$n + 1
            DA_year <- DA_qiao[which(DA_qiao$year == input$radio1),]

            chart <- DA_year[,c("DAUID", input$variable1, input$variable2)] %>%
                st_drop_geometry()
            colnames(chart)[c(2,3)] <- c("dep_variable", "ind_variable")
            
            #Bivarate Map
            chart <- chart %>%
                mutate(
                    # dep_q = ntile(dep_variable, n=3),
                    #    ind_q = ntile(ind_variable, n=3),
                       dep_q = cut(z_score(dep_variable), breaks = c(min(z_score(dep_variable)), -0.5, 0.5, max(z_score(dep_variable))), include.lowest=TRUE, labels = 1:3),
                       ind_q = cut(z_score(ind_variable), breaks = c(min(z_score(ind_variable)), -0.5, 0.5, max(z_score(ind_variable))), include.lowest=TRUE, labels = 1:3),
                       group = paste0(dep_q, " - ", ind_q))
            DA_year <- DA_year %>%
                left_join(chart, by = "DAUID") %>%
                left_join(colour_DA, by = "group")
            
            #Scatterplot
            if (input$variable1 == "cyc_dent"){
                y_label <- "Bicycle Network Density (km/sq.km)"
            } else {
                y_label <- "Bicycle Network Coverage (%)"
            }
            
            if (input$variable2 == "pop_den"){
                x_label <- "Population Density (persons/sq.km)"
            } else if (input$variable2 == "imm_per"){
                x_label <- "Recent Immigrant (%)"
            } else if (input$variable2 == "mino_per"){
                x_label <- "Visible Minority (%)"
            } else if (input$variable2 == "bla_per"){
                x_label <- "Black (%)"
            } else if (input$variable2 == "cer_per"){
                x_label <- "People with Low Level of Education (%)"
            } else {
                x_label <- "Median Household Income"
            }
            
            output$line_plot <- renderPlot({
                    ggplot(chart, aes(x=ind_variable, y=dep_variable)) +
                    geom_point() +
                    geom_smooth(method = lm) +
                    labs(x = x_label, y = y_label)
            })
            
            mapdeck_update(map_id = ns("mymap"))  %>%
                clear_polygon(layer_id = "DA") %>%
                add_polygon(data = DA_year,
                            fill_opacity = 0.5,
                            fill_colour = "fill",
                            stroke_colour = "#FFFFFF",
                            stroke_opacity = 0.2,
                            stroke_width = 40,
                            layer_id = "bivar",
                            id = "DAUID",
                            highlight_colour  = "#00FFFFFF",
                            auto_highlight = TRUE,
                            update_view = FALSE)
        })
        
        # observeEvent(input$variable1,{
        #     button$n <- 1
        # })
        # 
        # observeEvent(input$variable2,{
        #     button$n <- 1
        # })
        
        output$bttn <-  reactive({
            return(button$n)
        })
        outputOptions(output, "bttn", suspendWhenHidden = FALSE)
        
        ##Click Event--------   
        da_click <- reactiveValues(da_clicked=NULL)
        
        observeEvent(input$radio1,{
            da_click$da_clicked <- NULL
            button$n <- 1
        })
        
        observeEvent(input$mymap_polygon_click,{
            js <- input$mymap_polygon_click
            lst <- jsonlite::fromJSON(js)
            da_click$da_clicked <-lst$object$properties
            
            ##DA Info Table--------
            selected <- DA_qiao[which(DA_qiao$year == input$radio1 & DA_qiao$DAUID == da_click$da_clicked$id),]
            cyc_dent <- paste0(round(selected$cyc_dent,1)," km/sq(km)")
            coverage <- paste0(round((selected$coverage*100),0),"%")
            
            pop <- selected$pop
            pop_den <- paste0(round(selected$pop_den,0)," persons/sq(km)")
            imm <- selected$imm
            imm_per <- paste0(round(selected$imm_per,0)," %")
            mino <- selected$mino
            mino_per <- paste0(round(selected$mino_per,0)," %")
            black <- selected$black
            bla_per <- paste0(round(selected$bla_per,0)," %")
            no_cer <- selected$no_cer
            cer_per <- paste0(round(selected$cer_per,0)," %")
            median <- paste0("$",selected$median)
            
            info <- c(cyc_dent, coverage, 
                      paste0(pop, "/ ", pop_den), paste0(imm, "/ ", imm_per), paste0(mino, "/ ", mino_per), paste0(black, "/ ", bla_per), paste0(no_cer, "/ ", cer_per), median)
            DA_info <- data.frame(c("Bicycle Network Density", "Bicycle Network Coverage", 
                                    "Population/Population Density", "Recent Immigrant", "Visible Minority", "Black", "People with Low Level of Education", "Median Household Income"),
                                  info)
            output$table1 <- renderDT({
                DT::datatable(DA_info,
                              rownames = FALSE, colnames = c("",""), filter = "none",
                              style = "bootstrap",
                              options = list(
                                  dom = 'b', ordering = FALSE
                              )
                )
            })
        })
        
        output$da <- reactive({
            return(da_click$da_clicked)
        })
        
        outputOptions(output, "da", suspendWhenHidden = FALSE)
        
    })
}
