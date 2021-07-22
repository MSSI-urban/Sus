#### SLIDER MODULE ########################################################

# slider_range_init <- c(2006, 2016) # Initial value for the Range slider

# slider_years <- c("1991", "1992", "1993", "1994", "1995",
#                   "1996", "1997", "1998", "1999", "2000",
#                   "2001", "2002", "2003", "2004", "2005",
#                   "2006", "2007", "2008", "2009", "2010",
#                   "2011", "2012", "2013", "2014", "2015", 
#                   "2016", "2017", "2018", "2019", "2020",
#                   "2021")

# UI ----------------------------------------------------------------------

slider_UI <- function(id, slider_min, slider_max, slider_interval, slider_init) {
  
  sliderInput(NS(id, "slider"), "Select a year:",
              min = slider_min, max = slider_max,
              step = slider_interval,
              sep = "",
              value = slider_init)
  # checkboxInput("checkbox", "Compare between years", FALSE)
  
  
  
        # conditionalPanel(
          # condition = "input.checkbox==0",
          # sliderInput("slider", "Integer:",
          #             min = slider_min, max = slider_max,
          #             step = slider_int,
          #             sep = "",
          #             value = slider_init),
        # ),
        # conditionalPanel(
        #   condition = "input.checkbox==1",
        #   sliderInput("slider", "Range:",
        #               min = slider_min, max = slider_max,
        #               step = slider_int,
        #               sep = "",
        #               value = slider_range_init
        #   )
        # ),
      #   checkboxInput("checkbox", "Compare between years", FALSE)
      # ),
      
      # mainPanel()
    # )
  # )
}
  

# Server ------------------------------------------------------------------

slider_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    # slider_values <- reactiveVal(slider_init)
    
    # observe({
    #   if (input$checkbox == 1) {
    #     slider_values(isolate(input$range))
    #     updateSliderInput(session, "range", value = slider_range_init)
    #   } else {
    #     updateSliderInput(session, "integer", value = slider_init)
    #   }
    #   #outputOptions(output, "slider_values", suspendWhenHidden = FALSE)
    # })
    
    reactive(input$slider)
    
  })
  
}
  