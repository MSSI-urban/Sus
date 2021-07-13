# library(shiny)
# 
# # NOT RUN {
# ## Only run this example in interactive R sessions
# if (interactive()) {
#   # Basic dashboard page template
#   
#   shinyApp(
#     ui = dashboardPage(
#       dashboardHeader(),
#       dashboardSidebar(),
#       dashboardBody(),
#       title = "Dashboard example",
#       
#       sliderInput(inputId = "num", 
#                   label = "Choose a number", 
#                   value = 25, min = 1, max = 100),
#       
#       plotOutput("hist")
#     ),
#     
#     
#     
#     server = function(input, output) { }
#   )
# }
# # }





############## Template ###############3
# library(shiny)
# ui <- fluidPage()
# server <- function( input, output ) {}
# shinyApp(ui = ui, server = server)


library(shiny)
ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  plotOutput("hist")
)

server <- function( input, output ) {
  output$hist <- renderPlot({
    title = "x random normal vars"
    hist(rnorm(input$num), main = title)
  })
}
shinyApp(ui = ui, server = server)



