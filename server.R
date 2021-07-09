##### SUS SERVER SCRIPT ########################################################

shinyServer(function(input, output, session) {
  
  # Language button ---------------------------------------------------------
  
  sus_reactive_variables$active_language <- 
    eventReactive(input$language_button, {
      if (input$language_button[1] %% 2 != 0) "en" else "fr"
      }, ignoreNULL = FALSE)
  
  observeEvent(input$language_button,{
    if (input$language_button[1] %% 2 != 0) {
      update_lang(session, "en")
    } else {
      update_lang(session, "fr")
    }
  })
  
  
  # Modules -----------------------------------------------------------------

  canale_server("canale")
  ped_server("ped")
  climate_risk_server("climate_risk")
  # mode_switch_server("mode_switch")
  # accessibility_server("accessibility")
  street_server("street")
  why_dash_server("why_dash")
  meet_the_team_server("meet_the_team")

    
  # Waiter ------------------------------------------------------------------
  
  waiter_hide()

})
