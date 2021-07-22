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

  # accessibility_server("accessibility")
  alley_server("alley")
  canale_server("canale")
  climate_risk_server("climate_risk")
  crash_server("crash")
  housing_server("housing")
  meet_the_team_server("meet_the_team")
  # mode_switch_server("mode_switch")
  # ped_server("ped")
  why_dash_server("why_dash")

    
  # Waiter ------------------------------------------------------------------
  
  waiter_hide()

})
