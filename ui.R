##### SUS UI SCRIPT ############################################################

ui <- dashboardPage(

  dashboardHeader(tags$li(
    class = "dropdown", 
    tags$style(".main-header {max-height: 50px}"),
    tags$style(".main-header .logo {height: 50px}")),
    title = fluidRow(
      column(width = 3, loadingLogo("https://mssi.shinyapps.io/sus-mssi/", 
                                    "logo.png", "spinning_logo.gif")), 
      column(width = 9, p("SUS"), style = "text-align: left;")), 
    titleWidth = 250),
  
  
  ## Left sidebar ------------------------------------------------------------
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs", shiny.i18n::usei18n(i18n),
      
      menuItem(i18n$t("Home"), tabName = "home"),
      
      # Climate ----
      menuItem(
        i18n$t("Climate change"),
        tabName = "climate",

        # Climate change risk
        menuSubItem(
          i18n$t("Climate change risk"),
          tabName = "climate_risk"),
        conditionalPanel(
          condition = "input.tabs == 'climate_risk'",
          # The ID needs to be duplicated for complicated namespacing reasons!
          small_map_UI("climate_risk-left")),

        startExpanded = TRUE),
      
      # Covid ----
      menuItem(
        i18n$t("Covid-19"),
        tabName = "covid",

        # Pedestrian realm
        menuSubItem(
          i18n$t("Pedestrian realm"),
          tabName = "ped"),

        startExpanded = TRUE),

      # Ecology ----
      menuItem(
        i18n$t("Ecology"),
        tabName = "ecology",

        # Biodiversity
        menuSubItem(
          i18n$t("Biodiversity"),
          tabName = "biodiversity"),

        startExpanded = TRUE),
      
      # Housing realm
      menuItem(
        i18n$t("Housing realm"),
        tabName = "housing_realm",
        
        # Housing
        menuSubItem(
          i18n$t("Housing"),
          tabName = "housing"),
        conditionalPanel(
          condition = "input.tabs == 'housing'",
          # The ID needs to be duplicated for complicated namespacing reasons!
          # small_map_UI("housing-left")
        ),
        
        startExpanded = TRUE),
      
      # Transport ----
      menuItem(
        i18n$t("Transport"),
        tabName = "transport",

        # Accessibility
        menuSubItem(
          i18n$t("Accessibility to urban opportunities"),
          tabName = "accessibility"),

        # Mode switching
        menuSubItem(
          i18n$t("Commuter mode switching"),
          tabName = "mode"),
        
        # Safety
        menuSubItem(
          i18n$t("Safety"),
          tabName = "crash"),

        startExpanded = TRUE),
      
      # Urban life
      menuItem(
        i18n$t("Urban life"),
        tabName = "urban_life",

        # CanALE
        menuSubItem(
          i18n$t("Active living potential"),
          tabName = "canale"),
        conditionalPanel(
          condition = "input.tabs == 'canale'",
          # The ID needs to be duplicated for complicated namespacing reasons!
          small_map_UI("canale-left")),

        # Green alleys
        menuSubItem(
          i18n$t("Green alleys"),
          tabName = "alley"),
        conditionalPanel(
          condition = "input.tabs == 'alley'",
          # The ID needs to be duplicated for complicated namespacing reasons!
          small_map_UI("alley-left")
          ),

        startExpanded = TRUE),
      
      hr(),
      menuItem(i18n$t("Place explorer"), tabName = "place_explorer"),
      hr(),
      menuItem(i18n$t("Why a dashboard?"), tabName = "why_dash"),
      menuItem(i18n$t("Meet the team"), tabName = "meet_the_team")
      
      ), 
    collapsed = FALSE),
  
  
  ## Body --------------------------------------------------------------------
  
  dashboardBody(
    
    tags$head(tags$link(rel = "icon", type = "image/png", href = "logo.png")),
    tags$head(tags$script(HTML(js))),
    tags$head(tags$script(HTML(js2))),
    tags$head(tags$script(HTML(js3))),
    tags$head(tags$style(HTML(styler))),
    
    waiter::use_waiter(),
    waiter::waiter_show_on_load(
      html = shiny::tagList(img(src = "Sus_logo_LoadPage.png", 
                                style = "height:50vh; max-height:600px;"), 
                            spin_folding_cube()), 
      color = "#D8F5FF"),
    
    absolutePanel(
      id = "language_button", 
      style = "z-index: 9998; border-color: #FFFFFF00; background-color: #FFFFFF00;", 
      class = "panel panel-default", top = 10, right = 60, width = 0,
      tagList(usei18n(i18n), actionButton(
        "language_button", label = "EN/FR", 
        style = "color: #3C3C3B; background-color: #0096C940; 
        border-color: #FFFFFF;border-radius: 50px; 
        border-width: 1px;  padding:7px; font-size:100%"))
    ),
    
    tabItems(
      
      # Home page
      tabItem(tabName = "home", fluidPage(
        id = 'home', tags$style('#home {background-color: #FFFFFF;}'),
        fluidRow(
          img(src = "SUSLOGO.png", style = "height:65vh; max-height:600px;"), 
          align = "center"),
        fluidRow(hr()),
        fluidRow(
          img(src = "mssi_logo.png", style = "height:10vh; max-height:70px"), 
          align = "center"),
        fluidRow(
          HTML(paste0(
            "<h5>An initiative of the <a href = 'https://www.mcgill.ca/mssi/'>McGill ",
            "Sustainability Systems Initiative</a></h5>")), align = "center")
      )), 
      
      # Modules
      # tabItem(tabName = "accessibility", Accessibility_module_UI("accessibility_module", i18n = i18n)),
      tabItem(tabName = "alley", alley_UI("alley")),
      # tabItem(tabName = "biodiversity", Biodiversity_module_UI("biodiversity_module", i18n = i18n)),
      tabItem(tabName = "canale", canale_UI("canale")),
      tabItem(tabName = "climate_risk", climate_risk_UI("climate_risk")),
      tabItem(tabName = "crash", crash_UI("crash")),
      tabItem(tabName = "housing", housing_UI("housing")),
      tabItem(tabName = "meet_the_team", meet_the_team_UI("meet_the_team")),
      # tabItem(tabName = "mode", Mode_switch_module_UI("Mode_switch_module")),
      # tabItem(tabName = "ped", ped_UI("ped")),
      tabItem(tabName = "why_dash", why_dash_UI("why_dash"))
      
    )
  ),
  
  skin = "black", title = "Sus - towards a sustainable city"
)
