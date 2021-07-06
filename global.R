#### SUS GLOBALS ###############################################################

# Packages ----------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(shiny.i18n)
  library(waiter)
  
  library(dplyr)
  library(ggplot2)
  
  library(sf)
  library(mapdeck) 
  library(mapboxapi)
  
  library(DT)
  library(qs)
  library(glue)
})


# Shiny options -----------------------------------------------------------

shinyOptions(cache = cachem::cache_disk("./app-cache"))
options(shiny.trace = FALSE) # Set TRUE for debugging


# Data --------------------------------------------------------------------

qload("data/census.qsm")
grid <- qread("data/grid.qs")
qload("data/colours.qsm")
title_text <- qread("data/title_text.qs")
var_exp <- qread("data/var_exp.qs")


# Translation -------------------------------------------------------------

suppressWarnings({
  i18n <- Translator$new(translation_csvs_path = "translations/")})
i18n$set_translation_language("fr")
translation_fr <- qread("data/translation_fr.qs")
sus_reactive_variables <- reactiveValues()


# Map style ---------------------------------------------------------------

map_style <- "mapbox://styles/dwachsmuth/ckh6cg4wg05nw19p5yrs9tib7"
map_zoom <- 10.1
map_location <- c(-73.58, 45.53)


# Functions ---------------------------------------------------------------

right_panel <- function(id, ...) {
  
  absolutePanel(
    id = NS(id, "right_panel"), 
    style = paste0("z-index:500; max-height: 91vh; overflow-y: auto; ",
                   "overflow-x:hidden; padding: 5px; border-width: 0px;"),
    class = "panel panel-default", top = 70, right = 20, width = 300,
    ...
  )
}

loadingLogo <- 
  function(href, src, loadingsrc) {
    tagList(
      tags$head(
        tags$script(
          "setInterval(function() {
        if ($('html').attr('class')=='shiny-busy') {
        $('div.busy').show();
        $('div.notbusy').hide();
        } else {
        $('div.busy').hide();
        $('div.notbusy').show();
        }
        },100)")
      ),
      tags$a(href = href, div(class = "busy",
                              img(src = loadingsrc, height = 50, width = 50,
                                  style = "padding: 7px;")),
             div(class = 'notbusy', img(src = src, height = 50, width = 50, 
                                        style = "padding: 7px;"))
             )
    )
  }


# Load data ---------------------------------------------------------------
## THESE ALL NEED TO BE TURNED INTO QS BINARIES

# # Load data for pedestrian realm 
# load(file = "data/sidewalks_WSG.Rdata")
# load(file = "data/census_circular.Rdata")
# load(file = "data/original_VAS_plan.Rdata")
# load(file = "data/revised_VAS_plan.Rdata")
# load(file = "data/census_analysis_quantile.Rdata")
# load(file = "data/census_analysis_ct.Rdata")
# 
# census_analysis_quantile_WSG <- census_analysis_quantile %>% 
#   st_transform(4326)
# 
# #mode
# cycling1 <- loadRData("data/car_1_finals.Rdata")
# cycling2 <- loadRData("data/car_3_finals.Rdata")
# cycling_network <- loadRData("data/reseau_cyclable.Rdata")
# car_share <- loadRData("data/Car_Share.Rdata")
# cycling_access <- loadRData("data/Cycling_Access.Rdata")
# trip_distance <- loadRData("data/Trip_Distance.Rdata")
# 
# load("data/cycling_total_final.Rdata")

dropshadow_right <- normalizePath(file.path("www/dropshadow_right.png"))
dropshadow_left <- normalizePath(file.path("www/dropshadow_left.png"))
uni_legend_left <- normalizePath(file.path("www/univariate_left.png"))
uni_legend_right <- normalizePath(file.path("www/univariate_right.png"))


# Other prep --------------------------------------------------------------

# This doesn't work with module namespacing TKTK
module_style <- 
  tags$head(tags$style(HTML("
          #title_bar {border-width: 10px; border-color: rgb(255, 255, 255);}
          #input_control_overlay {border-width: 10px; 
          border-color: rgba(255,255,255,1);}
          #input_control_left {background-color: rgba(0,0,255,0.0);
          border-width: 0px;}
          #input_control_left2 {background-color: rgba(0,0,255,0.0);
          border-width: 0px;}
          #legend_container {background-color: rgba(0,0,255,0.0);
          border-width: 0px;}")))

js_ped_1 <- "$(document).ready(function(){
  $('#plotContainer').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
      $this.css('opacity', 0).animate({opacity: 1}, {duration: 1000});
    })
  });
});
"

js_ped <- "$(document).ready(function(){
  $('#plotContainer_ped').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
      $this.css('opacity', 0).animate({opacity: 1}, {duration: 1000});
    })
  });
});
"

js_ped_control <- "$(document).ready(function(){
  $('#plotContainer_ped_control').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
      $this.css('opacity', 0).animate({opacity: 1}, {duration: 1000});
    })
  });
});
"

js <- "
  $(document).ready(function(){
  $('#plotContainer').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
      $this.css('opacity', 0).animate({opacity: 1}, {duration: 1000});
    })
  });
});
"

js2 <- "
$(document).ready(function(){
  $('#menuContainer').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
       $(this).css('opacity', 1).animate({opacity: 0}, {duration: 1000});
    })
  });
});
"


js3 <- "
$(document).ready(function(){
  $('#plotContainer2').on('show', function(){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  }).on('hide', function(){
    var $this = $(this);
    setTimeout(function(){
       $(this).css('opacity', 1).animate({opacity: 0}, {duration: 1000});
    })
  });
});
"

styler <- '
      /* logo */
      .skin-black .main-header .logo {
      background-color: #FFFFFF;
      }
      
      /* logo when hovered */
      .skin-black .main-header .logo:hover {
      background-color: #FFFFFF;
      }
      
      /* navbar (rest of the header) */
      .skin-black .main-header .navbar {
      background-color: #FFFFFF;
      }
      
      /* main sidebar */
      .skin-black .main-sidebar {
      background-color: #FFFFFF;
      
      }
      
      /* active selected tab in the sidebarmenu */
      .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
      background-color: #0096C9;
      color: #FFFFFF;
      
      }
      
      /* other links in the sidebarmenu */
      .skin-black .main-sidebar .sidebar .sidebar-menu a{
      background-color: #FFFFFF50;
      color: #3C3C3B;
      height: 40px;
      }
      
      /* other links in the sidebarmenu when hovered */
      .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
      background-color: #0096C910;
      }
      
      /* toggle button when hovered  */
      .skin-black .main-header .navbar .sidebar-toggle:hover{
      background-color: #FFFFFF;
      }
      
      /* body */
      .content-wrapper, .right-side {
      background-color: #FFFFFF;
      }
      
      /* expanded menus */
      .skin-black .sidebar-menu > li > .treeview-menu {
      margin: 0 1px;
      background: #FFFFFF;
      }


                                '
