### STREET MODULE GLOBALS ######################################################

# Map token
token_street <- paste0("pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
                       "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ")

# Initialize reactive values
rv_street <- reactiveValues(prev_zoom = 1, zoom = 1, path_selected = NA)
