### PLACE EXPLORER MODULE GLOBALS ##############################################

# Regex used to detect a postal code
pc_regex <- c("(\\w\\d\\w\\d\\w\\d)|(\\w\\d\\w\\s\\d\\w\\d)")

# Load all postal codes and their geolocation
postal_codes <- qread("data/postal_codes.qs")
