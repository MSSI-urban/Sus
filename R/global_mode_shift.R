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
  color_value = c('#ECF4CD','#C6DE68','#B2D235','#8AA324','#5C6D18'),
  stringsAsFactors = F
)

# cycling_access <- left_join(cycling_access, df_pal1, by = "color")

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
  color_value = c('#CAF0F8','#90E0EF','#00B4D8','#0077B6','#005D7C'),
  stringsAsFactors = F
)

# car_share <- left_join(car_share, df_pal2, by = "color")

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
  color_value = c('#004BC9','#0071C9','#0096C9','#BE9735','#C95C34'),
  stringsAsFactors = F
)

# trip_distance <- left_join(trip_distance, df_pal3, by = "color")

legend_po3 <- legend_element(
  variables = c("2.3 - 6.4","6.5 - 7.8","7.9 - 8.9","9.0 - 10.4","10.5 - 22.6"),
  colours = c('#004BC9','#0071C9','#0096C9','#BE9735','#C95C34'),
  colour_type = "fill",
  variable_type = "category",
  title = "Average Commuting Distance (km)"
)

legend3 <- mapdeck_legend(legend_po3)
