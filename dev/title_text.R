#### title_text data setup #####################################################

library(tidyverse)
library(qs)

title_text <- 
  read_csv("dev/data/title_text.csv")

title_text <-
  title_text %>%
  add_row(tab = "ped", type = "title",
          text = "Pedestrian capacity for social distancing", .before = 6) %>% 
  add_row(tab = "housing", type = "title",
          text = "The housing realm", .before = 6) %>% 
  add_row(tab = "housing", type = "main",
          text = "Housing is important", .before = 6) %>% 
  add_row(tab = "housing", type = "extra",
          text = "VERY important", .before = 6) %>% 
  mutate(tab = if_else(tab == "pedestrian_ct", "ped", tab)) %>% 
  add_row(tab = "alley", type = "title",
          text = "Green alleys") %>% 
  add_row(tab = "alley", type = "main",
          text = "Text TKTK") %>% 
  add_row(tab = "alley", type = "extra",
          text = "Text TKTK") 
  
qsave(title_text, "data/title_text.qs")
