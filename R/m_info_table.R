#### INFO TABLE MODULE #########################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param var_left,var_right A reactive which resolves to a character string
#' representing the left and right variables to be analyzed. Each 
#' should have both a "raw" version and a quantile version with the suffix 
#' "_q3".
#' @param select A reactive which resolves to a character string giving the ID
#' of a row in the input data frame (`x`) which has been selected.


info_table_UI <- function(id) {
  htmlOutput(NS(id, "info_table"))
  }

info_table_server <- function(id, x, var_left, var_right, select, zoom, 
                              var_left_title, var_right_title,
                              var_left_label, var_right_label) {
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(select))
  stopifnot(is.reactive(zoom))
  stopifnot(is.reactive(var_left_title))
  
  moduleServer(id, function(input, output, session) {
    
    output$info_table <- renderUI({
      
      # Get data list
      z <- make_info_table_data(id, x, var_left, var_right, select, zoom, 
                                var_left_title, var_right_title,
                                var_left_label, var_right_label)
      
      print(z$table_type)
      
      # Special case for Kahnawake
      if (z$table_type == "kah_na") {
        out <- HTML(glue(sus_translate(paste0(
          "<strong>Kahnawake Mohawk Territory</strong>",
          "<p>Statistics Canada does not gather the same ",
          "data for indigenous reserves in the Census as it does ",
          "for other jurisdictions, so we cannot display findings ",
          "here."))))
      }
      
      # Special case for Kanestake
      if (z$table_type == "kan_na") {
        out <- HTML(glue(sus_translate(paste0(
          "<strong>Kanehsatà:ke</strong>",
          "<p>Statistics Canada does not gather the same ",
          "data for indigenous reserves in the Census as it does ",
          "for other jurisdictions, so we cannot display findings ",
          "here."))))
        }
        
      # Univariate, NA selection
      if (z$table_type == "uni_na") {
        out <- HTML(glue(sus_translate(paste0(
          "{z$place_name} has no data available on {z$exp_left}."))))
        }
      
      # Univariate, quantitative, no selection
      if (z$table_type == "uni_quant_all") {
        out <- HTML(glue(sus_translate(paste0(
          "At the {z$scale_singular} scale, {z$exp_left} varies from ",
          "{z$min_val} to {z$max_val}, with an average value of {z$mean_val} ",
          "and a median value of {z$median_val}. ",
          "Two thirds of {z$scale_plural} have a score between {z$quant_low} ",
          "and {z$quant_high}."))))
        }
      
      # Univariate, quantitative, valid selection
      if (z$table_type == "uni_quant_select") {
        out <- HTML(glue(sus_translate(paste0(
          "<strong>{z$place_heading}</strong>",
          "<p>{z$place_name} has a population of ",
          "{prettyNum(round(z$selection$population), ',')} and a ", 
          "{z$title_left} score of {round(z$poly_value, 2)}, which is ", 
          "{z$larger_smaller} the region-wide median of {z$median_val}.",
          "<p>{z$place_name} has {z$poor_strong} potential for active ",
          "living, with a {z$title_left} score higher than {z$percentile}% ",
          "of {z$scale_plural} in the Montreal region."))))
        }

      # Univariate, qualitative, no selection
      if (z$table_type == "uni_qual_all") {
        out <- HTML(glue(sus_translate(paste0(
          "At the {z$scale_singular} scale, {z$exp_left} varies from ",
          "'{z$min_val}' to '{z$max_val}'. A plurality of {z$scale_plural} ",
          "({z$mode_prop}) have a value of '{z$mode_val}', while ",
          "{z$mode_prop_2} have a value of '{z$mode_val_2}'."))))
        }

      # Univariate, qualitative, valid selection
      if (z$table_type == "uni_qual_select") {
        out <- HTML(glue(sus_translate(paste0(
          "<strong>{z$place_heading}</strong>",
          "<p>{z$place_name} has a population of ",
          "{prettyNum(round(z$selection$population), ',')} and a {z$title_left} ",
          "score of {round(z$poly_value, 2)}, which is {z$larger_smaller} ",
          "the region-wide median of {z$median_val}.",
          "<p>{z$place_name} has {z$poor_strong} potential for active ",
          "living, with a {z$title_left} score higher than {z$percentile}% ",
          "of {z$scale_plural} in the Montreal region."))))
      }
      
      # Bivariate, quantitative, no selection
      if (z$table_type == "bi_quant_xy_all") {
        # If correlation is close to zero
        if (z$correlation < 0.05 && z$correlation > -0.05) {
          out <- HTML(glue(sus_translate(paste0(
            "<p>'{z$var_name}' is {z$exp_right}.",
            "<p>The {z$title} has effectively no correlation ",
            "({z$correlation}) with {z$exp_right} at the ",
            "{z$scale_singular} scale.",
            "<p>This means that, at the {z$scale_singular} scale, ",
            "there is no relationship between the two variables."))))
          } else {
            out <- HTML(glue(sus_translate(paste0(
              "<p>'{z$var_name}' is {z$exp_right}.",
              "<p>The {z$title} has a {z$strong_weak} {z$pos_neg} ",
              "correlation ({z$correlation}) with {z$exp_right} at ",
              "the {z$scale_singular} scale.",
              "<p>This means that, in general, {z$scale_plural} with higher ",
              "potential for active living tend to have {z$higher_lower} ",
              "values for '{z$tolower(var_name)}', {z$high_low_disclaimer}."))))
            }

        return(out)
      }

      # var_name <- sus_translate(variable_explanations %>% 
      #                               filter(var_code == var_right()) %>%
      #                               pull(var_name))
      #   
      #   correlation <- 
      #     cor(x()$left_var_full, x()$right_var_full, use = "complete.obs") %>% 
      #     round(2)
      #   
      #   pos_neg <- if_else(correlation > 0, sus_translate("positive"), 
      #                      sus_translate("negative"))
      #   
      #   strong_weak <- case_when(
      #     abs(correlation) > 0.6 ~ sus_translate("strong"),
      #     abs(correlation) > 0.3 ~ sus_translate("moderate"),
      #     TRUE ~ "weak")
      #   
      #   higher_lower <- if_else(pos_neg == sus_translate("positive"),
      #                           sus_translate("higher"),
      #                           sus_translate("lower"))
      #   
      #   high_low_disclaimer <- case_when(
      #     strong_weak == sus_translate("strong") ~ 
      #       sus_translate("with only a few exceptions"),
      #     strong_weak == sus_translate("moderate") ~ 
      #       sus_translate("although with some exceptions"),
      #     strong_weak == sus_translate("weak") ~ 
      #       sus_translate("although with many exceptions"))
      #   
      #   # Case for no poly selected
      #   if (is.na(select())) {
      #     # If correlation is close to zero
      #     if (correlation < 0.05 && correlation > -0.05) {
      #       HTML(
      #         glue(sus_translate(paste0(
      #           "<p>'{var_name}' is {z$exp_right}.",
      #           "<p>The {z$title} has effectively no correlation ",
      #           "({correlation}) with {z$exp_right} at the ",
      #           "{z$scale_singular} scale.",
      #           "<p>This means that, at the {z$scale_singular} scale, ",
      #           "there is no relationship between the two variables."
      #         )))
      #       )
      #     } else {
      #       HTML(glue(sus_translate(paste0(
      #         "<p>'{var_name}' is {z$exp_right}.",
      #         "<p>The {z$title} has a {strong_weak} {pos_neg} ",
      #         "correlation ({correlation}) with {z$exp_right} at ", 
      #         "the {z$scale_singular} scale.",
      #         "<p>This means that, in general, {z$scale_plural} with higher ",
      #         "potential for active living tend to have {higher_lower} ",
      #         "values for '{tolower(var_name)}', {high_low_disclaimer}.")))
      #         )
      #       }
      #     
      #     # Case for poly selected
      #   } else {
      #     dat <- x() %>% filter(ID == select())
      #     
      #     vec_2 <-
      #       x() %>%
      #       filter(!is.na(right_var), !is.na(right_var_full)) %>%
      #       pull(right_var_full)
      #     
      #     poly_value_1 <- dat$left_var_full
      #     poly_value_2 <- dat$right_var_full
      #     
      #     # print("polyselect")
      #     place_name <- case_when(
      #       z$scale_singular == sus_translate("borough/city") ~ 
      #         glue("{dat$name}"),
      #       z$scale_singular == sus_translate("census tract") ~
      #         glue(sus_translate(paste0("Census tract {dat$name}"))),
      #       z$scale_singular == sus_translate("dissemination area") ~
      #         glue(sus_translate(paste0("Dissemination area {dat$name}")))
      #     )
      #     
      #     if (dat$name_2 == "Borough" | dat$name_2 == "City") {
      #       dat$name_2 <- sus_translate(glue("{dat$name_2}"))
      #     }
      #     
      #     place_heading <-
      #       if_else(z$scale_singular == sus_translate("borough/city"),
      #               glue(sus_translate(paste0("{dat$name_2} of {z$place_name}"))),
      #               glue("{z$place_name} ({dat$name_2})"))
      #     
      #     percentile_left <- 
      #       round(length(vec[vec <= dat$left_var_full]) / length(vec) * 100)
      #     
      #     percentile_right <- round(length(vec_2[vec_2 <= dat$right_var_full]) / 
      #                                 length(vec_2) * 100)
      #     
      #     relative_position <- case_when(
      #       abs(percentile_left - percentile_right) > 50 ~ 
      #         sus_translate("dramatically different"),
      #       abs(percentile_left - percentile_right) > 30 ~ 
      #         sus_translate("substantially different"),
      #       abs(percentile_left - percentile_right) > 10 ~ 
      #         sus_translate("considerably different"),
      #       TRUE ~ sus_translate("similar")
      #     )
      #     
      #     # Special case for Kahnawake
      #     if (dat$ID %in% c(56, "4620832.00", 24670285)) {
      #       HTML(
      #         glue(sus_translate(paste0(
      #           "<strong>Kahnawake Mohawk Territory</strong>",
      #           "<p>Statistics Canada does not gather the same ",
      #           "data for indigenous reserves in the Census as it does ",
      #           "for other jurisdictions, so we cannot display findings ",
      #           "here."
      #         )))
      #       )
      #     } else {
      #       HTML(
      #         glue(sus_translate(paste0(
      #           "<strong>{place_heading}</strong>",
      #           
      #           "<p>{z$place_name} has a population of ",
      #           "{prettyNum(dat$population, ',')}, a {z$title} score ",
      #           "of {round(poly_value_1, 2)}, and a '{tolower(var_name)}' ",
      #           "value of {round(poly_value_2, 2)}. ",
      #           
      #           "<p>These two scores are {relative_position}, in relative ",
      #           "terms. {z$place_name} has a {z$title} score higher ",
      #           "than {percentile_left}% of {z$scale_plural} and ",
      #           "a '{tolower(var_name)}' score higher than ",
      #           "{percentile_right}% of {z$scale_plural} in the ",
      #           "Montreal region.")))
      #       )
      #     }
      
      return(out)  
      })
  })
}
