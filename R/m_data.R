#### CENSUS DATA MODULE ########################################################

#' @param id A character string representing the module id. Not otherwise used.
#' @param var_left,var_right A reactive which resolves to a character string
#' representing the left and right variables to be mapped and analyzed. Each 
#' should have both a "raw" version and a quantile version with the suffix 
#' "_q3".
#' @param df A reactive which resolves to a character string representing the
#' underlying data set to be loaded. Currently available options are 
#' `c("borough", "CT", "DA", "DA_2" and "grid")`, although "DA_2" is silently 
#' converted to "DA" and is present as a convenience feature to avoid having to
#' separately specify the `zoom` argument.
#' @param zoom A reactive which resolves to a character string representing the
#' amount of transparency to be applied to the fill aesthetic in maps made from
#' the data. Currently available options are `c("borough", "CT", "DA", "DA_2")`
#' and by default this argument takes its value from the `df` parameter.
#' @return A reactive expression containing a data frame with the following
#' fields (If the `var_right` input is " ", the right_var_full and right_var 
#' fields will be omitted.):
#' - ID, name, name_2, population: The unmodified variables of the same names
#' from the input data frame.
#' - left_var_full, left_var: The unmodified and quantile versions, 
#' respectively, of the variable whose name was given in `var_left`.
#' - right_var_full, right_var: The unmodified and quantile versions, 
#' respectively, of the variable whose name was given in `var_right`.
#' - geometry: The unmodified geometry variable from the input data frame.
#' - group: A character field of form "X - Y", where X and Y are the quantile
#' values from left_var and right_var respectively.
#' - fill: A character vector of hex colour values to be passed to mapdeck for
#' colouring choropleths drawn from the data frame.

data_server <- function(id, var_left, var_right, df, zoom = df) {
  stopifnot(is.reactive(var_left))
  stopifnot(is.reactive(var_right))
  stopifnot(is.reactive(df))
  stopifnot(is.reactive(zoom))
  
  moduleServer(id, function(input, output, session) {
    reactive({
      
      # Get borough/CT/DA/grid, and turn DA_2 into DA
      data <- get(sub("_2", "", df()))
      
      # Set the colour transparency based on zoom level
      colour <- get(paste0("colour_", zoom()))

      # # Create proper var_left/var_right strings based on time
      # var_left <- paste0(var_left(), if (!missing(time)) "_", time)
      # if (var_right() != " ") var_right <- paste0(var_right(), 
      #                                             if (!missing(time)) "_", time)
      
      if (var_right() == " ") {
        data <-
          data %>%
          dplyr::select(ID, name, name_2, population, 
                        left_var_full = all_of(var_left()),
                        left_var = paste0(var_left(), "_q3")) %>% 
          mutate(group = paste(left_var, "- 1")) %>% 
          left_join(colour, by = "group")
        
        } else {
          data <-
            data %>%
            dplyr::select(ID, name, name_2, population, 
                          left_var_full = all_of(var_left()),
                          left_var = paste0(var_left(), "_q3"),
                          right_var_full = all_of(var_right()), 
                          right_var = paste0(var_right(), "_q3")) %>% 
            mutate(group = paste(left_var, "-", right_var)) %>% 
            left_join(colour, by = "group")
          }
      
      st_crs(data) <- 4326
      data
      })
    })
}
