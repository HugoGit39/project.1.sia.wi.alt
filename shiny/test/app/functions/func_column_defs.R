############################################################################################
#
#  Functions for column layout reactable
#
#############################################################################################

#bars
func_bar_column_defs <- function(df, bar_vars, rename_map) {
  setNames(
    lapply(bar_vars, function(var) {
      colDef(
        name = rename_map[[var]],
        cell = data_bars(
          df,
          fill_color = "#f15a29",
          min_value = 0,
          max_value = 10,
          bar_height = 10,
          text_position = "outside-end",
          text_size = 12,
          background = "transparent"
        )
      )
    }),
    bar_vars
  )
}

#yes/no
func_yn_column_defs <- function(yn_vars, rename_map) {
  setNames(
    lapply(yn_vars, function(var) {
      colDef(
        name = rename_map[[var]],
        cell = cells_yes_no
      )
    }),
    yn_vars
  )
}

#numerical colors:
func_numeric_column_defs <- function(df, numeric_vars, rename_map, numeric_var_ranges) {
  palette <- generate_alpha_palette("#1c75bc")

  setNames(
    lapply(numeric_vars, function(var) {
      var_range <- numeric_var_ranges[[var]]
      colDef(
        name = rename_map[[var]],
        style = function(value) {
          if (!is.na(value)) {
            scaled_color <- map_to_colors(
              value,
              rescale(value, to = c(1, length(palette)), from = var_range)
            )
            list(background = palette[round(scaled_color)])
          } else {
            list()
          }
        }
      )
    }),
    numeric_vars
  )
}




