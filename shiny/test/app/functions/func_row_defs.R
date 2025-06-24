############################################################################################
#
#  Functions for row layout reactable
#
#############################################################################################

func_bar_row_defs <- function(value, index, feature_vec, bar_vars, rename_map) {
  original_var <- names(rename_map)[match(feature_vec[index], rename_map)]
  if (is.null(original_var) || !(original_var %in% bar_vars)) return(value)

  if (is.na(value) || value == "") return("")

  val <- suppressWarnings(as.numeric(value))
  if (is.na(val)) return(value)

  bar_width <- paste0((val / 10) * 100, "%")

  div(
    style = list(display = "flex", alignItems = "center", gap = "6px"),
    div(
      style = list(
        background = "#f15a29",
        height = "10px",
        width = bar_width,
        minWidth = "10px"
      )
    ),
    span(
      style = list(fontSize = "12px"),
      round(val, 1)
    )
  )
}


#yes/no
func_yn_row_defs <- function(value, index, feature_vec, yn_vars, rename_map) {
  original_var <- names(rename_map)[match(feature_vec[index], rename_map)]
  if (is.null(original_var)) return(value)
  if (original_var %in% yn_vars) return(cells_yes_no(value))
  return(value)
}
