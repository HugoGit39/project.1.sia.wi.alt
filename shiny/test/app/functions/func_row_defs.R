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

#numerical colors
func_numeric_row_defs <- function(value, index, feature_internal, numeric_vars, numeric_var_ranges, palette) {
  var <- feature_internal[index]
  if (is.null(var) || !(var %in% numeric_vars)) return(value)

  rng <- numeric_var_ranges[[var]]
  if (is.null(rng) || any(!is.finite(rng)) || identical(rng[1], rng[2])) return(value)

  x <- suppressWarnings(as.numeric(value))
  if (!is.finite(x)) return(value)

  # normalize 0..1 and pick a color (white -> dark blue)
  p <- (x - rng[1]) / (rng[2] - rng[1])
  p <- max(0, min(1, p))
  idx <- 1 + floor(p * (length(palette) - 1))
  bg <- palette[idx]

  div(style = list(background = bg), value)
}

