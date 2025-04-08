############################################################################################
#
#  Function Feature (extensive) filter
#
#############################################################################################

# Range filter helper
range_filter <- function(df, var, input_range) {
  filter(df, is.na(.data[[var]]) | between(.data[[var]], input_range[1], input_range[2]))
}

# Checkbox filter helper
checkbox_filter <- function(df, var, input_value) {
  filter(df, !input_value | .data[[var]] == "Yes")
}

# SelectInput filter helper
select_filter <- function(df, var, selected) {
  if (is.null(selected)) return(df)
  filter(df, .data[[var]] %in% selected)
}
