############################################################################################
#
#  Function Feature (extensive) filter
#
#############################################################################################

# Helper function for range filtering
range_filter <- function(column, input_range) {
  if (is.null(input_range) || length(input_range) < 2) {
    TRUE  # If input is NULL or incomplete, return TRUE (include all rows)
  } else {
    is.na(df[[column]]) | (df[[column]] >= input_range[1] & df[[column]] <= input_range[2])
  }
}

# Helper function for selectInput filters
select_filter <- function(column, input_values) {
  if (is.null(input_values)) {
    TRUE  # If input is NULL, include all rows
  } else {
    df[[column]] %in% input_values
  }
}

# Helper function for checkbox filters
checkbox_filter <- function(column, input_value) {
  if (is.null(input_value)) {
    TRUE  # If input is NULL, include all rows
  } else {
    !input_value | df[[column]] == "Yes"
  }
}
