############################################################################################
#
#  Function for mandatory fields
#
#############################################################################################

# function to add red star (*) to mandatory labels
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

mandatoryfields_check <- function(fields, input) {
  vapply(fields, function(x) {
    value <- input[[x]]

    if (is.null(value)) {
      FALSE
    } else if (x == "email") {
      grepl("@", value)
    } else if (x %in% names(char_only_fields)) {
      is.character(value) && value != "" && !grepl("\\d", value)
    } else if (is.character(value)) {
      value != ""
    } else if (is.numeric(value)) {
      !is.na(value)
    } else if (inherits(value, "Date")) {
      !is.na(value)
    } else {
      TRUE
    }
  }, logical(1)) |> (\(res) {
    all(res)
  })()
}
