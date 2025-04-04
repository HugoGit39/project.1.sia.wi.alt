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

# function to check if all mandatory fields are filled
mandatoryfields_check <- function(fields, input) {
  mandatoryFilled <- vapply(
    fields,
    function(x) {
      value <- input[[x]]
      if (is.null(value) || value == "") {
        FALSE
      } else if (x == "email") {
        grepl("@", value)
      } else {
        TRUE
      }
    },
    logical(1)
  )
  all(mandatoryFilled)
}

