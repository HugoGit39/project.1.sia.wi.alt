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
mandatoryfields_check <- function(input, fieldsMandatory) {
  all(sapply(fieldsMandatory, function(x) {
    !is.null(input[[x]]) && input[[x]] != ""
  }))
}
