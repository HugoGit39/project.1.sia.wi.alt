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
# mandatoryfields_check <- function(fields, input) {
#   mandatoryFilled <- vapply(
#     fields,
#     function(x) {
#       value <- input[[x]]
#       if (is.null(value) || value == "") {
#         FALSE
#       } else if (x == "email") {
#         grepl("@", value)
#       } else {
#         TRUE
#       }
#     },
#     logical(1)
#   )
#   all(mandatoryFilled)
# }
#
# mandatoryfields_check <- function(fields, input) {
#   mandatoryFilled <- vapply(
#     fields,
#     function(x) {
#       value <- input[[x]]
#       if (is.null(value)) {
#         FALSE
#       } else if (is.character(value)) {
#         value != ""
#       } else if (is.numeric(value)) {
#         !is.na(value)
#       } else if (x == "email") {
#         grepl("@", value)
#       } else if (inherits(value, "Date")) {
#         !is.na(value)
#       } else {
#         TRUE
#       }
#     },
#     logical(1)
#   )
#   all(mandatoryFilled)
# }

# mandatoryfields_check <- function(fields, input) {
#   mandatoryFilled <- vapply(
#     fields,
#     function(x) {
#       value <- input[[x]]
#       if (is.null(value)) {
#         FALSE
#       } else if (x == "email") {
#         grepl("@", value)
#       } else if (is.character(value)) {
#         value != ""
#       } else if (is.numeric(value)) {
#         !is.na(value)
#       } else if (inherits(value, "Date")) {
#         !is.na(value)
#       } else {
#         TRUE
#       }
#     },
#     logical(1)
#   )
#   all(mandatoryFilled)
# }

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
    cat("ALL FIELDS VALID:", all(res), "\n\n")
    all(res)
  })()
}
