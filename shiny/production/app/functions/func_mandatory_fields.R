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
    } else if (x %in% char_no_digit_ids) {
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

# mandatoryfields_check <- function(fields, input) {
#   all(vapply(fields, function(id) {
#     val <- input[[id]]
#     if (is.null(val)) return(FALSE)
#
#     # email must be non-empty and look like an email
#     if (id == "email") {
#       return(is.character(val) && nzchar(trimws(val)) &&
#                grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", val))
#     }
#
#     # dates must not be NA
#     if (inherits(val, "Date")) return(!is.na(val))
#
#     # numbers: must be finite; 0 is fine
#     if (is.numeric(val)) return(is.finite(val) && !is.na(val))
#
#     # strings: must be non-empty after trim
#     if (is.character(val)) return(nzchar(trimws(val)))
#
#     # logicals (not used here): just not NA
#     if (is.logical(val)) return(!is.na(val))
#
#     # fallback
#     !is.null(val)
#   }, logical(1)))
# }
#
