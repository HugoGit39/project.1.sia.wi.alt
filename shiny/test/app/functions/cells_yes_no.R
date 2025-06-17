cells_yes_no <- function(value) {
  if (is.na(value)) return("")
  if (value == "Yes") {
    div(style = "color: #44AA99; font-weight: bold;", "✔ Yes")
  } else if (value == "No") {
    div(style = "color: #882255; font-weight: bold;", "✖ No")
  } else {
    value
  }
}
