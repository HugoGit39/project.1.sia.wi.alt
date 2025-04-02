############################################################################################
#
#  Function nodule  for controlbar
#
#############################################################################################

# ui
mod_control_ui <- function(id) {
  ns <- NS(id)

    dashboardControlbar(
      tags$head(
        tags$style(HTML("
      .control-sidebar {
        overflow-y: auto;
        padding-top: 25px !important; /* This moves everything inside 10px lower */
      }
    ")),
      ),
      skin = "light",
      pinned = FALSE,
      collapsed = TRUE,
      overlay = TRUE
    )
}

#server
mod_control__server <- function(id) {
  moduleServer(id, function(input, output, session) {


  })
}
