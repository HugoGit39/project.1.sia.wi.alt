############################################################################################
#
#  Function nodule for nfeauture filter (extensive)
#
#############################################################################################

#' Module UI for Time-out
mod_timeout_ui <- function(id) {
  ns <- NS(id)

  tagList(

  )
}

# Module server for Time-out M
mod_timeout_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    sever(html = disconnected, bg_color = "#DDDDDD75", color = "black", box = T)

  })
}


