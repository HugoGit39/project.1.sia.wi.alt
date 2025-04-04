############################################################################################
#
#  Function nodule for add data
#
#############################################################################################

# ui
mod_sub_data_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(width = 4,
             bs4Card(title = "Comming Soon",
                     status = "primary",
                     width = NULL,
                     collapsible = FALSE,
                     solidHeader = TRUE,
                     id = "submit_data")
      )
    )
  )
}

#server
mod_sub_data__server <- function(id) {
  moduleServer(id, function(input, output, session) {


  })
}
