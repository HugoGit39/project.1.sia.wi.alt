############################################################################################
#
#  Function nodule  for Article
#
#############################################################################################

# ui
mod_article_ui <- function(id) {
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
mod_article_server <- function(id) {
  moduleServer(id, function(input, output, session) {


  })
}
