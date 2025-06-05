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
      column(width = 12,
             bs4Card(title = "Original Paper",
                     width = 12,
                     collapsible = FALSE,
                     headerBorder = FALSE,
                     footer = NULL,
                     solidHeader = TRUE,
                     status = "primary",
                     tags$div(
                       style = "text-align: center; margin-bottom: 1rem;",
                       downloadButton(ns("download_pdf"), "Download PDF", class = "btn-primary")
                     ),
                     tags$iframe(
                       src = "wearables_database_paper.pdf",
                       style = "width:100%; height:842px; border:none;"
                     )
             )
      )
    )
  )
}

mod_article_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$download_pdf <- downloadHandler(
      filename = function() {
        "wearables_database_paper.pdf"
      },
      content = function(file) {
        file.copy("www/wearables_database_paper.pdf", file)
      }
    )
  })
}
