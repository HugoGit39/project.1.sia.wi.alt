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
             bs4Card(
               title = "Original Paper",
               width = 12,
               collapsible = FALSE,
               headerBorder = FALSE,
               footer = NULL,
               solidHeader = TRUE,
               status = "primary",
               tags$div(
                 style = "text-align: center; margin-bottom: 1rem; display: flex; justify-content: center; align-items: center; gap: 1rem;",
                 downloadButton(ns("download_pdf"), "Download PDF", class = "btn-primary"),
                 tags$a(
                   href = "https://link.springer.com/article/10.3758/s13428-025-02685-4",
                   target = "_blank",
                   tags$img(
                     src = "springer_link.png",
                     height = "40px",  # adjust size as needed
                     alt = "Springer Link"
                   )
                 )
               ),
               tags$iframe(
                 src = "s13428-025-02685-4.pdf",
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
        "s13428-025-02685-4.pdf"
      },
      content = function(file) {
        file.copy("www/s13428-025-02685-4.pdf", file)
      }
    )
  })
}
