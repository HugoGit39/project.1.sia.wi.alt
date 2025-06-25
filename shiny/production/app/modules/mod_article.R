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
      column(
        width = 12,
        bs4Card(
          title = "Summary of the Research",
          status = "secondary",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          fluidRow(
            column(
              width = 3,
              bs4Dash::bs4Card(
                title = "Why Needed?",
                status = "primary",
                solidHeader = FALSE,
                collapsible = FALSE,
                width = 12,
                style = "height: 450px; overflow-y: auto;",
                p("• Rapid growth of available consumer and research-oriented wearables", style = "text-align: justify;"),
                p("• To systematically choose a wearable among the multitude of available devices, one should perform:", style = "text-align: justify;"),
                p("o Get a detailed overview of technical and other device specific information", style = "text-align: justify;"),
                p("o Retrieve synopsis of Reliability, Validity and Usability studies per wearable", style = "text-align: justify;"),
                p(strong("To reduce the time and effort required to choose the appropriate wearable, the Stress in Action Wearables Database provides this detailed overview."), style = "text-align: justify;")
              )
            ),
            column(
              width = 3,
              bs4Card(
                title = "Literature Research",
                status = "primary",
                solidHeader = FALSE,
                collapsible = FALSE,
                width = 12,
                style = "height: 450px; overflow-y: auto;",
                p(
                  "Device-specific information was obtained from the device manual, while reliability, validity, and usability were assessed through a structured literature search (click SiA gauge) using the following string:", style = "text-align: justify;"),
                  p("“((Device Name) AND (valid* OR reliab* OR compar* OR accur* OR verif* OR usab* OR 'user experience' OR 'user friend*' OR user-friend*)”.", style = "text-align: justify;"),
                  p("Included studies assessed parameter-level convergent validity, test-retest reliability, and/or usability, focused on convergent validity, and were published as peer-reviewed articles or conference proceedings in English. Excluded were studies on construct validity only, those involving machine learning-based detection of secondary outcomes, meta-analyses, reviews, theses, grey literature, and any non-peer-reviewed texts.",
                  style = "text-align: justify;"
                ),
                sidebar = cardSidebar(
                  id = "sb_kr",
                  width = 100,
                  background = "white",
                  icon = tags$img(src = "favicon.ico", height = 25, width = 25),
                  tags$img(
                    src = "article_asreview_process.png",
                    style = "max-width: 100%; height: auto; display: block; margin: 0 auto;"
                  )
                )
              )
            ),
            column(
              width = 3,
              bs4Card(
                title = "SiA Expert Scoring",
                status = "primary",
                solidHeader = FALSE,
                collapsible = FALSE,
                width = 12,
                style = "height: 450px; overflow-y: auto;",
                p(
                  "Different criteria were rank-ordered in importance by the curators to score a device for short-term (2-day) and long-term (2+ week) research use.", style = "text-align: justify;"),
                p("Based on these criteria (click SiA gauge), each device was independently scored from 0 to 10 by the three first co-authors, who were blinded to each other's scores; these scores were then averaged to generate the short- and long-term \"SiA expert scores.\" ", style = "text-align: justify;"),
                p("High interrater reliability was achieved for both short-term (r = .87, 95% CI = [.78, .92], F(50, 100) = 8.0, p < .001) and long-term use (r = .85, 95% CI = [.76, .91], F(50, 100) = 6.6, p < .001).",
                  style = "text-align: justify;"
                ),
                sidebar = cardSidebar(
                  id = "sb_sia_es",
                  width = 100,
                  background = "white",
                  icon = tags$img(src = "favicon.ico", height = 25, width = 25),
                  tags$img(
                    src = "article_long_term_sia_es.png",
                    style = "max-width: 100%; height: auto; display: block; margin: 0 auto;"
                  ),
                  tags$img(
                    src = "article_short_term_sia_es.png",
                    style = "max-width: 100%; height: auto; display: block; margin: 0 auto;"
                  )
                )

              )
            ),
            column(
              width = 3,
              bs4Card(
                title = "Continuously Updated Database",
                status = "primary",
                solidHeader = FALSE,
                collapsible = FALSE,
                width = 12,
                style = "height: 450px; overflow-y: auto;",
                p(
                  "In contrast to the wearable inventories or review studies with a static document, the SiA wearables database will be continuously updated every 6 months. This enables the entry of newly released wearables and updating of the validity, reliability, and usability information on existing wearables.",
                  style = "text-align: justify;"
                ),
                img(
                  src = "article_update_loop.png",
                  style = "display: block; margin: 15px auto; max-width: 100%; height: auto;"
                )
              )
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        bs4Card(
          title = "Behavior Research Methods: Original Paper",
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
            src = "s13428-025-02685-4.pdf#toolbar=0&navpanes=0&scrollbar=0",
            style = "width:100%; height:842px; border:none;"
          )
        )
      ),
      column(
        width = 6,
        bs4Card(
          title = "Journal of Open Source Software Paper: Shiny App Details",
          height = "800px",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          tags$p("Coming soon")
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
