############################################################################################
#
#  Function nodule for about
#
#############################################################################################

# App Info Module (UI)
mod_about_ui <- function(id) {
  ns <- NS(id)

  tagList(

    fluidRow(
      column(
        width = 3,
        div(
          style = "display: flex; align-items: center; justify-content: center; height: 100%;",
          tags$img(src = "SiA_lab.jpg", style = "max-width: 100%; max-height: 100%;")
        )
      ),
      column(
        width = 1
      ),
      column(
        width = 4,
        bs4Card(
          title = "Stress in Action",
          style = "font-size: 18px; height: 450px; overflow-y: auto;",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          headerBorder = FALSE,
          p("Stress in Action capitalizes on the fast advances in technology and big data analytics to move stress research from the lab to daily life. The Consortium enables synergistic collaborations to discover 1) how responses to daily life stress arise from the temporal, dynamic interplay between context and person-specific factors, 2) how daily life stress can be reliably measured in a specific individual in real-time, and 3) how and when potential beneficial stress-response mechanisms turn into detrimental effects on mental and cardiometabolic health. This enables the development of novel monitoring and intervention strategies to track and reduce daily life stress and its health impact."),
          div(
            style = "text-align: center; margin-bottom: 10px;",
            actionButton(
              inputId = ns("read_more"),
              label = "Read More",
              status = "secondary",
              outline = TRUE,
              size = "sm",
              flat = TRUE,
              icon = NULL,
              block = TRUE,
              width = "25%",
              style = "border-width: 2px",
              onclick ="window.open('https://stress-in-action.nl/project-abstract/', '_blank')"
            )
          ),
          footer = div(
            style = "padding-top: 10px;",
            div(
              style = "text-align: center;",
              p(strong("Partners", style = "color: #1c75bc;"))
            ),
            div(
              style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 10px; padding-top: 5px;",
              tags$img(src = "VU_logo.png", height = "40px"),
              tags$img(src = "AUMC_logo.png", height = "40px"),
              tags$img(src = "UMCG_logo.png", height = "40px"),
              tags$img(src = "RUG_logo.png", height = "40px"),
              tags$img(src = "UU_logo.png", height = "40px"),
              tags$img(src = "UT_logo.png", height = "40px"),
              tags$img(src = "EMC_logo.png", height = "40px")
            )
          )
      )
    ),
    column(
      width = 1
    ),
    column(
      width = 3,
      div(
        style = "display: flex; align-items: center; justify-content: center; height: 100%;",
        tags$img(src = "SiA_measurements.jpg", style = "max-width: 100%; max-height: 100%;")
      )
    )
  )
  )

}

# App Info Module (Server)
mod_about_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server-side logic needed for static content
  })
}
