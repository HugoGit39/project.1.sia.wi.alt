############################################################################################
#
#  Function for app info
#
#############################################################################################

# App Info Module (UI)
mod_app_info_ui <- function(id) {
  ns <- NS(id)

  tagList(

    div(id = "app_info_bg"),

    fluidRow(
      column(
        width = 3,
        bs4Card(
          title = "Papers to Cite",
          status = "secondary",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          tags$img(src = "info_app_beh_paper.png", width = "100%", style = "margin-top: 10px;"),
          footer = tagList(
            actionButton(
              inputId = "copy_citation_btn",
              label = "Copy Citations",
              icon = icon("copy"),
              style = "background-color: #f15a29; color: white; margin-top: 5px;"
            )
          )
        )
      ),
      column(width = 4),
      column(
        width = 5,
        bs4Card(
          title = "Welcome to the SiA Wearables Database Web App",
          style = "font-size: 18px; height: 400px; overflow-y: auto;",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          headerBorder = FALSE,
          p("The Stress in Action Wearables Database (SiA-WD) is a new, comprehensive, well-sustained database of physiological wearable devices that have application potential in behavioral research, in particular stress research. It provides a large amount of information that a researcher would look for such as the general device information, recorded signals, technical specifications and data access, combined with a systematic validity, reliability and usability review of the available literature on a device. The SiA-WD will be iteratively expanded and the information, including that for devices already existing in the database, updated for an period of at least ten years. The user-friendly tool enables researchers to conveniently select the most suitable wearable for their study. ", style = "text-align: justify;"),
          footer = div(
            style = "padding-top: 10px;",
            div(style = "text-align: center; font-size: 18px;",
                p(strong(paste(n_wearables, "Wearables included"), style = "color: #f15a29;"),
                  br(),
                  "see the sidebar", tags$img(src = "controlbar.png", width = "15px", height = "15px"), "at the top right corner for an overview of", br(), "wearables and glossary."
                )
            )
          )
        )
      )
    ),

    # Second fluidRow: User Guide and Updates
    fluidRow(
      column(width = 1),  # left spacer

      column(
        width = 4,
        bs4Card(
          title = "User Guide",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          headerBorder = FALSE,
          style = "height: 300px; overflow-y: auto;",
          p(strong("Terms of citation")),
          p(
            "When using the wearables database and/or this web app, you must cite the two papers listed under ",
            strong("Papers to Cite", style = "color: #f15a29;"),
            " and/or ",
            strong("Research", style = "color: #1c75bc;"),
            "."
          ),
          p(strong("Using this app")),
          p(
            "Go to the ", strong("Filters"), " section to explore wearables based on your criteria.",
            style = "text-align: justify;"
          ),
          p(
            "Use the ", em("Product Filter"), " to compare multiple wearables side-by-side."
          ),
          p(
            "Use the ", em("Feature Filter"), " to select wearables based on capabilities and preferences."
          ),
          p(
            "For terminology, open the menu in the top right corner and browse the ",
            strong("Glossary"), ".",
            style = "text-align: justify;"
          ),
          p(
            "If you have new data to add, visit the ", strong("Submit Data"), " section.",
            style = "text-align: justify;"
          ),
          p(
            strong("Research"), "provides background information about the database and the two papers.",
            style = "text-align: justify;"
          ),
          p(
            "The ", strong("About"), " section describes Stress in Action (the project funding this database) and lists the team and institutions involved.",
            style = "text-align: justify;"
          ),
          p(
            "Questions? Reach out via ", strong("Contact Us"), ".",
            style = "text-align: justify;"
          )
        )
        ),
      column(width = 3),  # spacer

      column(
        width = 3,
        div(
          style = "margin-top: 100px;",
          bs4Card(
            title = "Recent Updates",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            headerBorder = FALSE,
            style = "max-height: 300px; overflow-y: auto;",
            p(strong("September 2025"), br(), "Live release."),
            p(strong("August 2025"), br(), "Tables updated with bars, yes/no and color coded cells."),
            p(strong("July 2025"), br(), "60 wearables still to be tested."),
            p(strong("June 2025"), br(), "54 wearables included in App."),
            p(strong("May 2025"), br(), "Stress in Action wearables database paper published!"),
            p(strong("March 2025"), br(), "Feature Filter live."),
            p(strong("February 2025"), br(), "Product Filter live."),
            p(strong("January 2025"), br(), "Test version (MVP) live.")
          )
        )
      ),

      column(width = 1),  # spacer
    )
  )
}

# App Info Module (Server)
mod_app_info_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server-side logic needed for static content
  })
}
