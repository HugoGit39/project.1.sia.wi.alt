############################################################################################
#
#  Function for app info
#
#############################################################################################

# App Info Module (UI)
mod_app_info_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # tags$head(tags$style(HTML("
    #   html, body {
    #     height: 100%;
    #     margin: 0;
    #     padding: 0;
    #     overflow-x: hidden;
    #   }
    #
    #   #app_info_bg {
    #     position: fixed;
    #     top: 0;
    #     left: 0;
    #     width: 100%;
    #     height: 100%;
    #     background-image: url('iStock_app_info.jpg');
    #     background-size: cover;
    #     background-position: center;
    #     z-index: -999;
    #   }
    #
    #   .content-wrapper, .tab-content, .content {
    #     background: transparent !important;
    #     padding: 0 !important;
    #     margin: 0 !important;
    #   }
    # "))),

    div(id = "app_info_bg"),

    # First fluidRow: Welcome card (aligned right)
    fluidRow(
      column(width = 7), # spacer
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
          p("The Stress in Action Wearables Database (SiA-WD) is a new, comprehensive, well-sustained database of physiological wearable devices that have application potential in behavioral research, in particular stress research. It provides a large amount of information that a researcher would look for such as the general device information, recorded signals, technical specifications and data access, combined with a systematic validity, reliability and usability review of the available literature on a device. The SiA-WD will be iteratively expanded and the information, including that for devices already existing in the database, updated for an period of at least ten years. The user-friendly tool enables researchers to conveniently select the most suitable wearable for their study. ", style = "text-align: justify;")
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
              p("To use this app, navigate to the 'Filters' section to explore wearables based on your
                criteria. If you have new data to add, visit the 'Submit Data' section.", style = "text-align: justify;"),
              p("For more details, check the documentation or contact support.", style = "text-align: justify;")
            )
          ),

      column(width = 3),  # spacer

      column(
        width = 3,
        div(
          style = "margin-top: 100px;",
          bs4Card(
            title = "Recent Updates",
            status = "secondary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            headerBorder = FALSE,
            style = "height: 300px; overflow-y: auto;",
            p("Version 1.0 - Initial release with filtering and submission features."),
            p("Version 1.1 - Added dynamic selection to remove already chosen wearables."),
            p("Version 1.2 - Enabled CSV download of comparison results."),
            p("Version 1.3 - Added App Info page with separate cards."),
            p("Version 1.4 - Improved UI with colored headers and scrollable updates."),
            p("Version 1.5 - Fixed navbar tab issue where multiple tabs opened."),
            p("Version 1.6 - Improved the download feature for comparison tables."),
            p("Version 1.7 - Enhanced styling and usability."),
            p("Version 1.8 - Fixed UI inconsistencies and spacing issues."),
            p("Version 1.9 - Optimized data filtering for better performance."),
            p("Version 2.0 - Improved accessibility and mobile compatibility.")
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
