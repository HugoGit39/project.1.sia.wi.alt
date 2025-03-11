############################################################################################
#
#  Function for app info
#
#############################################################################################

# App Info Module (UI)
mod_app_info_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(tags$style(HTML("
      /* Default (light mode) */
      .custom-card {
        background-color: white !important;
        color: black !important;
      }

      /* Dark mode styling */
      body.dark-mode .custom-card {
        background-color: #343a40 !important; /* Dark gray */
        color: white !important;
      }

      /* Adjust header colors dynamically */
      body.dark-mode .custom-header-primary {
        background-color: #1c75bc !important;
        color: white !important;
      }

      body.dark-mode .custom-header-secondary {
        background-color: #f15a29 !important;
        color: white !important;
      }
    "))),

    fluidRow(
      # App Info Card (Full width)
      bs4Card(
        title = tags$span(class = "custom-header-primary", "Welcome to the SiA Wearable Interface Web App"),
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        collapsible = FALSE,
        headerBorder = FALSE,
        class = "custom-card",  # Apply dark mode styling

        h4("About this App"),
        p("This web application helps users compare wearable devices based on various features like
          battery life, weight, and available sensors. You can filter devices and submit new wearable
          data for evaluation.")
      )
    ),

    fluidRow(
      # User Guide Card (Width 9, Fixed Height)
      bs4Card(
        title = tags$span(class = "custom-header-primary", "User Guide"),
        status = "primary",
        solidHeader = TRUE,
        width = 9,
        collapsible = FALSE,
        headerBorder = FALSE,
        class = "custom-card",  # Apply dark mode styling
        style = "height: 500px;",  # Fixed height

        p("To use this app, navigate to the 'Filters' section to explore wearables based on your
          criteria. If you have new data to add, visit the 'Submit Data' section."),

        p("For more details, check the documentation or contact support.")
      ),

      # Updates Card (Width 3, Fixed Height, Scrollable)
      bs4Card(
        title = tags$span(class = "custom-header-secondary", "Recent Updates"),
        status = "secondary",
        solidHeader = TRUE,
        width = 3,
        collapsible = FALSE,
        headerBorder = FALSE,
        class = "custom-card",  # Apply dark mode styling
        style = "height: 500px; overflow-y: auto;",  # Fixed height & scrollbar

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
  )
}

# App Info Module (Server)
mod_app_info_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server-side logic needed for static content
  })
}
