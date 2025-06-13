############################################################################################
#
#  Function module for navbar
#
#############################################################################################

# Navbar UI Module
mod_header_ui <- function(id) {
  ns <- NS(id)

  bs4DashNavbar(
    titleWidth = 220,
    title = tags$a(
      href = "https://www.stressinaction.nl", target = "_blank",
      tags$img(
        src = "SiA_Logo_png.png",
        height = "55px",
        style = "margin-top: 10px; margin-bottom: 10px; margin-left: 15px; margin-right: 15px;"
      )
    ),
    rightUi = tagList(
      tags$li(class = "dropdown", searchbar(inputId = ns("Search"), placeholder = "Search text here...", contextId = "body_app"))
    ),
    navbarMenu(
      id = ns("navmenu"),
      navbarTab(tabName = "app_info", text = "App Info"),
      navbarTab(
        text = "Filters",
        dropdownHeader(""),
        navbarTab(tabName = "product_filter", text = "Product Filter (simple)"),
        navbarTab(tabName = "feature_filter", text = "Feature Filter (extensive)")
      ),
      navbarTab(tabName = "submit_data", text = "Submit Data"),
      navbarTab(tabName = "article", text = "Research"),
      navbarTab(tabName = "about", text = "About"),
      navbarTab(tabName = "contact_us", text = "Contact Us")
    )
  )
}

# Navbar Server Module
mod_header_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$controller, {
      updateNavbarTabs(
        session,
        inputId = "navmenu",
        selected = paste0("Tab", input$controller)
      )
    }, ignoreInit = TRUE)
  })
}

