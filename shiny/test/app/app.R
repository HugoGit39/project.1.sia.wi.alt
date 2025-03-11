############################################################################################
#
#  App file
#
#############################################################################################

# Load packages

library(shiny)
library(bs4Dash)
library(here)
library(dplyr)
library(readxl)
library(fresh)
library(data.table)
library(DT)
library(shinySearchbar)
library(emayili)
library(shinyjs)

# Define UI
ui <- dashboardPage(
  dark = FALSE,
  freshTheme = colours_fresh(),
  title = "SiA Wearable Interface Web App",
  fullscreen = TRUE,
  skin = "light",
  help = NULL,

  # Header
  header = bs4DashNavbar(
    titleWidth = 220,
    tagList(
      tags$style(".main-header {min-height: 75px}"),
      tags$style(HTML("
      .dropdown-menu {
        min-width: 250px !important;  /* Adjust width as needed */
      }
      .dropdown-menu .dropdown-header {
        white-space: nowrap !important;  /* Prevents wrapping */
      }
    "))
    ),
    title = tags$img(
      src = "SiA_Logo_png.png",
      height = "55px",
      style = "margin-top: 10px; margin-bottom: 10px; margin-left: 15px; margin-right: 15px;"
    ),
    rightUi = tagList(
      tags$li(class = "dropdown", searchbar(inputId = "Search", placeholder = "Search text here...", contextId = "body_app"))
    ),
    navbarMenu(
      id = "navmenu",
      navbarTab(tabName = "app_info", text = "App Info"),
      navbarTab(
        text = "Filters",
        dropdownHeader(""),
        navbarTab(tabName = "product_filter", text = "Product Filter (simple)"),
        navbarTab(tabName = "feature_filter", text = "Feature Filter (extensive)")
      ),
      navbarTab(tabName = "submit_data", text = "Submit Data"),
      navbarTab(tabName = "about", text = "About"),
      navbarTab(tabName = "contact_us", text = "Contact Us")
    )
  ),

  # Sidebar (Hidden)
  sidebar = dashboardSidebar(disable = TRUE),


  # Body
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "app_info", mod_app_info_ui("app_info")),
      tabItem(tabName = "product_filter", mod_prod_fil_ui("product_comp")),
      tabItem(tabName = "submit_data", mod_sub_data_ui("add_data")),
      tabItem(tabName = "contact_us", mod_contact_ui("contact"))
    )
  ),

  # Controlbar
  controlbar = dashboardControlbar(
    tags$style(HTML(".control-sidebar { overflow-y: auto;}")),
    skin = "light",
    pinned = FALSE,
    collapsed = TRUE,
    overlay = TRUE
  ),

  # Footer
  footer = dashboardFooter(
    left = strong(HTML("<a href='mailto:disc@stress-in-action.nl'>E-mail Us!</a>")),
    right = HTML("<span style='color:#1c75bc;'>Copyright 2025 | Stress in Action | All rights Reserved</span>")
  ),

  scrollToTop = TRUE

)

# Define server logic
server <- function(input, output, session) {

  observeEvent(input$controller, {
    updateNavbarTabs(
      session,
      inputId = "navmenu",
      selected = paste0("Tab", input$controller)
    )
  },
  ignoreInit = TRUE
  )

  mod_prod_fil_ser("product_comp")

  mod_sub_data_ser("add_data")

  mod_contact_server("contact")

}

# Run the application
shinyApp(ui = ui, server = server)
