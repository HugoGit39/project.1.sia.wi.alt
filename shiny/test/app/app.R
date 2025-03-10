library(shiny)
library(bs4Dash)
library(here)
library(dplyr)
library(readxl)
library(fresh)
library(data.table)
library(DT)
library(shinySearchbar )

# Load functions

#custom theme
source(here("shiny", "test", "app", "functions", "colours_fresh.R"))

source(here("shiny", "test", "app", "functions", "mod_prod_filter.R"))

#load data
db_path <- here("shiny", "test", "app", "data", "database_maar_dan_goed.xlsx")

# Load and preprocess data

sia_df <- readxl::read_excel(db_path) %>%
  select(`Device costs`, Weight, `SiA Expert score (short-term)`, `SiA Expert score (long-term)`, `Battery life (hours)`, PPG, ECG, ICG, EMG, GPS, Location) %>%
  mutate(
    DeviceCosts = as.numeric(sub(";.*", "", `Device costs`)),
    Weight = as.numeric(sub(";.*", "", Weight)),
    SiAShort = as.numeric(sub(";.*", "", `SiA Expert score (short-term)`)),
    SiALong = as.numeric(sub(";.*", "", `SiA Expert score (long-term)`)),
    BatteryLife = as.numeric(sub(";.*", "", `Battery life (hours)`)),
    PPG = ifelse(grepl("^1;", PPG), 1, 0),
    ECG = ifelse(grepl("^1;", ECG), 1, 0),
    ICG = ifelse(grepl("^1;", ICG), 1, 0),
    EMG = ifelse(grepl("^1;", EMG), 1, 0),
    GPS = ifelse(grepl("^1;", GPS), 1, 0)
  )

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
      tags$style(".main-header {min-height: 75px}")
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
      navbarTab(tabName = "user_guide", text = "User Guide"),
      navbarTab(
        text = "Filters",
        dropdownHeader("Filter Options"),
        navbarTab(tabName = "product_filter", text = "Product Filter"),
        navbarTab(tabName = "feature_filter", text = "Feature Filter")
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
      tabItem(tabName = "product_filter", mod_prod_fil_ui("product_comp"))
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
  # observeEvent(input$Search, {
  #   req(input$Search)  # Ensure input$Search is not NULL
  #   showNotification(paste("Searching for:", input$Search), type = "message")
  # })
  #
  # observeEvent(input$navbar, {
  #   req(input$navbar)  # Ensure input$navbar is not NULL
  #   updateNavbarTabs(session, inputId = "navbar", selected = input$navbar)
  # })

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

}

# Run the application
shinyApp(ui = ui, server = server)
