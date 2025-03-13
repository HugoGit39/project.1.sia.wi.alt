############################################################################################
#
#  App file
#
#############################################################################################

# * 1 ui -----------------------------------------------------------
ui <- dashboardPage(
  dark = FALSE,
  freshTheme = colours_fresh(),
  title = "SiA Wearable Interface Web App",
  fullscreen = TRUE,
  skin = "light",
  help = NULL,

# * * 1.1 header -----------------------------------------------------------
header = mod_header_ui("header"),

# * * 1.2 sidebar -----------------------------------------------------------
  sidebar = dashboardSidebar(disable = TRUE),


# * * 1.3 body -----------------------------------------------------------
    body = dashboardBody(
    tabItems(
      tabItem(tabName = "app_info", mod_app_info_ui("app_info")),
      #tabItem(tabName = "product_filter", mod_prod_fil_ui("product_comp")),
      tabItem(tabName = "feature_filter", mod_feat_fil_ui("feature_comp")),
      tabItem(tabName = "submit_data", mod_sub_data_ui("add_data")),
      tabItem(tabName = "contact_us", mod_contact_ui("contact"))
    )
  ),

# * * 1.4 controlbar -----------------------------------------------------------
  controlbar = dashboardControlbar(
    tags$style(HTML(".control-sidebar { overflow-y: auto;}")),
    skin = "light",
    pinned = FALSE,
    collapsed = TRUE,
    overlay = TRUE
  ),

# * * 1.5 footer -----------------------------------------------------------
  footer = dashboardFooter(
    left = strong(HTML("<a href='mailto:disc@stress-in-action.nl'>E-mail Us!</a>")),
    right = HTML("<span style='color:#1c75bc;'>Copyright 2025 | Stress in Action | All rights Reserved</span>")
  ),

  scrollToTop = TRUE

)

# * 2 server -----------------------------------------------------------
server <- function(input, output, session) {

# * * 2.1 modules -----------------------------------------------------------

  mod_header_server("header")
  #mod_prod_fil_server("product_comp")
  mod_feat_fil_server("feature_comp", sia_df)
  #mod_sub_data_server("add_data")
  mod_contact_server("contact")

# * * 2.2 additional -----------------------------------------------------------

}

# Run the application
shinyApp(ui = ui, server = server)

