############################################################################################
#
#  App file
#
#############################################################################################

# * 1 ui -----------------------------------------------------------
ui <- dashboardPage(
  dark = NULL,
  freshTheme = colours_fresh(),
  title = "SiA Wearable Interface Web App",
  fullscreen = FALSE,
  skin = "light",
  help = NULL,

# * * 1.1 header -----------------------------------------------------------
header = mod_header_ui("header"),

# * * 1.2 sidebar -----------------------------------------------------------
  sidebar = dashboardSidebar(disable = TRUE),


# * * 1.3 body -----------------------------------------------------------
    body = dashboardBody(
      id = "body_app",
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
      useShinyjs(),
    tabItems(
      tabItem(tabName = "app_info", mod_app_info_ui("app_info")),
      tabItem(tabName = "product_filter", mod_prod_fil_ui("product_comp")),
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
  footer = mod_footer_ui("footer"),

  scrollToTop = TRUE

)

# * 2 server -----------------------------------------------------------
server <- function(input, output, session) {

# * * 2.1 modules -----------------------------------------------------------

  mod_header_server("header")
  mod_prod_fil_server("product_comp", sia_df_reactive)
  mod_feat_fil_server("feature_comp", sia_df_reactive )
  #mod_sub_data_server("add_data")
  mod_contact_server("contact")

# * * 2.2 additional -----------------------------------------------------------

  sia_df_reactive  <- reactive({sia_df})

}

# Run the application
shinyApp(ui = ui, server = server)


