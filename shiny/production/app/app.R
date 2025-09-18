############################################################################################
#
#  App file
#
#############################################################################################

source("global.R", local = FALSE)

# * 1 ui -----------------------------------------------------------
ui <-

  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js")
    ),
    dashboardPage(
    dark = NULL,
    freshTheme = colours_fresh(),
    title = "Stress in Action Wearables Database App",
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
          useShinyjs(),
          useSever(),
        tabItems(
          tabItem(tabName = "app_info", mod_app_info_ui("app_info")),
          tabItem(tabName = "product_filter", mod_prod_fil_ui("product_comp")),
          tabItem(tabName = "feature_filter", mod_feat_fil_ui("feature_comp")),
          tabItem(tabName = "submit_data", mod_sub_data_ui("add_data")),
          tabItem(tabName = "article", mod_article_ui("art")),
          tabItem(tabName = "about", mod_about_ui("about")),
          tabItem(tabName = "contact_us", mod_contact_ui("contact"))
        )
      ),

    # * * 1.4 controlbar -----------------------------------------------------------
      controlbar = mod_control_ui("controlbar"),

    # * * 1.5 footer -----------------------------------------------------------
      footer = mod_footer_ui("footer"),

      scrollToTop = TRUE

    )
  )

# * 2 server -----------------------------------------------------------
server <- function(input, output, session) {

  # * * 2.1 load data -----------------------------------------------------------

  sia_df_reactive  <- reactive({sia_df})

  # * * 2.2 modules -----------------------------------------------------------

  mod_header_server("header")
  mod_prod_fil_server("product_comp", sia_df_reactive)
  mod_feat_fil_server("feature_comp", sia_df_reactive )
  mod_sub_data__server("add_data")
  mod_article_server("art")
  mod_contact_server("contact")
  mod_control__server("controlbar", sia_df_reactive)
  mod_timeout_server("timeout")

}

# Run the application
shinyApp(ui = ui, server = server)

