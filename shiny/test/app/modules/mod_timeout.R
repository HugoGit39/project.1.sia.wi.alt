############################################################################################
#
#  Function nodule for nfeauture filter (extensive)
#
#############################################################################################

# Message

disconnected <- tagList(
  p(strong("Time Out!", style = "color: #332288; font-size:30px")),
  p("Your session has ended", br(),
    "Don't want session time-outs?", br(),
    "Join us on a 14-day free trial!", style = "font-size:16px"),
  p(own_button(), br(),
    "or", br(),
    reload_button("Refresh")),
  p("FYI The timeout ensures optimal speed", br(),
    "for all on this FREE 24hrs data.")
)

#' Module UI for Filtering Wearables

#none

# Time-out Module (Server)
mod_app_info_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    sever(html = disconnected, bg_color = "#DDDDDD75", color = "black", box = T)

  })
}


