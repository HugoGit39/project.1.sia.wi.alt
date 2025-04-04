############################################################################################
#
#  Function nodule for footer
#
#############################################################################################

# App Info Module (UI)
mod_footer_ui <- function(id) {
  ns <- NS(id)

    dashboardFooter(
      left = tagList(
        a(
          href = "mailto:disc@stress-in-action.nl",
          target = "_blank",
          img(
            src = "mail_sia.png",
            style = "width:25px; height:25px; vertical-align:middle;",
            alt = "Email"
          )
        ),
        a(
          href = "https://www.linkedin.com/company/stress-in-action/",
          target = "_blank",
          img(
            src = "linkedin_sia.png",
            style = "width:25px; height:25px; vertical-align:middle; margin-left:10px;",
            alt = "LinkedIn"
          )
        ),
        a(
          href = "https://github.com/HugoGit39/project.1.sia.wi.alt",
          target = "_blank",
          img(
            src = "github-sign_sia.png",
            style = "width:25px; height:25px; vertical-align:middle; margin-left:10px;",
            alt = "GitHub"
          )
        )
      ),
      right = tags$span(style = "color:#1c75bc;",
                        "Copyright 2025 | Stress in Action | All rights Reserved")
    )
}

# App Info Module (Server)
mod_footer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server-side logic needed for static content
  })
}
