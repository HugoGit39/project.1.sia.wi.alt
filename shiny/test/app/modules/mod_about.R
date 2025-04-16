############################################################################################
#
#  Function nodule for about
#
#############################################################################################

# App Info Module (UI)
mod_about_ui <- function(id) {
  ns <- NS(id)

  tagList(

    fluidRow(
      column(
        width = 4,
        div(
          tags$img(
            src = "SiA_lab.jpg",
            style = "max-width: 40%; max-height: 40%; margin-top: 10%; margin-left: 25%"
          )
        )
      ),
      column(
        width = 4,
        bs4Card(
          title = "Stress in Action",
          style = "font-size: 18px; height: 350px; overflow-y: auto;",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          headerBorder = FALSE,
          p("Stress in Action capitalizes on the fast advances in technology and big data analytics to move stress research from the lab to daily life. The Consortium enables synergistic collaborations to discover 1) how responses to daily life stress arise from the temporal, dynamic interplay between context and person-specific factors (RT1), 2) how daily life stress can be reliably measured in a specific individual in real-time", strong("(RT2)", style = "color: #f15a29; font-size:18px"), ", and 3) how and when potential beneficial stress-response mechanisms turn into detrimental effects on mental and cardiometabolic health. This enables the development of novel monitoring and intervention strategies to track and reduce daily life stress and its health impact (RT3)."), style = "text-align: justify;",
          div(
            style = "text-align: center; margin-bottom: 10px;",
            actionButton(
              inputId = ns("read_more"),
              label = "Read More",
              status = "secondary",
              outline = TRUE,
              size = "sm",
              flat = TRUE,
              icon = NULL,
              block = TRUE,
              width = "25%",
              style = "border-width: 2px",
              onclick ="window.open('https://stress-in-action.nl/project-abstract/', '_blank')"
            )
          ),
          footer = div(
            style = "padding-top: 10px;",
            div(
              style = "text-align: center;",
              p(strong("Partners", style = "color: #1c75bc;"))
            ),
            div(
              style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 10px; padding-top: 5px;",
              tags$img(src = "VU_logo.png", height = "40px"),
              tags$img(src = "AUMC_logo.png", height = "40px"),
              tags$img(src = "UMCG_logo.png", height = "40px"),
              tags$img(src = "RUG_logo.png", height = "40px"),
              tags$img(src = "UU_logo.png", height = "40px"),
              tags$img(src = "UT_logo.png", height = "40px"),
              tags$img(src = "EMC_logo.png", height = "40px")
            )
          )
        )
      ),
    column(
      width = 4,
      div(
        tags$img(
          src = "SiA_measurements.jpg",
          style = "max-width: 75%; max-height: 75%; margin-top: 20%; margin-left: 15%;"
        )
      )
    )
  ),
  fluidRow(
    column(width = 4,
           div(
           style = "margin-top: 125px;",
             bs4Card(
               title = "App Maintenance",
               status = "secondary",
               solidHeader = TRUE,
               width = 12,
               collapsible = FALSE,
               headerBorder = FALSE,
               p("This application is a publication of the department of Biological Psychology at the VU University Amsterdam.", style = "text-align: justify;"),
               p(
                 a(
                   href = "https://stress-in-action.nl/hugo-klarenberg/",
                   target = "_blank",
                   img(
                     src = "favicon.ico",
                     style = "width:25px; height:25px; vertical-align:middle; margin-right:5px;"
                   )
                 ),
                 "Hugo Klarenberg, PhD - Post Doc VU University"
               ),
               p("Faculty of Behavioural and Human Movement Sciences", br(),
                 "Department of Biological Psychology", br(),
                 "Van der Boechorststraat 7", br(),
                 "1081 BT Amsterdam", br(),
                 "Email: disc[at]stress-in-action.nl", br(),
                 "Website: ",
                   a(
                     href = "https://vu.nl/en/about-vu/faculties/faculty-of-behavioural-and-movement-sciences/departments/biological-psychology",
                     target = "_blank",
                     "Department of Biological Psychology"
                   )
                 ),
               p("We kindly ask you to report bugs or function requests via email.", style = "text-align: justify;")
               )
           )
    ),
    column(width = 1),
    column(width = 2,
           div(
             style = "margin-top: 50px;",
             p(img(src = "iStock_about.jpg", style = "max-width: 100%; height: auto;")),
               bs4Card(
                 title = "Image Credits",
                 status = "primary",
                 solidHeader = TRUE,
                 width = 12,
                 collapsible = FALSE,
                 headerBorder = FALSE,
                 p("All background images used in this application are sourced from iStock under a paid license.", style = "text-align: justify;"),
                 p("Photographer:", br(),
                   "Daniel de la Hoz"),
                 p("Website: ",
                   a(
                     href = "https://www.istockphoto.com/nl/portfolio/Hoverphoto",
                     target = "_blank",
                     "iStock profile"
                   )
                 )
               )
             )
           ),
    column(width = 1),
    column(width = 4,
           div(
             style = "margin-top: 50px;",
               bs4Card(
                 title = "Research Theme 2 (RT2) Wearbale Team",
                 status = "secondary",
                 solidHeader = TRUE,
                 width = 12,
                 collapsible = FALSE,
                 headerBorder = FALSE,
                 p("The goal of RT2 is to develop increasingly sophisticated versions of a cutting-edge, low-burden, ecologically valid ambulatory assessment toolkit to quantify stress in daily life, and to design, test, and iteratively improve stress interventions using that toolkit."), style = "text-align: justify;",
                 tagList(
                   p(
                     a(
                       href = "https://stress-in-action.nl/myrte-schoenmakers/",
                       target = "_blank",
                       img(
                         src = "favicon.ico",
                         style = "width:25px; height:25px; vertical-align:middle; margin-right:5px;"
                       )
                     ),
                     "Myrte Schoenmakers, PhD student VU University"
                   ),
                 p(
                   a(
                     href = "https://stress-in-action.nl/melisa-saygin/",
                     target = "_blank",
                     img(
                       src = "favicon.ico",
                       style = "width:25px; height:25px; vertical-align:middle; margin-right:5px;"
                     )
                   ),
                   "Melisa Saygin, PhD student VU University"
                 ),
                 p(
                   a(
                     href = "https://stress-in-action.nl/magdalena-sikora/",
                     target = "_blank",
                     img(
                       src = "favicon.ico",
                       style = "width:25px; height:25px; vertical-align:middle; margin-right:5px;"
                     )
                   ),
                   "Magdalena Sikora, PhD student Twente University"
                 ),
                 p(
                   a(
                     href = "https://stress-in-action.nl/artemis-stefani/",
                     target = "_blank",
                     img(
                       src = "favicon.ico",
                       style = "width:25px; height:25px; vertical-align:middle; margin-right:5px;"
                     )
                   ),
                   "Artemis Stefani, PhD - Post Doc VU University"
                 ),
                 p(
                   a(
                     href = "https://stress-in-action.nl/matthijs-noordzij/",
                     target = "_blank",
                     img(
                       src = "favicon.ico",
                       style = "width:25px; height:25px; vertical-align:middle; margin-right:5px;"
                     )
                   ),
                   "Matthijs Noordzij, Professor in Health Psychology and Technology Twente University"
                 ),
                 p(
                   a(
                     href = "https://stress-in-action.nl/eco-de-geus/",
                     target = "_blank",
                     img(
                       src = "favicon.ico",
                       style = "width:25px; height:25px; vertical-align:middle; margin-right:5px;"
                     )
                   ),
                   "Eco de Geus, Professor of Biological Psychology VU University"
                 )
               )
             )
           )
    )
  )
  )
}

# App Info Module (Server)
mod_about_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server-side logic needed for static content
  })
}
