############################################################################################
#
#  Function for contact us
#
#############################################################################################

# Fields that are mandatory
fieldsMandatory <- c("name", "email", "message")

# CSS to style mandatory stars
appCSS <- "
  .mandatory_star { color: #CC6677; font-size: 16px; }
"

# Function to add red star (*) to mandatory labels
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# Contact UI
mod_contact_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),  # Enable shinyjs for dynamic form controls
    tags$head(tags$style(HTML(appCSS))),  # Add CSS for red stars
    fluidRow(
      column(width = 4,
             bs4Card(title = div("Contact Us", style = 'color:black; font-size:18px;'),
                     id = "contact", width = NULL, height = "auto", collapsible = FALSE, maximizable = FALSE,
                     div(
                       textInput(ns("name"), labelMandatory("Name"), ""),
                       textInput(ns("email"), labelMandatory("Email"), ""),
                       textInput(ns("telephone"), "Telephone"),
                       textInput(ns("institution"), "Institution"),  # New optional field
                       textAreaInput(ns("message"), labelMandatory("Message"), ""),

                       # Submit button (starts disabled)
                       actionButton(ns("submit"), "Submit", disabled = TRUE)
                     )
             )
      )
    )
  )
}

# Contact Server
mod_contact_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Observe if all mandatory fields are filled
    observe({
      mandatory_filled <- all(sapply(fieldsMandatory, function(x) {
        !is.null(input[[x]]) && input[[x]] != ""
      }))

      shinyjs::toggleState(id = "submit", condition = mandatory_filled)  # Enable/disable submit button
    })

    # Call `send_email()` when Submit button is clicked
    observeEvent(input$submit, {
      send_email(name = input$name,
                 email = input$email,
                 telephone = input$telephone,
                 institution = input$institution,  # Include Institution in email
                 message = input$message)

      showModal(
        modalDialog(
          title = "Form Submitted",
          "Thank you for your message! We will get back to you soon."
        )
      )

      # Reset form fields
      updateTextInput(session, "name", value = "")
      updateTextInput(session, "email", value = "")
      updateTextInput(session, "telephone", value = "")
      updateTextInput(session, "institution", value = "")
      updateTextAreaInput(session, "message", value = "")
    })
  })
}

