############################################################################################
#
#  Function for contact us
#
#############################################################################################

# Contact UI
mod_contact_ui <- function(id) {

  ns <- NS(id)

  tagList(

    div(id = "app_contact_us_bg"),

    fluidRow(
      column(width = 4,
             bs4Card(title = "Contact Us",
                     status = "primary",
                     width = 12,
                     collapsible = FALSE,
                     solidHeader = TRUE,
                     id = "contact",
                     height = "auto",
                     p("Feel free to contact us with any questions about the project!"),
                     div(
                       textInput(ns("name"), labelMandatory("Name"), ""),
                       textInput(ns("email"), labelMandatory("Email"), ""),
                       textInput(ns("telephone"), "Telephone"),
                       textInput(ns("institution"), "Institution"),
                       textAreaInput(ns("message"), labelMandatory("Message"), ""),

                       # Submit button (starts disabled)
                       actionButton(ns("submit_email"), "Submit", disabled = TRUE)
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

      observe({

        toggleState(id = "submit_email", condition = mandatoryfields_check(fieldsMandatory_email, input))

      })

    # Call `send_email()` when Submit button is clicked
    observeEvent(input$submit, {

      # Create email body
      body <- paste("Name: ", input$name,
                    "\nEmail: ", input$email,
                    "\nTelephone: ", input$telephone,
                    "\n\nInstitution: ", input$institution,
                    "\nMessage: ", input$message)

      subject <- "Wearable Shiny App message"

      send_email(body, subject)

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

