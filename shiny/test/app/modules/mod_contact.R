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
                       uiOutput(ns("email_error")),
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

    output$email_error <- renderUI({
      val <- input$email
      if (!is.null(val) && nzchar(val) && !grepl("@", val)) {
        div(
          style = "color:#CC6677; font-size:12px;",
          strong("Email must contain '@' (e.g., name@example.com).")
        )
      }
    })

      observe({

        toggleState(id = "submit_email", condition = mandatoryfields_check(fieldsMandatory_email, input))

      })

    # Call `send_email()` when Submit button is clicked
    observeEvent(input$submit_email, {

      # Create email body
      body <- paste("Name: ", input$name,
                    "\nEmail: ", input$email,
                    "\nTelephone: ", input$telephone,
                    "\n\nInstitution: ", input$institution,
                    "\nMessage: ", input$message)

      subject <- "Wearable Shiny App message"

      send_email(body, subject)

      # Trigger JS alert from server
      session$sendCustomMessage("emailSubmitted", "Thank you for your message! We will get back to you soon.")

      # Reset all fields in one go
      reset_inputs_contact(session)
    })
  })
}

