############################################################################################
#
#  Function for add data
#
#############################################################################################

# ui
mod_sub_data_ui <- function(id) {
  ns <- NS(id)

  tagList(
    titlePanel("Submit New Wearable Data"),
    sidebarLayout(
      sidebarPanel(
        helpText("Researchers can enter new wearable data here."),
        actionButton(ns("submit_data"), "Submit Data"),
        br(), br(),
        helpText("Your submission will be sent to our team."),
        width = 3  # Slightly wider for form fields
      ),
      mainPanel(
        DTOutput(ns("data_input_table")),
        width = 9
      )
    )
  )
}

#server
mod_sub_data_ser <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Define an empty dataframe structure (editable by users)
    empty_data <- data.frame(
      Wearable = character(5),
      Battery_Life = numeric(5),
      Weight = numeric(5),
      Features = character(5),
      stringsAsFactors = FALSE
    )

    # Create reactive dataframe
    user_data <- reactiveVal(empty_data)

    output$data_input_table <- renderDT({
      datatable(user_data(), editable = "cell", options = list(dom = 't'))
    }, server = FALSE)

    # Observe table edits
    observeEvent(input$data_input_table_cell_edit, {
      info <- input$data_input_table_cell_edit
      new_data <- user_data()
      new_data[info$row, info$col] <- info$value
      user_data(new_data)
    })

    # Submit Data: Save CSV and Send Email
    observeEvent(input$submit_data, {
      file_path <- tempfile(fileext = ".csv")
      write.csv(user_data(), file_path, row.names = FALSE)

      # Setup email SMTP server
      smtp <- server(
        host = "smtp.your-email-provider.com",   # Change this to your SMTP provider
        port = 587,  # Use 465 for SSL, or 587 for TLS
        username = "your-email@example.com",    # Your email
        password = "your-password",             # Your email password or app-specific password
        tls = TRUE
      )

      # Create Email
      email <- envelope() %>%
        from("your-email@example.com") %>%
        to("hell@disc.nl") %>%
        subject("New Wearable Data Submission") %>%
        text("Please find the attached CSV with new wearable data submitted by a researcher.") %>%
        attachment(file_path, type = "text/csv")

      # Send Email
      smtp(email)

      # Notify user
      showNotification("Data submitted successfully and emailed!", type = "message")
    })
  })
}
