############################################################################################
#
#  Function for product filter
#
#############################################################################################

# Function to create the UI
mod_prod_fil_ui <- function(id) {
  ns <- NS(id)

  tagList(
    titlePanel("Wearable Product Comparison Filter"),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("product1"), "Select Product 1:", choices = unique(products_data$Product), selected = "Garmin"),
        selectInput(ns("product2"), "Select Product 2:", choices = unique(products_data$Product), selected = "Suunto"),
        selectInput(ns("product3"), "Select Product 3 (Optional):", choices = c("None", unique(products_data$Product)), selected = "None"),
        selectInput(ns("product4"), "Select Product 4 (Optional):", choices = c("None", unique(products_data$Product)), selected = "None"),
        selectInput(ns("product5"), "Select Product 5 (Optional):", choices = c("None", unique(products_data$Product)), selected = "None"),

        actionButton(ns("clear_selection"), "Clear Optional Wearables"),

        br(), br(),  # Add some space before the download button

        downloadButton(ns("download_csv"), "Download Table as CSV"),

        width = 2
      ),
      mainPanel(
        DTOutput(ns("comparison_table")),
        width = 10
      )
    )
  )
}


# Function to create the Server logic
mod_prod_fil_ser <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Function to get available choices excluding selected ones
    get_available_choices <- function(exclude = character(0)) {
      setdiff(unique(products_data$Product), exclude)
    }

    # Observe changes in any product selection
    observe({
      selected <- c(input$product1, input$product2, input$product3, input$product4, input$product5)
      selected <- selected[selected != "None"]  # Remove "None" from selection

      available_choices <- function(selected_so_far) {
        c("None", get_available_choices(selected_so_far))
      }

      updateSelectInput(session, "product2", choices = available_choices(input$product1), selected = input$product2)
      updateSelectInput(session, "product3", choices = available_choices(c(input$product1, input$product2)), selected = input$product3)
      updateSelectInput(session, "product4", choices = available_choices(c(input$product1, input$product2, input$product3)), selected = input$product4)
      updateSelectInput(session, "product5", choices = available_choices(c(input$product1, input$product2, input$product3, input$product4)), selected = input$product5)
    })

    # Clear selections when button is clicked
    observeEvent(input$clear_selection, {
      updateSelectInput(session, "product3", selected = "None")
      updateSelectInput(session, "product4", selected = "None")
      updateSelectInput(session, "product5", selected = "None")
    })

    # Create the table data reactively
    selected_data <- reactive({
      selected_products <- c(input$product1, input$product2)
      if (input$product3 != "None") selected_products <- c(selected_products, input$product3)
      if (input$product4 != "None") selected_products <- c(selected_products, input$product4)
      if (input$product5 != "None") selected_products <- c(selected_products, input$product5)

      wide_data[, c("Feature", selected_products), drop = FALSE]
    })

    # Render DataTable
    output$comparison_table <- renderDT({
      datatable(selected_data(), options = list(dom = 't', paging = FALSE))
    })

    # CSV Download Handler
    output$download_csv <- downloadHandler(
      filename = function() {
        paste("df_stress_in_action_prod_fil_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(selected_data(), file, row.names = FALSE)
      }
    )
  })
}
