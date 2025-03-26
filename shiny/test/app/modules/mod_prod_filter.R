############################################################################################
#
#  Function module for product filter
#
#############################################################################################

# Function to create the UI
mod_prod_fil_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 2,
        bs4Card(
          title = "Product Filter",
          status = "primary",
          width = 12,
          collapsible = FALSE,
          solidHeader = TRUE,
          div(
            style = "text-align: center; margin-bottom: 10px;",
            actionButton(
              inputId = ns("reset_filter"),
              label = "Reset Filter",
              status = "danger",
              outline = TRUE,
              size = "sm",
              flat = TRUE,
              icon = NULL,
              block = TRUE,
              width = "50%",
              style = "border-width: 2px"
            )
          ),
          bs4Card(title = "Select Products",
                  width = 12,
                  status = "secondary",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  selectInput(ns("product1"), "Product 1: Manufacturer", choices = unique(sia_df$manufacturer), selected = "Garmin"),
                  selectInput(ns("model1"), "Product 1: Model", choices = NULL),

                  selectInput(ns("product2"), "Product 2: Manufacturer", choices = unique(sia_df$manufacturer), selected = "Suunto"),
                  selectInput(ns("model2"), "Product 2: Model", choices = NULL),

                  selectInput(ns("product3"), "Product 3 (Optional): Manufacturer", choices = c("None", unique(sia_df$manufacturer)), selected = "None"),
                  selectInput(ns("model3"), "Product 3: Model", choices = NULL),

                  selectInput(ns("product4"), "Product 4 (Optional): Manufacturer", choices = c("None", unique(sia_df$manufacturer)), selected = "None"),
                  selectInput(ns("model4"), "Product 4: Model", choices = NULL),

                  selectInput(ns("product5"), "Product 5 (Optional): Manufacturer", choices = c("None", unique(sia_df$manufacturer)), selected = "None"),
                  selectInput(ns("model5"), "Product 5: Model", choices = NULL)
          )
        )
      ),
    # titlePanel("Wearable Product Comparison Filter"),
    # sidebarLayout(
    #   sidebarPanel(
    #     selectInput(ns("product1"), "Select Product 1:", choices = unique(products_data$Product), selected = "Garmin"),
    #     selectInput(ns("product2"), "Select Product 2:", choices = unique(products_data$Product), selected = "Suunto"),
    #     selectInput(ns("product3"), "Select Product 3 (Optional):", choices = c("None", unique(products_data$Product)), selected = "None"),
    #     selectInput(ns("product4"), "Select Product 4 (Optional):", choices = c("None", unique(products_data$Product)), selected = "None"),
    #     selectInput(ns("product5"), "Select Product 5 (Optional):", choices = c("None", unique(products_data$Product)), selected = "None"),
    #
    #     actionButton(ns("clear_selection"), "Clear Optional Wearables"),
    #
    #     br(), br(),  # Add some space before the download button
    #
    #     downloadButton(ns("download_csv"), "Download Table as CSV"),
    #
    #     width = 2
    #   ),
      mainPanel(
        DTOutput(ns("comparison_table")),
        width = 10
      )
    )
  )
}


# Function to create the Server logic
# mod_prod_fil__server <- function(id, data) {
#   moduleServer(id, function(input, output, session) {
#
#     # Function to get available choices excluding selected ones
#     get_available_choices <- function(exclude = character(0)) {
#       setdiff(unique(sia_df$manufacturer), exclude)
#     }
#
#     # Observe changes in any product selection
#     observe({
#       selected <- c(input$product1, input$product2, input$product3, input$product4, input$product5)
#       selected <- selected[selected != "None"]  # Remove "None" from selection
#
#       available_choices <- function(selected_so_far) {
#         c("None", get_available_choices(selected_so_far))
#       }
#
#       updateSelectInput(session, "product2", choices = available_choices(input$product1), selected = input$product2)
#       updateSelectInput(session, "product3", choices = available_choices(c(input$product1, input$product2)), selected = input$product3)
#       updateSelectInput(session, "product4", choices = available_choices(c(input$product1, input$product2, input$product3)), selected = input$product4)
#       updateSelectInput(session, "product5", choices = available_choices(c(input$product1, input$product2, input$product3, input$product4)), selected = input$product5)
#     })
#
#     # Clear selections when button is clicked
#     observeEvent(input$clear_selection, {
#       updateSelectInput(session, "product3", selected = "None")
#       updateSelectInput(session, "product4", selected = "None")
#       updateSelectInput(session, "product5", selected = "None")
#     })
#
#     # Create the table data reactively
#     selected_data <- reactive({
#       selected_products <- c(input$product1, input$product2)
#       if (input$product3 != "None") selected_products <- c(selected_products, input$product3)
#       if (input$product4 != "None") selected_products <- c(selected_products, input$product4)
#       if (input$product5 != "None") selected_products <- c(selected_products, input$product5)
#
#       wide_data[, c("Feature", selected_products), drop = FALSE]
#     })
#
#     # Render DataTable
#     output$comparison_table <- renderDT({
#       datatable(selected_data(), options = list(dom = 't', paging = FALSE))
#     })
#
#     # CSV Download Handler
#     output$download_csv <- downloadHandler(
#       filename = function() {
#         paste("df_stress_in_action_prod_fil_", Sys.Date(), ".csv", sep = "")
#       },
#       content = function(file) {
#         write.csv(selected_data(), file, row.names = FALSE)
#       }
#     )
#   })
# }
#
# mod_prod_fil_server <- function(id, data) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#
#     # Function to update model choices based on selected manufacturer
#     update_model_choices <- function(prod_input, model_input) {
#       observeEvent(input[[prod_input]], {
#         req(input[[prod_input]] != "None")
#         models <- unique(data()$model[data()$manufacturer == input[[prod_input]]])
#         updateSelectInput(session, model_input, choices = models)
#       })
#     }
#
#     # Apply for each manufacturer/model pair
#     update_model_choices("product1", "model1")
#     update_model_choices("product2", "model2")
#     update_model_choices("product3", "model3")
#     update_model_choices("product4", "model4")
#     update_model_choices("product5", "model5")
#
#     # Clear optional selections when button is clicked
#     observeEvent(input$clear_selection, {
#       updateSelectInput(session, "product3", selected = "None")
#       updateSelectInput(session, "product4", selected = "None")
#       updateSelectInput(session, "product5", selected = "None")
#
#       updateSelectInput(session, "model3", choices = NULL, selected = NULL)
#       updateSelectInput(session, "model4", choices = NULL, selected = NULL)
#       updateSelectInput(session, "model5", choices = NULL, selected = NULL)
#     })
#
#     # Create the table data reactively
#     selected_data <- reactive({
#       req(input$model1, input$model2)
#
#       selected_models <- c(input$model1, input$model2)
#
#       if (!is.null(input$model3) && input$product3 != "None") selected_models <- c(selected_models, input$model3)
#       if (!is.null(input$model4) && input$product4 != "None") selected_models <- c(selected_models, input$model4)
#       if (!is.null(input$model5) && input$product5 != "None") selected_models <- c(selected_models, input$model5)
#
#       # Filter data: use selected models from wide_data
#       req(all(selected_models %in% colnames(wide_data)))
#       wide_data[, c("Feature", selected_models), drop = FALSE]
#     })
#
#     # Render DataTable
#     output$comparison_table <- renderDT({
#       datatable(selected_data(), options = list(dom = 't', paging = FALSE))
#     })
#
#     # CSV Download Handler
#     output$download_csv <- downloadHandler(
#       filename = function() {
#         paste("df_stress_in_action_prod_fil_", Sys.Date(), ".csv", sep = "")
#       },
#       content = function(file) {
#         write.csv(selected_data(), file, row.names = FALSE)
#       }
#     )
#   })
# }
#

mod_prod_fil_server <- function(id, sia_df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper to update model choices based on selected manufacturer
    observe_model_update <- function(product_input, model_input) {
      observeEvent(input[[product_input]], {
        req(input[[product_input]])
        if (input[[product_input]] == "None") {
          updateSelectInput(session, model_input, choices = NULL, selected = NULL)
        } else {
          models <- unique(sia_df$model[sia_df$manufacturer == input[[product_input]]])
          updateSelectInput(session, model_input, choices = models, selected = models[1])
        }
      }, ignoreInit = TRUE)
    }

    # Apply to each product-model pair
    observe_model_update("product1", "model1")
    observe_model_update("product2", "model2")
    observe_model_update("product3", "model3")
    observe_model_update("product4", "model4")
    observe_model_update("product5", "model5")

    # Reset filter
    observeEvent(input$reset_filter, {
      updateSelectInput(session, "product1", selected = "Garmin")
      updateSelectInput(session, "product2", selected = "Suunto")
      updateSelectInput(session, "product3", selected = "None")
      updateSelectInput(session, "product4", selected = "None")
      updateSelectInput(session, "product5", selected = "None")
    })

    # Placeholder for output
    output$comparison_table <- renderDT({
      # Just show selected input for now
      data.frame(
        Product = paste0("Product", 1:5),
        Manufacturer = c(input$product1, input$product2, input$product3, input$product4, input$product5),
        Model = c(input$model1, input$model2, input$model3, input$model4, input$model5),
        stringsAsFactors = FALSE
      )
    })
  })
}

