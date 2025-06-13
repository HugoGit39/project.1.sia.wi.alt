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
        width = 3,
        bs4Card(
          title = "Product Filter",
          status = "primary",
          width = 12,
          collapsible = FALSE,
          solidHeader = TRUE,
          pickerInput(ns("product1"), "Product 1: Manufacturer",
                      choices = sort(unique(sia_df$manufacturer)),
                      selected = "Apple", multiple = FALSE),
          pickerInput(ns("model1"), "Product 1: Model",
                      choices = NULL, selected = NULL, multiple = FALSE),

          pickerInput(ns("product2"), "Product 2: Manufacturer",
                      choices = sort(unique(sia_df$manufacturer)),
                      selected = "Vrije Universiteit van Amsterdam", multiple = FALSE),
          pickerInput(ns("model2"), "Product 2: Model",
                      choices = NULL, selected = NULL, multiple = FALSE),
          pickerInput(ns("product3"), "Product 3: Manufacturer",
                      choices = c("Choose a product" = "", sort(unique(sia_df$manufacturer))),
                      selected = "", multiple = FALSE),
          pickerInput(ns("model3"), "Product 3: Model",
                      choices = NULL, selected = NULL, multiple = FALSE)
        )
      ),
      bs4Card(
        title = "Comparison Table",
        status = "primary",
        width = 9,
        collapsible = FALSE,
        solidHeader = TRUE,
        div(
          style = "text-align: center; margin-bottom: 10px;",
          downloadButton(ns("download_data"), "Download Filtered Products")
        ),
        DTOutput(ns("prod_filtered_table")) %>% withSpinner()
      )
    )
  )
}

# Function to create the Server logic
mod_prod_fil_server <- function(id, sia_df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Disable model3 initially
    disable("model3")

    # Update model1 choices based on selected product1
    observeEvent(input$product1, {
      df <- sia_df()
      updatePickerInput(
        session,
        "model1",
        choices = sort(unique(df$model[df$manufacturer == input$product1]))
      )
    })

    # Update model2 choices based on selected product2
    observeEvent(input$product2, {
      df <- sia_df()
      updatePickerInput(
        session,
        "model2",
        choices = sort(unique(df$model[df$manufacturer == input$product2]))
      )
    })

    # Handle product3 updates and enable/disable model3
    observeEvent(input$product3, {
      df <- sia_df()

      if (input$product3 == "None" || input$product3 == "") {
        disable("model3")
        updatePickerInput(session, "model3", choices = character(0), selected = "")
      } else {
        enable("model3")
        updatePickerInput(
          session,
          "model3",
          choices = c("Choose a model" = "", sort(unique(df$model[df$manufacturer == input$product3])))
        )
      }

      # If product3 was selected and "None" isn't in the list, update with it
      all_products <- sort(unique(df$manufacturer))
      if (!"None" %in% all_products && nzchar(input$product3)) {
        updatePickerInput(
          session,
          "product3",
          choices = c("None", all_products),
          selected = input$product3
        )
      }
    })

    # Reactive: Selected products
    selected_products <- reactive({
      df <- sia_df()

      # Always include product1/model1 first
      row1 <- df %>%
        filter(manufacturer == input$product1, model == input$model1)

      # Always include product2/model2 second
      row2 <- df %>%
        filter(manufacturer == input$product2, model == input$model2)

      # Optionally include product3/model3 third
      row3 <- NULL
      if (!is.null(input$model3) && nzchar(input$model3)) {
        row3 <- df %>%
          filter(manufacturer == input$product3, model == input$model3)
      }

      # Combine in fixed order
      bind_rows(row1, row2, row3)
    })

    # Output: Comparison table
    output$prod_filtered_table <- renderDT({
      df <- selected_products()

      # Only show years
      if ("release_date" %in% names(df)) {
        df$release_date <- format(df$release_date, "%Y")
      }

      # Transpose selected data
      df_t <- as.data.frame(t(df %>% select(-manufacturer, -model)))
      colnames(df_t) <- paste0(df$manufacturer, " - ", df$model)
      df_t <- rownames_to_column(df_t, var = "Variable")

      # Rename rows
      df_t$Variable <- rename_map[df_t$Variable]

      datatable(df_t,
                options = list(
                  pageLength = nrow(df_t),
                  scrollX = TRUE,
                  processing = FALSE))
    })

    # Download data
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("sia_product_filter_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        write.csv(selected_products(), file, row.names = FALSE)
      }
    )
  })
}

