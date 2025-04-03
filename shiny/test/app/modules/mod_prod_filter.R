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
          bs4Card(
            title = "Select Products",
            width = 12,
            status = "secondary",
            solidHeader = TRUE,
            collapsible = FALSE,

            selectInput(ns("product1"), "Product 1: Manufacturer", choices = sort(unique(sia_df$manufacturer)), selected = "Apple"),
            selectInput(ns("model1"), "Product 1: Model", choices = NULL),

            selectInput(ns("product2"), "Product 2: Manufacturer", choices = sort(unique(sia_df$manufacturer)), selected = "Vrije Universiteit van Amsterdam"),
            selectInput(ns("model2"), "Product 2: Model", choices = NULL),

            selectInput(ns("product3"), "Product 3: Manufacturer",
                        choices = c("Choose a product" = "", sort(unique(sia_df$manufacturer))),
                        selected = ""),
            selectInput(ns("model3"), "Product 3: Model", choices = NULL, selected = NULL)
          )
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
          downloadButton(ns("download_data"), "Download Filtered Results", class = "btn-secondary")
        ),
        DTOutput(ns("prod_filtered_table"))
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
      updateSelectInput(
        session,
        "model1",
        choices = sort(unique(df$model[df$manufacturer == input$product1]))
      )
    })

    # Update model2 choices based on selected product2
    observeEvent(input$product2, {
      df <- sia_df()
      updateSelectInput(
        session,
        "model2",
        choices = sort(unique(df$model[df$manufacturer == input$product2]))
      )
    })

    # Handle product3 updates and enable/disable model3
    observeEvent(input$product3, {
      df <- sia_df()

      if (input$product3 == "None" || input$product3 == "") {
        shinyjs::disable("model3")
        updateSelectInput(session, "model3", choices = character(0), selected = "")
      } else {
        shinyjs::enable("model3")
        updateSelectInput(
          session,
          "model3",
          choices = c("Choose a model" = "", sort(unique(df$model[df$manufacturer == input$product3])))
        )
      }

      # If product3 was selected and "None" isn't in the list, update with it
      all_products <- sort(unique(df$manufacturer))
      if (!"None" %in% all_products && nzchar(input$product3)) {
        updateSelectInput(
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
      req(input$model1, input$model2)
      df %>% filter(
        (manufacturer == input$product1 & model == input$model1) |
          (manufacturer == input$product2 & model == input$model2)
      )
    })

    # Reactive: Selected products
    selected_products <- reactive({
      df <- sia_df()
      req(input$model1, input$model2)

      filters <-
        (df$manufacturer == input$product1 & df$model == input$model1) |
        (df$manufacturer == input$product2 & df$model == input$model2)

      # Add product3/model3 filter if chosen
      if (!is.null(input$model3) && nzchar(input$model3)) {
        filters <- filters | (df$manufacturer == input$product3 & df$model == input$model3)
      }

      df %>% filter(filters)
    })

    # Output: Comparison table
    output$prod_filtered_table <- renderDT({
      df <- selected_products()

      if (nrow(df) == 0) return(NULL)

      # Create column labels
      col_labels <- paste0(df$manufacturer, " - ", df$model)

      # Transpose selected data
      df_t <- as.data.frame(t(df %>% select(-manufacturer, -model)))
      colnames(df_t) <- col_labels
      df_t <- rownames_to_column(df_t, var = "Variable")

      datatable(df_t, options = list(pageLength = nrow(df_t)))
    })
  })
}
