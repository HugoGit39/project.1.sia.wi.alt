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
        reactableOutput(ns("prod_filtered_table")) %>% withSpinner(),
        footer = tags$div(
          "Source: Schoenmakers M, Saygin M, Sikora M, Vaessen T, Noordzij M, de Geus E. ",
          "Stress in action wearables database: A database of noninvasive wearable monitors with systematic technical, reliability, validity, and usability information. ",
          tags$em("Behav Res Methods."),
          " 2025 May 13;57(6):171. doi: ",
          tags$a(
            href = "https://link.springer.com/article/10.3758/s13428-025-02685-4",
            target = "_blank",
            "10.3758/s13428-025-02685-4"
          ),
          "; PMID: 40360861; PMCID: ",
          tags$a(
            href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC12075381/",
            target = "_blank",
            "PMC12075381"
          ),
          style = "font-family: sans-serif; font-size: 10pt; color: #8C8C8C;"
        )
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
    output$prod_filtered_table <- reactable::renderReactable({
      df <- selected_products()

      # Format year
      if ("release_date" %in% names(df)) {
        df$release_date <- format(df$release_date, "%Y")
      }

      # Transpose and rename
      df_t <- as.data.frame(t(df %>% select(-manufacturer, -model)))
      colnames(df_t) <- paste0(df$manufacturer, " – ", df$model)
      df_t <- tibble::rownames_to_column(df_t, var = "Variable")
      df_t$Variable <- rename_map[df_t$Variable]

      # Define which rows get which formatting
      bar_rows <- c("Long-Term SiA Score", "Short-Term SiA Score")

      yes_no_rows <- df_t$Variable[apply(df_t[, -1], 1, function(x) all(na.omit(x) %in% c("Yes", "No")))]

      numeric_rows <- df_t$Variable[
        apply(df_t[, -1], 1, function(x) all(!is.na(suppressWarnings(as.numeric(x)))))
      ]

      numeric_fill_rows <- setdiff(numeric_rows, c(bar_rows, yes_no_rows))

      # Define custom palette
      pal_scale <- c(
        "#1c75bc00",  # fully transparent
        "#1c75bc22",  # ~13% opacity
        "#1c75bc44",  # ~27%
        "#1c75bc66",  # ~40%
        "#1c75bc88",  # ~53%
        "#1c75bcb3",  # ~70%
        "#1c75bcdd",  # ~87%
        "#1c75bc"   # fully opaque
      )


      # Cell renderer
      # Cell renderer
      custom_cell <- function(value, index, col_name) {
        var_label <- df_t$Variable[index]
        orig_var <- names(rename_map)[rename_map == var_label]

        # Bars for SiA scores
        if (var_label %in% bar_rows) {
          val <- suppressWarnings(as.numeric(value))
          if (!is.na(val)) {
            width <- paste0(round(scales::rescale(val, from = c(0, 10), to = c(0, 100))), "%")
            return(htmltools::tags$div(
              style = "background: transparent; display: flex; align-items: center;",
              htmltools::tags$div(style = paste0("background-color:#f15a29; height:10px; width:", width, ";")),
              htmltools::tags$div(style = "margin-left: 8px;", format(val, digits = 2))
            ))
          }
        }

        # Yes/No icons
        if (var_label %in% yes_no_rows) {
          if (is.na(value)) return("")
          if (value == "Yes") {
            return(htmltools::div(style = "color: #44AA99; font-weight: bold;", "✔ Yes"))
          } else if (value == "No") {
            return(htmltools::div(style = "color: #882255; font-weight: bold;", "✖ No"))
          }
        }

        # Numeric fill gradient (based on global min/max

        # Default fallback
        value
      }



      # Column definitions
      col_defs <- lapply(names(df_t)[-1], function(col) {
        reactable::colDef(cell = function(value, index) {
          custom_cell(value, index, col)
        })
      }) |> setNames(names(df_t)[-1])

      # Render table
      reactable(
        df_t,
        columns = col_defs,
        height = (nrow(df_t)*0.5) * 40,
        bordered = TRUE,
        highlight = TRUE,
        pagination = FALSE,
        searchable = TRUE
      )

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

