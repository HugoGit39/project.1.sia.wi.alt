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
          selectInput(ns("product1"), "Product 1: Manufacturer",
                      choices = sort(unique(sia_df$manufacturer)),
                      selected = "Apple", multiple = FALSE),
          selectInput(ns("model1"), "Product 1: Model",
                      choices = NULL, selected = NULL, multiple = FALSE),

          selectInput(ns("product2"), "Product 2: Manufacturer",
                      choices = sort(unique(sia_df$manufacturer)),
                      selected = "Vrije Universiteit van Amsterdam", multiple = FALSE),
          selectInput(ns("model2"), "Product 2: Model",
                      choices = NULL, selected = NULL, multiple = FALSE),
          div(
            style = "text-align: center; margin-bottom: 10px;",
            actionButton(
              inputId = ns("reset_prod_filter"),
              label = "Reset Product 3",
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
          selectInput(ns("product3"), "Product 3: Manufacturer",
                      choices = c("Choose a product" = "", sort(unique(sia_df$manufacturer))),
                      selected = "", multiple = FALSE),
          selectInput(ns("model3"), "Product 3: Model",
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

    disable("model3")

    observeEvent(input$product1, {
      df <- sia_df()
      updateSelectInput(session, "model1", choices = sort(unique(df$model[df$manufacturer == input$product1])))
    })

    observeEvent(input$product2, {
      df <- sia_df()
      updateSelectInput(session, "model2", choices = sort(unique(df$model[df$manufacturer == input$product2])))
    })

    observeEvent(input$product3, {
      df <- sia_df()
      if (input$product3 == "None" || input$product3 == "") {
        disable("model3")
        updateSelectInput(session, "model3", choices = character(0), selected = "")
      } else {
        enable("model3")
        updateSelectInput(session, "model3", choices = c("Choose a model" = "", sort(unique(df$model[df$manufacturer == input$product3]))))
      }
    })

    selected_products <- reactive({
      df <- sia_df()

      rows <- list(
        df %>% filter(manufacturer == input$product1, model == input$model1),
        df %>% filter(manufacturer == input$product2, model == input$model2)
      )

      if (!is.null(input$model3) && nzchar(input$model3)) {
        rows <- c(rows,
                  list(df %>% filter(manufacturer == input$product3,
                                     model == input$model3)))
      }

      bind_rows(rows) %>%
        distinct(manufacturer, model, .keep_all = TRUE)   # <- NEW
    })

    output$prod_filtered_table <- renderReactable({
      # 1) get selected rows
      df <- selected_products()

      # format release year if present
      if ("release_date" %in% names(df)) {
        df$release_date <- format(df$release_date, "%Y")
      }

      # 2) transpose to features-as-rows, models-as-columns
      df_t <- df %>%
        select(-manufacturer, -model) %>%
        t() %>%
        as.data.frame(stringsAsFactors = FALSE)

      colnames(df_t) <- paste0(df$manufacturer, " – ", df$model)
      # keep internal variable name BEFORE renaming
      df_t <- cbind(Feature_internal = rownames(df_t), df_t)
      rownames(df_t) <- NULL

      # display name
      df_t$Feature <- rename_map[df_t$Feature_internal] %||% df_t$Feature_internal

      # columns to render
      transposed_cols <- setdiff(names(df_t), c("Feature", "Feature_internal"))

      col_defs <- list(
        Feature = colDef(name = "Feature", minWidth = 50, style = list(fontWeight = "bold"))
      )

      # 3) per-column cell renderer (bars -> yes/no -> numeric heat -> plain)
      for (col in transposed_cols) {
        col_defs[[col]] <- colDef(cell = function(value, index) {
          # bars
          rendered <- func_bar_row_defs(value, index, df_t$Feature, bar_vars, rename_map)
          if (!identical(rendered, value)) return(rendered)

          # yes/no
          rendered <- func_yn_row_defs(value, index, df_t$Feature, yn_vars, rename_map)
          if (!identical(rendered, value)) return(rendered)

          # numeric (row-aware; uses Feature_internal + global pal_num_scale)
          rendered <- func_numeric_row_defs(
            value, index,
            feature_internal    = df_t$Feature_internal,
            numeric_vars        = numeric_vars,
            numeric_var_ranges  = numeric_var_ranges,
            palette             = pal_num_scale
          )
          if (!identical(rendered, value)) return(rendered)

          # fallback
          value
        })
      }


      # 4) render table
      reactable(
        df_t[, c("Feature", "Feature_internal", transposed_cols)],
        columns = c(
          col_defs,
          list(Feature_internal = colDef(show = FALSE)) # hide helper column
        ),
        height = (nrow(df_t) * 0.5) * 40,
        bordered = TRUE,
        highlight = TRUE,
        pagination = FALSE,
        searchable = TRUE,
        fullWidth = TRUE,
        resizable = TRUE
      )
    })

    #reset option 3
    observeEvent(input$reset_prod_filter, {
      updateSelectInput(session, "product3", selected = "")
      updateSelectInput(session, "model3", choices = character(0), selected = "")
      disable("model3")
    })


    output$download_data <- downloadHandler(
      filename = function() paste0("sia_product_filter_data_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        write.csv(selected_products(), file, row.names = FALSE, na = "")
        cat(
          "\n# Citation terms.\n",
          "# Thank you for using the Stress-in-Action Wearable Database!\n",
          "# If you use the SiA-WD and/or this web app, you must cite:\n",
          "# Schoenmakers M, Saygin M, Sikora M, Vaessen T, Noordzij M, de Geus E. Stress in action wearables database: A database of noninvasive wearable monitors with systematic technical, reliability, validity, and usability information. Behav Res Methods. 2025 May 13;57(6):171. doi: 10.3758/s13428-025-02685-4. PMID: 40360861; PMCID: PMC12075381.\n",
          "# [Shiny paper comming soon]\n",
          file = file, append = TRUE, sep = ""
        )
      },
      contentType = "text/csv"
    )
  })
}

