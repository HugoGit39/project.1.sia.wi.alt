#' ############################################################################################
#' #
#' #  Function nodule for nfeauture filter (extensive)
#' #
#' #############################################################################################
#'
#' #' Module UI for Filtering Wearables
#' mod_feat_fil_ui <- function(id) {
#'   ns <- NS(id)
#'   bs4Dash::bs4Card(
#'     title = "Filter Wearables",
#'     status = "primary",
#'     width = 12,
#'     collapsible = TRUE,
#'     solidHeader = TRUE,
#'     bs4Dash::column(
#'       width = 4,
#'       bs4Dash::bs4Card(
#'         title = "SiA Expert Score",
#'         sliderInput(ns("sia_score_short"), "Short-term Score", min = 0, max = 10, value = c(0, 10)),
#'         sliderInput(ns("sia_score_long"), "Long-term Score", min = 0, max = 10, value = c(0, 10))
#'       ),
#'       bs4Dash::bs4Card(
#'         title = "General Device Information",
#'         selectInput(ns("manufacturer"), "Manufacturer", choices = NULL, multiple = TRUE),
#'         dateRangeInput(ns("release_date"), "Release Date"),
#'         sliderInput(ns("cost"), "Cost", min = 0, max = 10000, value = c(0, 10000)),
#'         selectInput(ns("main_use"), "Main Use", choices = NULL, multiple = TRUE),
#'         selectInput(ns("type"), "Type", choices = NULL, multiple = TRUE),
#'         selectInput(ns("location"), "Location", choices = NULL, multiple = TRUE),
#'         sliderInput(ns("weight"), "Weight (g)", min = 0, max = 500, value = c(0, 500))
#'       ),
#'       bs4Dash::bs4Card(
#'         title = "Technical Specifications",
#'         checkboxInput(ns("water_resistant"), "Water Resistant", value = FALSE),
#'         sliderInput(ns("battery_life"), "Battery Life (hours)", min = 0, max = 100, value = c(0, 100)),
#'         sliderInput(ns("charging_duration"), "Charging Duration (hours)", min = 0, max = 24, value = c(0, 24)),
#'         checkboxInput(ns("bio_cueing"), "Bio Cueing", value = FALSE),
#'         checkboxInput(ns("bio_feedback"), "Bio Feedback", value = FALSE)
#'       ),
#'       bs4Dash::bs4Card(
#'         title = "Signal",
#'         checkboxGroupInput(ns("signal"), "Select Signal Types:",
#'                            choices = c("PPG", "ECG", "ICG", "Respiration", "EDA", "EEG", "BP", "Accelerometer", "Gyroscope", "GPS", "Skin Temperature", "Other"))
#'       ),
#'       bs4Dash::bs4Card(
#'         title = "Data Access",
#'         checkboxInput(ns("raw_data"), "Raw Data Available", value = FALSE),
#'         sliderInput(ns("device_storage"), "Device Storage (MB)", min = 0, max = 50000, value = c(0, 50000)),
#'         checkboxInput(ns("gdpr_compliant"), "GDPR Compliant", value = FALSE),
#'         checkboxInput(ns("fda_approved"), "FDA Approved", value = FALSE),
#'         checkboxInput(ns("ce_approved"), "CE Approved", value = FALSE)
#'       ),
#'       bs4Dash::bs4Card(
#'         title = "Validation Level",
#'         selectInput(ns("validation_level"), "Validation Level", choices = NULL),
#'         sliderInput(ns("num_validation_studies"), "No. of Validation Studies", min = 0, max = 50, value = c(0, 50))
#'       )
#'     ),
#'     DT::DTOutput(ns("filtered_table"))
#'   )
#' }
#'
#' #' Module Server for Filtering Wearables
#' mod_feat_fil_server <- function(id, sia_df) {
#'   moduleServer(id, function(input, output, session) {
#'     ns <- session$ns
#'
#'     observe({
#'       updateSelectInput(session, "manufacturer", choices = unique(sia_df$manufacturer))
#'       updateSelectInput(session, "main_use", choices = unique(sia_df$main_use))
#'       updateSelectInput(session, "type", choices = unique(sia_df$wearable_type))
#'       updateSelectInput(session, "location", choices = unique(sia_df$location))
#'       updateSelectInput(session, "validation_level", choices = unique(sia_df$validation_level))
#'     })
#'
#'     filtered_data <- reactive({
#'       if (all(
#'         is.null(input$manufacturer), is.null(input$main_use), is.null(input$type), is.null(input$location),
#'         input$sia_score_short[1] == 0, input$sia_score_short[2] == 10,
#'         input$sia_score_long[1] == 0, input$sia_score_long[2] == 10,
#'         input$cost[1] == 0, input$cost[2] == 10000,
#'         input$device_storage[1] == 0, input$device_storage[2] == 50000,
#'         input$num_validation_studies[1] == 0, input$num_validation_studies[2] == 50,
#'         !input$water_resistant, !input$bio_cueing, !input$bio_feedback,
#'         !input$raw_data, !input$gdpr_compliant, !input$fda_approved, !input$ce_approved
#'       )) {
#'         return(sia_df) # Return full table initially
#'       }
#'
#'       sia_df %>%
#'         filter(
#'           `SiA Expert score (short-term)` >= input$sia_score_short[1],
#'           `SiA Expert score (short-term)` <= input$sia_score_short[2],
#'           `SiA Expert score (long-term)` >= input$sia_score_long[1],
#'           `SiA Expert score (long-term)` <= input$sia_score_long[2],
#'           if (!is.null(input$manufacturer)) manufacturer %in% input$manufacturer else TRUE,
#'           if (!is.null(input$main_use)) main_use %in% input$main_use else TRUE,
#'           if (!is.null(input$type)) wearable_type %in% input$type else TRUE,
#'           if (!is.null(input$location)) location %in% input$location else TRUE,
#'           between(as.numeric(cost), input$cost[1], input$cost[2]),
#'           (`water_resistant` == 1 & input$water_resistant) | !input$water_resistant,
#'           (`bio_cueing` == 1 & input$bio_cueing) | !input$bio_cueing,
#'           (`bio_feedback` == 1 & input$bio_feedback) | !input$bio_feedback,
#'           (`raw_data` == 1 & input$raw_data) | !input$raw_data,
#'           (`gdpr_compliant` == 1 & input$gdpr_compliant) | !input$gdpr_compliant,
#'           (`fda_approved` == 1 & input$fda_approved) | !input$fda_approved,
#'           (`ce_approved` == 1 & input$ce_approved) | !input$ce_approved,
#'           between(as.numeric(device_storage), input$device_storage[1], input$device_storage[2]),
#'           if (!is.null(input$validation_level)) validation_level %in% input$validation_level else TRUE,
#'           between(as.numeric(num_validation_studies), input$num_validation_studies[1], input$num_validation_studies[2])
#'         )
#'     })
#'
#'     output$filtered_table <- DT::renderDT({
#'       DT::datatable(filtered_data(), options = list(pageLength = 10, autoWidth = TRUE))
#'     })
#'   })
#' }
#'

#' Module UI for Filtering Wearables
mod_feat_fil_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 4,
      bs4Dash::bs4Card(
        title = "Filter Wearables",
        status = "primary",
        width = 12,
        collapsible = TRUE,
        solidHeader = TRUE,
        bs4Dash::bs4Card(
          title = "SiA Expert Score",
          sliderInput(ns("sia_score_short"), "Short-term Score", min = 0, max = 10, value = c(0, 10)),
          sliderInput(ns("sia_score_long"), "Long-term Score", min = 0, max = 10, value = c(0, 10))
        ),
        bs4Dash::bs4Card(
          title = "General Device Information",
          selectInput(ns("manufacturer"), "Manufacturer", choices = NULL, multiple = TRUE),
          dateRangeInput(ns("release_date"), "Release Date"),
          sliderInput(ns("cost"), "Cost", min = 0, max = 10000, value = c(0, 10000)),
          selectInput(ns("main_use"), "Main Use", choices = NULL, multiple = TRUE),
          selectInput(ns("type"), "Type", choices = NULL, multiple = TRUE),
          selectInput(ns("location"), "Location", choices = NULL, multiple = TRUE),
          sliderInput(ns("weight"), "Weight (g)", min = 0, max = 500, value = c(0, 500))
        ),
        bs4Dash::bs4Card(
          title = "Technical Specifications",
          checkboxInput(ns("water_resistant"), "Water Resistant", value = FALSE),
          sliderInput(ns("battery_life"), "Battery Life (hours)", min = 0, max = 100, value = c(0, 100)),
          sliderInput(ns("charging_duration"), "Charging Duration (hours)", min = 0, max = 24, value = c(0, 24)),
          checkboxInput(ns("bio_cueing"), "Bio Cueing", value = FALSE),
          checkboxInput(ns("bio_feedback"), "Bio Feedback", value = FALSE)
        ),
        bs4Dash::bs4Card(
          title = "Signal",
          checkboxGroupInput(ns("signal"), "Select Signal Types:",
                             choices = c("PPG", "ECG", "ICG", "Respiration", "EDA", "EEG", "BP", "Accelerometer", "Gyroscope", "GPS", "Skin Temperature", "Other"))
        ),
        bs4Dash::bs4Card(
          title = "Data Access",
          checkboxInput(ns("raw_data"), "Raw Data Available", value = FALSE),
          sliderInput(ns("device_storage"), "Device Storage (MB)", min = 0, max = 50000, value = c(0, 50000)),
          checkboxInput(ns("gdpr_compliant"), "GDPR Compliant", value = FALSE),
          checkboxInput(ns("fda_approved"), "FDA Approved", value = FALSE),
          checkboxInput(ns("ce_approved"), "CE Approved", value = FALSE)
        ),
        bs4Dash::bs4Card(
          title = "Validation Level",
          selectInput(ns("validation_level"), "Validation Level", choices = NULL),
          sliderInput(ns("num_validation_studies"), "No. of Validation Studies", min = 0, max = 50, value = c(0, 50))
        )
      )
    ),
    column(
      width = 8,
      bs4Dash::bs4Card(
        title = "Filtered Results",
        status = "primary",
        width = 12,
        collapsible = TRUE,
        solidHeader = TRUE,
        DT::DTOutput(ns("filtered_table"))
      )
    )
  )
}

#' Module Server for Filtering Wearables
mod_feat_fil_server <- function(id, sia_df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      updateSelectInput(session, "manufacturer", choices = unique(sia_df$manufacturer))
      updateSelectInput(session, "main_use", choices = unique(sia_df$main_use))
      updateSelectInput(session, "type", choices = unique(sia_df$wearable_type))
      updateSelectInput(session, "location", choices = unique(sia_df$location))
      updateSelectInput(session, "validation_level", choices = unique(sia_df$level_validation))
    })

    filtered_data <- reactive({
      sia_df %>%
        filter(
          sia_es_short >= input$sia_score_short[1] & sia_es_short <= input$sia_score_short[2],
          sia_es_long >= input$sia_score_long[1] & sia_es_long <= input$sia_score_long[2],
          if (!is.null(input$manufacturer)) manufacturer %in% input$manufacturer else TRUE,
          if (!is.null(input$main_use)) main_use %in% input$main_use else TRUE,
          if (!is.null(input$type)) wearable_type %in% input$type else TRUE,
          if (!is.null(input$location)) location %in% input$location else TRUE,
          between(as.numeric(device_cost), input$device_cost[1], input$device_cost[2]),
          (`water_resistant` == 1 & input$water_resistant) | !input$water_resistant,
          (`bio_cueing` == 1 & input$bio_cueing) | !input$bio_cueing,
          (`bio_feedback` == 1 & input$bio_feedback) | !input$bio_feedback,
          (`raw_data` == 1 & input$raw_data) | !input$raw_data,
          (`gdpr_compliant` == 1 & input$gdpr_compliant) | !input$gdpr_compliant,
          (`fda_approved` == 1 & input$fda_approved) | !input$fda_approved,
          (`ce_approved` == 1 & input$ce_approved) | !input$ce_approved,
          between(as.numeric(device_storage), input$device_storage[1], input$device_storage[2]),
          if (!is.null(input$level_validation)) level_validation %in% input$level_validation else TRUE,
          between(as.numeric(num_validation_studies), input$num_validation_studies[1], input$num_validation_studies[2])
        )
    })

    output$filtered_table <- DT::renderDT({
      DT::datatable(filtered_data(), options = list(pageLength = 10, autoWidth = TRUE))
    })
  })
}
