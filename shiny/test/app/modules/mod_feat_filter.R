############################################################################################
#
#  Function nodule for nfeauture filter (extensive)
#
#############################################################################################

#' Module UI for Filtering Wearables
mod_feat_fil_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 2,
      bs4Dash::bs4Card(
        title = "Filter Wearables",
        status = "primary",
        width = 12,
        collapsible = FALSE,
        solidHeader = TRUE,
        bs4Card(title = "SiA Expert Score",
                width = 12,
                status = "secondary",
                solidHeader = TRUE,
                collapsible = FALSE,
                sliderInput(ns("sia_es_long"), "Long-Term", min = 0, max = 10, value = c(0,10)),
                sliderInput(ns("sia_es_short"), "Short-Term", min = 0, max = 10, value = c(0,10))
        ),
        bs4Card(title = "General Device Information",
                width = 12,
                status = "secondary",
                collapsible = FALSE,
                selectInput(ns("manufacturer"), "Manufacturer", choices = NULL, multiple = TRUE),
                selectInput(ns("model"), "Model", choices = NULL, multiple = TRUE),
                dateRangeInput(ns("release_date"), "Release Date", start = min(sia_df$release_date, na.rm = TRUE), end = max(sia_df$release_date, na.rm = TRUE), format = "yyyy"),
                selectInput(ns("market_status"), "Market Status", choices = NULL, multiple = TRUE),
                selectInput(ns("main_use"), "Main Use", choices = NULL, multiple = TRUE),
                sliderInput(ns("device_cost"), "Cost (€)", min = 0, max = max(sia_df$device_cost, na.rm = TRUE), value = c(0, max(sia_df$device_cost, na.rm = TRUE))),
                selectInput(ns("wearable_type"), "Type", choices = NULL, multiple = TRUE),
                selectInput(ns("location"), "Location", choices = NULL, multiple = TRUE),
                sliderInput(ns("weight"), "Weight (g)", min=0, max = max(sia_df$weight, na.rm = TRUE), value = c(0,max(sia_df$weight, na.rm = TRUE))),
                selectInput(ns("size"), "Size", choices = NULL, multiple = TRUE)
        ),
        bs4Card(title = "Technical Specifications",
                width = 12,
                status = "secondary",
                collapsible = FALSE,
                checkboxInput(ns("water_resistance"), "Water Resistant"),
                sliderInput(ns("battery_life"), "Battery Life (min)", min = 0, max = max(sia_df$battery_life, na.rm = TRUE), value = c(0, max(sia_df$battery_life, na.rm = TRUE))),
                selectInput(ns("charging_method"), "Charging Method", choices = NULL, multiple = TRUE),
                sliderInput(ns("charging_duration"), "Charging Duration (min)", min = 0, max = max(sia_df$charging_duration, na.rm = TRUE), value = c(0, 10000)),
                checkboxInput(ns("bio_cueing"), "Bio Cueing"),
                checkboxInput(ns("bio_feedback"), "Bio Feedback")
        ),
        bs4Card(title = "Signals",
                width = 12,
                status = "secondary",
                collapsible = FALSE,
                checkboxInput(ns("ppg"), "Photoplethysmogram (PPG)"),
                checkboxInput(ns("ecg"), "Electrocardiogram (ECG)"),
                checkboxInput(ns("icg"), "Impedance cardiography (ICG)"),
                checkboxInput(ns("emg"), "Electromyography (EMG)"),
                checkboxInput(ns("respiration"), "Respiration"),
                checkboxInput(ns("eda"), "Electrodermal activity (EDA)"),
                checkboxInput(ns("eeg"), "Electroencephalography (EEG)"),
                checkboxInput(ns("bp"), "Blood Pressure"),
                checkboxInput(ns("accelerometer"), "Accelerometer"),
                checkboxInput(ns("gyroscope"), "Gyroscope"),
                checkboxInput(ns("gps"), "Global Positioning System (GPS)"),
                checkboxInput(ns("skin_temperature"), "Skin Temperature"),
                selectInput(ns("other_signals"), "Other Signals", choices = NULL, multiple = TRUE)
        ),
        bs4Card(title = "Validation, Reliability & Usability",
                width = 12,
                status = "secondary",
                collapsible = FALSE,
                selectInput(ns("level_validation"), "Validation Level", choices = NULL, multiple = TRUE),
                sliderInput(ns("no_studies_val_rel_reviewed"), "Validation Studies", min = 0, max = max(sia_df$no_studies_val_rel_reviewed, na.rm = TRUE), value = c(0, max(sia_df$no_studies_val_rel_reviewed, na.rm = TRUE))),
                sliderInput(ns("no_studies_usab_reviewed"), "Usability Studies", min = 0, max = max(sia_df$no_studies_usab_reviewed, na.rm = TRUE), value = c(0, max(sia_df$no_studies_val_rel_reviewed, na.rm = TRUE)))
        )
      )
    ),
    column(
      width = 10,
      bs4Dash::bs4Card(
        title = "Filtered Results",
        status = "primary",
        width = 12,
        collapsible = FALSE,
        solidHeader = TRUE,
        div(
          style = "overflow-x: auto;",  # Enable horizontal scrolling
          DT::DTOutput(ns("filtered_table"))
        )
      )
    )
  )
}

#' Module Server for Filtering Wearables
mod_feat_fil_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Observe data and update selectInput choices dynamically
    observe({
      df <- data()

      #General Device Information
      updateSelectInput(session, "manufacturer", choices = unique(df$manufacturer))
      updateSelectInput(session, "model", choices = unique(df$model))
      updateSelectInput(session, "market_status", choices = unique(df$market_status))
      updateSelectInput(session, "main_use", choices = unique(df$main_use))
      updateSelectInput(session, "wearable_type", choices = unique(df$wearable_type))
      updateSelectInput(session, "location", choices = unique(df$location))
      updateSelectInput(session, "size", choices = unique(df$size))

      #Technical Specifications
      updateSelectInput(session, "wearable_type", choices = unique(df$wearable_type))
      updateSelectInput(session, "location", choices = unique(df$location))
      updateSelectInput(session, "size", choices = unique(df$size))
      updateSelectInput(session, "charging_method", choices = unique(df$charging_method))

      #Signals
      updateSelectInput(session, "other_signals", choices = unique(df$other_signals))

      #Validation, Reliability & Usability
      updateSelectInput(session, "level_validation", choices = unique(df$level_validation))

    })



    filtered_data <- reactive({

      data() %>%
        filter(
          # Expert Score Filters
          is.na(sia_es_long) | (sia_es_long >= input$sia_es_long[1] & sia_es_long <= input$sia_es_long[2]),
          is.na(sia_es_short) | (sia_es_short >= input$sia_es_short[1] & sia_es_short <= input$sia_es_short[2]),


          # General Device Information Filters (Fixing selectInput)
          (is.null(input$manufacturer) | manufacturer %in% input$manufacturer),
          (is.null(input$model) | model %in% input$model),
          (is.null(input$market_status) | market_status %in% input$market_status),
          (is.null(input$main_use) | main_use %in% input$main_use),
          (is.null(input$wearable_type) | wearable_type %in% input$wearable_type),
          (is.null(input$location) | location %in% input$location),
          (is.null(input$size) | size %in% input$size),

          # Date Range Filter
          (is.null(input$release_date[1]) | is.null(input$release_date[2]) |
             is.na(release_date) | (release_date >= input$release_date[1] & release_date <= input$release_date[2])),

          # Numeric Range Filters
          is.na(device_cost) | (device_cost >= input$device_cost[1] & device_cost <= input$device_cost[2]),
          is.na(weight) | (weight >= input$weight[1] & weight <= input$weight[2]),

          #Technical Specifications
          is.na(battery_life) | (battery_life >= input$battery_life[1] & battery_life <= input$battery_life[2]),
          is.na(charging_duration) | (charging_duration >= input$charging_duration[1] & charging_duration <= input$charging_duration[2]),
          (is.null(input$charging_method) | charging_method %in% input$charging_method),
          (!input$water_resistance | water_resistance == "yes") &
          (!input$bio_cueing | bio_cueing == "yes") &
          (!input$bio_feedback | bio_feedback == "yes"),

          #Signals
          (!input$ppg | ppg == "yes") &
            (!input$ecg | ecg == "yes") &
            (!input$icg | icg == "yes") &
            (!input$emg | emg == "yes") &
            (!input$respiration | respiration == "yes") &
            (!input$eda | eda == "yes") &
            (!input$eeg | eeg == "yes") &
            (!input$bp | bp == "yes") &
            (!input$accelerometer | accelerometer == "yes") &
            (!input$gyroscope | gyroscope == "yes") &
            (!input$gps | gps == "yes") &
            (!input$skin_temperature | skin_temperature == "yes"),
          (is.null(input$other_signals) | other_signals %in% input$other_signals),

          #Validation, Reliability & Usability
          (is.null(input$no_studies_val_rel_reviewed) | no_studies_val_rel_reviewed %in% input$no_studies_val_rel_reviewed),
          (is.null(input$no_studies_usab_reviewed) | no_studies_usab_reviewed %in% input$no_studies_usab_reviewed)

        )

    })

    output$filtered_table <- DT::renderDT({
      df <- filtered_data()

      # Define custom column names mapping
      # colnames(df) <- c(
      #   "Long-Term Score", "Short-Term Score",
      #   "Manufacturer", "Model", "Release Date", "Market Status", "Main Use",
      #   "Cost (€)", "Type", "Location", "Weight (g)", "Size",
      #   "Battery Life (min)", "Charging Method", "Charging Duration (min)",
      #   "Photoplethysmogram (PPG)", "Electrocardiogram (ECG)", "Impedance Cardiography (ICG)",
      #   "Electromyography (EMG)", "Respiration", "Electrodermal Activity (EDA)",
      #   "Electroencephalography (EEG)", "Blood Pressure", "Accelerometer",
      #   "Gyroscope", "GPS", "Skin Temperature"
      # )

      DT::datatable(
        df,
        options = list(
          pageLength = 30,      # Show 25 entries by default
          autoWidth = TRUE,     # Adjust column widths automatically
          scrollX = TRUE        # Enable horizontal scrolling within DataTable
        )
      )
    })

  })
}
