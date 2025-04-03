############################################################################################
#
#  Function nodule for nfeauture filter (extensive)
#
#############################################################################################

#' Module UI for Filtering Wearables
mod_feat_fil_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 3,
        bs4Card(
          title = "Feature Filter",
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
                  uiOutput(ns("manufacturer_ui")),
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
          bs4Card(title = "Data Acces",
                  width = 12,
                  status = "secondary",
                  collapsible = FALSE,
                  checkboxInput(ns("raw_data_available"), "Raw Data"),
                  selectInput(ns("data_trans_method"), "Data rans_method", choices = NULL, multiple = TRUE),
                  checkboxInput(ns("int_storage_met"), "Internal Storage"),
                  checkboxInput(ns("server_data_storage"), "Server Storage"),
                  sliderInput(ns("dev_storage_cap_mb"), "Device Storage (size in MB)", min = 0, max = max(sia_df$dev_storage_cap_mb, na.rm = TRUE), value = c(0, max(sia_df$dev_storage_cap_mb, na.rm = TRUE))),
                  sliderInput(ns("dev_storage_cap_hrs"), "Device Storage (time in hrs)", min = 0, max = max(sia_df$dev_storage_cap_hrs, na.rm = TRUE), value = c(0, max(sia_df$dev_storage_cap_hrs, na.rm = TRUE))),
                  checkboxInput(ns("gdpr_comp"), "GDPR Compliant"),
                  checkboxInput(ns("fda_app_clear"), "FDA Approved"),
                  checkboxInput(ns("ce_app_label"), "CE Label")
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
        width = 9,
        bs4Card(
          title = "Filtered Results",
          status = "primary",
          width = 12,
          collapsible = FALSE,
          solidHeader = TRUE,
          div(
            style = "text-align: center; margin-bottom: 10px;",
            downloadButton(ns("download_data"), "Download Filtered Results", class = "btn-secondary")
          ),
          DTOutput(ns("feat_filtered_table")) %>% withSpinner()
        )
      )
    )
  )
}

############################################################################################
#
#  Function nodule for nfeauture filter (extensive)
#
#############################################################################################


mod_feat_fil_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Step 1: Reactive data after sliders, checkboxes, and date
    filtered_for_dropdowns <- reactive({
      df <- data()
      df %>%
        filter(
          is.na(sia_es_long) | between(sia_es_long, input$sia_es_long[1], input$sia_es_long[2]),
          is.na(sia_es_short) | between(sia_es_short, input$sia_es_short[1], input$sia_es_short[2]),
          is.na(device_cost) | between(device_cost, input$device_cost[1], input$device_cost[2]),
          is.na(weight) | between(weight, input$weight[1], input$weight[2]),
          is.na(battery_life) | between(battery_life, input$battery_life[1], input$battery_life[2]),
          is.na(charging_duration) | between(charging_duration, input$charging_duration[1], input$charging_duration[2]),
          is.na(dev_storage_cap_mb) | between(dev_storage_cap_mb, input$dev_storage_cap_mb[1], input$dev_storage_cap_mb[2]),
          is.na(dev_storage_cap_hrs) | between(dev_storage_cap_hrs, input$dev_storage_cap_hrs[1], input$dev_storage_cap_hrs[2]),
          is.na(no_studies_val_rel_reviewed) | between(no_studies_val_rel_reviewed, input$no_studies_val_rel_reviewed[1], input$no_studies_val_rel_reviewed[2]),
          is.na(no_studies_usab_reviewed) | between(no_studies_usab_reviewed, input$no_studies_usab_reviewed[1], input$no_studies_usab_reviewed[2]),
          is.null(input$release_date[1]) | is.null(input$release_date[2]) |
            is.na(release_date) | between(release_date, input$release_date[1], input$release_date[2]),
          (!input$water_resistance | water_resistance == "Yes"),
          (!input$bio_cueing | bio_cueing == "Yes"),
          (!input$bio_feedback | bio_feedback == "Yes"),
          (!input$ppg | ppg == "Yes"),
          (!input$ecg | ecg == "Yes"),
          (!input$icg | icg == "Yes"),
          (!input$emg | emg == "Yes"),
          (!input$respiration | respiration == "Yes"),
          (!input$eda | eda == "Yes"),
          (!input$eeg | eeg == "Yes"),
          (!input$bp | bp == "Yes"),
          (!input$accelerometer | accelerometer == "Yes"),
          (!input$gyroscope | gyroscope == "Yes"),
          (!input$gps | gps == "Yes"),
          (!input$skin_temperature | skin_temperature == "Yes"),
          (!input$int_storage_met | int_storage_met == "Yes"),
          (!input$server_data_storage | server_data_storage == "Yes"),
          (!input$raw_data_available | raw_data_available == "Yes"),
          (!input$gdpr_comp | gdpr_comp == "Yes"),
          (!input$ce_app_label | ce_app_label == "Yes"),
          (!input$fda_app_clear | fda_app_clear == "Yes")
        )
    })

    # Step 2: Dynamically update selectInput choices
    observe({
      df <- filtered_for_dropdowns()

      select_inputs <- c("manufacturer", "model", "market_status", "main_use",
                         "wearable_type", "location", "size", "charging_method",
                         "other_signals", "data_trans_method", "level_validation")

      for (input_id in select_inputs) {
        # Filter df based on all other selectInputs except current one
        df_filtered <- df
        for (other_id in setdiff(select_inputs, input_id)) {
          selected <- input[[other_id]]
          if (!is.null(selected)) {
            df_filtered <- df_filtered %>% filter(.data[[other_id]] %in% selected)
          }
        }

        # Update current input choices based on filtered data
        valid_choices <- sort(unique(df_filtered[[input_id]]))
        selected_now <- input[[input_id]]
        updateSelectInput(session, input_id,
                          choices = valid_choices,
                          selected = selected_now[selected_now %in% valid_choices])
      }
    })


    # Step 3: Apply full filtering (sliders + checkboxes + dropdowns)
    filtered_data <- reactive({
      df <- filtered_for_dropdowns()  # Use already partially filtered data

      df %>%
        filter(
          is.null(input$manufacturer) | manufacturer %in% input$manufacturer,
          is.null(input$model) | model %in% input$model,
          is.null(input$market_status) | market_status %in% input$market_status,
          is.null(input$main_use) | main_use %in% input$main_use,
          is.null(input$wearable_type) | wearable_type %in% input$wearable_type,
          is.null(input$location) | location %in% input$location,
          is.null(input$size) | size %in% input$size,
          is.null(input$charging_method) | charging_method %in% input$charging_method,
          is.null(input$other_signals) | other_signals %in% input$other_signals,
          is.null(input$data_trans_method) | data_trans_method %in% input$data_trans_method,
          is.null(input$level_validation) | level_validation %in% input$level_validation
        )
    })

    # Observe Reset Filter button
    observeEvent(input$reset_filter, {
      # Reset sliders
      updateSliderInput(session, "sia_es_long", value = c(0, 10))
      updateSliderInput(session, "sia_es_short", value = c(0, 10))
      updateSliderInput(session, "device_cost", value = c(0, max(sia_df$device_cost, na.rm = TRUE)))
      updateSliderInput(session, "weight", value = c(0, max(sia_df$weight, na.rm = TRUE)))
      updateSliderInput(session, "battery_life", value = c(0, max(sia_df$battery_life, na.rm = TRUE)))
      updateSliderInput(session, "charging_duration", value = c(0, 10000))
      updateSliderInput(session, "dev_storage_cap_mb", value = c(0, max(sia_df$dev_storage_cap_mb, na.rm = TRUE)))
      updateSliderInput(session, "dev_storage_cap_hrs", value = c(0, max(sia_df$dev_storage_cap_hrs, na.rm = TRUE)))
      updateSliderInput(session, "no_studies_val_rel_reviewed", value = c(0, max(sia_df$no_studies_val_rel_reviewed, na.rm = TRUE)))
      updateSliderInput(session, "no_studies_usab_reviewed", value = c(0, max(sia_df$no_studies_usab_reviewed, na.rm = TRUE)))

      # Reset date
      updateDateRangeInput(session, "release_date",
                           start = min(sia_df$release_date, na.rm = TRUE),
                           end = max(sia_df$release_date, na.rm = TRUE)
      )

      # Reset checkboxInputs
      checkbox_inputs <- c("water_resistance", "bio_cueing", "bio_feedback", "ppg", "ecg", "icg",
                           "emg", "respiration", "eda", "eeg", "bp", "accelerometer", "gyroscope",
                           "gps", "skin_temperature", "raw_data_available", "int_storage_met",
                           "server_data_storage", "gdpr_comp", "fda_app_clear", "ce_app_label")
      lapply(checkbox_inputs, function(id) updateCheckboxInput(session, id, value = FALSE))

      # Reset selectInputs
      select_inputs <- c("manufacturer", "model", "market_status", "main_use",
                         "wearable_type", "location", "size", "charging_method",
                         "other_signals", "data_trans_method", "level_validation")
      lapply(select_inputs, function(id) updateSelectInput(session, id, selected = character(0)))
    })

    # Step 4: Render filtered table
    output$feat_filtered_table <- renderDT({

      df <- filtered_data()

      # Define the column renaming map
      rename_map <- c(
        "sia_es_long" = "Long-Term SiA Score",
        "sia_es_short" = "Short-Term SiA Score",
        "manufacturer" = "Manufacturer",
        "model" = "Model",
        "release_date" = "Release Date",
        "market_status" = "Market Status",
        "main_use" = "Main Use",
        "device_cost" = "Cost (€)",
        "wearable_type" = "Type",
        "location" = "Location",
        "weight" = "Weight (g)",
        "size" = "Size",
        "water_resistance" = "Water Resistant",
        "battery_life" = "Battery Life (min)",
        "charging_method" = "Charging Method",
        "charging_duration" = "Charging Duration (min)",
        "bio_cueing" = "Bio Cueing",
        "bio_feedback" = "Bio Feedback",
        "ppg" = "PPG",
        "ecg" = "ECG",
        "icg" = "ICG",
        "emg" = "EMG",
        "respiration" = "Respiration",
        "eda" = "EDA",
        "eeg" = "EEG",
        "bp" = "Blood Pressure",
        "accelerometer" = "Accelerometer",
        "gyroscope" = "Gyroscope",
        "gps" = "GPS",
        "skin_temperature" = "Skin Temperature",
        "other_signals" = "Other Signals",
        "raw_data_available" = "Raw Data Available",
        "data_trans_method" = "Data Transmission Method",
        "int_storage_met" = "Internal Storage",
        "server_data_storage" = "Server Data Storage",
        "dev_storage_cap_hrs" = "Device Storage (hrs)",
        "dev_storage_cap_mb" = "Device Storage (MB)",
        "gdpr_comp" = "GDPR Compliant",
        "fda_app_clear" = "FDA Approved",
        "ce_app_label" = "CE Label",
        "level_validation" = "Validation Level",
        "no_studies_val_rel_reviewed" = "Validation Studies Reviewed",
        "no_studies_usab_reviewed" = "Usability Studies Reviewed"
      )

      # Rename only the columns that exist in df
      existing_cols <- names(df)
      new_colnames <- rename_map[existing_cols]  # Get mapped names for existing cols
      names(df) <- ifelse(!is.na(new_colnames), new_colnames, existing_cols)  # Rename safely

      datatable(
        df,
        options = list(
          pageLength = 40,
          autoWidth = TRUE,
          scrollX = TRUE,
          columnDefs = list(
            list(width = "150px", targets = "_all")
          )
        )
      )
    })

    # Step 5: Download Handler
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("filtered_data-", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(filtered_data(), file, row.names = FALSE)
      }
    )
  })
}

# mod_feat_fil_server <- function(id, data) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#
#     # --- Helper functions ---
#     between_or_na <- function(var, rng) {
#       is.na(var) | between(var, rng[1], rng[2])
#     }
#
#     date_between_or_na <- function(var, rng) {
#       is.null(rng[1]) | is.null(rng[2]) | is.na(var) | between(var, rng[1], rng[2])
#     }
#
#     is_selected <- function(x) {
#       !is.null(x) && length(x) > 0
#     }
#
#     select_inputs <- c("manufacturer", "model", "market_status", "main_use",
#                        "wearable_type", "location", "size", "charging_method",
#                        "other_signals", "data_trans_method", "level_validation")
#
#     # --- Step 1: Reactive base filter (sliders, checkboxes, date) ---
#     filtered_base <- reactive({
#       df <- data()
#
#       df %>%
#         filter(
#           between_or_na(sia_es_long, input$sia_es_long),
#           between_or_na(sia_es_short, input$sia_es_short),
#           between_or_na(device_cost, input$device_cost),
#           between_or_na(weight, input$weight),
#           between_or_na(battery_life, input$battery_life),
#           between_or_na(charging_duration, input$charging_duration),
#           between_or_na(dev_storage_cap_mb, input$dev_storage_cap_mb),
#           between_or_na(dev_storage_cap_hrs, input$dev_storage_cap_hrs),
#           between_or_na(no_studies_val_rel_reviewed, input$no_studies_val_rel_reviewed),
#           between_or_na(no_studies_usab_reviewed, input$no_studies_usab_reviewed),
#           date_between_or_na(release_date, input$release_date),
#
#           !input$water_resistance | water_resistance == "Yes",
#           !input$bio_cueing | bio_cueing == "Yes",
#           !input$bio_feedback | bio_feedback == "Yes",
#           !input$ppg | ppg == "Yes",
#           !input$ecg | ecg == "Yes",
#           !input$icg | icg == "Yes",
#           !input$emg | emg == "Yes",
#           !input$respiration | respiration == "Yes",
#           !input$eda | eda == "Yes",
#           !input$eeg | eeg == "Yes",
#           !input$bp | bp == "Yes",
#           !input$accelerometer | accelerometer == "Yes",
#           !input$gyroscope | gyroscope == "Yes",
#           !input$gps | gps == "Yes",
#           !input$skin_temperature | skin_temperature == "Yes",
#           !input$int_storage_met | int_storage_met == "Yes",
#           !input$server_data_storage | server_data_storage == "Yes",
#           !input$raw_data_available | raw_data_available == "Yes",
#           !input$gdpr_comp | gdpr_comp == "Yes",
#           !input$ce_app_label | ce_app_label == "Yes",
#           !input$fda_app_clear | fda_app_clear == "Yes"
#         )
#     })
#
#     # --- Step 2: Cross-filtering for selectInput choices ---
#     observe({
#       df <- filtered_base()
#
#       for (input_id in select_inputs) {
#         df_filtered <- df
#         for (other_id in setdiff(select_inputs, input_id)) {
#           if (is_selected(input[[other_id]])) {
#             df_filtered <- df_filtered %>% filter(.data[[other_id]] %in% input[[other_id]])
#           }
#         }
#
#         valid_choices <- sort(unique(df_filtered[[input_id]]))
#         selected_now <- input[[input_id]]
#         updateSelectInput(session, input_id,
#                           choices = valid_choices,
#                           selected = selected_now[selected_now %in% valid_choices])
#       }
#     })
#
#     # --- Step 3: Final filtered data (all filters + dropdowns) ---
#     filtered_data <- reactive({
#       df <- filtered_base()
#
#       for (col in select_inputs) {
#         selected <- input[[col]]
#         if (is_selected(selected)) {
#           df <- df %>% filter(.data[[col]] %in% selected)
#         }
#       }
#
#       df
#     })
#
#     # --- Step 4: Render filtered table ---
#     output$filtered_table <- renderDT({
#       datatable(
#         filtered_data(),
#         options = list(
#           pageLength = 40,
#           autoWidth = TRUE,
#           columnDefs = list(
#             list(width = "150px", targets = "_all")
#           )
#         )
#       )
#     })
#
#     # --- Step 5: Download filtered data ---
#     output$download_data <- downloadHandler(
#       filename = function() {
#         paste0("filtered_data-", Sys.Date(), ".csv")
#       },
#       content = function(file) {
#         write.csv(filtered_data(), file, row.names = FALSE)
#       }
#     )
#   })
# }
