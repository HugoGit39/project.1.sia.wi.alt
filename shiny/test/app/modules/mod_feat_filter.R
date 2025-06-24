############################################################################################
#
#  Function nodule for feauture filter (extensive)
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
            downloadButton(ns("download_filter_settings"), "Download Filter Settings")
          ),
          div(
            style = "text-align: center; margin-bottom: 10px;",
            actionButton(
              inputId = ns("reset_feat_filter"),
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
                  noUiSliderInput(ns("sia_es_long"), label = "Long-Term Expert Score", min = 0, max = 10, value = c(0,10),
                                  pips = list(mode = "values", values = list(0, 10), density = 10),
                                  connect = TRUE, color = "#f15a29", format = wNumbFormat(decimals = 1)),
                  noUiSliderInput(ns("sia_es_short"), label = "Short-Term Expert Score", min = 0, max = 10, value = c(0, 10),
                                  pips = list(mode = "values", values = list(0, 10), density = 10),
                                  connect = TRUE, color = "#f15a29", format = wNumbFormat(decimals = 1)),
                  tags$div(
                    tags$label("Exclude missing SiA scores"),
                    switchInput(inputId = ns("exclude_na_sia"), onLabel = "YES", offLabel = "NO",
                                value = FALSE, size = "sm", onStatus = "secondary", offStatus = "primary")
                  )
          ),
          bs4Card(title = "General Device Information",
                  width = 12,
                  status = "secondary",
                  collapsible = FALSE,
                  pickerInput(ns("manufacturer"), "Manufacturer", choices = NULL, multiple = TRUE),
                  pickerInput(ns("model"), "Model", choices = NULL, multiple = TRUE),
                  airDatepickerInput(ns("release_date"), "Release Year Range",
                                     range = TRUE, view = "years", minView = "years",
                                     dateFormat = "yyyy",
                                     value = c(min(sia_df$release_date, na.rm = TRUE),
                                               max(sia_df$release_date, na.rm = TRUE))),
                  pickerInput(ns("market_status"), "Market Status", choices = NULL, multiple = TRUE),
                  pickerInput(ns("main_use"), "Main Use", choices = NULL, multiple = TRUE),
                  noUiSliderInput(ns("device_cost"), label = "Device Cost (€)", min = 0,
                                  max = max(sia_df$device_cost, na.rm = TRUE),
                                  value = c(0, max(sia_df$device_cost, na.rm = TRUE)),
                                  pips = list(mode = "values", values = list(0, max(sia_df$device_cost, na.rm = TRUE)), density = 10),
                                  connect = TRUE, color = "#1c75bc", format = wNumbFormat(decimals = 0)),
                  pickerInput(ns("wearable_type"), "Type", choices = NULL, multiple = TRUE),
                  pickerInput(ns("location"), "Location", choices = NULL, multiple = TRUE),
                  noUiSliderInput(ns("weight"), label = "Weight (g)", min = 0,
                                  max = max(sia_df$weight, na.rm = TRUE),
                                  value = c(0, max(sia_df$weight, na.rm = TRUE)),
                                  pips = list(mode = "values", values = list(0, max(sia_df$weight, na.rm = TRUE)), density = 10),
                                  connect = TRUE, color = "#1c75bc", format = wNumbFormat(decimals = 0)),
                  pickerInput(ns("size"), "Size", choices = NULL, multiple = TRUE)
          ),
          bs4Card(title = "Technical Specifications",
                  width = 12,
                  status = "secondary",
                  collapsible = FALSE,
                  prettyCheckbox(ns("water_resistance"), label = "Water Resistant", icon = icon("check"), status = "primary"),
                  noUiSliderInput(ns("battery_life"), label = "Battery Life (min)", min = 0,
                                  max = max(sia_df$battery_life, na.rm = TRUE),
                                  value = c(0, max(sia_df$battery_life, na.rm = TRUE)),
                                  pips = list(mode = "values", values = list(0, max(sia_df$battery_life, na.rm = TRUE)), density = 10),
                                  connect = TRUE, color = "#1c75bc", format = wNumbFormat(decimals = 0)),
                  pickerInput(ns("charging_method"), "Charging Method", choices = NULL, multiple = TRUE),
                  noUiSliderInput(ns("charging_duration"), label = "Charging Duration (min)", min = 0,
                                  max = max(sia_df$charging_duration, na.rm = TRUE),
                                  value = c(0, max(sia_df$charging_duration, na.rm = TRUE)),
                                  pips = list(mode = "values", values = list(0, max(sia_df$charging_duration, na.rm = TRUE)), density = 10),
                                  connect = TRUE, color = "#1c75bc", format = wNumbFormat(decimals = 0)),
                  prettyCheckbox(ns("bio_cueing"), label = "Bio Cueing", icon = icon("check"), status = "primary"),
                  prettyCheckbox(ns("bio_feedback"), label = "Bio Feedback", icon = icon("check"), status = "primary")
          ),
          bs4Card(title = "Signals",
                  width = 12,
                  status = "secondary",
                  collapsible = FALSE,
                  prettyCheckbox(ns("ppg"), label = "Photoplethysmogram (PPG)", icon = icon("check"), status = "primary"),
                  prettyCheckbox(ns("ecg"), label = "Electrocardiogram (ECG)", icon = icon("check"), status = "primary"),
                  prettyCheckbox(ns("icg"), label = "Impedance cardiography (ICG)", icon = icon("check"), status = "primary"),
                  prettyCheckbox(ns("emg"), label = "Electromyography (EMG)", icon = icon("check"), status = "primary"),
                  prettyCheckbox(ns("respiration"), label = "Respiration", icon = icon("check"), status = "primary"),
                  prettyCheckbox(ns("eda"), label = "Electrodermal activity (EDA)", icon = icon("check"), status = "primary"),
                  prettyCheckbox(ns("eeg"), label = "Electroencephalography (EEG)", icon = icon("check"), status = "primary"),
                  prettyCheckbox(ns("bp"), label = "Blood Pressure", icon = icon("check"), status = "primary"),
                  prettyCheckbox(ns("accelerometer"), label = "Accelerometer", icon = icon("check"), status = "primary"),
                  prettyCheckbox(ns("gyroscope"), label = "Gyroscope", icon = icon("check"), status = "primary"),
                  prettyCheckbox(ns("gps"), label = "Global Positioning System (GPS)", icon = icon("check"), status = "primary"),
                  prettyCheckbox(ns("skin_temperature"), label = "Skin Temperature", icon = icon("check"), status = "primary"),
                  pickerInput(ns("other_signals"), "Other Signals", choices = NULL, multiple = TRUE)
          ),
          bs4Card(title = "Data Access",
                  width = 12,
                  status = "secondary",
                  collapsible = FALSE,
                  prettyCheckbox(ns("raw_data_available"), label = "Raw Data", icon = icon("check"), status = "primary"),
                  pickerInput(ns("data_trans_method"), "Data Transmission Method", choices = NULL, multiple = TRUE),
                  prettyCheckbox(ns("int_storage_met"), label = "Internal Storage", icon = icon("check"), status = "primary"),
                  prettyCheckbox(ns("server_data_storage"), label = "Server Storage", icon = icon("check"), status = "primary"),
                  noUiSliderInput(ns("dev_storage_cap_mb"), label = "Storage Capacity (MB)", min = 0,
                                  max = max(sia_df$dev_storage_cap_mb, na.rm = TRUE),
                                  value = c(0, max(sia_df$dev_storage_cap_mb, na.rm = TRUE)),
                                  pips = list(mode = "values", values = list(0, max(sia_df$dev_storage_cap_mb, na.rm = TRUE)), density = 10),
                                  connect = TRUE, color = "#1c75bc", format = wNumbFormat(decimals = 0)),
                  noUiSliderInput(ns("dev_storage_cap_hrs"), label = "Storage Capacity (hrs)", min = 0,
                                  max = max(sia_df$dev_storage_cap_hrs, na.rm = TRUE),
                                  value = c(0, max(sia_df$dev_storage_cap_hrs, na.rm = TRUE)),
                                  pips = list(mode = "values", values = list(0, max(sia_df$dev_storage_cap_hrs, na.rm = TRUE)), density = 10),
                                  connect = TRUE, color = "#1c75bc", format = wNumbFormat(decimals = 0)),
                  prettyCheckbox(ns("gdpr_comp"), label = "GDPR Compliant", icon = icon("check"), status = "primary"),
                  prettyCheckbox(ns("fda_app_clear"), label = "FDA Approved", icon = icon("check"), status = "primary"),
                  prettyCheckbox(ns("ce_app_label"), label = "CE Label", icon = icon("check"), status = "primary")
          ),
          bs4Card(title = "Validation, Reliability & Usability",
                  width = 12,
                  status = "secondary",
                  collapsible = FALSE,
                  pickerInput(ns("level_validation"), "Validation Level", choices = NULL, multiple = TRUE),
                  noUiSliderInput(ns("no_studies_val_rel_reviewed"), label = "# Validation/Reliability Studies", min = 0,
                                  max = max(sia_df$no_studies_val_rel_reviewed, na.rm = TRUE),
                                  value = c(0, max(sia_df$no_studies_val_rel_reviewed, na.rm = TRUE)),
                                  pips = list(mode = "values", values = list(0, max(sia_df$no_studies_val_rel_reviewed, na.rm = TRUE)), density = 10),
                                  connect = TRUE, color = "#1c75bc", format = wNumbFormat(decimals = 0)),
                  noUiSliderInput(ns("no_studies_usab_reviewed"), label = "# Usability Studies", min = 0,
                                  max = max(sia_df$no_studies_usab_reviewed, na.rm = TRUE),
                                  value = c(0, max(sia_df$no_studies_val_rel_reviewed, na.rm = TRUE)),
                                  pips = list(mode = "values", values = list(0, max(sia_df$no_studies_val_rel_reviewed, na.rm = TRUE)), density = 10),
                                  connect = TRUE, color = "#1c75bc", format = wNumbFormat(decimals = 0))
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
            downloadButton(ns("download_data"), "Download Filtered Results")
          ),
          reactableOutput(ns("feat_filtered_table")) %>% withSpinner(),
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

    range_vars <- c("sia_es_long", "sia_es_short", "device_cost", "weight", "battery_life",
                    "charging_duration", "dev_storage_cap_mb", "dev_storage_cap_hrs",
                    "no_studies_val_rel_reviewed", "no_studies_usab_reviewed")

    checkbox_vars <- c("water_resistance", "bio_cueing", "bio_feedback", "ppg", "ecg", "icg",
                       "emg", "respiration", "eda", "eeg", "bp", "accelerometer", "gyroscope",
                       "gps", "skin_temperature", "int_storage_met", "server_data_storage",
                       "raw_data_available", "gdpr_comp", "ce_app_label", "fda_app_clear")

    select_inputs <- c("manufacturer", "model", "market_status", "main_use",
                       "wearable_type", "location", "size", "charging_method",
                       "other_signals", "data_trans_method", "level_validation")

    # Step 1: Filter for dropdowns
    filtered_for_dropdowns <- reactive({
      df <- data()

      for (var in range_vars) {
        df <- range_filter(df, var, input[[var]])
      }

      # Release date range
      if (!is.null(input$release_date[1]) && !is.null(input$release_date[2])) {
        df <- df %>%
          filter(
            is.na(release_date) |
              (year(release_date) >= year(input$release_date[1]) &
                 year(release_date) <= year(input$release_date[2]))
          )
      }

      for (var in checkbox_vars) {
        df <- checkbox_filter(df, var, input[[var]])
      }

      # Exclude rows with missing SIA scores if checkbox is ticked
      if (isTRUE(input$exclude_na_sia)) {
        df <- df %>% filter(!is.na(sia_es_short), !is.na(sia_es_long))
      }

      df
    })

    # Step 2: Update pickerInput options
    observe({
      df <- filtered_for_dropdowns()
      for (input_id in select_inputs) {
        df_filtered <- df
        for (other_id in setdiff(select_inputs, input_id)) {
          selected <- input[[other_id]]
          if (!is.null(selected)) {
            df_filtered <- select_filter(df_filtered, other_id, selected)
          }
        }
        valid_choices <- sort(unique(df_filtered[[input_id]]))
        selected_now <- input[[input_id]]
        updatePickerInput(session, input_id,
                          choices = valid_choices,
                          selected = selected_now[selected_now %in% valid_choices])
      }
    })

    # Step 3: Apply full filtering
    filtered_data <- reactive({
      df <- filtered_for_dropdowns()
      for (var in select_inputs) {
        df <- select_filter(df, var, input[[var]])
      }
      df
    })

    # Step 4: Reset filters
    observeEvent(input$reset_feat_filter, {
      for (var in range_vars) {
        if (var %in% c("sia_es_long", "sia_es_short")) {
          updateSliderInput(session, var, value = c(0, 10))
        } else {
          updateSliderInput(session, var, value = c(0, max(sia_df[[var]], na.rm = TRUE)))
        }
      }

      updateDateRangeInput(session, "release_date",
                           start = min(sia_df$release_date, na.rm = TRUE),
                           end = max(sia_df$release_date, na.rm = TRUE))

      lapply(checkbox_vars, function(id) updateCheckboxInput(session, id, value = FALSE))

      lapply(select_inputs, function(id) updatePickerInput(session, id, selected = character(0)))

      updateSwitchInput(session, "exclude_na_sia", value = FALSE)

    })

    # Step 5: Render table
    output$feat_filtered_table <- renderReactable({
      df <- filtered_data()

      # Format release date to year only
      if ("release_date" %in% names(df)) {
        df$release_date <- format(df$release_date, "%Y")
      }

      df <- filtered_data()

      #create bars
      bar_column_defs <- func_bar_column_defs(df, bar_vars, rename_map)

      #create yes/np
      yn_column_defs <- func_yn_column_defs(yn_vars, rename_map)

      #create colored numerical cells
      numeric_column_defs <- func_numeric_column_defs(df, numeric_vars, rename_map, numeric_var_ranges)

      #char columns rename
      char_column_defs <- func_char_column_defs(char_vars, rename_map)

      # Render table and hide 'id' column
      reactable(
        df,
        columns = c(
          list(
            id = colDef(show = FALSE),
            manufacturer = colDef(
              name = "Manufacturer",
              sticky = "left",
              style = sticky_style,
              headerStyle = sticky_style,
              minWidth = 200
            ),
            model = colDef(
              name = "Model",
              sticky = "left",
              style = sticky_style,
              headerStyle = sticky_style,
              minWidth = 200
            )
          ),
          bar_column_defs,
          yn_column_defs,
          numeric_column_defs,
          char_column_defs
        ),
        bordered = TRUE,
        highlight = TRUE,
        striped = FALSE,
        pagination = TRUE,
        searchable = TRUE,
        resizable = TRUE,
        fullWidth = TRUE,
        style = list(maxHeight = "1000px", overflowY = "auto")
      )
    })

    # Step 6: Download
    output$download_data <- downloadHandler(
      filename = function() paste0("sia_feature_filter_data_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) write.csv(filtered_data(), file, row.names = FALSE)
    )

    # Step 7: Download filter settings
    output$download_filter_settings <- downloadHandler(
      filename = function() paste0("sia_filter_settings_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        settings <- list()

        # Sliders (rounded to integers)
        for (var in range_vars) {
          range_vals <- as.integer(round(input[[var]]))
          settings[[var]] <- paste(range_vals[1], range_vals[2], sep = "|")
        }

        # Checkboxes
        for (var in checkbox_vars) {
          settings[[var]] <- if (isTRUE(input[[var]])) "YES" else "YES|NO"
        }

        # SelectInputs
        for (var in select_inputs) {
          settings[[var]] <- paste(input[[var]], collapse = "|")
        }

        # Date range
        settings[["release_date"]] <- paste(
          format(input$release_date[1], "%Y-%m-%d"),
          format(input$release_date[2], "%Y-%m-%d"),
          sep = "|"
        )

        # Exclude NA SiA scores
        settings[["exclude_na_sia"]] <- if (isTRUE(input$exclude_na_sia)) "YES" else "YES|NO"

        # Convert list to data frame
        df <- data.frame(t(unlist(settings)), check.names = FALSE)
        names(df) <- names(settings)

        # Define order: sia_es_long, sia_es_short, exclude_na_sia, then the rest
        ordered_vars <- c(
          "sia_es_long",
          "sia_es_short",
          "exclude_na_sia",
          setdiff(c(names(rename_map), "release_date"), c("sia_es_long", "sia_es_short"))
        )

        # Filter only existing variables
        ordered_vars <- ordered_vars[ordered_vars %in% names(df)]
        df <- df[ordered_vars]

        # Keep column names as internal variable names
        write.table(df, file, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
      }
    )

  })
}



# mod_feat_fil_server <- function(id, data) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#
#     # Step 1: Reactive data after sliders, checkboxes, and date
#     filtered_for_dropdowns <- reactive({
#       df <- data()
#       df %>%
#         filter(
#           is.na(sia_es_long) | between(sia_es_long, input$sia_es_long[1], input$sia_es_long[2]),
#           is.na(sia_es_short) | between(sia_es_short, input$sia_es_short[1], input$sia_es_short[2]),
#           is.na(device_cost) | between(device_cost, input$device_cost[1], input$device_cost[2]),
#           is.na(weight) | between(weight, input$weight[1], input$weight[2]),
#           is.na(battery_life) | between(battery_life, input$battery_life[1], input$battery_life[2]),
#           is.na(charging_duration) | between(charging_duration, input$charging_duration[1], input$charging_duration[2]),
#           is.na(dev_storage_cap_mb) | between(dev_storage_cap_mb, input$dev_storage_cap_mb[1], input$dev_storage_cap_mb[2]),
#           is.na(dev_storage_cap_hrs) | between(dev_storage_cap_hrs, input$dev_storage_cap_hrs[1], input$dev_storage_cap_hrs[2]),
#           is.na(no_studies_val_rel_reviewed) | between(no_studies_val_rel_reviewed, input$no_studies_val_rel_reviewed[1], input$no_studies_val_rel_reviewed[2]),
#           is.na(no_studies_usab_reviewed) | between(no_studies_usab_reviewed, input$no_studies_usab_reviewed[1], input$no_studies_usab_reviewed[2]),
#           is.null(input$release_date[1]) | is.null(input$release_date[2]) |
#             is.na(release_date) | between(release_date, input$release_date[1], input$release_date[2]),
#           (!input$water_resistance | water_resistance == "Yes"),
#           (!input$bio_cueing | bio_cueing == "Yes"),
#           (!input$bio_feedback | bio_feedback == "Yes"),
#           (!input$ppg | ppg == "Yes"),
#           (!input$ecg | ecg == "Yes"),
#           (!input$icg | icg == "Yes"),
#           (!input$emg | emg == "Yes"),
#           (!input$respiration | respiration == "Yes"),
#           (!input$eda | eda == "Yes"),
#           (!input$eeg | eeg == "Yes"),
#           (!input$bp | bp == "Yes"),
#           (!input$accelerometer | accelerometer == "Yes"),
#           (!input$gyroscope | gyroscope == "Yes"),
#           (!input$gps | gps == "Yes"),
#           (!input$skin_temperature | skin_temperature == "Yes"),
#           (!input$int_storage_met | int_storage_met == "Yes"),
#           (!input$server_data_storage | server_data_storage == "Yes"),
#           (!input$raw_data_available | raw_data_available == "Yes"),
#           (!input$gdpr_comp | gdpr_comp == "Yes"),
#           (!input$ce_app_label | ce_app_label == "Yes"),
#           (!input$fda_app_clear | fda_app_clear == "Yes")
#         )
#     })
#
#     # Step 2: Dynamically update selectInput choices
#     observe({
#       df <- filtered_for_dropdowns()
#
#       select_inputs <- c("manufacturer", "model", "market_status", "main_use",
#                          "wearable_type", "location", "size", "charging_method",
#                          "other_signals", "data_trans_method", "level_validation")
#
#       for (input_id in select_inputs) {
#         # Filter df based on all other selectInputs except current one
#         df_filtered <- df
#         for (other_id in setdiff(select_inputs, input_id)) {
#           selected <- input[[other_id]]
#           if (!is.null(selected)) {
#             df_filtered <- df_filtered %>% filter(.data[[other_id]] %in% selected)
#           }
#         }
#
#         # Update current input choices based on filtered data
#         valid_choices <- sort(unique(df_filtered[[input_id]]))
#         selected_now <- input[[input_id]]
#         updateSelectInput(session, input_id,
#                           choices = valid_choices,
#                           selected = selected_now[selected_now %in% valid_choices])
#       }
#     })
#
#
#     # Step 3: Apply full filtering (sliders + checkboxes + dropdowns)
#     filtered_data <- reactive({
#       df <- filtered_for_dropdowns()  # Use already partially filtered data
#
#       df %>%
#         filter(
#           is.null(input$manufacturer) | manufacturer %in% input$manufacturer,
#           is.null(input$model) | model %in% input$model,
#           is.null(input$market_status) | market_status %in% input$market_status,
#           is.null(input$main_use) | main_use %in% input$main_use,
#           is.null(input$wearable_type) | wearable_type %in% input$wearable_type,
#           is.null(input$location) | location %in% input$location,
#           is.null(input$size) | size %in% input$size,
#           is.null(input$charging_method) | charging_method %in% input$charging_method,
#           is.null(input$other_signals) | other_signals %in% input$other_signals,
#           is.null(input$data_trans_method) | data_trans_method %in% input$data_trans_method,
#           is.null(input$level_validation) | level_validation %in% input$level_validation
#         )
#     })
#
#     # Observe Reset Filter button
#     observeEvent(input$reset_filter, {
#       # Reset sliders
#       updateSliderInput(session, "sia_es_long", value = c(0, 10))
#       updateSliderInput(session, "sia_es_short", value = c(0, 10))
#       updateSliderInput(session, "device_cost", value = c(0, max(sia_df$device_cost, na.rm = TRUE)))
#       updateSliderInput(session, "weight", value = c(0, max(sia_df$weight, na.rm = TRUE)))
#       updateSliderInput(session, "battery_life", value = c(0, max(sia_df$battery_life, na.rm = TRUE)))
#       updateSliderInput(session, "charging_duration", value = c(0, 10000))
#       updateSliderInput(session, "dev_storage_cap_mb", value = c(0, max(sia_df$dev_storage_cap_mb, na.rm = TRUE)))
#       updateSliderInput(session, "dev_storage_cap_hrs", value = c(0, max(sia_df$dev_storage_cap_hrs, na.rm = TRUE)))
#       updateSliderInput(session, "no_studies_val_rel_reviewed", value = c(0, max(sia_df$no_studies_val_rel_reviewed, na.rm = TRUE)))
#       updateSliderInput(session, "no_studies_usab_reviewed", value = c(0, max(sia_df$no_studies_usab_reviewed, na.rm = TRUE)))
#
#       # Reset date
#       updateDateRangeInput(session, "release_date",
#                            start = min(sia_df$release_date, na.rm = TRUE),
#                            end = max(sia_df$release_date, na.rm = TRUE)
#       )
#
#       # Reset checkboxInputs
#       checkbox_inputs <- c("water_resistance", "bio_cueing", "bio_feedback", "ppg", "ecg", "icg",
#                            "emg", "respiration", "eda", "eeg", "bp", "accelerometer", "gyroscope",
#                            "gps", "skin_temperature", "raw_data_available", "int_storage_met",
#                            "server_data_storage", "gdpr_comp", "fda_app_clear", "ce_app_label")
#       lapply(checkbox_inputs, function(id) updateCheckboxInput(session, id, value = FALSE))
#
#       # Reset selectInputs
#       select_inputs <- c("manufacturer", "model", "market_status", "main_use",
#                          "wearable_type", "location", "size", "charging_method",
#                          "other_signals", "data_trans_method", "level_validation")
#       lapply(select_inputs, function(id) updateSelectInput(session, id, selected = character(0)))
#     })
#
#     # Step 4: Render filtered table
#     output$feat_filtered_table <- renderDT({
#
#       df <- filtered_data()
#
#       # Rename only the columns that exist in df
#       existing_cols <- names(df)
#       new_colnames <- rename_map[existing_cols]  # Get mapped names for existing cols
#       names(df) <- ifelse(!is.na(new_colnames), new_colnames, existing_cols)  # Rename safely
#
#       datatable(
#         df,
#         options = list(
#           pageLength = 40,
#           autoWidth = TRUE,
#           scrollX = TRUE,
#           columnDefs = list(
#             list(width = "150px", targets = "_all")
#           )
#         )
#       )
#     })
#
#     # Step 5: Download Handler
#     output$download_data <- downloadHandler(
#       filename = function() {
#         paste0("sia_filtered_data_", Sys.Date(), ".csv")
#       },
#       content = function(file) {
#         write.csv(filtered_data(), file, row.names = FALSE)
#       }
#     )
#   })
# }
