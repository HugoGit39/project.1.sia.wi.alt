############################################################################################
#
#  Function nodule for add data
#
#############################################################################################

# ui
mod_sub_data_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 4,
        bs4Card(
          title = "Fill In Form",
          status = "primary",
          width = 12,
          collapsible = FALSE,
          solidHeader = TRUE,
          uiOutput("submit_ui"),
          textOutput("status"),
            bs4Card(
              title = "General Device Information",
              status = "secondary",
              width = 12,
              collapsible = FALSE,
              textInput("manufacturer", labelMandatory("Manufacturer")),
              textInput("model", labelMandatory("Model")),
              textInput("website", labelMandatory("Website")),
              dateInput("release_date", labelMandatory("Release Date")),
              textInput("market_status", labelMandatory("Market Status")),
              uiOutput("market_status_error"),
              textInput("main_use", labelMandatory("Main Use")),
              uiOutput("main_use_error"),
              numericInput("device_cost", labelMandatory("Cost (€)"), value = NA),
              textInput("wearable_type", labelMandatory("Type")),
              uiOutput("wearable_type_error"),
              textInput("location", labelMandatory("Location")),
              uiOutput("location_error"),
              numericInput("weight", labelMandatory("Weight (g)"), value = NA),
              numericInput("size", labelMandatory("Size"), value = NA)
            ),
            bs4Card(
              title = "Technical Specifications",
              status = "secondary",
              width = 12,
              collapsible = FALSE,
              checkboxInput("water_resistance", "Water Resistant"),
              numericInput("battery_life", "Battery Life (min)", value = NA),
              textInput("charging_method", "Charging Method"),
              numericInput("charging_duration", "Charging Duration (min)", value = NA),
              checkboxInput("bio_cueing", "Bio Cueing"),
              checkboxInput("bio_feedback", "Bio Feedback")
            ),
            bs4Card(
              title = "Signals",
              status = "secondary",
              width = 12,
              collapsible = FALSE,
              checkboxInput("ppg", "PPG"),
              checkboxInput("ecg", "ECG"),
              checkboxInput("icg", "ICG"),
              checkboxInput("emg", "EMG"),
              textInput("respiration", "Respiration"),
              checkboxInput("eda", "EDA"),
              checkboxInput("eeg", "EEG"),
              checkboxInput("bp", "Blood Pressure"),
              checkboxInput("accelerometer", "Accelerometer"),
              checkboxInput("gyroscope", "Gyroscope"),
              checkboxInput("gps", "GPS"),
              checkboxInput("skin_temperature", "Skin Temperature"),
              textInput("other_signals", "Other Signals"),
              uiOutput("other_signals_error")
            ),
            bs4Card(
              title = "Data Access",
              status = "secondary",
              width = 12,
              collapsible = FALSE,
              checkboxInput("raw_data_available", "Raw Data Available"),
              textInput("data_trans_method", "Data Transmission Method"),
              uiOutput("data_trans_method_error"),
              numericInput("int_storage_met", "Internal Storage (MB)", value = NA),
              checkboxInput("server_data_storage", "Server Data Storage"),
              numericInput("dev_storage_cap_hrs", "Device Storage (hrs)", value = NA),
              numericInput("dev_storage_cap_mb", "Device Storage (MB)", value = NA),
              checkboxInput("gdpr_comp", "GDPR Compliant"),
              checkboxInput("fda_app_clear", "FDA Approved"),
              checkboxInput("ce_app_label", "CE Label")
            )
        )
      )
    )
  )
}

#server
mod_sub_data__server <- function(id) {
  moduleServer(id, function(input, output, session) {

    invalid_char_fields <- reactive({
      sapply(names(char_only_fields), function(field) {
        val <- input[[field]]
        !is.null(val) && !is.na(val) && grepl("\\d", val)
      })
    })

    observe({
      isFilled <- mandatoryfields_check(fieldsMandatory, input)
      charValid <- !any(invalid_char_fields())
      output$submit_ui <- renderUI({
        if (isFilled && charValid) actionButton("submit", "Submit") else NULL
      })
    })

    # Character field validation UI outputs with friendly labels
    lapply(names(char_only_fields), function(field) {
      output[[paste0(field, "_error")]] <- renderUI({
        if (invalid_char_fields()[[field]]) {
          div(style = "color:red;", paste(char_only_fields[[field]], "should not be a number."))
        }
      })
    })

    observeEvent(input$submit, {
      formData <- data.frame(
        manufacturer = input$manufacturer,
        model = input$model,
        website = input$website,
        release_date = as.character(input$release_date),
        market_status = input$market_status,
        main_use = input$main_use,
        device_cost = input$device_cost,
        wearable_type = input$wearable_type,
        location = input$location,
        weight = input$weight,
        size = input$size,
        water_resistance = ifelse(input$water_resistance, "Yes", ""),
        battery_life = input$battery_life,
        charging_method = input$charging_method,
        charging_duration = input$charging_duration,
        bio_cueing = ifelse(input$bio_cueing, "Yes", ""),
        bio_feedback = ifelse(input$bio_feedback, "Yes", ""),
        ppg = ifelse(input$ppg, "Yes", ""),
        ecg = ifelse(input$ecg, "Yes", ""),
        icg = ifelse(input$icg, "Yes", ""),
        emg = ifelse(input$emg, "Yes", ""),
        respiration = input$respiration,
        eda = ifelse(input$eda, "Yes", ""),
        eeg = ifelse(input$eeg, "Yes", ""),
        bp = ifelse(input$bp, "Yes", ""),
        accelerometer = ifelse(input$accelerometer, "Yes", ""),
        gyroscope = ifelse(input$gyroscope, "Yes", ""),
        gps = ifelse(input$gps, "Yes", ""),
        skin_temperature = ifelse(input$skin_temperature, "Yes", ""),
        other_signals = input$other_signals,
        raw_data_available = ifelse(input$raw_data_available, "Yes", ""),
        data_trans_method = input$data_trans_method,
        int_storage_met = input$int_storage_met,
        server_data_storage = ifelse(input$server_data_storage, "Yes", ""),
        dev_storage_cap_hrs = input$dev_storage_cap_hrs,
        dev_storage_cap_mb = input$dev_storage_cap_mb,
        gdpr_comp = ifelse(input$gdpr_comp, "Yes", ""),
        fda_app_clear = ifelse(input$fda_app_clear, "Yes", ""),
        ce_app_label = ifelse(input$ce_app_label, "Yes", ""),
        stringsAsFactors = FALSE
      )

      tmpfile <- tempfile(fileext = ".csv")
      write.csv(formData, tmpfile, row.names = FALSE)

      send_email_with_attachment(
        name = input$manufacturer,
        email = "disc@stress-in-action.nl",
        institution = input$model,
        message = paste("Form submission for", input$model),
        csv_path = tmpfile
      )

      output$status <- renderText("Form submitted! Thank you.")
    })

  }
  )
}

