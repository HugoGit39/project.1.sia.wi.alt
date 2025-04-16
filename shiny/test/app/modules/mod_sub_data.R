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
          title = "Create Draft Form",
          status = "primary",
          width = 12,
          collapsible = FALSE,
          solidHeader = TRUE,
          div(
            p("Fill in at least all mandatory fields", strong("*", style = "color: #CC6677;"), "and create a draft version before sending out to us."),
            p(actionButton(ns("create_draft"), "Create", disabled = TRUE))
          ),
          textOutput(ns("status")),
          bs4Card(
            title = "General Device Information",
            status = "secondary",
            width = 12,
            collapsible = FALSE,
            textInput(ns("manufacturer"), labelMandatory("Manufacturer")),
            textInput(ns("model"), labelMandatory("Model")),
            textInput(ns("website"), labelMandatory("Website")),
            dateInput(ns("release_date"), labelMandatory("Release Date")),
            textInput(ns("market_status"), labelMandatory("Market Status")),
            uiOutput(ns("market_status_error")),
            textInput(ns("main_use"), labelMandatory("Main Use")),
            uiOutput(ns("main_use_error")),
            numericInput(ns("device_cost"), labelMandatory("Cost (€)"), value = NA),
            textInput(ns("wearable_type"), labelMandatory("Type")),
            uiOutput(ns("wearable_type_error")),
            textInput(ns("location"), labelMandatory("Location")),
            uiOutput(ns("location_error")),
            numericInput(ns("weight"), labelMandatory("Weight (g)"), value = NA),
            numericInput(ns("size"), labelMandatory("Size"), value = NA)
          ),
          bs4Card(
            title = "Technical Specifications",
            status = "secondary",
            width = 12,
            collapsible = FALSE,
            checkboxInput(ns("water_resistance"), "Water Resistant", value = FALSE),
            numericInput(ns("battery_life"), "Battery Life (min)", value = NA),
            textInput(ns("charging_method"), "Charging Method"),
            numericInput(ns("charging_duration"), "Charging Duration (min)", value = NA),
            checkboxInput(ns("bio_cueing"), "Bio Cueing", value = FALSE),
            checkboxInput(ns("bio_feedback"), "Bio Feedback", value = FALSE)
          ),
          bs4Card(
            title = "Signals",
            status = "secondary",
            width = 12,
            collapsible = FALSE,
            checkboxInput(ns("ppg"), "PPG", value = FALSE),
            checkboxInput(ns("ecg"), "ECG", value = FALSE),
            checkboxInput(ns("icg"), "ICG", value = FALSE),
            checkboxInput(ns("emg"), "EMG", value = FALSE),
            textInput(ns("respiration"), "Respiration"),
            checkboxInput(ns("eda"), "EDA", value = FALSE),
            checkboxInput(ns("eeg"), "EEG", value = FALSE),
            checkboxInput(ns("bp"), "Blood Pressure", value = FALSE),
            checkboxInput(ns("accelerometer"), "Accelerometer", value = FALSE),
            checkboxInput(ns("gyroscope"), "Gyroscope", value = FALSE),
            checkboxInput(ns("gps"), "GPS", value = FALSE),
            checkboxInput(ns("skin_temperature"), "Skin Temperature", value = FALSE),
            textInput(ns("other_signals"), "Other Signals"),
            uiOutput(ns("other_signals_error"))
          ),
          bs4Card(
            title = "Data Access",
            status = "secondary",
            width = 12,
            collapsible = FALSE,
            checkboxInput(ns("raw_data_available"), "Raw Data Available", value = FALSE),
            textInput(ns("data_trans_method"), "Data Transmission Method"),
            uiOutput(ns("data_trans_method_error")),
            numericInput(ns("int_storage_met"), "Internal Storage (MB)", value = NA),
            checkboxInput(ns("server_data_storage"), "Server Data Storage", value = FALSE),
            numericInput(ns("dev_storage_cap_hrs"), "Device Storage (hrs)", value = NA),
            numericInput(ns("dev_storage_cap_mb"), "Device Storage (MB)", value = NA),
            checkboxInput(ns("gdpr_comp"), "GDPR Compliant", value = FALSE),
            checkboxInput(ns("fda_app_clear"), "FDA Approved", value = FALSE),
            checkboxInput(ns("ce_app_label"), "CE Label", value = FALSE)
          )
        )
      ),
      column(
        width = 4,
        bs4Card(
          title = "Check Draft Form",
          status = "primary",
          width = 12,
          collapsible = FALSE,
          solidHeader = TRUE,
          p("If everything is correct, slide the button to the right to be ready to send out to us."),
          p(materialSwitch(inputId = ns("draft_ok"), status = "success")),
          bs4Card(
            title = "Draft Form Output",
            width = 12,
            status = "secondary",
            solidHeader = TRUE,
            collapsible = FALSE,
            dataTableOutput(ns("draft_table"))
          )
        )
      )
    )
  )
}

#server
mod_sub_data__server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    #create reactive data frame
    draft_data <- reactiveVal()

    # Create empty form
    form_template <- data.frame(
      Variable = names(rename_map),
      Value = rep(NA, length(rename_map))
    )

    #Fill reactive value
    draft_data(formData_template)

    # Check char fields correct
    invalid_char_fields <- reactive({
      sapply(names(char_only_fields), function(field) {
        val <- input[[field]]
        !is.null(val) && !is.na(val) && grepl("\\d", val)
      })
    })

    # Character field validation UI outputs with friendly labels
    lapply(names(char_only_fields), function(field) {
      output[[paste0(field, "_error")]] <- renderUI({
        if (invalid_char_fields()[[field]]) {
          div(style = "color:#CC6677;", paste(char_only_fields[[field]], "should not contain a number."))
        }
      })
    })

    # Correct Filled in Fileds & Mandatory fields
    observe({

      toggleState("create_draft", condition = mandatoryfields_check(fieldsMandatory_data, input) && !any(invalid_char_fields()))

    })

    #Render draft table
    output$draft_table <- renderDataTable({
      datatable(
        draft_data(),
        rownames = FALSE,
        options = list(dom = 't', ordering = FALSE)
      )
    })


    observeEvent(input$create_draft, {
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

