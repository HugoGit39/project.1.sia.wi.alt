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
          title = "1. Create Draft Form",
          status = "primary",
          width = 12,
          collapsible = FALSE,
          solidHeader = TRUE,
          div(
            p("Make sure to complete at least the mandatory fields ", strong("*", style = "color: #CC6677;"), " to create a draft before submitting.", style = "text-align: justify;"),
            p(actionButton(ns("create_draft"), "Create", disabled = TRUE))
          ),
          textOutput(ns("status")),
          bs4Card(
            title = "Your Information",
            status = "secondary",
            width = 12,
            collapsible = FALSE,
            textInput(ns("name"), labelMandatory("Name")),
            textInput(ns("email"), labelMandatory("Email")),
            textInput(ns("telephone"), "Telephone"),
            textInput(ns("institution"), "Institution")
            ),
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
          ),
          bs4Card(title = "Validation, Reliability & Usability",
                  width = 12,
                  status = "secondary",
                  collapsible = FALSE,
                  textInput(ns("level_validation"), "Validation Level"),
                  numericInput(ns("no_studies_val_rel_reviewed"), "Validation Studies", value = NA),
                  numericInput(ns("no_studies_usab_reviewed"), "Usability Studies", value = NA)
          ),
          bs4Card(
            title = "Further Details",
            status = "secondary",
            width = 12,
            collapsible = FALSE,
            textAreaInput(ns("additional_information"), "Additional Information", rows = 4)
          )
        )
      ),
      column(
        width = 4,
        bs4Card(
          title = "2. Check Draft Form",
          status = "primary",
          width = 12,
          collapsible = FALSE,
          solidHeader = TRUE,
          p("Please verify that prvided fields are correct, then slide the toggle to enable submission.", style = "text-align: justify;"),
          p(materialSwitch(inputId = ns("draft_ok"), label = "Slide to the right when ready!", right = TRUE, status = "success")),
          bs4Card(
            title = "Draft Form Output",
            width = 12,
            status = "secondary",
            solidHeader = TRUE,
            collapsible = FALSE,
            dataTableOutput(ns("draft_table"))  %>% withSpinner()
          )
        )
      ),
      column(
        width = 4,
        bs4Card(
          title = "3. Send Final Form",
          status = "primary",
          width = 12,
          collapsible = FALSE,
          solidHeader = TRUE,
            p("When you approve your draft, the option to send it to us will become available.", style = "text-align: justify;"),
            p(actionButton(ns("submit_final"), "Submit", disabled = TRUE)),
            p("A copy of your submission will be sent to the email address you provided. We will reach out to you to discuss it in more detail.", style = "text-align: justify;")
        ),
        div(
          style = "text-align: center;",
          tags$img(
            src = "iStock_submit_data.jpg",
            style = "width: 75%; height: auto; margin-top: 100px; border-radius: 5px;"
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

    create_clicked <- reactiveVal(FALSE)

    disable("draft_ok")

    #create reactive data frame
    draft_data <- reactiveVal()

    # Create empty form
    form_template <- data.frame(
      Variable = rename_subm,
      Value = rep(NA, length(rename_subm))
    )

    #Fill reactive value
    draft_data(form_template)

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

    # Correct Filled in fields & Mandatory fields
    observe({

      toggleState("create_draft", condition = mandatoryfields_check(fieldsMandatory_data, input) && !any(invalid_char_fields()))

    })

    #Render draft table
    output$draft_table <- renderDataTable({
      datatable(
        draft_data(),
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          processing = FALSE,
          pageLength = nrow(draft_data()),
          dom = 't'
        )
      )
    })

    observeEvent(input$create_draft, {
      create_clicked(TRUE)  # mark that draft was created

      updated_form <- draft_data()
      for (i in seq_len(nrow(updated_form))) {
        varname <- updated_form$Variable[i]
        val <- input[[varname]]

        if (!is.null(val) && !is.na(val) && !(is.character(val) && val == "")) {
          # Convert logicals to "Yes"/"No"
          if (is.logical(val)) {
            updated_form$Value[i] <- ifelse(val, "Yes", "No")
          }
          # Format dates as string
          else if (inherits(val, "Date")) {
            updated_form$Value[i] <- format(val, "%Y-%m-%d")
          }
          # Otherwise just store value
          else {
            updated_form$Value[i] <- val
          }
        }
      }

      draft_data(updated_form)
    })



    observe({
      valid_form <- mandatoryfields_check(fieldsMandatory_data, input) && !any(invalid_char_fields())

      # Enable or disable toggle based on validity + if Create was clicked
      toggleState("draft_ok", condition = create_clicked() && valid_form)

      # If form becomes invalid, also reset the toggle switch to OFF
      if (!valid_form) {
        updateMaterialSwitch(session = session, inputId = "draft_ok", value = FALSE)
      }
    })

    #check if toggle is switched
    observe({
      toggleState("submit_final", condition = isTRUE(input$draft_ok))
    })

    observeEvent(input$submit_final, {

      # Save to a temporary CSV
      tmp_file <- tempfile(fileext = ".csv")
      write.csv(draft_data(), tmp_file, row.names = FALSE)

      # Create email body
      body <- paste("Name: ", input$name,
                    "\nEmail: ", input$email,
                    "\nTelephone: ", input$telephone,
                    "\n\nInstitution: ", input$institution)

      subject <- "Wearable Shiny App new data submission"

      receiver_email <- input$email

      send_email(body, subject, receiver_email, tmp_file)

      showModal(
        modalDialog(
          title = "Data Submitted",
          "Thank you for your data submission! We will get back to you soon."
        )
      )
    })



  }
  )
}

