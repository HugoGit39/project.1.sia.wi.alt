
############################################################################################
#
#  Module: Submit Data (live draft; no create button; switch enables on validity)
#
############################################################################################

# UI
mod_sub_data_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 4,
        bs4Card(
          title = "1. Draft Form",
          status = "primary",
          width = 12,
          collapsible = FALSE,
          solidHeader = TRUE,
          div(
            p(
              "The draft updates live as you type. Complete the mandatory fields ",
              strong("*", style = "color: #CC6677;"),
              " to enable submission.",
              style = "text-align: justify;"
            )
          ),
          textOutput(ns("status")),
          bs4Card(
            title = "Your Information",
            status = "secondary",
            width = 12,
            collapsible = FALSE,
            textInput(ns("name"), labelMandatory("Name")),
            textInput(ns("email"), labelMandatory("Email")),
            uiOutput(ns("email_error")),
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
            dateInput(ns("release_date"), "Release Date"),
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
            checkboxInput(ns("respiration"), "Respiration", value = FALSE),
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
          bs4Card(
            title = "Validation, Reliability & Usability",
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
          p(
            "Please verify that provided fields are correct, then slide the toggle to enable submission.",
            style = "text-align: justify;"
          ),
          p(
            switchInput(
              inputId   = ns("draft_ok"),
              onLabel   = "YES",
              offLabel  = "NO",
              value     = FALSE,
              size      = "sm",
              onStatus  = "secondary",
              offStatus = "primary"
            )
          ),
          p(strong("All required fields complete?")
          ),
          bs4Card(
            title = "Draft Form Output",
            width = 12,
            status = "secondary",
            solidHeader = TRUE,
            collapsible = FALSE,
            reactableOutput(ns("draft_table")) %>% withSpinner()
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
          p(
            "When you approve your draft, the option to send it to us will become available.",
            style = "text-align: justify;"
          ),
          p(actionButton(ns("submit_final"), "Submit", disabled = TRUE)),
          p(
            "A copy of your submission will be sent to the email address you provided. We will reach out to you to discuss it in more detail.",
            style = "text-align: justify;"
          )
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

# Server
mod_sub_data__server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Start with switch disabled
    disable("draft_ok")
    updateSwitchInput(session, "draft_ok", value = FALSE)

    # Live builder for the draft table from inputs
    build_form <- reactive({
      data.frame(
        Variable = rename_subm,
        Value = vapply(rename_subm, function(varname) {
          val <- input[[varname]]

          # Checkboxes: show Yes/No (default No)
          if (varname %in% yn_vars) {
            return(if (isTRUE(val)) "Yes" else "No")
          }

          # Dates -> YYYY-MM-DD
          if (inherits(val, "Date")) {
            return(if (!is.null(val) && !is.na(val)) format(val, "%Y-%m-%d") else NA_character_)
          }

          # Empty -> NA; else as character
          if (is.null(val) || (is.character(val) && val == "")) return(NA_character_)
          as.character(val)
        }, character(1)),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })

    #email check @ value
    output$email_error <- renderUI({
      val <- input$email
      if (!is.null(val) && nzchar(val) && !grepl("@", val)) {
        div(style = "color:#CC6677;",
            "Email must contain '@' (e.g., name@example.com).")
      }
    })

    # Character-only field validation (no digits allowed)
    invalid_char_fields <- reactive({
      sapply(names(char_only_fields), function(field) {
        val <- input[[field]]
        !is.null(val) && !is.na(val) && grepl("\\d", val)
      })
    })

    # Inline validation messages
    lapply(names(char_only_fields), function(field) {
      output[[paste0(field, "_error")]] <- renderUI({
        if (invalid_char_fields()[[field]]) {
          div(style = "color:#CC6677;", paste(char_only_fields[[field]], "should not contain a number."))
        }
      })
    })

    # Check invalid char fields
    invalid_char_fields <- reactive({
      sapply(char_only_targets, function(field) {
        val <- input[[field]]
        is.character(val) && nzchar(val) && grepl("\\d", val)
      })
    })

    # Enable/disable the YES/NO switch based on mandatory fields + char checks
    observe({
      valid_form <- mandatoryfields_check(fieldsMandatory_data, input) && !any(invalid_char_fields())

      # enable only when valid; otherwise disable
      toggleState("draft_ok", condition = valid_form)

      # if it becomes invalid, also force the switch back to NO
      if (!valid_form && isTRUE(input$draft_ok)) {
        updateSwitchInput(session, "draft_ok", value = FALSE)
      }
    })

    # Submit button only enabled when switch is YES
    observe({
      toggleState("submit_final", condition = isTRUE(input$draft_ok))
    })

    # Render the draft table
    output$draft_table <- renderReactable({
      df <- build_form()

      reactable(
        df,
        rownames = FALSE,
        columns = list(
          Variable = colDef(
            name = "Variables",
            sticky = "left",
            minWidth = 220,
            cell = function(value) {
              div(
                style = list(display = "inline-flex", alignItems = "center", whiteSpace = "nowrap"),
                if (value %in% fieldsMandatory_data) labelMandatory(value) else value
              )
            }
          ),
          Value = colDef(
            name = "Value",
            minWidth = 380,
            cell = function(value, index) {
              # Which field (row) is this?
              var <- df$Variable[index]
              raw <- input[[var]]  # raw, live input for that field

              # 1) Email: show error message instead of faulty text
              if (identical(var, "email") && is.character(raw) && nzchar(raw) && !grepl("@", raw)) {
                return(div(style = list(color = "#CC6677", fontStyle = "italic"),
                           "Invalid email"))
              }

              # 2) Char-only fields: show error message instead of faulty text
              if (var %in% names(char_only_fields) &&
                  is.character(raw) && nzchar(raw) && grepl("\\d", raw)) {
                return(div(style = list(color = "#CC6677", fontStyle = "italic"),
                           "Invalid character field"))
              }

              # 3) Normal display
              if (is.na(value) || (is.character(value) && nzchar(value) == FALSE)) return("—")
              div(style = list(whiteSpace = "pre-wrap"), value)
            }
          )
        ),
        bordered   = TRUE,
        highlight  = TRUE,
        striped    = FALSE,
        pagination = FALSE,
        resizable  = TRUE,
        fullWidth  = TRUE,
        defaultColDef = colDef(align = "left")
      )
    })


    # Final submit (no email code here)
    observeEvent(input$submit_final, {
      req(isTRUE(input$draft_ok))  # only when the YES/NO switch is ON

      # Build CSV named with the email
      csv_path <- file.path(
        tempdir(),
        paste0("sia_submission_from_", input$email, ".csv")
      )
      write.csv(build_form(), csv_path, row.names = FALSE)

      # Subject + body
      subject <- sprintf("SiA Wearables submission: %s", input$email)
      body <- paste0(
        "New submission received.\n\n",
        "Name: ",        input$name,        "\n",
        "Email: ",       input$email,       "\n",
        "Telephone: ",   input$telephone,   "\n",
        "Institution: ", input$institution, "\n"
      )

      # Send (your send_email already targets your inbox by default)
      send_email(body = body, subject = subject, attachment = csv_path)

      showModal(modalDialog(
        title = "Data Submitted",
        "Thank you for your data submission! We will get back to you soon."
      ))

      reset_inputs_sub_data(session, input)

    })
  })
}

