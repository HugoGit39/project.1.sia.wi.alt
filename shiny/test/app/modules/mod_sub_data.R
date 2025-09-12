
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

          # --- Your Information ---
          bs4Card(
            title = "Your Information",
            status = "secondary",
            width = 12,
            collapsible = FALSE,
            textInput(ns("name"), labelMandatory("Name")),
            uiOutput(ns("name_error")),                     # digits/CSV blocked (your existing handler)
            textInput(ns("email"), labelMandatory("Email")),
            uiOutput(ns("email_error")),
            textInput(ns("telephone"), "Telephone"),
            uiOutput(ns("telephone_csv_error")),            # CSV-only
            textInput(ns("institution"), "Institution"),
            uiOutput(ns("institution_csv_error"))           # CSV-only
          ),

          # --- General Device Information ---
          bs4Card(
            title = "General Device Information",
            status = "secondary",
            width = 12,
            collapsible = FALSE,
            textInput(ns("manufacturer"), labelMandatory("Manufacturer")),
            uiOutput(ns("manufacturer_csv_error")),         # CSV-only
            textInput(ns("model"), labelMandatory("Model")),
            uiOutput(ns("model_csv_error")),                # CSV-only
            textInput(ns("website"), labelMandatory("Website")),
            uiOutput(ns("website_csv_error")),              # CSV-only
            dateInput(ns("release_date"), "Release Date"),
            textInput(ns("market_status"), labelMandatory("Market Status")),
            uiOutput(ns("market_status_error")),            # digits/CSV blocked (your existing handler)
            textInput(ns("main_use"), labelMandatory("Main Use")),
            uiOutput(ns("main_use_error")),                 # digits/CSV blocked
            numericInput(ns("device_cost"), labelMandatory("Cost (€)"), value = NA),
            textInput(ns("wearable_type"), labelMandatory("Type")),
            uiOutput(ns("wearable_type_error")),            # digits/CSV blocked
            textInput(ns("location"), labelMandatory("Location")),
            uiOutput(ns("location_error")),                 # digits/CSV blocked
            numericInput(ns("weight"), labelMandatory("Weight (g)"), value = NA),
            numericInput(ns("size"), labelMandatory("Size"), value = NA)
          ),

          # --- Technical Specifications ---
          bs4Card(
            title = "Technical Specifications",
            status = "secondary",
            width = 12,
            collapsible = FALSE,
            checkboxInput(ns("water_resistance"), "Water Resistant", value = FALSE),
            numericInput(ns("battery_life"), "Battery Life (min)", value = NA),
            textInput(ns("charging_method"), "Charging Method"),
            uiOutput(ns("charging_method_csv_error")),      # CSV-only
            numericInput(ns("charging_duration"), "Charging Duration (min)", value = NA),
            checkboxInput(ns("bio_cueing"), "Bio Cueing", value = FALSE),
            checkboxInput(ns("bio_feedback"), "Bio Feedback", value = FALSE)
          ),

          # --- Signals ---
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
            uiOutput(ns("other_signals_error"))             # digits/CSV blocked
          ),

          # --- Data Access ---
          bs4Card(
            title = "Data Access",
            status = "secondary",
            width = 12,
            collapsible = FALSE,
            checkboxInput(ns("raw_data_available"), "Raw Data Available", value = FALSE),
            textInput(ns("data_trans_method"), "Data Transmission Method"),
            uiOutput(ns("data_trans_method_error")),        # digits/CSV blocked
            numericInput(ns("int_storage_met"), "Internal Storage (MB)", value = NA),
            checkboxInput(ns("server_data_storage"), "Server Data Storage", value = FALSE),
            numericInput(ns("dev_storage_cap_hrs"), "Device Storage (hrs)", value = NA),
            numericInput(ns("dev_storage_cap_mb"), "Device Storage (MB)", value = NA),
            checkboxInput(ns("gdpr_comp"), "GDPR Compliant", value = FALSE),
            checkboxInput(ns("fda_app_clear"), "FDA Approved", value = FALSE),
            checkboxInput(ns("ce_app_label"), "CE Label", value = FALSE)
          ),

          # --- Validation, Reliability & Usability ---
          bs4Card(
            title = "Validation, Reliability & Usability",
            width = 12,
            status = "secondary",
            collapsible = FALSE,
            textInput(ns("level_validation"), "Validation Level"),
            uiOutput(ns("level_validation_csv_error")),     # CSV-only
            numericInput(ns("no_studies_val_rel_reviewed"), "Validation Studies", value = NA),
            numericInput(ns("no_studies_usab_reviewed"), "Usability Studies", value = NA)
          ),

          # --- Further Details ---
          bs4Card(
            title = "Further Details",
            status = "secondary",
            width = 12,
            collapsible = FALSE,
            textAreaInput(ns("additional_information"), "Additional Information", rows = 4),
            uiOutput(ns("additional_information_csv_error")) # CSV-only
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
          downloadLink(ns("dl_csv_submit"), "", style = "display:none;"),
          p(
            "A copy of will be downloaded automatically when submitting. We will reach out to you to discuss it in more detail.",
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
        div(
      style = "color:#CC6677; font-size:12px;",
      strong("Email must contain '@' (e.g., name@example.com).")
    )
      }
    })

    # --- Local sets limited to inputs present in this form (avoid OOB) ---
    char_no_digit_ids_submit <- intersect(char_no_digit_ids, rename_subm)
    csv_only_ids_submit      <- intersect(csv_only_ids,      rename_subm)

    # 1) invalid: digits or CSV delimiters for "no-digit" fields
    invalid_no_digit <- reactive({
      pat_csv <- csv_delims_pattern          # from global: "[,;\"\\r\\n]"
      sapply(char_no_digit_ids_submit, function(field) {
        v <- input[[field]]
        is.character(v) && nzchar(v) && (grepl("\\d", v) || grepl(pat_csv, v))
      })
    })

    # 2) invalid: CSV delimiters for all other character fields
    invalid_csv_only <- reactive({
      pat_csv <- csv_delims_pattern
      sapply(csv_only_ids_submit, function(field) {
        v <- input[[field]]
        is.character(v) && nzchar(v) && grepl(pat_csv, v)
      })
    })

    # 3) per-field UI messages (only where you have uiOutput(...) in the UI)
    lapply(char_no_digit_ids_submit, function(field) {
      output[[paste0(field, "_error")]] <- renderUI({
        if (isTRUE(invalid_no_digit()[[field]])) {
          div(style = "color:#CC6677; font-size:12px;",
              strong(paste0(char_no_digit_fields[[field]],
                            " should not contain numbers or CSV delimiters ",
                            "(, ;). Please use pipes (|).")))
        }
      })
    })

    # If you also added uiOutput(...) slots for some CSV-only fields,
    # same shape as your no-digit lapply
    lapply(csv_only_ids_submit, function(field) {
      output[[paste0(field, "_csv_error")]] <- renderUI({
        if (isTRUE(invalid_csv_only()[[field]])) {
          # pretty label if you have rename_map; fallback to id with spaces
          lbl <- if (!is.null(rename_map[[field]])) rename_map[[field]] else gsub("_", " ", field)
          div(
            style = "color:#CC6677; font-size:12px;",
            strong(paste0(
              lbl,
              " should not contain CSV delimiters (, ;). Please use pipes (|)."
            ))
          )
        }
      })
    })


    # 4) Gate the YES/NO switch
    observe({
      # any CSV delimiter anywhere in ANY character field blocks
      any_csv_bad <- any(invalid_csv_only()) || any(invalid_no_digit())
      # digits rule applies only to mandatory no-digit fields
      bad_no_digit_mand <- any(invalid_no_digit()[ intersect(char_no_digit_ids_submit, fieldsMandatory_data) ])

      valid_form <- mandatoryfields_check(fieldsMandatory_data, input) &&
        !any_csv_bad && !bad_no_digit_mand

      toggleState("draft_ok", condition = valid_form)
      if (!valid_form && isTRUE(input$draft_ok)) {
        updateSwitchInput(session, "draft_ok", value = FALSE)
      }
    })

    # # 1) Any char-only field invalid? (digits OR CSV delimiters)
    # invalid_char_fields <- reactive({
    #   bad <- "(\\d|[,;\"\\r\\n])"  # digits or CSV delimiters
    #   sapply(names(char_only_fields), function(field) {
    #     v <- input[[field]]
    #     is.character(v) && nzchar(v) && grepl(bad, v)
    #   })
    # })
    #
    # # 2) Inline error messages (now mention CSV delimiters)
    # lapply(names(char_only_fields), function(field) {
    #   output[[paste0(field, "_error")]] <- renderUI({
    #     if (invalid_char_fields()[[field]]) {
    #       div(style = "color:#CC6677; font-size:12px;",
    #           strong(paste0(char_only_fields[[field]],
    #                         " should not contain numbers or CSV delimiters ",
    #                         "(comma, semicolon). Please use pipes (|)")))
    #     }
    #   })
    # })
    #
    # # 3) Only gate on MANDATORY char-only fields
    # invalid_man_char_fields <- reactive({
    #   bad <- "(\\d|[,;\"\\r\\n])"
    #   sapply(char_only_mand_fields, function(field) {
    #     v <- input[[field]]
    #     is.character(v) && nzchar(v) && grepl(bad, v)
    #   })
    # })

    # # ----  B) CSV-ONLY for other character fields  ----
    # csv_only_labels <- c(
    #   name="Name", manufacturer="Manufacturer", model="Model", website="Website",
    #   institution="Institution", charging_method="Charging Method",
    #   level_validation="Validation Level", additional_information="Additional Information",
    #   telephone="Telephone"
    # )
    #
    # invalid_csv_only_fields <- reactive({
    #   bad_csv <- "[,;\"\\r\\n]"     # CSV delimiters only
    #   sapply(char_csv_only_fields, function(field) {
    #     v <- input[[field]]
    #     is.character(v) && nzchar(v) && grepl(bad_csv, v)
    #   })
    # })
    #
    # # show messages for CSV-only fields (to the *_csv_error slots you added in UI)
    # lapply(char_csv_only_fields, function(field) {
    #   output[[paste0(field, "_csv_error")]] <- renderUI({
    #     if (invalid_csv_only_fields()[[field]]) {
    #       lbl <- csv_only_labels[[field]] %||% field
    #       div(style = "color:#CC6677; font-size:12px;",
    #           strong(paste0(lbl, " should not contain CSV delimiters ",
    #                         "(comma, semicolon). Please use pipes (|)")))
    #     }
    #   })
    # })

    # Enable/disable the YES/NO switch based on mandatory fields + char checks
    # observe({
    #   valid_form <- mandatoryfields_check(fieldsMandatory_data, input) && !any(invalid_man_char_fields())
    #
    #   # enable only when valid; otherwise disable
    #   toggleState("draft_ok", condition = valid_form)
    #
    #   # if it becomes invalid, also force the switch back to NO
    #   if (!valid_form && isTRUE(input$draft_ok)) {
    #     updateSwitchInput(session, "draft_ok", value = FALSE)
    #   }
    # })

    # snapshot of last submitted form
    last_submission <- reactiveVal(NULL)

    output$dl_csv_submit <- downloadHandler(
      filename    = function() sprintf("sia_submission_from_%s.csv", input$email),
      content     = function(file) write.csv(req(last_submission()), file, row.names = FALSE),
      contentType = "text/csv"
    )
    outputOptions(output, "dl_csv_submit", suspendWhenHidden = FALSE)

    # Submit button only enabled when switch is YES
    observe({
      toggleState("submit_final", condition = isTRUE(input$draft_ok))
    })

    # # Render the draft table
    # output$draft_table <- renderReactable({
    #   df <- build_form()
    #
    #   reactable(
    #     df,
    #     rownames = FALSE,
    #     columns = list(
    #       Variable = colDef(
    #         name = "Variables",
    #         sticky = "left",
    #         minWidth = 220,
    #         cell = function(value) {
    #           div(
    #             style = list(display = "inline-flex", alignItems = "center", whiteSpace = "nowrap"),
    #             if (value %in% fieldsMandatory_data) labelMandatory(value) else value
    #           )
    #         }
    #       ),
    #       Value = colDef(
    #         name = "Value",
    #         minWidth = 380,
    #         cell = function(value, index) {
    #           # Which field (row) is this?
    #           var <- df$Variable[index]
    #           raw <- input[[var]]  # raw, live input for that field
    #
    #           # 1) Email: show error message instead of faulty text
    #           if (identical(var, "email") && is.character(raw) && nzchar(raw) && !grepl("@", raw)) {
    #             return(div(style = list(color = "#CC6677"),
    #                        strong("Invalid email")))
    #           }
    #
    #           # 2) Char-only fields: show error message instead of faulty text
    #           if (var %in% names(char_only_fields) &&
    #               is.character(raw) && nzchar(raw) && grepl("\\d", raw)) {
    #             return(div(style = list(color = "#CC6677"),
    #                        strong("Invalid character field")))
    #           }
    #
    #           # 3) Normal display
    #           if (is.na(value) || (is.character(value) && nzchar(value) == FALSE)) return("—")
    #           div(style = list(whiteSpace = "pre-wrap"), value)
    #         }
    #       )
    #     ),
    #     bordered   = TRUE,
    #     highlight  = TRUE,
    #     striped    = FALSE,
    #     pagination = FALSE,
    #     resizable  = TRUE,
    #     fullWidth  = TRUE,
    #     defaultColDef = colDef(align = "left")
    #   )
    # })

    # 5) (Optional) also reflect invalids in the Draft table cells
    output$draft_table <- renderReactable({
      df <- build_form()
      pat_csv <- csv_delims_pattern

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
                style = list(display="inline-flex", alignItems="center", whiteSpace="nowrap"),
                if (value %in% fieldsMandatory_data) labelMandatory(value) else value
              )
            }
          ),
          Value = colDef(
            name = "Value",
            minWidth = 380,
            cell = function(value, index) {
              var <- df$Variable[index]
              raw <- input[[var]]

              # email inline message
              if (identical(var, "email") && is.character(raw) && nzchar(raw) && !grepl("@", raw)) {
                return(div(style = list(color = "#CC6677"), strong("Invalid email")))
              }

              # no-digit fields: digits or CSV delimiters
              if (var %in% char_no_digit_ids_submit &&
                  is.character(raw) && nzchar(raw) &&
                  (grepl("\\d", raw) || grepl(pat_csv, raw))) {
                return(div(style = list(color = "#CC6677"),
                           strong("Invalid characters")))
              }

              # csv-only fields: CSV delimiters
              if (var %in% csv_only_ids_submit &&
                  is.character(raw) && nzchar(raw) && grepl(pat_csv, raw)) {
                return(div(style = list(color = "#CC6677"),
                           strong("Contains CSV delimiters")))
              }

              # normal display
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


    observeEvent(input$submit_final, {

      # 1) snapshot the form once
      df <- build_form()
      last_submission(df)

      # 2) build + send email attachment from the snapshot
      csv_path <- file.path(tempdir(), paste0("sia_submission_from_", input$email, ".csv"))
      write.csv(df, csv_path, row.names = FALSE)

      subject <- sprintf("SiA Wearables submission: %s", input$email)
      body <- paste0(
        "New submission received.\n\n",
        "Name: ",        input$name,        "\n",
        "Email: ",       input$email,       "\n",
        "Telephone: ",   input$telephone,   "\n",
        "Institution: ", input$institution, "\n"
      )
      send_email(body = body, subject = subject, attachment = csv_path)

      # 3) trigger the hidden download (now backed by last_submission)
      session$onFlushed(function() {
        runjs(sprintf("document.getElementById('%s').click();", ns("dl_csv_submit")))
      }, once = TRUE)

      session$sendCustomMessage("dataSubmitted",
                                "Thank you for your data submission! We will get back to you soon.")

      # 4) finally reset inputs
      reset_inputs_sub_data(session, input)
    })

  })
}

