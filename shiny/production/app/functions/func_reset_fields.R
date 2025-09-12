############################################################################################
#
#  Function reset fields
#
#############################################################################################

# Generic reset that uses rename_subm + each input's value type
reset_inputs_sub_data <- function(session, input) {
  for (id in rename_subm) {
    val <- isolate(input[[id]])
    if (is.null(val)) next

    if (is.logical(val)) {
      updateCheckboxInput(session, id, value = FALSE)
    } else if (inherits(val, "Date")) {
      # use NULL to clear dates safely
      updateDateInput(session, id, value = NULL)
    } else if (is.numeric(val)) {
      updateNumericInput(session, id, value = NA)
    } else if (is.character(val)) {
      if (id %in% c("additional_information")) {
        updateTextAreaInput(session, id, value = "")
      } else {
        updateTextInput(session, id, value = "")
      }
    }
  }

  # reset the YES/NO switch
  updateSwitchInput(session, "draft_ok", value = FALSE)
  disable("draft_ok")
}



# Generic reset for the Contact form
reset_inputs_contact <- function(session) {
  updateTextInput(session, "name", value = "")
  updateTextInput(session, "email", value = "")
  updateTextInput(session, "telephone", value = "")
  updateTextInput(session, "institution", value = "")
  updateTextAreaInput(session, "message", value = "")

  # Make sure the button is off again until fields are valid
  disable("submit_email")
}

