############################################################################################
#
#  Function Feature (extensive) filter
#
#############################################################################################

#update selectInput
update_select <- function(input_id, column) {
  valid_choices <- sort(unique(df[[column]]))
  selected <- input[[input_id]]
  updateSelectInput(session, input_id,
                    choices = valid_choices,
                    selected = selected[selected %in% valid_choices])
}
