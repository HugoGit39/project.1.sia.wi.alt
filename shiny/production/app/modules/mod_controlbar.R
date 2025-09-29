############################################################################################
#
#  Function nodule  for controlbar
#
#############################################################################################

# ui
mod_control_ui <- function(id) {
  ns <- NS(id)

    dashboardControlbar(
      skin = "light",
      pinned = FALSE,
      collapsed = TRUE,
      overlay = TRUE,
      controlbarMenu(
        id = "controlbarmenu",
        type = "pills",
        controlbarItem(
          title = "Wearables",
          reactableOutput(ns("wearables_table")) %>% withSpinner()
        ),
        controlbarItem(
          title = "Glossery",
          accordion(
            id = "accordion_glossary",
            accordionI(div("A", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$A),
            accordionI(div("B", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$B),
            accordionI(div("C", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$C),
            accordionI(div("D", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$D),
            accordionI(div("E", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$E),
            accordionI(div("F", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$F),
            accordionI(div("G", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$G),
            accordionI(div("H", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$H),
            accordionI(div("I", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$I),
            accordionI(div("J", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$J),
            accordionI(div("K", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$K),
            accordionI(div("L", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$L),
            accordionI(div("M", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$M),
            accordionI(div("N", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$N),
            accordionI(div("O", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$O),
            accordionI(div("P", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$P),
            accordionI(div("Q", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$Q),
            accordionI(div("R", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$R),
            accordionI(div("S", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$S),
            accordionI(div("T", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$T),
            accordionI(div("U", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$U),
            accordionI(div("V", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$V),
            accordionI(div("W", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$W),
            accordionI(div("X", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$X),
            accordionI(div("Y", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$Y),
            accordionI(div("Z", style='color:#1c75bc; font-size:14px;'), "white", TRUE,  GLOS$Z)
          )
        )
      )
    )
}

#server
mod_control__server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    output$wearables_table <- renderReactable({
      df <- data() %>%
        arrange(manufacturer)

      reactable(
        data.frame(
          manufacturer = df$manufacturer,
          model = df$model,
          stringsAsFactors = FALSE),
        columns = list(manufacturer = colDef(name = "Manufacturer", width = 200),
                       model = colDef(name = "Model")),
        searchable = TRUE,
        sortable = TRUE,
        defaultPageSize = 10,
        pagination = TRUE,
        bordered = TRUE,
        highlight = TRUE,
        resizable = TRUE,
        fullWidth = TRUE
      )
    })

  })
}





