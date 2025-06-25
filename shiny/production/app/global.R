############################################################################################
#
#  Global file
#
#############################################################################################

# * 1 Load libraries ----------------------- -------------------------------

# list of required packages
required_packages <- c(
  "shiny", "bs4Dash", "here", "dplyr", "scales", "DT", "fresh", "tibble", "rlang", "lubridate",
  "shinySearchbar", "emayili", "shinyjs", "sever", "shinycssloaders", "shinyWidgets",
  "reactablefmtr", "reactable", "htmltools", "htmlwidgets"
)

# Load each package
invisible(lapply(required_packages, function(pkg) {
  library(pkg, character.only = TRUE)
}))

# * 2 Load functions -----------------------------------------------------------

source("functions/accordion.R")
source("functions/colours_fresh.R")
source("functions/cell_layout.R")
source("functions/email.R")
source("functions/filters.R")
source("functions/func_column_defs.R")
source("functions/func_row_defs.R")
source("functions/mandatory_fields.R")

# * 3 Load modules -----------------------------------------------------------

source("modules/mod_controlbar.R")
source("modules/mod_header.R")
source("modules/mod_app_info.R")
source("modules/mod_prod_filter.R")
source("modules/mod_feat_filter.R")
source("modules/mod_sub_data.R")
source("modules/mod_article.R")
source("modules/mod_about.R")
source("modules/mod_contact.R")
source("modules/mod_footer.R")
source("modules/mod_timeout.R")

#  * 4 load data -----------------------------------------------
sia_df <- get(load(here("data/df_sia_wearable_app.RData")))

#remove column id
sia_df$id <- NULL

#  * 5 calculate no of wearables for home page -----------------------------------------------
n_wearables <- nrow(sia_df)

#  * 6 set spinner table -----------------------------------------------
options(spinner.type = 5, spinner.color = "#f15a29", spinner.size = 0.5, hide.ui = FALSE)

#  * 7 reactable layout -----------------------------------------------

#  * * 7.1 colours -----------------------------------------------

#  sticky columns color
sticky_style <- list(backgroundColor = "#f7f7f7")

# base color palette numerical columns
pal_num_scale <- generate_alpha_palette("#1c75bc", 100)

#  * * 7.2 cells -----------------------------------------------

#bars columns
bar_vars <- c("sia_es_long", "sia_es_short")

# numerical columns
numeric_vars <- names(sia_df)[sapply(sia_df, is.numeric) & !names(sia_df) %in% bar_vars]

# save min and max per column
numeric_var_ranges <- lapply(numeric_vars, function(var) {
  range(sia_df[[var]], na.rm = TRUE)
})
names(numeric_var_ranges) <- numeric_vars

# yes/no columns
yn_vars <- names(sia_df)[sapply(sia_df, is.character) & names(sia_df) != "release_date" & sapply(sia_df, function(x) any(x %in% c("Yes", "No"), na.rm = TRUE))]

#char columns to rename
char_vars <- setdiff(names(sia_df), c(names(bar_vars), names(yn_vars), names(numeric_vars), "id"))

#  * 8 Mandatory fields ---------------------------

# * * 8.1 data
fieldsMandatory_data <- c("manufacturer", "model", "website", "release_date", "market_status", "main_use",
                          "device_cost", "wearable_type", "location", "weight", "size")

char_only_fields <- list(
  market_status = "Market Status",
  main_use = "Main Use",
  wearable_type = "Type",
  location = "Location",
  other_signals = "Other Signals",
  data_trans_method = "Data Transmission Method"
)

# * * 8.2 email
fieldsMandatory_email <- c("name", "email", "message")

# * 9 Rename table variables ---------------------------

# * * 9.1 Filters ---------------------------

rename_map <- c(
  "sia_es_long" = "Long-Term SiA Score",
  "sia_es_short" = "Short-Term SiA Score",
  "manufacturer" = "Manufacturer",
  "model" = "Model",
  "website" = "Website",
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

# * * 9.1 Submit data ---------------------------

rename_subm<- names(rename_map)

rename_subm <- rename_subm[!rename_subm %in% c("sia_es_long", "sia_es_short")]

rename_subm <- c("name", "email", "telephone", "institution", rename_subm, "additional_information")

#  * 10 Time-out message -----------------------------------------------
disconnected <- tagList(
  p(strong("Time Out!", style = "color: #1c75bc; font-size:30px")),
  p(tags$img(src = "favicon.ico", height = 100, width = 100)),
  p("You haven't been active for over 1 hour", br(),
    "or your system went into sleep mode.", br(),
    "To help", strong("Un-Stress", style = "color: #f15a29; font-size:18px"), "the server", br(),
    "your session has ended.", style = "font-size:16px"),
  p(reload_button("Refresh")),
  p("Just hit refresh to continue", br(),
    "where you left off!", style = "font-size:16px")
)
