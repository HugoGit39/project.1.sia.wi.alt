############################################################################################
#
#  Global file
#
#############################################################################################

# * 1 Load libraries ----------------------- -------------------------------

# list of required packages
required_packages <- c(
  "shiny", "bs4Dash", "here", "dplyr", "readxl", "fresh", "data.table", "DT",
  "shinySearchbar", "emayili", "shinyjs", "sever", "shinycssloaders", "tibble"
)

# install missing packages
install_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# apply function to list of required packages
sapply(required_packages, install_missing)

# load libraries
library(shiny)
library(bs4Dash)
library(here)
library(dplyr)
library(readxl)
library(fresh)
library(data.table)
library(DT)
library(shinySearchbar)
library(emayili)
library(shinyjs)
library(sever)
library(shinycssloaders)

# * 2 Load functions -----------------------------------------------------------

source(here("shiny", "test", "app", "functions", "accordion.R"))
source(here("shiny", "test", "app", "functions", "colours_fresh.R"))
source(here("shiny", "test", "app", "functions", "email.R"))
source(here("shiny", "test", "app", "functions", "filters.R"))
source(here("shiny", "test", "app", "functions", "mandatory_fields.R"))

# * 3 Load modules -----------------------------------------------------------

source(here("shiny", "test", "app", "modules", "mod_controlbar.R"))
source(here("shiny", "test", "app", "modules", "mod_header.R"))
source(here("shiny", "test", "app", "modules", "mod_app_info.R"))
source(here("shiny", "test", "app", "modules", "mod_prod_filter.R"))
source(here("shiny", "test", "app", "modules", "mod_feat_filter.R"))
source(here("shiny", "test", "app", "modules", "mod_sub_data.R"))
source(here("shiny", "test", "app", "modules", "mod_article.R"))
source(here("shiny", "test", "app", "modules", "mod_about.R"))
source(here("shiny", "test", "app", "modules", "mod_contact.R"))
source(here("shiny", "test", "app", "modules", "mod_footer.R"))
source(here("shiny", "test", "app", "modules", "mod_timeout.R"))

#  * 4 load data -----------------------------------------------
sia_df <- get(load(here("shiny", "test", "app", "data", "df_sia_wearable_app.RData")))

#  * 5 set spinner table -----------------------------------------------
options(spinner.type = 5, spinner.color = "#f15a29", spinner.size = 0.5, hide.ui = FALSE)

#  * 6 Time-out message -----------------------------------------------
disconnected <- tagList(
  p(strong("Time Out!", style = "color: #1c75bc; font-size:30px")),
  p(tags$img(src = "favicon.ico", height = 100, width = 100)),
  p("You haven't been active for over 1 hour", br(),
    "or your system went into sleep mode.", br(),
    "To help", strong("De-Stress", style = "color: #f15a29; font-size:18px"), "the server", br(),
    "your session has ended.", style = "font-size:16px"),
  p(reload_button("Refresh")),
  p("Just hit refresh to continue", br(),
    "where you left off!", style = "font-size:16px")
)

#  * 7 Mandatory email fields ---------------------------
fieldsMandatory <- c("name", "email", "message")

# * 8 Rename table variables ---------------------------
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


