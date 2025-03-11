############################################################################################
#
#  Global file
#
#############################################################################################

# * 1 Load packages -----------------------------------------------------------

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

# * 2 Load functions -----------------------------------------------------------

source(here("shiny", "test", "app", "functions", "colours_fresh.R"))
source(here("shiny", "test", "app", "functions", "email.R"))

# * 3 Load modules -----------------------------------------------------------

source(here("shiny", "test", "app", "modules", "mod_header.R"))
source(here("shiny", "test", "app", "modules", "mod_app_info.R"))
source(here("shiny", "test", "app", "modules", "mod_prod_filter.R"))
source(here("shiny", "test", "app", "modules", "mod_sub_data.R"))
source(here("shiny", "test", "app", "modules", "mod_contact.R"))

# * 4 Load data -----------------------------------------------------------
db_path <- here("shiny", "test", "app", "data", "database_maar_dan_goed.xlsx")

# Load and preprocess data

sia_df <- readxl::read_excel(db_path) %>%
  select(`Device costs`, Weight, `SiA Expert score (short-term)`, `SiA Expert score (long-term)`, `Battery life (hours)`, PPG, ECG, ICG, EMG, GPS, Location) %>%
  mutate(
    DeviceCosts = as.numeric(sub(";.*", "", `Device costs`)),
    Weight = as.numeric(sub(";.*", "", Weight)),
    SiAShort = as.numeric(sub(";.*", "", `SiA Expert score (short-term)`)),
    SiALong = as.numeric(sub(";.*", "", `SiA Expert score (long-term)`)),
    BatteryLife = as.numeric(sub(";.*", "", `Battery life (hours)`)),
    PPG = ifelse(grepl("^1;", PPG), 1, 0),
    ECG = ifelse(grepl("^1;", ECG), 1, 0),
    ICG = ifelse(grepl("^1;", ICG), 1, 0),
    EMG = ifelse(grepl("^1;", EMG), 1, 0),
    GPS = ifelse(grepl("^1;", GPS), 1, 0)
  )
