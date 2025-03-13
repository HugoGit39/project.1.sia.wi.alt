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
source(here("shiny", "test", "app", "modules", "mod_feat_filter.R"))
source(here("shiny", "test", "app", "modules", "mod_sub_data.R"))
source(here("shiny", "test", "app", "modules", "mod_contact.R"))

# * 4 Load data -----------------------------------------------------------
sia_df <- get(load(here("shiny", "test", "app", "data", "df_sia_wearable_app.RData")))


