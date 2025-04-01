############################################################################################
#
#  Global file
#
#############################################################################################

# * 1 Load libraries ----------------------- -------------------------------

# list of required packages
required_packages <- c(
  "shiny", "bs4Dash", "here", "dplyr", "readxl", "fresh", "data.table", "DT",
  "shinySearchbar", "emayili", "shinyjs", "sever", "shinycssloaders"
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

source(here("shiny", "test", "app", "functions", "colours_fresh.R"))
source(here("shiny", "test", "app", "functions", "email.R"))
source(here("shiny", "test", "app", "functions", "filters.R"))
source(here("shiny", "test", "app", "functions", "mandatory_fields.R"))

# * 3 Load modules -----------------------------------------------------------

source(here("shiny", "test", "app", "modules", "mod_header.R"))
source(here("shiny", "test", "app", "modules", "mod_app_info.R"))
source(here("shiny", "test", "app", "modules", "mod_prod_filter.R"))
source(here("shiny", "test", "app", "modules", "mod_feat_filter.R"))
source(here("shiny", "test", "app", "modules", "mod_sub_data.R"))
source(here("shiny", "test", "app", "modules", "mod_contact.R"))

# * 4 Load data -----------------------------------------------------------
sia_df <- get(load(here("shiny", "test", "app", "data", "df_sia_wearable_app.RData")))

#  * 5 set spinner table -----------------------------------------------
options(spinner.type = 5, spinner.color = "#f15a29", spinner.size = 0.5)


