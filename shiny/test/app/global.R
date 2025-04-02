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

source(here("shiny", "test", "app", "modules", "mod_controlbar.R"))
source(here("shiny", "test", "app", "modules", "mod_header.R"))
source(here("shiny", "test", "app", "modules", "mod_app_info.R"))
source(here("shiny", "test", "app", "modules", "mod_prod_filter.R"))
source(here("shiny", "test", "app", "modules", "mod_feat_filter.R"))
source(here("shiny", "test", "app", "modules", "mod_sub_data.R"))
source(here("shiny", "test", "app", "modules", "mod_article.R"))
source(here("shiny", "test", "app", "modules", "mod_contact.R"))
source(here("shiny", "test", "app", "modules", "mod_footer.R"))
source(here("shiny", "test", "app", "modules", "mod_timeout.R"))

# * 4 Load data -----------------------------------------------------------
sia_df <- get(load(here("shiny", "test", "app", "data", "df_sia_wearable_app.RData")))

#  * 5 set spinner table -----------------------------------------------
options(spinner.type = 5, spinner.color = "#f15a29", spinner.size = 0.5)

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



