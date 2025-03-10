#' The global file is used to load the data and functions that are used in the app.

#' @importFrom data.table fread

# load functions
source("R/colours_fresh.R")

# load SiA dataframe
sia_df <- readxl::read_excel("inst/app/extdata/database_maar_dan_goed.xlsx")

# only keep DeviceCosts, Weight, SiAShort, SiALong, BatteryLife, PPG, ECG, ICG, EMG, GPS
sia_df <- sia_df %>%
  select(`Device costs`, Weight, `SiA Expert score (short-term)`, `SiA Expert score (long-term)`, `Battery life (hours)`, PPG, ECG, ICG, EMG, GPS, Location)

# Preprocess the data to extract the first type of data before the ";"
sia_df <- sia_df %>%
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

# Product data
products_data <- data.frame(
  Product = rep(c("Garmin", "Suunto", "Polar", "Whoop", "Coros", "Fitbit", "Apple Watch"), each = 10),
  Feature = rep(c("Battery Life", "Heart Rate Monitor", "GPS Accuracy", "Sleep Tracking", "VO2 Max", "Water Resistance", "App Integration", "Weight", "Display Type", "Price"), times = 7),
  Value = c(
    "7 Days", "Yes", "High", "Advanced", "Yes", "50m", "Garmin Connect", "50g", "AMOLED", "$399",
    "5 Days", "Yes", "Medium", "Basic", "No", "30m", "Suunto App", "55g", "LCD", "$349",
    "6 Days", "Yes", "High", "Intermediate", "Yes", "50m", "Polar Flow", "45g", "AMOLED", "$379",
    "4 Days", "Yes", "Low", "Advanced", "Yes", "10m", "Whoop App", "35g", "E-Ink", "$299",
    "8 Days", "Yes", "High", "Intermediate", "Yes", "100m", "Coros App", "48g", "LCD", "$429",
    "6 Days", "Yes", "Medium", "Advanced", "Yes", "50m", "Fitbit App", "40g", "OLED", "$199",
    "1 Day", "Yes", "Very High", "Advanced", "Yes", "50m", "Apple Health", "38g", "Retina", "$499"
  )
)



