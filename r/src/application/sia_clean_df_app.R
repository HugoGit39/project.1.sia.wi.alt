
# list of required packages
required_packages <- c(
  "dplyr", "here", "tcltk", "readxl", "readr", "writexl", "stringr"
)

# check if installed and load
lapply(required_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
})

# Open a dialog box to select the file
file_path <- tclvalue(tkgetOpenFile(filetypes = "{{Excel Macro-Enabled Files} {.xlsm}}"))
df <- read_excel(file_path)

# Keep only the first 54 columns
df <- df[, 1:54]

# Move columns 53 and 54 (SiA Expert Scores) to positions 2 and 3
df <- df %>%
  select(1:2, 53, 54, 3:52)

# Keep only the first 54 rows
df <- df[1:54, ]

# Rename columns
df_app <- df %>%
  rename(
    manufacturer = `Manufacturer`,
    model = `Model`,
    sia_es_long = `SiA Expert score (long-term)`,
    sia_es_short = `SiA Expert score (short-term)`,
    website = `Website`,
    release_date = `Release date`,
    market_status = `Market status`,
    main_use = `Main use`,
    device_cost = `Device costs`,
    wearable_type = `Wearable type`,
    location = `Location`,
    size = `Size`,
    weight = `Weight`,
    ppg = `PPG`,
    ecg = `ECG`,
    icg = `ICG`,
    emg = `EMG`,
    respiration = `Respiration`,
    eda = `EDA`,
    eeg = `EEG`,
    bp = `BP`,
    accelerometer = `Accelerometer`,
    gyroscope = `Gyroscope`,
    gps = `GPS`,
    skin_temperature = `Skin temperature`,
    other_signals = `Other signals`,
    water_resistance = `Water resistance`,
    battery_life = `Battery life`,
    charging_method = `Charging method`,
    charging_duration = `Charging duration`,
    bio_cueing = `Bio-cueing`,
    bio_feedback = `Bio-feedback`,
    raw_data_available = `Raw data available`,
    data_trans_method = `Data transfer method`,
    int_storage_met = `Internal storage method`,
    server_data_storage = `Server Data Storage`,
    dev_storage_cap = `Device storage capacity`,
    gdpr_comp = `GDPR compliance`,
    fda_app_clear = `FDA approval/clearance`,
    ce_app_label = `CE approval/label`,
    level_validation = `Highest level of Validation Evidence`,
    no_studies_val_rel_reviewed = `Number of validity and reliability studies reviewed`,
    no_studies_usab_reviewed = `Number of usability studies reviewed`
  ) %>%
  # Keep only the renamed columns
  select(
    manufacturer, model, sia_es_long, sia_es_short, website, release_date, market_status,
    main_use, device_cost, wearable_type, location, size, weight, ppg, ecg, icg, emg,
    respiration, eda, eeg, bp, accelerometer, gyroscope, gps, skin_temperature,
    other_signals, water_resistance, battery_life, charging_method, charging_duration,
    bio_cueing, bio_feedback, raw_data_available, data_trans_method, int_storage_met,
    server_data_storage, dev_storage_cap, gdpr_comp, fda_app_clear, ce_app_label,
    level_validation, no_studies_val_rel_reviewed, no_studies_usab_reviewed
  )

# Extract values before ";" in necessary columns
df_app <- df_app %>%
  mutate(
    release_date = sub(";.*", "", release_date),
    device_cost = sub(";.*", "", device_cost),
    ppg = sub(";.*", "", ppg),
    ecg = sub(";.*", "", ecg),
    icg = sub(";.*", "", icg),
    emg = sub(";.*", "", emg),
    respiration = sub(";.*", "", respiration),
    eda = sub(";.*", "", eda),
    eeg = sub(";.*", "", eeg),
    bp = sub(";.*", "", bp),
    accelerometer = sub(";.*", "", accelerometer),
    gyroscope = sub(";.*", "", gyroscope),
    gps = sub(";.*", "", gps),
    skin_temperature = sub(";.*", "", skin_temperature),
    water_resistance = sub(";.*", "", water_resistance),
    bio_cueing = sub(";.*", "", bio_cueing),
    bio_feedback = sub(";.*", "", bio_feedback),
    int_storage_met = sub(";.*", "", int_storage_met),
    server_data_storage = sub(";.*", "", server_data_storage),
    gdpr_comp = sub(";.*", "", gdpr_comp),
    fda_app_clear = sub(";.*", "", fda_app_clear),
    ce_app_label = sub(";.*", "", ce_app_label),
    no_studies_val_rel_reviewed = sub(";.*", "", no_studies_val_rel_reviewed))

# Split storage capacity column into two separate values
df_app <- df_app %>%
  mutate(
    dev_storage_cap_hrs = sub(";.*", "", dev_storage_cap),
    dev_storage_cap_mb = sapply(str_split(dev_storage_cap, ";"), function(x) ifelse(length(x) >= 2, x[2], NA))
  ) %>%

  # Remove old storage column
  select(-dev_storage_cap)

# move dev_storage_cap_hrs & dev_storage_cap_mb to column 37 & 38
df_app <- df_app %>%
  select(1:36, 43, 44, 37:ncol(df_app))

# Replace "NP" with NA
df_app <- df_app %>%
  mutate(across(everything(), ~ ifelse(. == "NP", NA, .)))

# Replace "0" with NA in column Other Signals
df_app <- df_app %>%
  mutate(other_signals = ifelse(other_signals == "0", NA, other_signals))

# Convert column types
df_app <- df_app %>%
  mutate(
    # Convert multiple columns to numeric
    across(c(sia_es_long, sia_es_short, device_cost, weight, battery_life, int_storage_met,
             no_studies_val_rel_reviewed, no_studies_usab_reviewed, charging_duration,
             dev_storage_cap_hrs, dev_storage_cap_mb), as.numeric),

    # Convert binary columns (-1 or 1 → "yes", 0 → "no")
    across(c(ppg, ecg, icg, emg, respiration, eda, eeg, bp, accelerometer, gyroscope,
             gps, skin_temperature, water_resistance, bio_cueing, bio_feedback,
             raw_data_available, int_storage_met, server_data_storage, gdpr_comp, fda_app_clear,
             ce_app_label, level_validation), ~ case_when(
               . %in% c(1, -1) ~ "Yes",
               . == 0 ~ "No",
               TRUE ~ as.character(.)
             )),

    # Convert multiple columns to character
    across(c(manufacturer, model, website, market_status, main_use, wearable_type,
             location, size, other_signals, charging_method,
             data_trans_method), as.character),

    # Convert realease date to date
    release_date = case_when(
      str_detect(release_date, "^\\d{4}$") ~ paste0(release_date, "-01-01"),
      TRUE ~ NA_character_
    ),

    # Convert cleaned string to Date
    release_date = as.Date(release_date)

  )

#round values sia_es_long and sia_es_short on 1 decimal
df_app <- df_app %>%
  mutate(
    sia_es_long = round(sia_es_long, 1),
    sia_es_short = round(sia_es_short, 1)
  )

# Create identifier
id <-  paste0(
  sprintf("%03d", seq_len(nrow(sia_df))),
  "_sia_wd_",
  substr(tolower(sia_df$manufacturer), 1, 3)
)

# Add identifier as first column
df_app <- cbind(id = id, df_app)


# rename df_app to sia_df
sia_df <- df_app

# Save the cleaned data as an RData file
save(sia_df, file = here("r", "data", "processed", "df_sia_wearable_app.RData"))

# Optional: Save as Excel for checking
write_xlsx(sia_df, here("r", "data", "processed", "df_sia_wearable_app.xlsx"))

# Optional: Save as CSV for easy viewing
write_csv(sia_df, here("r", "data", "processed", "df_sia_wearable_app.csv"))

