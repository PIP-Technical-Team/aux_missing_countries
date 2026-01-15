
# This program file use latest covariates.Rda as an input 
# into PIP_MissingPoverty_Calculator_Pilot_v2.xlsx sheet "Input_Data"

# Install and load required packages
if (!require("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
  library("openxlsx")
}

# show working directory
cat("Current working directory:", getwd(), "\n")

# Load the covariates data
covariates <- tempfile(fileext = ".Rda")
download.file(
  "https://raw.githubusercontent.com/PIP-Technical-Team/aux_missing_countries/main/03-intermediatedata/covariates.Rda",
  covariates,
  mode = "wb",
  quiet = TRUE
)
load(covariates)

#load('covariates.Rda')

# Define file paths
excel_file <- "PIP_MissingPoverty_Calculator.xlsx"
sheet_name <- "Input_Data"

# Load the existing workbook or create new one
if (file.exists(excel_file)) {
  wb <- loadWorkbook(excel_file)
} else {
  wb <- createWorkbook()
}

# Check if the sheet exists, if not create it
if (!sheet_name %in% names(wb)) {
  addWorksheet(wb, sheet_name)
}

# Get the sheet
ws <- wb[[sheet_name]]

# Write the covariates data to the sheet, starting from cell A1
# This will clear any existing data and write all new data
writeData(wb, sheet = sheet_name, x = covariates, startRow = 1, startCol = 1)

# Rename headers according to the mapping table
header_mapping <- list(
  "year" = "year",
  "code" = "country code",
  "eca" = "Europe & Central Asia",
  "lac" = "Latin America & Caribbean",
  "ssa" = "Sub-Saharan Africa",
  "lmc" = "Lower-middle income",
  "umc" = "Upper-middle income",
  "hic" = "High income",
  "inc" = "Welfare Type: Income = 1",
  "gdp2021" = "Natural Log GDP per capita (2021 PPP)",
  "gdp2017" = "Natural Log GDP per capita (2017 PPP)",
  "rps" = "Rural population share (%)",
  "leb" = "Life expectancy (year)",
  "u5m" = "Natural Log under-5 mortality"
)

# Get current headers
current_headers <- names(covariates)

# Create new headers by mapping old names to new names
new_headers <- sapply(current_headers, function(x) {
  if (x %in% names(header_mapping)) {
    return(header_mapping[[x]])
  } else {
    return(x)  # Keep original name if not in mapping
  }
})

# Write new headers to the first row
for (i in seq_along(new_headers)) {
  writeData(wb, sheet = sheet_name, x = new_headers[i], startRow = 1, startCol = i)
}

# Save the workbook
saveWorkbook(wb, excel_file, overwrite = TRUE)

# Print confirmation message
cat("Data import completed successfully!\n")
cat("Imported data dimensions:", nrow(covariates), "rows x", ncol(covariates), "columns\n")
cat("File saved to:", excel_file, "\n")
cat("Sheet name:", sheet_name, "\n")

