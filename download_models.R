# Load the osfr package
library(osfr)

# Define the OSF project ID (extracted from the OSF URL)
project_id <- "hdv38"

# Define the expected model filenames
fitted_models <- c(
  "GAMM-I.rds", 
  "GAMM-II.rds", 
  "GAMM-III.rds", 
  "GAMM-IV.rds", 
  "GAMM-V.rds",
  "LM-PreySpeed.rds"
)

# Define the local directory to save the downloaded models
local_dir <- file.path(getwd(), "outputs", "outputs_models")
if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

# Retrieve the OSF project
project <- osf_retrieve_node(project_id)

# List all files in the project's OSF Storage
files <- osf_ls_files(project)

# Check for the presence of each model file
available_files <- files$name
missing_files <- setdiff(fitted_models, available_files)

if (length(missing_files) > 0) {
  message("The following fitted model files are missing in the OSF project:")
  print(missing_files)
} else {
  message("All fitted model files are present. Proceeding to download...")
  
  # Filter the files to only include the expected model files
  model_files <- files[files$name %in% fitted_models, ]
  
  # Download each model file
  downloaded_files <- osf_download(model_files, path = local_dir, verbose = TRUE)
  
  # Verify that each file was successfully downloaded
  for (i in seq_along(fitted_models)) {
    local_path <- file.path(local_dir, fitted_models[i])
    if (file.exists(local_path)) {
      message(paste("Successfully downloaded:", fitted_models[i]))
    } else {
      warning(paste("Failed to download:", fitted_models[i]))
    }
  }
}