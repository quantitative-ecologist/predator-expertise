# =======================================================================

#                       GAMM model diagnostics                          #

# =======================================================================





# =======================================================================
# 1. Prepare session
# =======================================================================

# Load libraries
options(mc.cores = parallel::detectCores())
library(data.table)
library(brms)
library(parallel)
library(ggpubr)

# Get model name from command line
args <- commandArgs(trailingOnly = TRUE)
mod <- args[1]

# Define paths
renv::load()
model_path <- file.path(getwd(), "outputs", "outputs_models")
output_path <- file.path(getwd(), "outputs", "outputs_model-validation")
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# Define expected model files
model_files <- list(
  LM_PreySpeed = "LM-PreySpeed.rds",
  GAMM_I = "GAMM-I.rds",
  GAMM_II = "GAMM-II.rds",
  GAMM_III = "GAMM-III.rds",
  GAMM_IV = "GAMM-IV.rds",
  GAMM_V = "GAMM-V.rds"
)

if (!mod %in% names(model_files)) {
  stop("Unknown model: ", mod)
}

# =======================================================================
# =======================================================================





# =======================================================================
# 2. Load models and generate diagnostics
# =======================================================================

file_name <- model_files[[mod]]
file_path <- file.path(model_path, file_name)
png_path <- file.path(output_path, paste0("checks_", mod, ".png"))
trace_path <- file.path(output_path, paste0("trace_", mod, ".pdf"))

if (!file.exists(file_path)) {
  message(sprintf("Missing model file: %s\nPlease download it from OSF: %s", file_name, model_path))
  quit(save = "no")
}

# Load model
model <- readRDS(file_path)

# Generate trace plot if missing
if (!file.exists(trace_path)) {
  params <- grep("^(b_|phi|sd_|sds_|sigma)", parnames(model), value = TRUE)
  n_per_page <- 4
  n_pages <- ceiling(length(params) / n_per_page)

  pdf(trace_path, width = 12, height = 8)
  for (i in seq_len(n_pages)) {
    idx <- ((i - 1) * n_per_page + 1):min(i * n_per_page, length(params))
    plot(model, variable = params[idx], ask = FALSE)
  }
  dev.off()
  message("Saved trace plot for ", mod, " to: ", trace_path)
} else {
  message("Skipping trace plot for ", mod, " — file already exists in:", trace_path)
}

# Diagnostics
if (!file.exists(png_path)) {
  custom_theme <- theme(legend.position = "bottom")

  p1 <- pp_check(model, type="dens_overlay", ndraws=10) +
    ggtitle(paste(mod, "- pp_check")) + 
    custom_theme

  p2 <- pp_check(model, type="stat_2d", ndraws=100) +
    ggtitle(paste(mod, "- stat_2d")) + 
    custom_theme

  p3 <- pp_check(model, type="stat", stat="median", ndraws=100) +
    ggtitle(paste(mod, "- median")) + 
    custom_theme

  p4 <- pp_check(model, type="error_scatter_avg") +
    ggtitle(paste(mod, "- error_scatter")) + 
    custom_theme

  combined_plot <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
  ggsave(filename = png_path, plot = combined_plot, width = 14, height = 10, dpi = 300)

  message("Saved diagnostic plot for ", mod, " to: ", png_path)
} else {
  message("Skipping ", mod, " — diagnostic plot already exists in: ", png_path)
}

# =======================================================================
# =======================================================================