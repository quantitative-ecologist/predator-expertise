.PHONY: download-models checks checks-redo waic-local waic-table

# -----------------------------------------------------------------
# Model outputs
# -----------------------------------------------------------------

# Command to download the model fits from OSF
# Repository --> https://osf.io/hdv38/

download-models:
	Rscript download_models.R


# -----------------------------------------------------------------
# Model validation
# -----------------------------------------------------------------

VALID_DIR := code/code_model-validation
VALID_OUT := outputs/outputs_model-validation

# Trace plots + posterior predictive checks for all models
# (models whose output files already exist are skipped)
checks:
	./$(VALID_DIR)/run_checks_models.sh

# Delete and regenerate the checks of specific models
checks-redo:
	@test -n "$(MODELS)" || { echo 'Usage: make checks-redo MODELS="LM-PreySpeed LM-PreySpace"'; exit 1; }
	for m in $(MODELS); do \
	  rm -f $(VALID_OUT)/checks_$$m.png $(VALID_OUT)/trace_$$m.pdf; \
	  Rscript $(VALID_DIR)/checks_models.R $$m; \
	done


# -----------------------------------------------------------------
# WAIC
# -----------------------------------------------------------------
# WAIC computation (needs enough RAM)
waic-local:
	for m in $(or $(MODELS),asym-baseline asym-I asym-II asym-III); do \
	  Rscript $(VALID_DIR)/waic-compute.R $$m || exit 1; \
	done

# WAIC comparison table from the waic_asym-*.rds files (runs locally)
waic-table:
	Rscript $(VALID_DIR)/waic-table.R