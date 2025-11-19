#!/bin/bash
#SBATCH --account=def-monti
#SBATCH --nodes=1
#SBATCH --cpus-per-task=32
#SBATCH --mem=64000M
#SBATCH --time=00-18:00
#SBATCH --mail-user=fraser_franco.maxime@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2023
module load gcc/12.3
module load r-bundle-bioconductor/3.20
module load r/4.4.0

export R_LIBS="/home/maxime11/R/x86_64-pc-linux-gnu-library/4.4"
Rscript fit_asym-IV.R
