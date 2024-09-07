# Online repository for Fraser Franco et al. (XXXX)

![](https://img.shields.io/badge/license-CC%20BY--NC%204.0-green?style=for-the-badge)

## Description

Online repository to reproduce the results presented in :

Fraser Franco, M., Santostefano, F., Martin, G. A., Julien, Kelly, Clint D., Montiglio, P.-O. (submitted to *The American Naturalist*) Prey movement shapes the development of predator expertise in a virtual bi-trophic system.

## General coding workflow

The subfolders in the [code folder](./code) are enumerated in order to reflect the worflow. 

Here are the steps highlighting the process with a link to the code subfolders :

1. [Run models](./code/code_models)
2. [Model validation](./code/code_model-validation)
3. [Model processing](./code/model-processing)
4. [Produce figures](./code/code_figures)


You will find the specific outputs generated from the R scripts in the [outputs](./outputs) folder. Each output file has a number that correponds to the R script used to generate it.

## Workflow to compute the models

I ran all the models on [Cedar](https://docs.alliancecan.ca/wiki/Cedar), a computer cluster managed by the [Digital Research Alliance of Canada](https://www.alliancecan.ca/en). You can open the .sh files in the [jobs_slurm](./jobs_slurm) folder to see the computer specifications used to run the models. The computer cluster runs on CentOS Linux 7.

Here is a table showing the workflow employed to generate the model outputs using R. This workflow is exclusively for `.R` files used to compute the Bayesian models with the "brms" package (see the [code_models](./code/code_models) folder).

| Generator              | Operating system | Programming language | Code               | Results                  |
| ---------------------- | ---------------- | -------------------- | ------------------ | ------------------------ |
| Digital Research Alliance of Canada / Cedar | CentOS Linux 7   | R                    | [code](./code) folder | [outputs](./outputs) folder |

**Note** : The model outputs are archived on [this OSF repository](https://osf.io/hdv38/).

## Reproducibility

### Model outputs

The model fitting cannot be reproduced on your personal computer as it requires important computing resources.

Here are the necessary steps to reproduce our analyses :

1. Download the model output files (`.rds`) on this [OSF repository](https://osf.io/hdv38/).

2. Put the model outputs in a folder that you should name "outputs_models".

3. Put this folder as a subfolder in the [outputs](./outputs) folder.

### R packages versions

I used the [renv](https://rstudio.github.io/renv/index.html) package to contain each individual R package version within the project. If you wish to reproduce our results in your personal computer (excluding the model files that were run on Cedar), please refer to the official renv [collaborating vignette](https://rstudio.github.io/renv/articles/collaborating.html) to implemement the workflow on your personal computer.