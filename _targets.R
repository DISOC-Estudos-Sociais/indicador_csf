# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(PNADcIBGE)
library(dplyr)
library(survey)
library(glmnet)
library(ranger)
library(tibble)
library(ROSE)
library(tidymodels)


# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble", "readr", "dplyr", "ggplot2", "survey", "PNADcIBGE", "tidymodels") # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(raw_data, get_data()),
  tar_target(clean_data, modify_data(raw_data)),
  tar_target(clean_data_svy, pnadc_design(clean_data)),
  tar_target(splited_data, initial_split(clean_data, prop = 0.80)),
  tar_target(train_data, training(splited_data)),
  tar_target(test_data, testing(splited_data)),
  tar_target(train_data_svy, pnadc_design(train_data)),
  
  tar_target(model1, fit_model_5009(train_data_svy)),
  tar_target(model2, fit_model_5008(train_data_svy)),
  tar_target(model3, fit_logit_quasi(train_data_svy)),
  tar_target(model4, fit_probit(train_data_svy)),
  
  tar_target(model5, fit_lasso(train_data)),
  tar_target(model6, fit_rf(train_data)),
  
  tar_target(os_data, oversample(train_data)),
  tar_target(model7, fit_logit_oversample(os_data))
)

