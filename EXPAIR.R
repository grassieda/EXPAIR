## Test code that executes time-activity profiles script 
## creates a probabilistic daily profile
## matches it with probabilistic indoor concentrations in each ME
## Last updated: 2025-10-10 by Duncan to account for new activity profiles 

rm(list = ls(all.names = TRUE)) # clear working directory

library(ggplot2)
library(tidyverse)
library(dplyr)

BaseWD <- "Z:/Projects & research/INHABIT/Code Sharing folder/EXPAIR v5" ## km- "U:/Projects & research/INHABIT/Code Sharing folder/EXPAIR v5"
setwd(BaseWD)
inpath <- paste0(BaseWD, "/intermediate/") #dgedit
concpath <- paste0(inpath, "concentrations/")
actpath <- paste0(inpath, "activity_profiles")
outpath <- paste0(BaseWD, "/outputdata/")
figpath <- paste0(BaseWD, "/figures/")

## Reading in input files

pollutant <- "PM2.5" #dgedit-replace with bit in filename
# (a) concentrations
conc_filenames <- list.files(path = concpath, pattern = "*.csv", full.names = TRUE)
conc_inputdata <- list()
for (conc_file in conc_filenames) {
  # Extract the base filename without path
  conc_basename <- basename(conc_file)

  # Replace "Transport_other" with "Transport-other" if present
  if (grepl("Transport_other", conc_basename)) {
    conc_basename <- sub("Transport_other", "Transport-other", conc_basename)
  }
  # Replace "Cout" with "C_Out" if present
  if (grepl("Cout", conc_basename)) {
    conc_basename <- sub("Cout", "C_Out", conc_basename)
  }

  # Remove the .csv extension and split by underscore
  conc_name_parts <- strsplit(tools::file_path_sans_ext(conc_basename), "_")[[1]]
  
  # Read the CSV file
  conc_data <- read.csv(conc_file, check.names=FALSE)
  
  # Only process files with at least 6 parts
  if (length(conc_name_parts) >= 6) {
    conc_inputdata[[conc_basename]] <- list(
      C_type = conc_name_parts[1],
      micro_env = conc_name_parts[2],
      season = conc_name_parts[3],
      day_type = conc_name_parts[4],
      timestep = conc_name_parts[5],
      distribution_type = conc_name_parts[6],
      conc_data = conc_data
    )
  } else {
    warning(paste("Filename format unexpected:", conc_basename))
  }
}

# Check if all expected fields exist in each sublist
all_fields <- c("season", "day_type", "micro_env")

check_fields <- sapply(conc_inputdata, function(x) all(all_fields %in% names(x)))
if (any(!check_fields)) {
  cat("Some entries are missing expected fields:\n")
  print(names(conc_inputdata)[!check_fields])
}

# (b) activity profiles
act_filenames <- list.files(path = actpath, pattern = "*.csv", full.names = TRUE)
act_inputdata <- list()
for (act_file in act_filenames) {
  # Extract the base filename without path
  act_basename <- basename(act_file)
  
  # Remove the .csv extension and split by underscore
  act_name_parts <- strsplit(tools::file_path_sans_ext(act_basename), "_")[[1]]
  
  # Read the CSV file
  act_data <- read.csv(act_file, check.names=FALSE)
  
  # Store in the results list
  act_inputdata[[act_basename]] <- list(
    population_group = act_name_parts[4],
    act_data = act_data
  )
}

# Loop over population groups
setwd(BaseWD)
source("functions_EXPAIR.R")
for (i in seq_along(act_inputdata)) {
  pop_group_act_data<- act_inputdata[[i]]
  population<-pop_group_act_data$population_group
  act_data_by_pop_group<-pop_group_act_data$act_data
  df_samples<-main(population, pollutant, act_data_by_pop_group,conc_inputdata,plot=TRUE)
  str(df_samples)
}



## For plotting of population groups
pop_summaries <- sweep_files(outpath, "summary")
plot_boxplots_popgroups(pop_summaries, figpath)
