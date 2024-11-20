## Naming conventions
## 'sim' --> Related to Ecosim
## 'spa' --> Related to Ecospace
## 'obs' --> Related to observed timeseries data, i.e., that Ecosim was fitted to. 
## 'B'   --> Denotes biomass
## 'C'   --> Denotes catch

## Test push/pull from new computer

## Setup -----------------------------------------------------------------------
rm(list=ls())
source("./functions.R") ## Pull in functions
library(dplyr)

## Input set up ----------------------------------------------------------------
ewe_out_fold = "Model-outputs"
sim_scenario = "ecosim_sim01_test run"
obs_TS_name  = ""
srt_year     = 2020

## Set up inputs ---------------------------------------------------------------
## A group of Ecospace simulations to compare are termed an "Experiment"

experiment_choice <- 1  # Set to 1 for Experiment 1, or 2 for Experiment 2

if (experiment_choice == 1) {
  ## Experiment 1 --------------------------------------------------------------
  ## Compares scenarios with piece-wise environmental drivers
  spa_scenarios  = c("sp01_base", "sp02_+SIW", 
                     "sp03_+ecoengineer", "sp04_+SIW+ecoengineer")
  spa_scen_names = c("01 Base",  "02 +SIW",   
                     "03 +EcoEng", "04 +SIW +EcoEng")
  out_file_notes = "test-STEdrivers"  ## label outputs
  
  dir_out <- "./Compare-outputs/"  ## Folder where outputs will be stored
  col_spa <- c("darkgoldenrod", "indianred2", "steelblue4", "darkorchid4")
  

} else if (experiment_choice == 2) {
  ## Experiment 2 --------------------------------------------------------------
  ## 
#  spa_scenarios  = c("spa_00", "spa_01", 
#                     "spa_02_MOM6-ISIMIP3a", "spa_03_MOM6-ISIMIP3a_PP-phyc-vint")
#  spa_scen_names = c("01 No PP",     "02 MODIS ChlA", 
#                     "03 MOM6 ChlA", "04 MOM6 Vint Phy")
#  out_file_notes = "comp-PPdrivers"
#  dir_out <- "./Scenario_comps/Compare_ppDrivers/"  ## Folder where outputs will be stored
#  col_spa <- c("goldenrod", "darkorchid3", "deeppink", "blue")
}

## Check if the files for the scenarios exist
for (i in 1:length(spa_scenarios)) {
  dir_spa <- paste0("./", ewe_out_fold, "/", spa_scenarios[i], "/")
  if (dir.exists(dir_spa)) print(paste0("Directory exists: ", dir_spa))
  else message("Directory does NOT exist: ", dir_spa)
}

## Create the output folder if it doesn't exist
if (!dir.exists(dir_out)) {
  dir.create(dir_out, recursive = TRUE)  ## Create the folder if it doesn't exist
}

## User-defined parameters for plotting-----------------------------------------
num_plot_pages = 1 ## Sets number of pages for PDF file
init_years_toscale = 1 ## In plotting, this sets the "1 line" to the average of this number of years
plot_name_xY = paste0("BxY_scaled_", init_years_toscale, "y-", out_file_notes, ".PDF")

## Plot output names
dir_pdf_out  = paste0(dir_out)  ## Folder for plots
dir_tab_out  = paste0(dir_out) ## Folder for tables with fit metrics
if (!dir.exists(dir_pdf_out)) dir.create(dir_pdf_out, recursive = TRUE) ## Create the folder if it doesn't exist
if (!dir.exists(dir_tab_out)) dir.create(dir_tab_out, recursive = TRUE) ## Create the folder if it doesn't exist
folder_name <- paste0(dir_tab_out, "Scaled_", init_years_toscale, "y") ## Folder name based on `init_years_toscale`
pdf_file_name_xY = paste0(dir_pdf_out, plot_name_xY)




## -----------------------------------------------------------------------------
##
## Read-in ANNUAL Observed, Ecosim, and Ecospace TS
dir_sim = paste0("./", ewe_out_fold, "/" , sim_scenario, "/")

## Read-in Ecosim annual biomass 
filename = paste0(dir_sim, "biomass_annual.csv")
num_skip_sim = f.find_start_line(filename, flag = srt_year)

simB_xY <- read.csv(paste0(dir_sim, "biomass_annual.csv"), skip = num_skip_sim)
years = simB_xY$year.group ## Get date range from Ecosim
simB_xY$year.group = NULL

## Read-in Ecosim annual catches
simC_xY <- read.csv(paste0(dir_sim, "catch_annual.csv"), skip = num_skip_sim)
simC_xY$year.group = NULL

## Prepare months and date series objects
start_y <- min(years)
end_y   <- max(years)
date_series <- seq(as.Date(paste0(start_y, "-01-01")), as.Date(paste0(end_y,   "-12-01")), by = "1 month")
year_series <- seq(as.Date(paste0(start_y, "-01-01")), as.Date(paste0(end_y,   "-12-01")), by = "1 year")
ym_series <- format(date_series, "%Y-%m")

## Read in Ecosim monthly biomasses
simB_xM <- read.csv(paste0(dir_sim, "biomass_monthly.csv"), skip = num_skip_sim); simB_xM$timestep.group = NULL
simC_xM <- read.csv(paste0(dir_sim, "catch_monthly.csv"), skip = num_skip_sim); simC_xM$timestep.group = NULL
rownames(simB_xM) = ym_series

## -----------------------------------------------------------------------------
##
## Read-in Ecospace annual biomass and catches ---------------------------------

## Initialize empty lists (4) for biomasses and catches by year and month
ls_spaB_xY <- list(); ls_spaC_xY <- list()

## Loop through scenarios to populate lists
for (i in 1:length(spa_scenarios)) {
  (dir_spa = paste0("./", ewe_out_fold, "/", spa_scenarios[i], "/"))
  
  ## Read in Annual Biomass
  filename <- paste0("Ecospace_Annual_Average_Biomass.csv")    ## Set filename
  filepath <- paste0(dir_spa, filename)                        ## Set filepath based on directory 
  num_skip_spa <- f.find_start_line(filepath, flag = "Year")   ## Apply function to find the start line to start reading table
  spaB_xY <- read.csv(filepath, skip = num_skip_spa, header = TRUE); spaB_xY$Year = NULL
  
  ## Read in Monthly Biomass
  filename <- paste0("Ecospace_Average_Biomass.csv")
  filepath <- paste0(dir_spa, filename)
  num_skip_spa <- f.find_start_line(filepath, flag = "TimeStep")
  spaB_xM <- read.csv(filepath, skip = num_skip_spa, header = TRUE); spaB_xM$TimeStep = NULL
  
  ## Read in Annual Catches
  filename <- paste0("Ecospace_Annual_Average_Catch.csv")        ## Set filename
  filepath <- paste0(dir_spa, filename)                          ## Set filepath based on directory 
  num_skip_spa <- f.find_start_line(filepath, flag = "Year") ## Apply function to find the start line to start reading table
  spaC_xY <- read.csv(filepath, skip = num_skip_spa, header = TRUE); spaC_xY$Year = NULL
  
  ## Read in Monthly Catches
  filename <- paste0("Ecospace_Average_Catch.csv")
  filepath <- paste0(dir_spa, filename)
  num_skip_spa <- f.find_start_line(filepath, flag = "TimeStep")
  spaC_xM <- read.csv(filepath, skip = num_skip_spa, header = TRUE); spaC_xM$TimeStep = NULL
  
  ## Standardize Ecospace FG names ------------------------------
  fg_names = f.standardize_group_names(colnames(spaB_xY))
  num_fg = length(fg_names)
  fg_df <- data.frame(
    pool_code  = 1:num_fg,
    group_name = paste(sprintf("%02d", 1:num_fg),
                       gsub("_", " ", fg_names))
  )
  
  ## Set row and column names
  rownames(spaB_xY) = rownames(spaC_xY) = years
  rownames(spaB_xM) = rownames(spaC_xM) = ym_series
  colnames(spaB_xM) = colnames(simB_xY) = fg_df$group_name
  
  ## Add to list
  ls_spaB_xY[[i]] <- spaB_xY; ls_spaB_xM[[i]] <- spaB_xM
  ls_spaC_xY[[i]] <- spaC_xY; ls_spaC_xM[[i]] <- spaC_xM
}

