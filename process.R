#Dependencies
library(dplyr)
library(raster)
library(ggplot2)
library(marmap)

#Global variables
scens = c("base","double_reef","half_reef")
start_year = 1980
end_year = 2017
nts <- (end_year-start_year)
fgs <- read.csv("fgs.csv")
fishers <- read.csv("fishers.csv")
indic <- read.csv("indicators.csv")
resdir = "./Results"
nrows = 53
ncols = 131
n_region = 4

source("ste_functions.R")

#-----------------------------------------------
#  PROCESS DISTRIBUTIONS
#-----------------------------------------------
#Could parallelize this.
f.process.distributions("Biomass",scens,fgs,nrows,ncols,nts,resdir)
f.process.distributions("Catch",scens,fgs,nrows,ncols,nts,resdir)
f.process.distributions("Discards",scens,fgs,nrows,ncols,nts,resdir)
f.process.distributions("Effort",scens,fishers,nrows,ncols,nts,resdir)
f.process.distributions("HabitatCapacity",scens,fgs,nrows,ncols,nts,resdir)
f.process.indicators("Indicator",scens,indic,nrows,ncols,nts,resdir)

#-----------------------------------------------
#  PROCESS TRENDS
#-----------------------------------------------
#Biomass
f.process.biomass.trends(scens,fgs,nts,resdir,n_region)
f.process.catch.trends("Catch",scens,fishers,fgs,nts,resdir,n_region)
f.process.catch.trends("Landings",scens,fishers,fgs,nts,resdir,n_region)
f.process.annual.biomass.trends(scens,fgs,nts,resdir,n_region)
f.process.annual.catch.trends("Catch",scens,fishers,fgs,nts,resdir,n_region)
f.process.annual.catch.trends("Landings",scens,fishers,fgs,nts,resdir,n_region)