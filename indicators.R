#Indicators for Ecospace Analysis
#MSW

#Dependencies
library(dplyr)
library(tidyr)
library(raster)
library(ggplot2)
library(marmap)
library(geosphere)
library(patchwork)
library(e1071)
library(spdep)
library(stats)
library(rnaturalearth)
library(rnaturalearthdata)

#Global variables
scens = c("base","double_reef","half_reef")
start_year = 1980
end_year = 2017
area_per_cell = 0.1335878
nts <- (end_year-start_year)
fgs <- read.csv("fgs.csv")
fishers <- read.csv("fishers.csv")
indic <- read.csv("indicators.csv")
out_dir = "./Outputs"
res_dir = "./Results"
nrows = 53
ncols = 131
n_region = 4
areas = c("LaTex Shelf","MS River","WFS","Dry Tortugas","Full Domain")

source("ste_functions.R")

#-----------------------------------------------
#  QUANTIFY CENTER OF MASS CHANGES
#-----------------------------------------------

load(file = paste0(res_dir, "/", "Biomass", ".RData"))
f.center.mass("Biomass", scens, fgs, nts, data_array, start_year, nrows, ncols, out_dir, res_dir)

load(file = paste0(res_dir, "/", "Catch", ".RData"))
f.center.mass("Catch", scens, fgs, nts, data_array, start_year, nrows, ncols, out_dir, res_dir)

load(file = paste0(res_dir, "/", "Discards", ".RData"))
f.center.mass("Discards", scens, fgs, nts, data_array, start_year, nrows, ncols, out_dir, res_dir)

load(file = paste0(res_dir, "/", "Effort", ".RData"))
f.center.mass("Effort", scens, fishers, nts, data_array, start_year, nrows, ncols, out_dir, res_dir)

load(file = paste0(res_dir, "/", "HabitatCapacity", ".RData"))
f.center.mass("HabitatCapacity", scens, fgs, nts, data_array, start_year, nrows, ncols, out_dir, res_dir)

load(file = paste0(res_dir, "/", "Indicator", ".RData"))
f.center.mass("Indicator", scens, indic, nts, data_array, start_year, nrows, ncols, out_dir, res_dir)

#-----------------------------------------------
#  COMPARE FINISH TO START
#-----------------------------------------------
load(file = paste0(res_dir, "/", "Biomass", ".RData"))
f.end.start("Biomass", scens, fgs, nts, data_array, start_year, nrows, ncols, out_dir, res_dir)

load(file = paste0(res_dir, "/", "Catch", ".RData"))
f.end.start("Catch", scens, fgs, nts, data_array, start_year, nrows, ncols, out_dir, res_dir)

load(file = paste0(res_dir, "/", "Discards", ".RData"))
f.end.start("Discards", scens, fgs, nts, data_array, start_year, nrows, ncols, out_dir, res_dir)

load(file = paste0(res_dir, "/", "Effort", ".RData"))
f.end.start("Effort", scens, fishers, nts, data_array, start_year, nrows, ncols, out_dir, res_dir)

load(file = paste0(res_dir, "/", "HabitatCapacity", ".RData"))
f.end.start("HabitatCapacity", scens, fgs, nts, data_array, start_year, nrows, ncols, out_dir, res_dir)

load(file = paste0(res_dir, "/", "Indicator", ".RData"))
f.end.start("Indicator", scens, indic, nts, data_array, start_year, nrows, ncols, out_dir, res_dir)

#-----------------------------------------------
#  DISPERSION METRICS
#-----------------------------------------------
load(file = paste0(res_dir, "/", "Biomass", ".RData"))
f.dispersion("Biomass", scens, fgs, nts, data_array, start_year, out_dir, res_dir)

load(file = paste0(res_dir, "/", "Catch", ".RData"))
f.dispersion("Catch", scens, fgs, nts, data_array, start_year,out_dir, res_dir)

load(file = paste0(res_dir, "/", "Discards", ".RData"))
f.dispersion("Discards", scens, fgs, nts, data_array, start_year,out_dir, res_dir)

load(file = paste0(res_dir, "/", "Effort", ".RData"))
f.dispersion("Effort", scens, fishers, nts, data_array, start_year,out_dir, res_dir)

load(file = paste0(res_dir, "/", "HabitatCapacity", ".RData"))
f.dispersion("HabitatCapacity", scens, fgs, nts, data_array, start_year,out_dir, res_dir)

load(file = paste0(res_dir, "/", "Indicator", ".RData"))
f.dispersion("Indicator", scens, indic, nts, data_array, start_year, out_dir, res_dir)

#-----------------------------------------------
#  TIME SERIES TREND METRICS
#-----------------------------------------------
load(file = paste0(res_dir,"/","Biomass Trend",".RData"))
f.plot_regional_biomass_trend("Biomass",data_array,fgs,scens,n_regions,start_year,out_dir,areas)

load(file = paste0(res_dir,"/","Catch Trend",".RData"))
f.plot_regional_catch_trend("Catch",data_array,fgs,fishers,scens,n_regions,start_year,out_dir,areas)

load(file = paste0(res_dir,"/","Landings Trend",".RData"))
f.plot_regional_catch_trend("Landings",data_array,fgs,fishers,scens,n_regions,start_year,out_dir,areas)

## Annual catch trends
load(file = paste0(res_dir,"/","Annual Biomass Trend",".RData"))
f.plot_regional_annual_biomass_trend("Biomass",data_array,fgs,scens,n_regions,start_year,out_dir,areas)

load(file = paste0(res_dir,"/","Annual Catch Trend",".RData"))
f.plot_regional_annual_catch_trend("Catch",data_array,fgs,fishers,scens,n_regions,start_year,out_dir,areas)

load(file = paste0(res_dir,"/","Annual Landings Trend",".RData"))
f.plot_regional_annual_catch_trend("Landings",data_array,fgs,fishers,scens,n_regions,start_year,out_dir,areas)

#-----------------------------------------------
#  CORRELATION ANALYSES
#-----------------------------------------------

f.correlation.timeseries.biomass("Biomass",scens,areas,fgs,res_dir)
f.correlation.timeseries.catch("Catch",scens,areas,fgs,fishers,res_dir)
f.correlation.timeseries.catch("Landings",scens,areas,fgs,fishers,res_dir)
f.plot.correlation.biomass("Biomass",res_dir,out_dir)
f.plot.correlation.catch("Catch",res_dir,out_dir)
f.plot.correlation.catch("Landings",res_dir,out_dir)

#-----------------------------------------------
#  OFFSET CENTER OF MASSES
#-----------------------------------------------
biom_dat = read.csv("./Results/Biomass Center of Masses.csv")
catch_dat = read.csv("./Results/Catch Center of Masses.csv")

all_dat = data.frame(Scenario = character(),Group = character(), Difference_km = numeric())
for (i in 1:n_distinct(biom_dat$Group)){
  sub_catch = catch_dat %>% filter(Group == unique(biom_dat$Group)[i])
  sub_biom = biom_dat %>% filter(Group == unique(biom_dat$Group)[i])
  
  if (nrow(sub_catch) > 0){
    full_dat = merge(sub_biom,sub_catch,by=c("Scenario","Group","Time")) %>% 
      mutate(Distance_km = round(distHaversine(cbind(Lon.x, Lat.x), cbind(Lon.y, Lat.y)) / 1000,2))
    
    start = full_dat %>% filter(Time == min(Time))
    end = full_dat %>% filter(Time == max(Time))
    
    diff = data.frame(Scenario = start$Scenario,Group = unique(biom_dat$Group)[i], Difference_km = end$Distance_km-start$Distance_km)
    all_dat = all_dat %>% add_row(diff)
  }
}
color_limit <- max(abs(min(all_dat$Difference_km)), abs(max(all_dat$Difference_km)))

all_dat = all_dat %>%
  mutate(FacetGroup = ceiling(as.numeric(factor(Group)) / 30))

for (i in 1:n_distinct(all_dat$FacetGroup)){
  sub_dat = all_dat %>% filter(FacetGroup %in% i)
  
  plt = ggplot(sub_dat,aes(x=Scenario,y=Group,fill=Difference_km))+
    geom_tile(color="white")+
    theme_classic()+
    scale_fill_gradient2(low="red",mid="white",high="blue",limits = range(c(-color_limit, color_limit)),breaks = c(-color_limit, 0, color_limit),labels = c(as.character(round(-color_limit, 2)), "0", as.character(round(color_limit, 2))))+
    labs(x="",y="",title = "Growth Between Biomass and Catch CM Over Time")
  
  filename = paste0("./Outputs/Biomass and Catch CM Difference_",i,".png")
  ggsave(filename,plt,height = unit(8,"in"),width=unit(9,"in"))
}


#-----------------------------------------------
#  OFFSHORE WIND COMPARISIONS
#-----------------------------------------------
#Biomass trend before and after implementation
scens = c("full_wind","partial_wind","base_wind","base_wind_fishing","partial_wind_fishing","full_wind_fishing")

## Compare just region 5
### Biomass trend by group
cols = colorblind_palette(n_distinct(scens))
all_dat = data.frame()
for (scen in 1:n_distinct(scens)){
  file = read.csv(paste0("./Experiments/",scens[scen],"/Ecospace_Annual_Average_Region_5_Biomass.csv"),check.names = F)
  dat = file %>% pivot_longer(names_to = "Group",values_to = "Value",cols = c(colnames(file)[2:ncol(file)])) %>% mutate(Scenario = scens[scen])
  all_dat = rbind(all_dat,dat)
}
for (fg in 1:nrow(fgs)){
  sub_dat = all_dat %>% filter(Group %in% unique(fgs$Name)[fg])
  plt = ggplot(sub_dat,aes(x=start_year+Year,y=Value,color=Scenario))+
    geom_vline(xintercept = 2000,linetype="dashed",alpha=0.4)+
    annotate("text",x=2005,y=max(sub_dat$Value),label="Wind Farm\nImplementation")+
    geom_line()+
    theme_classic()+
    scale_color_manual(values = cols)+
    labs(x="Time",y="Biomass (t/km2)",name="Scenario",title=paste0(unique(fgs$Name)[fg]," Biomass Trend"),subtitle = "Wind Lease Areas Only")
  
  filename = paste0(out_dir,"/Wind/Absolute_Biomass/",unique(fgs$Name)[fg]," Biomass Trend.png")
  ggsave(filename,plt)
}

### Catch trend by group
cols = colorblind_palette(n_distinct(scens))
all_dat = data.frame(Scenario = character(),Group = character(),Fleet=character(),Time = numeric(),Value = numeric)

for (scen in 1:n_distinct(scens)){
  file = read.csv(paste0("./Experiments/",scens[scen],"/Ecospace_Annual_Average_Region_5_Catch.csv"),check.names = F)
  interactions <- strsplit(colnames(file)[2:ncol(file)], "\\|") # Get column names
  for (action in 1:length(interactions)) {
    fish <- interactions[[action]][1]  # Index of the matching fleet
    fg <- interactions[[action]][2]  # Prey
    
    for (t in 1:nrow(file)){
      all_dat <- all_dat %>% dplyr::add_row(Scenario = scens[scen],Group = fg,Fleet = fish,Time = (start_year+t),Value = file[t,(action+1)])
    }
  }
}

### Food web
#### Avg. Consumption Before vs. Avg. Consumption After



