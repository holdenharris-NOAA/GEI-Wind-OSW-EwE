#-------------------------------------------------------------------------------
# READ ECOSPACE REGION CONSUMPTION MATRIX
#-------------------------------------------------------------------------------
# This function reads and processes consumption output data for each region from
# an Ecospace run (assumed to be in CSV format). It returns an array of 
# consumption matrices that align with the Ecopath consumption matrix.
#
# The function requires:
# - header: Filepath header for the consumption CSV files
# - fgs: Dataframe of functional group names and numbers
# - regions: Number of model regions
# - ts: Number of timesteps
#
f.read_consumption_timeseries <- function(header, fgs, regions, ts) {
  n_fg <- nrow(fgs)
  consumption_array <- array(0, dim = c(n_fg, n_fg, ts, regions))
  for (region in 1:regions) {  # Skips region 0, adjust if needed
    filename <- paste0(header, region, "_Consumption.csv")
    if (file.exists(filename)) {
      cons <- read.csv(filename, check.names = FALSE)  # Skip "Timestep" row
      interactions <- strsplit(colnames(cons)[2:ncol(cons)], "\\|")  # Get column names
      for (action in 1:length(interactions)) {
        inter1 <- fgs[fgs$FG == interactions[[action]][1], "Number"]  # Predator
        inter2 <- fgs[fgs$FG == interactions[[action]][2], "Number"]  # Prey
        consumption_array[inter2, inter1, , region] <- cons[ , (action + 1)]
      }
    } else {
      cat("REGION", region, "DOES NOT EXIST! ADJUST IF IT SHOULD!\n")
    }
  }
  return(consumption_array)
}

#-------------------------------------------------------------------------------
# READ ECOSPACE REGION BIOMASS MATRIX
#-------------------------------------------------------------------------------
# This function reads and processes biomass output data for each region from
# an Ecospace run (assumed to be in CSV format). It returns an array of 
# biomass matrices that align with the Ecopath consumption matrix.
#
# The function requires:
# - header: Filepath header for the consumption CSV files
# - fgs: Dataframe of functional group names and numbers
# - regions: Number of model regions
# - ts: Number of timesteps
#
f.read_biomass_timeseries <- function(header, fgs, regions, ts) {
  n_fg <- nrow(fgs)
  biomass_array <- array(0, dim = c(ts,n_fg, regions))
  for (region in 1:regions) {  # Skips region 0, adjust if needed
    filename <- paste0(header, region, "_Consumption.csv")
    if (file.exists(filename)) {
      biom <- read.csv(filename, check.names = FALSE)  # Skip "Timestep" row
      for (action in 1:nrow(fgs)) {
        biomass_array[,action , region] <- biom[ ,(action + 1)]
      }
    } else {
      cat("REGION", region, "DOES NOT EXIST! ADJUST IF IT SHOULD!\n")
    }
  }
  return(biomass_array)
}

#-------------------------------------------------------------------------------
# CREATE A COLORBLIND FRIENDLY PALETTE FOR PLOTS
#-------------------------------------------------------------------------------
# This function reads in the number of regions in the model (user-defined) and
# creates a vector of colorblind friendly colors
#
# The function requires:
# - n = Number of regions/colors needed
#
colorblind_palette <- function(n) {
  # Define colorblind-friendly colors
  base_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  
  # If user requests more colors than available, interpolate colors
  if (n > length(base_colors)) {
    color_palette <- colorRampPalette(base_colors)(n)
  } else {
    color_palette <- base_colors[1:n]
  }
  
  return(color_palette)
}

#-------------------------------------------------------------------------------
# PLOT ABSOLUTE BIOMASS TRENDS FOR EACH REGION
#-------------------------------------------------------------------------------
# This function reads the biomass output data and creates a plot for each
# functional group, showing the time series trend in each region.
#
# The function requires:
# - biomass_array: The biomass array created in @f.read_biomass_timeseries
# - fgs: Dataframe of functional group names and numbers
# - cols: Vector of colorblind friendly colors
# - start_year: Start year of the model for plotting
#
f.plot_regional_biomass_absolute = function(biomass_array,fgs,cols,start_year){
  dir_path = "./Regional_Output/Biomass/Absolute"
  if (!dir.exists(dir_path)){
    dir.create(dir_path,recursive = T)
  }
  fg_name = fgs$FG  
  n_region = dim(biomass_array)[3]
  
  for (fg in 1:nrow(fgs)){
    dat_plot = data.frame(Time = seq(1,dim(biomass_array)[1],1))
    
    for (region in 1:n_region){
      new_col = paste0("Region ",region)
      dat_plot[[new_col]] <- biomass_array[, fg, region]
    }
    dat_long <- stack(dat_plot, select = paste0("Region ", 1:n_region))
    dat_long <- data.frame(Time = rep(dat_plot$Time, n_region), Region = dat_long$ind, Value = dat_long$values)
    
    plt = ggplot(dat_long,aes(x=start_year+Time/12,y=Value,color=Region))+
      geom_line(size=1.5)+
      scale_color_manual(values = cols)+
      labs(x="Year",y="Biomass (t/km2)",title=fgs$FG[fg])+
      theme_classic()+
      theme(legend.title = element_blank())
    
    filename = paste0("./Regional_Output/Biomass/Absolute/",fgs$FG[fg]," Absolute Biomass.png")
    ggsave(filename,plt,height=unit(7,"in"),width=unit(8,"in"))
  }
}

#-------------------------------------------------------------------------------
# PLOT RELATIVE BIOMASS TRENDS FOR EACH REGION
#-------------------------------------------------------------------------------
# This function reads the biomass output data and creates a plot for each
# functional group, showing the time series trend in each region.
#
# The function requires:
# - biomass_array: The biomass array created in @f.read_biomass_timeseries
# - fgs: Dataframe of functional group names and numbers
# - cols: Vector of colorblind friendly colors
# - start_year: Start year of the model for plotting
#
f.plot_regional_biomass_relative = function(biomass_array,fgs,cols,start_year){
  dir_path = "./Regional_Output/Biomass/Relative"
  if (!dir.exists(dir_path)){
    dir.create(dir_path,recursive = T)
  }
  fg_name = fgs$FG  
  n_region = dim(biomass_array)[3]
  relative_change <- array(NA, dim = dim(biomass_array))
  
  # Calculate relative change for each group and region
  for (group in 1:nrow(fgs)) {
    for (region in 1:n_region) {
      start_value <- biomass_array[1, group, region]
      relative_change[, group, region] <- (biomass_array[, group, region] - start_value) / start_value
    }
  }  
  
  for (fg in 1:nrow(fgs)){
    dat_plot = data.frame(Time = seq(1,dim(relative_change)[1],1))
    
    for (region in 1:n_region){
      new_col = paste0("Region ",region)
      dat_plot[[new_col]] <- relative_change[, fg, region]
    }
    dat_long <- stack(dat_plot, select = paste0("Region ", 1:n_region))
    dat_long <- data.frame(Time = rep(dat_plot$Time, n_region), Region = dat_long$ind, Value = dat_long$values)
    
    plt = ggplot(dat_long,aes(x=start_year+Time/12,y=Value,color=Region))+
      geom_hline(yintercept = 0)+
      geom_line(size=1.5)+
      scale_color_manual(values = cols)+
      labs(x="Year",y="Biomass {(End-Start)/Start}",title=fgs$FG[fg])+
      theme_classic()+
      theme(legend.title = element_blank())
    
    filename = paste0("./Regional_Output/Biomass/Relative/",fgs$FG[fg]," Relative Biomass.png")
    ggsave(filename,plt,height=unit(7,"in"),width=unit(8,"in"))
  }
}

#-------------------------------------------------------------------------------
# READ ECOSPACE REGION CATCH MATRIX
#-------------------------------------------------------------------------------
# This function reads and processes catch output data for each region from
# an Ecospace run (assumed to be in CSV format). It returns an array of 
# biomass matrices that align with the Ecopath consumption matrix.
#
# The function requires:
# - header: Filepath header for the consumption CSV files
# - fgs: Dataframe of functional group names and numbers
# - regions: Number of model regions
# - ts: Number of timesteps
#
f.read_catch_timeseries = function(header, fgs, regions, ts){
  n_fg <- nrow(fgs)
  
  #Find number of fleets
  filename <- paste0(header, 1, "_Catch.csv")
  catch <- read.csv(filename, check.names = FALSE)  # Skip "Timestep" row
  interactions <- strsplit(colnames(catch)[2:ncol(catch)], "\\|")  # Get column names
  fleets <- unique(sapply(interactions[1:length(interactions)], `[`, 1))
  n_fleet = length(fleets)
  
  catch_array <- array(0, dim = c(ts,n_fg, n_fleet, regions))
  for (region in 1:regions) {  # Skips region 0, adjust if needed
    filename <- paste0(header, region, "_Catch.csv")
    if (file.exists(filename)) {
      catch <- read.csv(filename, check.names = FALSE)  # Skip "Timestep" row
      interactions <- strsplit(colnames(catch)[2:ncol(catch)], "\\|")  # Get column names
      for (action in 1:length(interactions)) {
        inter1 <- which(fleets == interactions[[action]][1])  # Index of the matching fleet
        inter2 <- fgs[fgs$FG == interactions[[action]][2], "Number"]  # Prey
        catch_array[,inter2, inter1 , region] <- catch[ , (action + 1)]
      }
    } else {
      cat("REGION", region, "DOES NOT EXIST! ADJUST IF IT SHOULD!\n")
    }
  }
  return(catch_array)
}

#-------------------------------------------------------------------------------
# PLOT ABSOLUTE CATCH TRENDS FOR EACH REGION
#-------------------------------------------------------------------------------
# This function reads the catch output data and creates a plot for each
# functional group, showing the time series trend in each region.
#
# The function requires:
# - catch_array: The biomass array created in @f.read_catch_timeseries
# - fgs: Dataframe of functional group names and numbers
# - cols: Vector of colorblind friendly colors
# - start_year: Start year of the model for plotting
#
f.plot_regional_catch_absolute_all_fleets = function(catch_array,fgs,cols,start_year){
  dir_path = "./Regional_Output/Catch/Aggregate/Absolute"
  if (!dir.exists(dir_path)){
    dir.create(dir_path,recursive = T)
  }
  fg_name = fgs$FG  
  n_region = dim(catch_array)[4]
  
  catch_all <- apply(catch_array, c(1,2, 4), sum)

  for (fg in 1:nrow(fgs)){
    dat_plot = data.frame(Time = seq(1,dim(catch_all)[1],1))
    
    for (region in 1:n_region){
      new_col = paste0("Region ",region)
      dat_plot[[new_col]] <- catch_all[, fg, region]
    }
    
    if (min(dat_plot$`Region 1`) != 0){
      dat_long <- stack(dat_plot, select = paste0("Region ", 1:n_region))
      dat_long <- data.frame(Time = rep(dat_plot$Time, n_region), Region = dat_long$ind, Value = dat_long$values)
      
      plt = ggplot(dat_long,aes(x=start_year+Time/12,y=Value,color=Region))+
        geom_line(size=1.5)+
        scale_color_manual(values = cols)+
        labs(x="Year",y="Catch (t/km2)",title=fgs$FG[fg])+
        theme_classic()+
        theme(legend.title = element_blank())
      
      filename = paste0("./Regional_Output/Catch/Aggregate/Absolute/",fgs$FG[fg]," Absolute Catch.png")
      ggsave(filename,plt,height=unit(7,"in"),width=unit(8,"in"))
    }
  }
}

#-------------------------------------------------------------------------------
# PLOT RELATIVE CATCH TRENDS FOR EACH REGION
#-------------------------------------------------------------------------------
# This function reads the catch output data and creates a plot for each
# functional group, showing the time series trend in each region.
#
# The function requires:
# - catch_array: The biomass array created in @f.read_biomass_timeseries
# - fgs: Dataframe of functional group names and numbers
# - cols: Vector of colorblind friendly colors
# - start_year: Start year of the model for plotting
#
f.plot_regional_catch_relative_all_fleets = function(catch_array,fgs,cols,start_year){
  dir_path = "./Regional_Output/Catch/Aggregate/Relative"
  if (!dir.exists(dir_path)){
    dir.create(dir_path,recursive = T)
  }
  fg_name = fgs$FG  
  n_region = dim(catch_array)[4]
  
  catch_all <- apply(catch_array, c(1,2, 4), sum)
  relative_change <- array(NA, dim = dim(catch_all))
  
  # Calculate relative change for each group and region
  for (group in 1:nrow(fgs)) {
    for (region in 1:n_region) {
      start_value <- catch_all[1, group, region]
      relative_change[, group, region] <- (catch_all[, group, region] - start_value) / start_value
    }
  }  
  
  for (fg in 1:nrow(fgs)){
    dat_plot = data.frame(Time = seq(1,dim(relative_change)[1],1))
    
    for (region in 1:n_region){
      new_col = paste0("Region ",region)
      dat_plot[[new_col]] <- relative_change[, fg, region]
    }
    
    if (!is.nan(dat_plot$`Region 1`[1])){
      dat_long <- stack(dat_plot, select = paste0("Region ", 1:n_region))
      dat_long <- data.frame(Time = rep(dat_plot$Time, n_region), Region = dat_long$ind, Value = dat_long$values)
      
      plt = ggplot(dat_long,aes(x=start_year+Time/12,y=Value,color=Region))+
        geom_hline(yintercept = 0)+
        geom_line(size=1.5)+
        scale_color_manual(values = cols)+
        labs(x="Year",y="Catch {(End-Start)/Start}",title=fgs$FG[fg])+
        theme_classic()+
        theme(legend.title = element_blank())
      
      filename = paste0("./Regional_Output/Catch/Aggregate/Relative/",fgs$FG[fg]," Relative Catch.png")
      ggsave(filename,plt,height=unit(7,"in"),width=unit(8,"in"))
    }
  }
}

#-------------------------------------------------------------------------------
# PLOT ABSOLUTE CATCH TRENDS FOR EACH REGION
#-------------------------------------------------------------------------------
# This function reads the catch output data and creates a plot for each
# functional group by each fishing fleet, showing the time series trend in each 
# region.
#
# The function requires:
# - header: Filepath header for the consumption CSV files
# - catch_array: The biomass array created in @f.read_biomass_timeseries
# - fgs: Dataframe of functional group names and numbers
# - cols: Vector of colorblind friendly colors
# - start_year: Start year of the model for plotting
#
f.plot_regional_catch_absolute_by_fleet = function(header,catch_array,fgs,cols,start_year){
  dir_path = "./Regional_Output/Catch/By Fleet/Absolute"
  if (!dir.exists(dir_path)){
    dir.create(dir_path,recursive = T)
  }
  fg_name = fgs$FG  
  n_region = dim(catch_array)[4]

  #Find fleets
  filename <- paste0(header, 1, "_Catch.csv")
  catch_fleet <- read.csv(filename, check.names = FALSE)  # Skip "Timestep" row
  interactions <- strsplit(colnames(catch_fleet)[2:ncol(catch_fleet)], "\\|")  # Get column names
  fleets <- unique(sapply(interactions[1:length(interactions)], `[`, 1))
  
  for (fleet in 1:length(fleets)){
    for (fg in 1:nrow(fgs)){
      dat_plot = data.frame(Time = seq(1,dim(catch_array)[1],1))
      
      for (region in 1:n_region){
        new_col = paste0("Region ",region)
        dat_plot[[new_col]] <- catch_array[, fg,fleet, region]
      }
      
      if (min(dat_plot$`Region 1`) != 0){
        #Skip if no catch
      
        dat_long <- stack(dat_plot, select = paste0("Region ", 1:n_region))
        dat_long <- data.frame(Time = rep(dat_plot$Time, n_region), Region = dat_long$ind, Value = dat_long$values)
        
        plt = ggplot(dat_long,aes(x=start_year+Time/12,y=Value,color=Region))+
          geom_line(size=1.5)+
          scale_color_manual(values = cols)+
          labs(x="Year",y="Catch (t/km2)",title=paste0(fleets[fleet]," to ",fgs$FG[fg]))+
          theme_classic()+
          theme(legend.title = element_blank())
        
        filename = paste0("./Regional_Output/Catch/By Fleet/Absolute/",fleets[fleet],"_",fgs$FG[fg]," Absolute Catch.png")
        ggsave(filename,plt,height=unit(7,"in"),width=unit(8,"in"))
      }
    }
  }
}

#-------------------------------------------------------------------------------
# PLOT RELATIVE CATCH TRENDS FOR EACH REGION
#-------------------------------------------------------------------------------
# This function reads the catch output data and creates a plot for each
# functional group by each fishing fleet, showing the time series trend in each 
# region.
#
# The function requires:
# - header: Filepath header for the consumption CSV files
# - catch_array: The biomass array created in @f.read_biomass_timeseries
# - fgs: Dataframe of functional group names and numbers
# - cols: Vector of colorblind friendly colors
# - start_year: Start year of the model for plotting
#
f.plot_regional_catch_relative_by_fleet = function(header,catch_array,fgs,cols,start_year){
  dir_path = "./Regional_Output/Catch/By Fleet/Relative"
  if (!dir.exists(dir_path)){
    dir.create(dir_path,recursive = T)
  }
  fg_name = fgs$FG  
  n_region = dim(catch_array)[4]
  
  #Find fleets
  filename <- paste0(header, 1, "_Catch.csv")
  catch_fleet <- read.csv(filename, check.names = FALSE)  # Skip "Timestep" row
  interactions <- strsplit(colnames(catch_fleet)[2:ncol(catch_fleet)], "\\|")  # Get column names
  fleets <- unique(sapply(interactions[1:length(interactions)], `[`, 1))
  
  relative_change <- array(NA, dim = dim(catch_array))
  
  # Calculate relative change for each group and region
  for (group in 1:nrow(fgs)) {
    for (fleet in 1:length(fleets)){
      for (region in 1:n_region) {
        start_value <- catch_array[1, group,fleet, region]
        relative_change[, group,fleet, region] <- (catch_array[, group,fleet, region] - start_value) / start_value
      }
    }
  }  
  
  for (fleet in 1:length(fleets)){
    for (fg in 1:nrow(fgs)){
      dat_plot = data.frame(Time = seq(1,dim(relative_change)[1],1))
      
      for (region in 1:n_region){
        new_col = paste0("Region ",region)
        dat_plot[[new_col]] <- relative_change[, fg,fleet, region]
      }

      if (!is.nan(dat_plot$`Region 1`[1])){
        #Skip if no catch
        
        dat_long <- stack(dat_plot, select = paste0("Region ", 1:n_region))
        dat_long <- data.frame(Time = rep(dat_plot$Time, n_region), Region = dat_long$ind, Value = dat_long$values)
        
        plt = ggplot(dat_long,aes(x=start_year+Time/12,y=Value,color=Region))+
          geom_hline(yintercept = 0)+
          geom_line(size=1.5)+
          scale_color_manual(values = cols)+
          labs(x="Year",y="Catch (t/km2)",title=paste0(fleets[fleet]," to ",fgs$FG[fg]))+
          theme_classic()+
          theme(legend.title = element_blank())
        
        filename = paste0("./Regional_Output/Catch/By Fleet/Relative/",fleets[fleet],"_",fgs$FG[fg]," Absolute Catch.png")
        ggsave(filename,plt,height=unit(7,"in"),width=unit(8,"in"))
      }
    }
  }
}

#-------------------------------------------------------------------------------
# READ ECOSPACE REGION LANDINGS MATRIX
#-------------------------------------------------------------------------------
# This function reads and processes catch output data for each region from
# an Ecospace run (assumed to be in CSV format). It returns an array of 
# biomass matrices that align with the Ecopath consumption matrix.
#
# The function requires:
# - header: Filepath header for the consumption CSV files
# - fgs: Dataframe of functional group names and numbers
# - regions: Number of model regions
# - ts: Number of timesteps
#
f.read_landings_timeseries = function(header, fgs, regions, ts){
  n_fg <- nrow(fgs)
  
  #Find number of fleets
  filename <- paste0(header, 1, "_Landings.csv")
  landing <- read.csv(filename, check.names = FALSE)  # Skip "Timestep" row
  interactions <- strsplit(colnames(landing)[2:ncol(landing)], "\\|")  # Get column names
  fleets <- unique(sapply(interactions[1:length(interactions)], `[`, 1))
  n_fleet = length(fleets)
  
  landing_array <- array(0, dim = c(ts,n_fg, n_fleet, regions))
  for (region in 1:regions) {  # Skips region 0, adjust if needed
    filename <- paste0(header, region, "_Landings.csv")
    if (file.exists(filename)) {
      landing <- read.csv(filename, check.names = FALSE)  # Skip "Timestep" row
      interactions <- strsplit(colnames(landing)[2:ncol(landing)], "\\|")  # Get column names
      for (action in 1:length(interactions)) {
        inter1 <- which(fleets == interactions[[action]][1])  # Index of the matching fleet
        inter2 <- fgs[fgs$FG == interactions[[action]][2], "Number"]  # Prey
        landing_array[,inter2, inter1 , region] <- landing[ , (action + 1)]
      }
    } else {
      cat("REGION", region, "DOES NOT EXIST! ADJUST IF IT SHOULD!\n")
    }
  }
  return(landing_array)
}

#-------------------------------------------------------------------------------
# PLOT ABSOLUTE LANDINGS TRENDS FOR EACH REGION
#-------------------------------------------------------------------------------
# This function reads the landings output data and creates a plot for each
# functional group, showing the time series trend in each region.
#
# The function requires:
# - landing_array: The landings array created in @f.read_landings_timeseries
# - fgs: Dataframe of functional group names and numbers
# - cols: Vector of colorblind friendly colors
# - start_year: Start year of the model for plotting
#
f.plot_regional_landings_absolute_all_fleets = function(landing_array,fgs,cols,start_year){
  dir_path = "./Regional_Output/Landings/Aggregate/Absolute"
  if (!dir.exists(dir_path)){
    dir.create(dir_path,recursive = T)
  }
  fg_name = fgs$FG  
  n_region = dim(landing_array)[4]
  
  landing_all <- apply(landing_array, c(1,2, 4), sum)
  
  for (fg in 1:nrow(fgs)){
    dat_plot = data.frame(Time = seq(1,dim(landing_all)[1],1))
    
    for (region in 1:n_region){
      new_col = paste0("Region ",region)
      dat_plot[[new_col]] <- landing_all[, fg, region]
    }
    
    if (min(dat_plot$`Region 1`) != 0){
      dat_long <- stack(dat_plot, select = paste0("Region ", 1:n_region))
      dat_long <- data.frame(Time = rep(dat_plot$Time, n_region), Region = dat_long$ind, Value = dat_long$values)
      
      plt = ggplot(dat_long,aes(x=start_year+Time/12,y=Value,color=Region))+
        geom_line(size=1.5)+
        scale_color_manual(values = cols)+
        labs(x="Year",y="Landings (t/km2)",title=fgs$FG[fg])+
        theme_classic()+
        theme(legend.title = element_blank())
      
      filename = paste0("./Regional_Output/Landings/Aggregate/Absolute/",fgs$FG[fg]," Absolute Landings.png")
      ggsave(filename,plt,height=unit(7,"in"),width=unit(8,"in"))
    }
  }
}

#-------------------------------------------------------------------------------
# PLOT RELATIVE LANDING TRENDS FOR EACH REGION
#-------------------------------------------------------------------------------
# This function reads the landings output data and creates a plot for each
# functional group, showing the time series trend in each region.
#
# The function requires:
# - landing_array: The biomass array created in @f.read_landings_timeseries
# - fgs: Dataframe of functional group names and numbers
# - cols: Vector of colorblind friendly colors
# - start_year: Start year of the model for plotting
#
f.plot_regional_landings_relative_all_fleets = function(landing_array,fgs,cols,start_year){
  dir_path = "./Regional_Output/Landings/Aggregate/Relative"
  if (!dir.exists(dir_path)){
    dir.create(dir_path,recursive = T)
  }
  fg_name = fgs$FG  
  n_region = dim(landing_array)[4]
  
  landing_all <- apply(landing_array, c(1,2, 4), sum)
  relative_change <- array(NA, dim = dim(landing_all))
  
  # Calculate relative change for each group and region
  for (group in 1:nrow(fgs)) {
    for (region in 1:n_region) {
      start_value <- landing_all[1, group, region]
      relative_change[, group, region] <- (landing_all[, group, region] - start_value) / start_value
    }
  }  
  
  for (fg in 1:nrow(fgs)){
    dat_plot = data.frame(Time = seq(1,dim(relative_change)[1],1))
    
    for (region in 1:n_region){
      new_col = paste0("Region ",region)
      dat_plot[[new_col]] <- relative_change[, fg, region]
    }
    
    if (!is.nan(dat_plot$`Region 1`[1])){
      dat_long <- stack(dat_plot, select = paste0("Region ", 1:n_region))
      dat_long <- data.frame(Time = rep(dat_plot$Time, n_region), Region = dat_long$ind, Value = dat_long$values)
      
      plt = ggplot(dat_long,aes(x=start_year+Time/12,y=Value,color=Region))+
        geom_hline(yintercept = 0)+
        geom_line(size=1.5)+
        scale_color_manual(values = cols)+
        labs(x="Year",y="Landings {(End-Start)/Start}",title=fgs$FG[fg])+
        theme_classic()+
        theme(legend.title = element_blank())
      
      filename = paste0("./Regional_Output/Landings/Aggregate/Relative/",fgs$FG[fg]," Relative Landings.png")
      ggsave(filename,plt,height=unit(7,"in"),width=unit(8,"in"))
    }
  }
}

#-------------------------------------------------------------------------------
# PLOT ABSOLUTE LANDINGS TRENDS FOR EACH REGION
#-------------------------------------------------------------------------------
# This function reads the landings output data and creates a plot for each
# functional group by each fishing fleet, showing the time series trend in each 
# region.
#
# The function requires:
# - header: Filepath header for the consumption CSV files
# - landing_array: The biomass array created in @f.read_landings_timeseries
# - fgs: Dataframe of functional group names and numbers
# - cols: Vector of colorblind friendly colors
# - start_year: Start year of the model for plotting
#
f.plot_regional_landings_absolute_by_fleet = function(header,landing_array,fgs,cols,start_year){
  dir_path = "./Regional_Output/Landings/By Fleet/Absolute"
  if (!dir.exists(dir_path)){
    dir.create(dir_path,recursive = T)
  }
  fg_name = fgs$FG  
  n_region = dim(landing_array)[4]
  
  #Find fleets
  filename <- paste0(header, 1, "_Catch.csv")
  landing_fleet <- read.csv(filename, check.names = FALSE)  # Skip "Timestep" row
  interactions <- strsplit(colnames(landing_fleet)[2:ncol(landing_fleet)], "\\|")  # Get column names
  fleets <- unique(sapply(interactions[1:length(interactions)], `[`, 1))
  
  for (fleet in 1:length(fleets)){
    for (fg in 1:nrow(fgs)){
      dat_plot = data.frame(Time = seq(1,dim(landing_array)[1],1))
      
      for (region in 1:n_region){
        new_col = paste0("Region ",region)
        dat_plot[[new_col]] <- landing_array[, fg,fleet, region]
      }
      
      if (min(dat_plot$`Region 1`) != 0){
        #Skip if no catch
        
        dat_long <- stack(dat_plot, select = paste0("Region ", 1:n_region))
        dat_long <- data.frame(Time = rep(dat_plot$Time, n_region), Region = dat_long$ind, Value = dat_long$values)
        
        plt = ggplot(dat_long,aes(x=start_year+Time/12,y=Value,color=Region))+
          geom_line(size=1.5)+
          scale_color_manual(values = cols)+
          labs(x="Year",y="Landings (t/km2)",title=paste0(fleets[fleet]," to ",fgs$FG[fg]))+
          theme_classic()+
          theme(legend.title = element_blank())
        
        filename = paste0("./Regional_Output/Landings/By Fleet/Absolute/",fleets[fleet],"_",fgs$FG[fg]," Absolute Landings.png")
        ggsave(filename,plt,height=unit(7,"in"),width=unit(8,"in"))
      }
    }
  }
}

#-------------------------------------------------------------------------------
# PLOT RELATIVE LANDINGS TRENDS FOR EACH REGION
#-------------------------------------------------------------------------------
# This function reads the landings output data and creates a plot for each
# functional group by each fishing fleet, showing the time series trend in each 
# region.
#
# The function requires:
# - header: Filepath header for the consumption CSV files
# - landing_array: The biomass array created in @f.read_landings_timeseries
# - fgs: Dataframe of functional group names and numbers
# - cols: Vector of colorblind friendly colors
# - start_year: Start year of the model for plotting
#
f.plot_regional_landings_relative_by_fleet = function(header,landing_array,fgs,cols,start_year){
  dir_path = "./Regional_Output/Landings/By Fleet/Relative"
  if (!dir.exists(dir_path)){
    dir.create(dir_path,recursive = T)
  }
  fg_name = fgs$FG  
  n_region = dim(landing_array)[4]
  
  #Find fleets
  filename <- paste0(header, 1, "_Catch.csv")
  landing_fleet <- read.csv(filename, check.names = FALSE)  # Skip "Timestep" row
  interactions <- strsplit(colnames(landing_fleet)[2:ncol(landing_fleet)], "\\|")  # Get column names
  fleets <- unique(sapply(interactions[1:length(interactions)], `[`, 1))
  
  relative_change <- array(NA, dim = dim(landing_array))
  
  # Calculate relative change for each group and region
  for (group in 1:nrow(fgs)) {
    for (fleet in 1:length(fleets)){
      for (region in 1:n_region) {
        start_value <- landing_array[1, group,fleet, region]
        relative_change[, group,fleet, region] <- (landing_array[, group,fleet, region] - start_value) / start_value
      }
    }
  }  
  
  for (fleet in 1:length(fleets)){
    for (fg in 1:nrow(fgs)){
      dat_plot = data.frame(Time = seq(1,dim(relative_change)[1],1))
      
      for (region in 1:n_region){
        new_col = paste0("Region ",region)
        dat_plot[[new_col]] <- relative_change[, fg,fleet, region]
      }
      
      if (!is.nan(dat_plot$`Region 1`[1])){
        #Skip if no catch
        
        dat_long <- stack(dat_plot, select = paste0("Region ", 1:n_region))
        dat_long <- data.frame(Time = rep(dat_plot$Time, n_region), Region = dat_long$ind, Value = dat_long$values)
        
        plt = ggplot(dat_long,aes(x=start_year+Time/12,y=Value,color=Region))+
          geom_hline(yintercept = 0)+
          geom_line(size=1.5)+
          scale_color_manual(values = cols)+
          labs(x="Year",y="Catch (t/km2)",title=paste0(fleets[fleet]," to ",fgs$FG[fg]))+
          theme_classic()+
          theme(legend.title = element_blank())
        
        filename = paste0("./Regional_Output/Landings/By Fleet/Relative/",fleets[fleet],"_",fgs$FG[fg]," Absolute Landings.png")
        ggsave(filename,plt,height=unit(7,"in"),width=unit(8,"in"))
      }
    }
  }
}

#-------------------------------------------------------------------------------
# PLOT ABSOLUTE DISCARDS TRENDS FOR EACH REGION
#-------------------------------------------------------------------------------
# This function reads the discards output data and creates a plot for each
# functional group, showing the time series trend in each region.
#
# The function requires:
# - discard_array: The array created by subtracting catch and landing arrays
# - fgs: Dataframe of functional group names and numbers
# - cols: Vector of colorblind friendly colors
# - start_year: Start year of the model for plotting
#
f.plot_regional_discards_absolute_all_fleets = function(discard_array,fgs,cols,start_year){
  dir_path = "./Regional_Output/Discards/Aggregate/Absolute"
  if (!dir.exists(dir_path)){
    dir.create(dir_path,recursive = T)
  }
  fg_name = fgs$FG  
  n_region = dim(discard_array)[4]
  
  discard_all <- apply(discard_array, c(1,2, 4), sum)
  
  for (fg in 1:nrow(fgs)){
    dat_plot = data.frame(Time = seq(1,dim(discard_all)[1],1))
    
    for (region in 1:n_region){
      new_col = paste0("Region ",region)
      dat_plot[[new_col]] <- discard_all[, fg, region]
    }
    
    if (min(dat_plot$`Region 1`) != 0){
      dat_long <- stack(dat_plot, select = paste0("Region ", 1:n_region))
      dat_long <- data.frame(Time = rep(dat_plot$Time, n_region), Region = dat_long$ind, Value = dat_long$values)
      
      plt = ggplot(dat_long,aes(x=start_year+Time/12,y=Value,color=Region))+
        geom_line(size=1.5)+
        scale_color_manual(values = cols)+
        labs(x="Year",y="Discards (t/km2)",title=fgs$FG[fg])+
        theme_classic()+
        theme(legend.title = element_blank())
      
      filename = paste0("./Regional_Output/Discards/Aggregate/Absolute/",fgs$FG[fg]," Absolute Discards.png")
      ggsave(filename,plt,height=unit(7,"in"),width=unit(8,"in"))
    }
  }
}

#-------------------------------------------------------------------------------
# PLOT RELATIVE DISCARD TRENDS FOR EACH REGION
#-------------------------------------------------------------------------------
# This function reads the discards output data and creates a plot for each
# functional group, showing the time series trend in each region.
#
# The function requires:
# - discard_array: The array created by subtracting catch and landing arrays
# - fgs: Dataframe of functional group names and numbers
# - cols: Vector of colorblind friendly colors
# - start_year: Start year of the model for plotting
#
f.plot_regional_discards_relative_all_fleets = function(discard_array,fgs,cols,start_year){
  dir_path = "./Regional_Output/Discards/Aggregate/Relative"
  if (!dir.exists(dir_path)){
    dir.create(dir_path,recursive = T)
  }
  fg_name = fgs$FG  
  n_region = dim(discard_array)[4]
  
  discard_all <- apply(discard_array, c(1,2, 4), sum)
  relative_change <- array(NA, dim = dim(discard_all))
  
  # Calculate relative change for each group and region
  for (group in 1:nrow(fgs)) {
    for (region in 1:n_region) {
      start_value <- discard_all[1, group, region]
      relative_change[, group, region] <- (discard_all[, group, region] - start_value) / start_value
    }
  }  
  
  for (fg in 1:nrow(fgs)){
    dat_plot = data.frame(Time = seq(1,dim(relative_change)[1],1))
    
    for (region in 1:n_region){
      new_col = paste0("Region ",region)
      dat_plot[[new_col]] <- relative_change[, fg, region]
    }
    
    if (!is.nan(dat_plot$`Region 1`[1])){
      dat_long <- stack(dat_plot, select = paste0("Region ", 1:n_region))
      dat_long <- data.frame(Time = rep(dat_plot$Time, n_region), Region = dat_long$ind, Value = dat_long$values)
      
      plt = ggplot(dat_long,aes(x=start_year+Time/12,y=Value,color=Region))+
        geom_hline(yintercept = 0)+
        geom_line(size=1.5)+
        scale_color_manual(values = cols)+
        labs(x="Year",y="Discards {(End-Start)/Start}",title=fgs$FG[fg])+
        theme_classic()+
        theme(legend.title = element_blank())
      
      filename = paste0("./Regional_Output/Discards/Aggregate/Relative/",fgs$FG[fg]," Relative Discards.png")
      ggsave(filename,plt,height=unit(7,"in"),width=unit(8,"in"))
    }
  }
}

#-------------------------------------------------------------------------------
# PLOT ABSOLUTE DISCARDS TRENDS FOR EACH REGION
#-------------------------------------------------------------------------------
# This function reads the discards output data and creates a plot for each
# functional group by each fishing fleet, showing the time series trend in each 
# region.
#
# The function requires:
# - header: Filepath header for the consumption CSV files
# - discard_array: The array created by subtracting catch and landing arrays
# - fgs: Dataframe of functional group names and numbers
# - cols: Vector of colorblind friendly colors
# - start_year: Start year of the model for plotting
#
f.plot_regional_discards_absolute_by_fleet = function(header,discard_array,fgs,cols,start_year){
  dir_path = "./Regional_Output/Discards/By Fleet/Absolute"
  if (!dir.exists(dir_path)){
    dir.create(dir_path,recursive = T)
  }
  fg_name = fgs$FG  
  n_region = dim(discard_array)[4]
  
  #Find fleets
  filename <- paste0(header, 1, "_Catch.csv")
  discard_fleet <- read.csv(filename, check.names = FALSE)  # Skip "Timestep" row
  interactions <- strsplit(colnames(discard_fleet)[2:ncol(discard_fleet)], "\\|")  # Get column names
  fleets <- unique(sapply(interactions[1:length(interactions)], `[`, 1))
  
  for (fleet in 1:length(fleets)){
    for (fg in 1:nrow(fgs)){
      dat_plot = data.frame(Time = seq(1,dim(discard_array)[1],1))
      
      for (region in 1:n_region){
        new_col = paste0("Region ",region)
        dat_plot[[new_col]] <- discard_array[, fg,fleet, region]
      }
      
      if (min(dat_plot$`Region 1`) != 0){
        #Skip if no catch
        
        dat_long <- stack(dat_plot, select = paste0("Region ", 1:n_region))
        dat_long <- data.frame(Time = rep(dat_plot$Time, n_region), Region = dat_long$ind, Value = dat_long$values)
        
        plt = ggplot(dat_long,aes(x=start_year+Time/12,y=Value,color=Region))+
          geom_line(size=1.5)+
          scale_color_manual(values = cols)+
          labs(x="Year",y="Discards (t/km2)",title=paste0(fleets[fleet]," to ",fgs$FG[fg]))+
          theme_classic()+
          theme(legend.title = element_blank())
        
        filename = paste0("./Regional_Output/Discards/By Fleet/Absolute/",fleets[fleet],"_",fgs$FG[fg]," Absolute Discards.png")
        ggsave(filename,plt,height=unit(7,"in"),width=unit(8,"in"))
      }
    }
  }
}

#-------------------------------------------------------------------------------
# PLOT RELATIVE DISCARDS TRENDS FOR EACH REGION
#-------------------------------------------------------------------------------
# This function reads the discards output data and creates a plot for each
# functional group by each fishing fleet, showing the time series trend in each 
# region.
#
# The function requires:
# - header: Filepath header for the consumption CSV files
# - discard_array: The array created by subtracting catch and landing arrays
# - fgs: Dataframe of functional group names and numbers
# - cols: Vector of colorblind friendly colors
# - start_year: Start year of the model for plotting
#
f.plot_regional_discards_relative_by_fleet = function(header,discard_array,fgs,cols,start_year){
  dir_path = "./Regional_Output/Discards/By Fleet/Relative"
  if (!dir.exists(dir_path)){
    dir.create(dir_path,recursive = T)
  }
  fg_name = fgs$FG  
  n_region = dim(discard_array)[4]
  
  #Find fleets
  filename <- paste0(header, 1, "_Catch.csv")
  discard_fleet <- read.csv(filename, check.names = FALSE)  # Skip "Timestep" row
  interactions <- strsplit(colnames(discard_fleet)[2:ncol(discard_fleet)], "\\|")  # Get column names
  fleets <- unique(sapply(interactions[1:length(interactions)], `[`, 1))
  
  relative_change <- array(NA, dim = dim(discard_array))
  
  # Calculate relative change for each group and region
  for (group in 1:nrow(fgs)) {
    for (fleet in 1:length(fleets)){
      for (region in 1:n_region) {
        start_value <- discard_array[1, group,fleet, region]
        relative_change[, group,fleet, region] <- (discard_array[, group,fleet, region] - start_value) / start_value
      }
    }
  }  
  
  for (fleet in 1:length(fleets)){
    for (fg in 1:nrow(fgs)){
      dat_plot = data.frame(Time = seq(1,dim(relative_change)[1],1))
      
      for (region in 1:n_region){
        new_col = paste0("Region ",region)
        dat_plot[[new_col]] <- relative_change[, fg,fleet, region]
      }
      
      if (!is.nan(dat_plot$`Region 1`[1])){
        #Skip if no catch
        
        dat_long <- stack(dat_plot, select = paste0("Region ", 1:n_region))
        dat_long <- data.frame(Time = rep(dat_plot$Time, n_region), Region = dat_long$ind, Value = dat_long$values)
        
        plt = ggplot(dat_long,aes(x=start_year+Time/12,y=Value,color=Region))+
          geom_hline(yintercept = 0)+
          geom_line(size=1.5)+
          scale_color_manual(values = cols)+
          labs(x="Year",y="Discards (t/km2)",title=paste0(fleets[fleet]," to ",fgs$FG[fg]))+
          theme_classic()+
          theme(legend.title = element_blank())
        
        filename = paste0("./Regional_Output/Discards/By Fleet/Relative/",fleets[fleet],"_",fgs$FG[fg]," Absolute Discards.png")
        ggsave(filename,plt,height=unit(7,"in"),width=unit(8,"in"))
      }
    }
  }
}



#-------------------------------------------------------------------------------
## Note: This file must be in the same directory as your output, or else you must specify the directory below
#setwd("YOUR ECOSPACE OUTPUT DIRECTORY")

#Dependencies
library(scales)
library(ggplot2)

# Define variables
region_header <- "Ecospace_Average_Region_"
n_region <- 4
year_start <- 1980
year_end <- 2017
n_year <- year_end-year_start
n_ts <- n_year * 12

# Gather functional group names and numbers
biomass_all <- colnames(read.csv("Ecospace_Annual_Average_Biomass.csv", check.names = FALSE))
fgs <- data.frame(FG = biomass_all[2:length(biomass_all)], Number = seq(1, length(biomass_all) - 1))

# Run consumption time series function
consumption <- f.read_consumption_timeseries(region_header, fgs, n_region, n_ts)

# Run biomass time series function
biomass <- f.read_biomass_timeseries(region_header, fgs, n_region, n_ts)

# Find colorblind friendly colors
palette <- colorblind_palette(n_region) 

#Plot Biomass trends by region
f.plot_regional_biomass_absolute(biomass,fgs,palette,year_start)
f.plot_regional_biomass_relative(biomass,fgs,palette,year_start)

#Run catch time series function
catch <- f.read_catch_timeseries(region_header,fgs,n_region,n_ts)

#Plot total catch (all fleets aggregated) by region
f.plot_regional_catch_absolute_all_fleets(catch,fgs,palette,year_start)
f.plot_regional_catch_relative_all_fleets(catch,fgs,palette,year_start)

#Plot total catch by fleet by region
f.plot_regional_catch_absolute_by_fleet(region_header,catch,fgs,palette,year_start)
f.plot_regional_catch_relative_by_fleet(region_header,catch,fgs,palette,year_start)

#Run Landings time series function
landings <- f.read_landings_timeseries(region_header, fgs, n_region, n_ts)

#Plot total landings (all fleets aggregated) by region
f.plot_regional_landings_absolute_all_fleets(landings,fgs,palette,year_start)
f.plot_regional_landings_relative_all_fleets(landings,fgs,palette,year_start)

#Plot total landings by fleet by region
f.plot_regional_landings_absolute_by_fleet(region_header,landings,fgs,palette,year_start)
f.plot_regional_landings_relative_by_fleet(region_header,landings,fgs,palette,year_start)

#Calculate discards (Catch - Landings)
discards <- catch - landings

#Plot total discards (all fleets aggregated) by region
f.plot_regional_discards_absolute_all_fleets(discards,fgs,palette,year_start)
f.plot_regional_discards_relative_all_fleets(discards,fgs,palette,year_start)

#Plot total discards by fleet by region
f.plot_regional_discards_absolute_by_fleet(region_header,discards,fgs,palette,year_start)
f.plot_regional_discards_relative_by_fleet(region_header,discards,fgs,palette,year_start)