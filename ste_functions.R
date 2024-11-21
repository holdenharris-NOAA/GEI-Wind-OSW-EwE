#-----------------------------------------------
#  PROCESSING FUNCTIONS
#-----------------------------------------------
f.process.distributions <- function(type, scenarios, groups, rows, columns, times, results) {
  # Preallocate the data array
  data_array <- array(0, dim = c(rows, columns, times, nrow(groups), length(scenarios)))
  
  # Initialize lons and lats here, before processing scenarios
  lons <- NULL
  lats <- NULL
  
  # Loop over scenarios
  for (scen in seq_along(scenarios)) {
    # Filter files by type
    files <- list.files(paste0("./Experiments/", scenarios[scen], "/asc/"), pattern = paste0(type, ".*\\.asc$"), full.names = TRUE)
    
    if (length(files) == 0) next  # Skip if no files found
    
    # Loop over functional groups
    for (fg in seq_len(nrow(groups))) {
      # Escape special characters in group names for grep
      pattern <- gsub("([()\\-\\+])", "\\\\\\1", groups$Name[fg])
      
      # Filter files by group pattern
      fg_files <- grep(pattern, files, value = TRUE)
      if (length(fg_files) == 0) next  # Skip if no group files found
      
      # Initialize extent and coordinate grids only once
      dat <- raster(fg_files[1])
      extent_dat <- extent(dat)
      lons <- seq(from = xmin(extent_dat), to = xmax(extent_dat), length.out = columns)
      lats <- seq(from = ymin(extent_dat), to = ymax(extent_dat), length.out = rows)

      # Loop over time steps
      for (ts in seq_along(fg_files)) {
        dat <- raster(fg_files[ts])
        data_points <- as.data.frame(rasterToPoints(dat))
        colnames(data_points)[3] <- type
        
        # Vectorized approach to find closest indices
        data_points$x_index <- findInterval(data_points$x, lons)
        data_points$y_index <- findInterval(data_points$y, lats)
        
        # Filter valid indices (within bounds)
        valid_idx <- with(data_points, x_index > 0 & x_index <= columns & y_index > 0 & y_index <= rows)
        data_points <- data_points[valid_idx, ]
        
        # Use matrix indexing to update the data array
        if (nrow(data_points) > 0) {
          data_array[cbind(data_points$y_index, data_points$x_index, ts, fg, scen)] <- data_points[[type]]
        }
      }
      cat("Processed group:", fg, "\n")
    }
  }
  
  # Save results
  save(lons, lats, data_array, file = paste0(results, "/", type, ".RData"))
}

f.process.indicators <- function(type, scenarios, indicators, rows, columns, times, results) {
  # Preallocate the data array
  data_array <- array(0, dim = c(rows, columns, times, nrow(indicators), length(scenarios)))
  
  # Loop over scenarios
  for (scen in seq_along(scenarios)) {
    files <- list.files(paste0("./Experiments/", scenarios[scen], "/"), pattern = ".asc$", full.names = TRUE)
    
    # Loop over indicators
    for (ind in seq_len(nrow(indicators))) {
      ind_files <- grep(indicators$Name[ind], files, value = TRUE)
      
      if (length(ind_files) > 0) {
        # Initialize extent and coordinate grids only once
        dat <- raster(ind_files[1])
        extent_dat <- extent(dat)
        lons <- seq(from = xmin(extent_dat), to = xmax(extent_dat), length.out = columns)
        lats <- seq(from = ymin(extent_dat), to = ymax(extent_dat), length.out = rows)
        
        # Loop over time steps
        for (time in seq_along(ind_files)) {
          dat <- raster(ind_files[time])
          data_points <- as.data.frame(rasterToPoints(dat))
          colnames(data_points)[3] <- type
          
          # Vectorized approach to find closest indices
          data_points$x_index <- findInterval(data_points$x, lons)
          data_points$y_index <- findInterval(data_points$y, lats)
          
          # Filter valid indices
          valid_idx <- with(data_points, x_index > 0 & y_index > 0)
          data_points <- data_points[valid_idx, ]
          
          # Fill the data_array using matrix indexing
          data_array[cbind(data_points$y_index, data_points$x_index, time, ind, scen)] <- data_points[[type]]
        }
      }
      cat("Processed indicator:", ind, "\n")
    }
  }
  
  save(lons, lats, data_array, file = paste0(results, "/", type, ".RData"))
}

f.process.biomass.trends = function(scenarios,groups,times,results,regions){
  times_adj = times*12
  
  #Check if file exists
  filename = paste0("./Experiments/",scenarios[1],"/Ecospace_Average_Biomass.csv")
  if (file.exists(filename)){
    data_array <- array(0, dim = c(times_adj, nrow(groups),regions+1,length(scenarios)))
  } else {
    data_array <- array(0, dim = c(times_adj, nrow(groups),regions,length(scenarios)))
  }
  for (scen in 1:n_distinct(scenarios)){
    #Region attribute
    for (region in 1:regions){
      dat = read.csv(paste0("./Experiments/",scenarios[scen],"/Ecospace_Average_Region_",region,"_Biomass.csv"),check.names = F)
      data_array[,,region,scen] = as.matrix(dat[,-1])
    }
    #Global attribute
    if (file.exists(filename)){
      dat = read.csv(paste0("./Experiments/",scenarios[scen],"/Ecospace_Average_Biomass.csv"),check.names = F)
      data_array[,,regions+1,scen] = as.matrix(dat[,-1])
    }
  }
  save(data_array, file = paste0(results, "/Biomass Trend.RData"))
}

f.process.catch.trends = function(type,scenarios,fish,groups,times,results,regions){
  times_adj = times*12
  
  n_fg <- nrow(groups)
  n_fleet = nrow(fish)
  filename = paste0("./Experiments/",scenarios[1],"/Ecospace_Average_",type,".csv")
  if (file.exists(filename)){
    data_array <- array(0, dim = c(times_adj,n_fg, n_fleet, regions+1,n_distinct(scenarios)))
  } else {
    data_array <- array(0, dim = c(times_adj,n_fg, n_fleet, regions,n_distinct(scenarios)))
  }

  #Find number of fleets
  for (scen in 1:n_distinct(scens)){
    filename = paste0("./Experiments/",scenarios[scen],"/Ecospace_Average_",type,".csv")
    if (file.exists(filename)){
      dat <- read.csv(paste0("./Experiments/",scenarios[scen],"/Ecospace_Average_",type,".csv"), check.names = FALSE)
      interactions <- strsplit(colnames(dat)[2:ncol(dat)], "\\|")  # Get column names
      for (action in 1:length(interactions)) {
        inter1 <- which(fish$Name == interactions[[action]][1])  # Index of the matching fleet
        inter2 <- groups[groups$Name == interactions[[action]][2], "Number"]  # Prey
        data_array[,inter2, inter1 , regions+1,scen] <- dat[ , (action + 1)]
      }
    }
    
    for (region in 1:regions) {  # Skips region 0, adjust if needed
      dat <- read.csv(paste0("./Experiments/",scenarios[scen],"/Ecospace_Average_Region_",region,"_",type,".csv"), check.names = FALSE) 
      interactions <- strsplit(colnames(dat)[2:ncol(dat)], "\\|")  # Get column names
      for (action in 1:length(interactions)) {
        inter1 <- which(fish$Name == interactions[[action]][1])  # Index of the matching fleet
        inter2 <- groups[groups$Name == interactions[[action]][2], "Number"]  # Prey
        data_array[,inter2, inter1 , region,scen] <- dat[ , (action + 1)]
      }
    }
  }
  save(data_array, file = paste0(results, "/",type," Trend.RData"))
}

f.process.annual.biomass.trends = function(scenarios,groups,times,results,regions){
  times_adj = times
  
  #Check if file exists
  filename = paste0("./Experiments/",scenarios[1],"/Ecospace_Annual_Average_Biomass.csv")
  if (file.exists(filename)){
    data_array <- array(0, dim = c(times_adj, nrow(groups),regions+1,length(scenarios)))
  } else {
    data_array <- array(0, dim = c(times_adj, nrow(groups),regions,length(scenarios)))
  }
  for (scen in 1:n_distinct(scenarios)){
    #Region attribute
    for (region in 1:regions){
      dat = read.csv(paste0("./Experiments/",scenarios[scen],"/Ecospace_Annual_Average_Region_",region,"_Biomass.csv"),check.names = F)
      data_array[,,region,scen] = as.matrix(dat[,-1])
    }
    #Global attribute
    if (file.exists(filename)){
      dat = read.csv(paste0("./Experiments/",scenarios[scen],"/Ecospace_Annual_Average_Biomass.csv"),check.names = F)
      data_array[,,regions+1,scen] = as.matrix(dat[,-1])
    }
  }
  save(data_array, file = paste0(results, "/Annual Biomass Trend.RData"))
}

f.process.annual.catch.trends = function(type,scenarios,fish,groups,times,results,regions){
  times_adj = times
  
  n_fg <- nrow(groups)
  n_fleet = nrow(fish)
  filename = paste0("./Experiments/",scenarios[1],"/Ecospace_Annual_Average_",type,".csv")
  if (file.exists(filename)){
    data_array <- array(0, dim = c(times_adj,n_fg, n_fleet, regions+1,n_distinct(scenarios)))
  } else {
    data_array <- array(0, dim = c(times_adj,n_fg, n_fleet, regions,n_distinct(scenarios)))
  }
  
  #Find number of fleets
  for (scen in 1:n_distinct(scens)){
    filename = paste0("./Experiments/",scenarios[scen],"/Ecospace_Annual_Average_",type,".csv")
    if (file.exists(filename)){
      dat <- read.csv(paste0("./Experiments/",scenarios[scen],"/Ecospace_Annual_Average_",type,".csv"), check.names = FALSE)
      interactions <- strsplit(colnames(dat)[2:ncol(dat)], "\\|")  # Get column names
      for (action in 1:length(interactions)) {
        inter1 <- which(fish$Name == interactions[[action]][1])  # Index of the matching fleet
        inter2 <- groups[groups$Name == interactions[[action]][2], "Number"]  # Prey
        data_array[,inter2, inter1 , regions+1,scen] <- dat[ , (action + 1)]
      }
    }
    
    for (region in 1:regions) {  # Skips region 0, adjust if needed
      dat <- read.csv(paste0("./Experiments/",scenarios[scen],"/Ecospace_Annual_Average_Region_",region,"_",type,".csv"), check.names = FALSE) 
      interactions <- strsplit(colnames(dat)[2:ncol(dat)], "\\|")  # Get column names
      for (action in 1:length(interactions)) {
        inter1 <- which(fish$Name == interactions[[action]][1])  # Index of the matching fleet
        inter2 <- groups[groups$Name == interactions[[action]][2], "Number"]  # Prey
        data_array[,inter2, inter1 , region,scen] <- dat[ , (action + 1)]
      }
    }
  }
  save(data_array, file = paste0(results, "/Annual ",type," Trend.RData"))
}

#-----------------------------------------------
#  ANALYSIS FUNCTIONS
#-----------------------------------------------

f.center.mass = function(type, scenarios, groups, times, dat, year_start, rows, columns, outdir, resdir) {
  cat("Printing", type, "plots at:\n", outdir, "\n")
  
  # Get bathymetry data
  bathy_data <- getNOAA.bathy(lon1 = -98, lon2 = -80, lat1 = 18, lat2 = 31, resolution = 1)
  bathy_df <- fortify.bathy(bathy_data)
  
  # Initialize an empty data frame for all functional groups
  full_center_masses = data.frame(Scenario = character(), Group = character(), 
                                  Time = numeric(), Lon = numeric(), Lat = numeric())
  
  cols = colorblind_palette(length(scenarios))
  
  for (fg in 1:nrow(groups)) {
    plot_list_full = list()
    plot_list_diff = list()
    center_masses_fg = data.frame(Scenario = character(), Group = character(), 
                                  Time = numeric(), Lon = numeric(), Lat = numeric())
    radial_data <- data.frame(Bearing = numeric(), Distance = numeric(), Scenario = character())
    
    for (scen in 1:n_distinct(scenarios)) {
      for (time in 1:times) {
        data_mat = dat[,,time,fg,scen]
        sum_data <- sum(data_mat, na.rm = TRUE)
        
        if (sum_data == 0) next
        
        row_indices <- row(data_mat)
        col_indices <- col(data_mat)
        
        center_of_mass_row <- sum(row_indices * data_mat, na.rm = TRUE) / sum_data
        center_of_mass_col <- sum(col_indices * data_mat, na.rm = TRUE) / sum_data
        
        ## Gather indices of lat and lon
        lower_row_index <- floor(center_of_mass_row)
        upper_row_index <- ceiling(center_of_mass_row)
        proportion_row <- center_of_mass_row - lower_row_index
        
        # Interpolate latitude
        center_of_mass_lat <- (1 - proportion_row) * lats[lower_row_index] + 
          proportion_row * lats[upper_row_index]
        
        # Calculate the proportional position for longitude
        lower_col_index <- floor(center_of_mass_col)
        upper_col_index <- ceiling(center_of_mass_col)
        proportion_col <- center_of_mass_col - lower_col_index
        
        # Interpolate longitude
        center_of_mass_lon <- (1 - proportion_col) * lons[lower_col_index] + 
          proportion_col * lons[upper_col_index]
        
        # Add row to the functional group-specific data frame
        center_masses_fg <- center_masses_fg %>% 
          add_row(Scenario = scenarios[scen], Group = groups$Name[fg], 
                  Time = year_start + time, Lon = center_of_mass_lon, Lat = center_of_mass_lat)
      }
      
      # Filter the data for the current group and scenario
      group_cm = center_masses_fg %>% filter(Group == groups$Name[fg], Scenario == scenarios[scen])
      
      if (nrow(group_cm) == 0) next
      
      # First and Last Data Points
      first = group_cm %>% filter(Time == min(Time))
      last = group_cm %>% filter(Time == max(Time))
      distance <- distHaversine(c(first$Lon, first$Lat), c(last$Lon, last$Lat)) / 1000 # Distance in km
      bearing <- atan2(sin((last$Lon - first$Lon) * pi / 180) * cos(last$Lat * pi / 180),
                       cos(first$Lat * pi / 180) * sin(last$Lat * pi / 180) -
                         sin(first$Lat * pi / 180) * cos(last$Lat * pi / 180) * 
                         cos((last$Lon - first$Lon) * pi / 180)) * (180 / pi)
      
      # Adjust bearing to [0, 360] degrees range
      if (bearing < 0) bearing <- bearing + 360
      
      radial_data <- radial_data %>% add_row(Bearing = bearing, Distance = distance, Scenario = scenarios[scen])
      
      radial_data$Scenario = factor(radial_data$Scenario,levels=radial_data$Scenario)
      
      # Generate Radial Plot (Rose Plot)
      radial_plot <- ggplot(radial_data, aes(x = Bearing, y = Distance,color=Scenario,fill=Scenario)) +
        geom_bar(stat = 'identity', aes(fill = Scenario),alpha = 0.5, width = 5) +
        coord_polar(start = 0) +  # Start the plot at the top (North)
        scale_x_continuous(limits = c(0, 360), breaks = seq(0, 360, 45), labels = function(x) paste0(x, "°")) +
        scale_color_manual(values = cols)+
        scale_fill_manual(values = cols)+
        labs(title = paste("Movement Vectors by Scenario"), y = "Distance (km)",x="") +
        theme_minimal()+
        theme(legend.position = "none")
      
      # Plot for full time series
      plot_list_full[[scen]] <- ggplot() +
        geom_contour(data = bathy_df, aes(x = x, y = y, z = z), color = "grey", breaks = c(-400, -200, -100, 0)) + 
        geom_point(data = group_cm, aes(x = Lon, y = Lat, color = Time), size = 3) +
        scale_color_gradient(low = "lightblue", high = "darkblue") +
        labs(title = paste0(type, " | ", groups$Name[fg], " | ", scenarios[scen]), 
             x = "Longitude", y = "Latitude", 
             subtitle = paste0("Distance Moved: ", round(distance, 1), " km")) +
        theme_minimal()
      
      # Plot for first vs last time point
      plot_list_diff[[scen]] <- ggplot() +
        geom_contour(data = bathy_df, aes(x = x, y = y, z = z), color = "grey", breaks = c(-400, -200, -100, 0)) + 
        geom_point(data = first, aes(x = Lon, y = Lat), color = "lightblue", size = 3) +
        geom_point(data = last, aes(x = Lon, y = Lat), color = "darkblue", size = 3) +
        geom_segment(data = data.frame(first_lon = first$Lon, first_lat = first$Lat,
                                       last_lon = last$Lon, last_lat = last$Lat), 
                     aes(x = first_lon, y = first_lat, xend = last_lon, yend = last_lat),
                     arrow = arrow(length = unit(0.3, "cm")), linewidth = 1, 
                     color = cols[scen]) +
        labs(title = paste0(type, " | ", groups$Name[fg], " | ", scenarios[scen]), 
             x = "Longitude", y = "Latitude", 
             subtitle = paste0("Distance Moved: ", round(distance, 1), " km")) +
        theme_minimal()
      
      # Append Radial Plot to Plot Lists
      plot_list_full[[length(plot_list_full) + 1]] <- radial_plot
      plot_list_diff[[length(plot_list_diff) + 1]] <- radial_plot
    }
    
    # Combine and save plots for the current functional group
    if (length(plot_list_full) > 0) {
      combined_full_plot <- wrap_plots(plot_list_full, ncol = 3) + plot_layout(guides = "collect")
      combined_diff_plot <- wrap_plots(plot_list_diff, ncol = 3) + plot_layout(guides = "collect")
      
      # Save the plots
      filename_full = paste0(outdir, "/", type, "/center_masses/full/Full ", type, " Center of Mass ", groups$Name[fg], ".png")
      ggsave(filename_full, combined_full_plot, height = unit(9, "in"), width = unit(14, "in"))
      
      filename_diff = paste0(outdir, "/", type, "/center_masses/diff/Compared ", type, " Center of Mass ", groups$Name[fg], ".png")
      ggsave(filename_diff, combined_diff_plot, height = unit(9, "in"), width = unit(14, "in"))
    }
    
    # Append the results to the full data frame
    full_center_masses <- bind_rows(full_center_masses, center_masses_fg)
  }
  
  # Save the full results to CSV
  filename_csv = paste0(resdir, "/", type, " Center of Masses.csv")
  write.csv(full_center_masses, filename_csv, row.names = FALSE)
}

f.end.start = function(type, scenarios, groups, times, dat, year_start, rows, columns, outdir, resdir){
  cat("Printing", type, "plots at:\n", outdir, "\n")
  
  # Get bathymetry data
  bathy_data <- getNOAA.bathy(lon1 = -98, lon2 = -80, lat1 = 18, lat2 = 31, resolution = 1)
  bathy_df <- fortify.bathy(bathy_data)
  
  for (fg in 1:nrow(groups)) {
    plot_list = list()
    
    for (scen in 1:n_distinct(scenarios)) {
      dat_first = dat[,,1,fg,scen]
      all_zero <- all(dat_first==0)
      if (all_zero == FALSE){
        
        dat_last = dat[,,dim(dat)[3],fg,scen]

        rel_change = (dat_last-dat_first)/dat_first
        rel_change[is.na(rel_change)] <- 0
        
        rel_change = rel_change[nrow(rel_change):1,]
        
        raster_obj = raster(rel_change)
        extent(raster_obj) <- c(min(lons), max(lons), min(lats), max(lats))
        raster_df <- as.data.frame(rasterToPoints(raster_obj))
        colnames(raster_df) <- c("x", "y", "value")
        
        # Plot for full time series
        min_val <- min(raster_df$value, na.rm = TRUE)
        max_val <- max(raster_df$value, na.rm = TRUE)
        
        if (is.infinite(min_val) || is.nan(min_val)) {
          min_val <- -1  # Set to a default value, or handle as needed
        }
        
        if (is.infinite(max_val) || is.nan(max_val)) {
          max_val <- 1  # Set to a default value, or handle as needed
        }
        
        # Set the color scale limits to be symmetric around 0
        color_limit <- max(abs(min_val), abs(max_val))
        
        if (is.null(color_limit) || is.na(color_limit)) {
          color_limit <- max(abs(raster_df$value), na.rm = TRUE)  # or any default value you want
        } else if (color_limit == 0){
          color_limit = 1
        }

        plot_list[[scen]] <- ggplot() +
          geom_raster(data = raster_df, aes(x = x, y = y, fill = value)) +
          geom_contour(data = bathy_df, aes(x = x, y = y, z = z), color = "grey", breaks = c(-400, -200, -100, 0)) +
          scale_fill_gradient2(
            low = "red", mid = "white", high = "blue",
            limits = range(c(-color_limit, color_limit)),  # Adjust limits dynamically
            breaks = c(-color_limit, 0, color_limit),
            labels = c(as.character(round(-color_limit, 2)), "0", as.character(round(color_limit, 2)))
          ) +
          ggtitle(paste0(type, " | ", groups$Name[fg], " | ", scenarios[scen])) +
          labs(x = "Longitude", y = "Latitude") +
          theme_minimal() +
          theme(legend.position = c(0.85, 0.15))
      }
    }
    
    # Combine and save plots for the current functional group
    if (length(plot_list) > 0) {
      combined_full_plot <- wrap_plots(plot_list, ncol = 3)
      
      # Save the plots
      filename_full = paste0(outdir, "/", type, "/end_start/", type, " Relative Change ", groups$Name[fg], ".png")
      ggsave(filename_full, combined_full_plot, height = unit(9, "in"), width = unit(14, "in"))
    }
    print(fg)
  }
}

f.dispersion = function(type,scenarios,groups,times,dat,year_start,outdir,resdir){
  
  cat("Calculating Dispersion for ", type, "plots at:\n", resdir, "\n")
  
  dispersion = data.frame(Scenario = character(),Group = character(), range = numeric(), variance = numeric(), standard_deviation = numeric(), coefficient_of_variation = numeric(), interquartile_range = numeric(), skewness = numeric(), kurtosis = numeric())
  
  for (scen in 1:n_distinct(scenarios)){
    for (fg in 1:nrow(groups)){
      group_dis = data.frame(Scenario = character(),Group = character(),Time = numeric(), range = numeric(), variance = numeric(), standard_deviation = numeric(), coefficient_of_variation = numeric(), interquartile_range = numeric(), skewness = numeric(), kurtosis = numeric())
      for (time in 1:times){
        
        dat_mat = dat[,,time,fg,scen]
        dat_mat[dat_mat == 0] <- NA
        
        dat_vector <- as.vector(dat_mat)
        
        #Range
        ranges <- range(dat_vector, na.rm = TRUE)
        range_value <- diff(ranges)
        
        #Variance
        variances <- var(dat_vector, na.rm = TRUE)
        
        #Standard Deviation
        deviations <- sd(dat_vector, na.rm = TRUE)
        
        #CV
        means <- mean(dat_vector, na.rm = TRUE)
        cv <- deviations / means * 100  # CV as percentage
        
        #IQR
        iqrs <- IQR(dat_vector, na.rm = TRUE)
        
        #Skewdness
        skewdness <- skewness(dat_vector, na.rm = TRUE)
        
        #Kurtosis
        kurtosises <- kurtosis(dat_vector, na.rm = TRUE)
        
        #Moran's I
        #dat_mat = dat[,,time,fg,scen]
        #dat_vector <- as.vector(dat_mat)
        
        # Step 1: Create a spatial weight matrix (use rook contiguity)
        # Create a neighborhood list using the matrix dimensions
        #nrows <- nrow(dat_mat)
        #ncols <- ncol(dat_mat)
        
        # Convert matrix into a vector, ignore NAs
        #dat_vector <- as.vector(dat_mat)
        #vector_na_ignored <- dat_vector[!is.na(dat_vector)]
        
        # Create spatial weights matrix using rook contiguity (i.e., adjacent cells)
        #coords <- expand.grid(1:nrows, 1:ncols)
        #nb <- cell2nb(nrows, ncols, type = "rook")  # Create a neighborhood object
        #listw <- nb2listw(nb, style = "W", zero.policy = TRUE)  # Convert to listw object
        
        # Step 2: Perform Moran's I test
        #morans_I <- moran.test(vector_na_ignored, listw, na.action = na.exclude)
        
        group_dis = group_dis %>% add_row(Scenario = scenarios[scen],Group = groups$Name[fg],Time = year_start+time, range = range_value, variance = variances, standard_deviation = deviations, coefficient_of_variation = cv, interquartile_range = iqrs, skewness = skewdness, kurtosis = kurtosises)
      }
      
      relative_change_df <- group_dis %>%
        summarise(across(4:ncol(group_dis), ~ (last(.x) - first(.x)) / first(.x)))
      
      dispersion = dispersion %>% add_row(Scenario = scenarios[scen],Group = groups$Name[fg],range = relative_change_df$range, variance = relative_change_df$variance, standard_deviation = relative_change_df$standard_deviation, coefficient_of_variation = relative_change_df$coefficient_of_variation, interquartile_range = relative_change_df$interquartile_range, skewness = relative_change_df$skewness, kurtosis = relative_change_df$kurtosis)
    }
    #Create heatmaps
    ## By scenario
    scen_disperse = dispersion %>% filter(Scenario == scens[scen]) %>% dplyr::select(-Scenario)
    df_long <- scen_disperse %>%
      pivot_longer(cols = 2:ncol(scen_disperse), names_to = "Metric", values_to = "Value")
    
    df_long <- df_long %>%
      group_by(Metric) %>% 
      mutate(Z_Score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE)) %>%
      ungroup()
    
    max_species_per_plot <- 30
    
    df_long <- df_long %>%
      mutate(Species_ID = as.numeric(factor(Group))) %>%
      mutate(Group_ID = ceiling(Species_ID / max_species_per_plot))
    max_zscore <- max(abs(df_long$Z_Score), na.rm = TRUE)
    fill_limits <- c(-max_zscore, max_zscore)
    
    plots <- df_long %>%
      group_split(Group_ID) %>%
      lapply(function(data_chunk) {
        plot <- ggplot(data_chunk, aes(x = Metric, y = Group, fill = Z_Score)) +
          geom_tile(color = "white") +
          scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                               name = "Scaled Value", midpoint = 0, limits = fill_limits) +
          labs(title = paste("Relative Change of Metrics by Group (Z-score standardization)"),
               x = "Metric", y = "Group") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        filename <- paste0(outdir, "/", type, "/dispersion/group/",scens[scen], "-heatmap_", unique(data_chunk$Group_ID), ".png")
        ggsave(filename, plot, width = 8, height = 6, dpi = 300)
      })
  }
  ## Heatmap by Functional Group
  
  for (fg in 1:nrow(groups)){
    fg_disperse = dispersion %>% filter(Group == unique(groups$Name)[fg]) %>% dplyr::select(-Group)
    
    df_long <- fg_disperse %>%
      pivot_longer(cols = 2:ncol(fg_disperse), names_to = "Metric", values_to = "Value")
    
    df_long <- df_long %>%
      group_by(Metric) %>% 
      mutate(Z_Score = (Value - mean(Value, na.rm = TRUE)) / sd(Value, na.rm = TRUE)) %>%
      ungroup()
    
    max_zscore <- max(abs(df_long$Z_Score), na.rm = TRUE)
    fill_limits <- c(-max_zscore, max_zscore)
    
    plot = ggplot(df_long,aes(x=Metric,y=Scenario,fill=Z_Score))+
      geom_tile(color="white")+
      scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                           name = "Scaled Value", midpoint = 0, limits = fill_limits) +
      labs(title = paste("Relative Change of Metrics by Scenario (Z-score standardization)"),
           x = "Metric", y = "Group") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    filename <- paste0(outdir,"/",type, "/dispersion/scenario/",unique(groups$Name)[fg], "-heatmap.png")
    ggsave(filename, plot, width = 8, height = 6, dpi = 300)
  }
  
  # Save the full results to CSV
  filename_csv = paste0(resdir, "/", type, " Dispersal Metrics.csv")
  write.csv(dispersion, filename_csv, row.names = FALSE)
}

f.plot_regional_biomass_trend = function(type,dat,groups,scenarios,regions,year_start,outdir,region_names){
  
  fg_name = groups$Name  
  area = dim(dat)[3]
  relative_change <- array(NA, dim = dim(dat))
  
  cols = colorblind_palette(area)
  
  # Calculate relative change for each group and region
  for (scen in 1:n_distinct(scenarios)){
    for (group in 1:nrow(groups)) {
      for (region in 1:area) {
        start_value <- dat[1, group, region,scen]
        relative_change[, group, region,scen] <- (dat[, group, region,scen] - start_value) / start_value
      }
    }  
  }
  
  for (fg in 1:nrow(groups)){
    plot_list = list()
    for (scen in 1:n_distinct(scenarios)){
      dat_plot = data.frame(Time = seq(1,dim(relative_change)[1],1))
      for (region in 1:area){
        new_col = paste0("Region ",region)
        dat_plot[[new_col]] <- relative_change[, fg, region,scen]
      }
      dat_long <- dat_plot %>%
        pivot_longer(cols = starts_with("Region"), names_to = "Area", values_to = "Value")

      plot_dat = dat_long %>% mutate(Region = ifelse(Area == "Region 1",region_names[1],ifelse(Area == "Region 2",region_names[2],ifelse(Area == "Region 3",region_names[3],ifelse(Area == "Region 4",region_names[4],region_names[5])))))
      
      plot_dat$Region = factor(plot_dat$Region,levels=unique(plot_dat$Region))
      plot_list[[scen]] = ggplot(plot_dat,aes(x=year_start+Time/12,y=Value,color=Region))+
        geom_hline(yintercept = 0)+
        geom_line(size=1)+
        scale_color_manual(values = cols)+
        labs(x="Year",y="Biomass {(End-Start)/Start}",title=paste0(scens[scen]," | ",groups$Name[fg]))+
        theme_classic()+
        theme(legend.title = element_blank())
    }
    
    if (length(plot_list) > 0) {
      combined_full_plot <- wrap_plots(plot_list, ncol = 3)
      filename = paste0(outdir,"/",type,"/trend/monthly/",groups$Name[fg]," Relative Biomass.png")
      ggsave(filename,combined_full_plot,height=unit(7,"in"),width=unit(10,"in"))
    }
  }
}

f.plot_regional_catch_trend = function(type,dat,groups,fish,scenarios,regions,year_start,outdir,region_names){

  fg_name = groups$Name  
  fleet_name = fish$Name
  area = dim(dat)[4]
  relative_change <- array(NA, dim = dim(dat))
  
  cols = colorblind_palette(area)
  
  # Calculate relative change for each group and region
  for (scen in 1:n_distinct(scenarios)){
    for (group in 1:nrow(groups)) {
      for (fleet in 1:nrow(fish)){
        for (region in 1:area) {
          start_value <- dat[1, group,fleet, region,scen]
          relative_change[, group,fleet, region,scen] <- (dat[, group,fleet, region,scen] - start_value) / start_value
        }
      }
    }  
  }

  for (fg in 1:nrow(groups)){
    for (fleet in 1:nrow(fish)){
      plot_list = list()
      if (all(is.nan(relative_change[,fg,fleet,,])) == FALSE){
        for (scen in 1:n_distinct(scenarios)){
          dat_plot = data.frame(Time = seq(1,dim(relative_change)[1],1))
          
          for (region in 1:area){
            new_col = paste0("Region ",region)
            dat_plot[[new_col]] <- relative_change[, fg,fleet, region,scen]
          }
          dat_long <- dat_plot %>%
            pivot_longer(cols = starts_with("Region"), names_to = "Area", values_to = "Value")

          plot_dat = dat_long %>% mutate(Region = ifelse(Area == "Region 1",region_names[1],ifelse(Area == "Region 2",region_names[2],ifelse(Area == "Region 3",region_names[3],ifelse(Area == "Region 4",region_names[4],region_names[5])))))
          
          plot_dat$Region = factor(plot_dat$Region,levels=unique(plot_dat$Region))
          
          plot_list[[scen]] = ggplot(plot_dat,aes(x=year_start+Time/12,y=Value,color=Region))+
            geom_hline(yintercept = 0)+
            geom_line(size=1)+
            scale_color_manual(values = cols)+
            labs(x="Year",y=paste0(type," {(End-Start)/Start}"),title=paste0(fish$Name[fleet]," - ",groups$Name[fg]),subtitle = scens[scen])+
            theme_classic()+
            theme(legend.title = element_blank())
        }
        
        if (length(plot_list) > 0) {
          combined_full_plot <- wrap_plots(plot_list, ncol = 3)
          filename = paste0(outdir,"/",type,"/trend/monthly/",fish$Name[fleet]," - ",groups$Name[fg]," Relative ", type,".png")
          ggsave(filename,combined_full_plot,height=unit(7,"in"),width=unit(10,"in"))
        }
      }
    }
  }
}

f.plot_regional_annual_biomass_trend = function(type,dat,groups,scenarios,regions,year_start,outdir,region_names){
  
  fg_name = groups$Name  
  area = dim(dat)[3]
  relative_change <- array(NA, dim = dim(dat))
  
  cols = colorblind_palette(area)
  
  # Calculate relative change for each group and region
  for (scen in 1:n_distinct(scenarios)){
    for (group in 1:nrow(groups)) {
      for (region in 1:area) {
        start_value <- dat[1, group, region,scen]
        relative_change[, group, region,scen] <- (dat[, group, region,scen] - start_value) / start_value
      }
    }  
  }
  
  for (fg in 1:nrow(groups)){
    plot_list = list()
    for (scen in 1:n_distinct(scenarios)){
      dat_plot = data.frame(Time = seq(1,dim(relative_change)[1],1))
      
      for (region in 1:area){
        new_col = paste0("Region ",region)
        dat_plot[[new_col]] <- relative_change[, fg, region,scen]
      }
      dat_long <- dat_plot %>%
        pivot_longer(cols = starts_with("Region"), names_to = "Area", values_to = "Value")
      
      plot_dat = dat_long %>% mutate(Region = ifelse(Area == "Region 1",region_names[1],ifelse(Area == "Region 2",region_names[2],ifelse(Area == "Region 3",region_names[3],ifelse(Area == "Region 4",region_names[4],region_names[5])))))
      
      plot_dat$Region = factor(plot_dat$Region,levels=unique(plot_dat$Region))
      plot_list[[scen]] = ggplot(plot_dat,aes(x=year_start+Time,y=Value,color=Region))+
        geom_hline(yintercept = 0)+
        geom_line(size=1)+
        scale_color_manual(values = cols)+
        labs(x="Year",y="Biomass {(End-Start)/Start}",title=paste0(scens[scen]," | ",groups$Name[fg]))+
        theme_classic()+
        theme(legend.title = element_blank())
    }
    
    if (length(plot_list) > 0) {
      combined_full_plot <- wrap_plots(plot_list, ncol = 3)
      filename = paste0(outdir,"/",type,"/trend/annual/",groups$Name[fg]," Relative Biomass.png")
      ggsave(filename,combined_full_plot,height=unit(7,"in"),width=unit(10,"in"))
    }
  }
}

f.plot_regional_annual_catch_trend = function(type,dat,groups,fish,scenarios,regions,year_start,outdir,region_names){
  
  fg_name = groups$Name  
  fleet_name = fish$Name
  area = dim(dat)[4]
  relative_change <- array(NA, dim = dim(dat))
  
  cols = colorblind_palette(area)
  
  # Calculate relative change for each group and region
  for (scen in 1:n_distinct(scenarios)){
    for (group in 1:nrow(groups)) {
      for (fleet in 1:nrow(fish)){
        for (region in 1:area) {
          start_value <- dat[1, group,fleet, region,scen]
          relative_change[, group,fleet, region,scen] <- (dat[, group,fleet, region,scen] - start_value) / start_value
        }
      }
    }  
  }
  
  for (fg in 1:nrow(groups)){
    for (fleet in 1:nrow(fish)){
      plot_list = list()
      if (all(is.nan(relative_change[,fg,fleet,,])) == FALSE){
        for (scen in 1:n_distinct(scenarios)){
          dat_plot = data.frame(Time = seq(1,dim(relative_change)[1],1))
          
          for (region in 1:area){
            new_col = paste0("Region ",region)
            dat_plot[[new_col]] <- relative_change[, fg,fleet, region,scen]
          }
          dat_long <- dat_plot %>%
            pivot_longer(cols = starts_with("Region"), names_to = "Area", values_to = "Value")

          plot_dat = dat_long %>% mutate(Region = ifelse(Area == "Region 1",region_names[1],ifelse(Area == "Region 2",region_names[2],ifelse(Area == "Region 3",region_names[3],ifelse(Area == "Region 4",region_names[4],region_names[5])))))
          
          plot_dat$Region = factor(plot_dat$Region,levels=unique(plot_dat$Region))
          
          plot_list[[scen]] = ggplot(plot_dat,aes(x=year_start+Time,y=Value,color=Region))+
            geom_hline(yintercept = 0)+
            geom_line(size=1)+
            scale_color_manual(values = cols)+
            labs(x="Year",y=paste0(type," {(End-Start)/Start}"),title=paste0(fish$Name[fleet]," - ",groups$Name[fg]),subtitle = scens[scen])+
            theme_classic()+
            theme(legend.title = element_blank())
        }
        
        if (length(plot_list) > 0) {
          combined_full_plot <- wrap_plots(plot_list, ncol = 3)
          filename = paste0(outdir,"/",type,"/trend/annual/",fish$Name[fleet]," - ",groups$Name[fg]," Relative ", type,".png")
          ggsave(filename,combined_full_plot,height=unit(7,"in"),width=unit(10,"in"))
        }
      }
    }
  }
}

#-----------------------------------------------
#  CORRELATION ANALYSES
#-----------------------------------------------
f.correlation.timeseries.biomass = function(type,scenarios,region,groups,resdir){
  load(file = paste0(res_dir,"/",type," Trend",".RData"))
  cor_df = data.frame(Scenario = character(),Group = character(),Region = character(),R = numeric())
  for (spot in 1:length(region)){
    for (fg in 1:dim(data_array)[2]){
      for (scen in 1:n_distinct(scenarios)){
        if (scen != 3){ #May change depending on where the scenario falls in the dataframe
          ts1 = data_array[,fg,spot,3]
          ts2 = data_array[,fg,spot,scen]
          r = cor(ts1,ts2,method="spearman")
          cor_df = cor_df %>% add_row(Scenario = scenarios[scen],Group = groups$Name[fg],Region = region[spot],R=r)
        }
      }
    }
  }
  filename = paste0(resdir,"/",type," Correlations.csv")
  write.csv(cor_df,filename)
}

f.correlation.timeseries.catch = function(type,scenarios,region,groups,fleets,resdir){
  load(file = paste0(res_dir,"/",type," Trend",".RData"))
  cor_df = data.frame(Scenario = character(),Group = character(),Fleet = character(),Region = character(),R = numeric())
  for (spot in 1:length(region)){
    for (fg in 1:dim(data_array)[2]){
      for (fleet in 1:dim(data_array)[3]){
        if (all(data_array[,fg,fleet,,] == 0) == FALSE){
          for (scen in 1:n_distinct(scenarios)){
            if ((scen != 3) && (spot <= dim(data_array)[4])){ #May change depending on where the scenario falls in the dataframe
              ts1 = data_array[,fg,fleet,spot,3]
              ts2 = data_array[,fg,fleet,spot,scen]
              r = cor(ts1,ts2,method="spearman")
              cor_df = cor_df %>% add_row(Scenario=scenarios[scen],Group=groups$Name[fg],Fleet=fleets$Name[fleet],Region = region[spot],R=r)
            }
          }
        }
      }
    }
  }
  filename = paste0(resdir,"/",type," Correlations.csv")
  write.csv(cor_df,filename)
}

f.plot.correlation.biomass = function(type,resdir,outdir){
  filename = paste0(resdir,"/",type," Correlations.csv")
  dat = read.csv(filename) %>%
    mutate(FacetGroup = ceiling(as.numeric(factor(Group)) / 30))
  
  for (i in 1:n_distinct(dat$FacetGroup)){
    sub_dat = dat %>% filter(FacetGroup == i)
    
    plt = ggplot(sub_dat,aes(x=Region,y=Scenario,fill=R))+
      geom_tile(color="white")+
      facet_wrap(.~Group)+
      theme_classic()+
      scale_fill_gradient2(low="red",mid="white",high="blue",limits=c(-1,1),breaks=c(-1,0,1),labels=c("-1","0","1"),name="Spearman\nCorrelation")+
      labs(title=paste0(type," comparison to all_driver Scenario"))+
      theme(axis.text.x = element_text(angle=75,hjust=1))
    
    filename = paste0(outdir,"/",type,"/trend/",type," Correlations_",i,".png")
    ggsave(filename,plt,height=unit(8,"in"),width=unit(9,"in"))
  }
}

f.plot.correlation.catch = function(type,resdir,outdir){
  filename = paste0(resdir,"/",type," Correlations.csv")
  dat = read.csv(filename) %>% mutate(Name = paste0(Fleet,"-\n",Group))%>%
    mutate(FacetGroup = ceiling(as.numeric(factor(Name)) / 30))
  
  for (i in 1:n_distinct(dat$FacetGroup)){
    sub_dat = dat %>% filter(FacetGroup == i)
    
    plt = ggplot(sub_dat,aes(x=Region,y=Scenario,fill=R))+
      geom_tile(color="white")+
      facet_wrap(.~Name)+
      theme_classic()+
      scale_fill_gradient2(low="red",mid="white",high="blue",limits=c(-1,1),breaks=c(-1,0,1),labels=c("-1","0","1"),name="Spearman\nCorrelation")+
      labs(title=paste0(type," comparison to all_driver Scenario"))+
      theme(axis.text.x = element_text(angle=75,hjust=1))
    
    filename = paste0(outdir,"/",type,"/trend/",type," Correlations_",i,".png")
    ggsave(filename,plt,height=unit(8,"in"),width=unit(9,"in"))
  }
}

#-----------------------------------------------
#  GENERAL FUNCTIONS
#-----------------------------------------------
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
