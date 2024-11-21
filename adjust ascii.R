#Create new ascii file and save in the same directory
library(raster)
input_file <- "./reefs.asc"
output_file <- "half_reefs.asc"
raster_data <- raster(input_file)
raster_data <- raster_data * 0.5
writeRaster(raster_data, output_file, format = "ascii", overwrite = TRUE)
