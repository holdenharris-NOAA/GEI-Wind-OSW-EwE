#Calculate area proportions

# Constants
grid_size_km <- 8             # Grid size in km
spacing_nm <- 1               # Spacing in nautical miles
nm_to_km <- 1.852            # Conversion factor from nautical miles to km
halo_radius_m <- 92          # Radius of the halo in meters
halo_radius_km <- halo_radius_m / 1000 # Convert halo radius to kilometers

# Step 1: Calculate the number of structures
spacing_km <- spacing_nm * nm_to_km
num_structures_per_side <- floor(grid_size_km / spacing_km) + 1
total_structures <- num_structures_per_side^2

# Step 2: Calculate area influenced by halos
halo_area_per_structure <- pi * halo_radius_km^2  # Area of one halo in km²
total_halo_area <- halo_area_per_structure * total_structures

# Step 3: Total grid area
grid_area <- grid_size_km^2

# Fraction of grid area influenced by halos (assuming no overlap)
influence_fraction <- total_halo_area / grid_area

# Results
list(
  num_structures_per_side = num_structures_per_side,
  total_structures = total_structures,
  total_halo_area_km2 = total_halo_area,
  grid_area_km2 = grid_area,
  influence_fraction = influence_fraction
)
