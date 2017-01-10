#'========================================================================================================================================
#' Project:  CCAFS South Asia
#' Subject:  Code to process netCDF files with water demand per SSP
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("WDI", "countrycode", "R.utils", "gdxrrw")


### SET WORKING DIRECTORY
wdPath <- "~/CCAFS_SouthAsia"
setwd(wdPath)

### SET DATAPATH
dataPath <- "P:\\globiom\\Projects\\Water\\CCAFS_SouthAsia\\Data"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### FUNCTIONS
# Source functions to create GDX file
source("Code/r_code/R2GDX.r")

# Function to extract data from raster
downscale_f <- function(i, file_info){
  
  # Load stack
  stack <- stack(file_info$full_filename)
  # extract raster
  rast <- stack[[i]]
  # Set year
  y = file_info$sy + i -1
  print(y)
  
  ### 1. Calculate water demand per km2
  # Calculate area of cells
  rast_area <- area(rast)
  # Combine with water demand and calculate demand/km2
  rast_km <- rast/rast_area
  
  ### 2. Resample the 0.5 degree water demand/km2 map to a resolution of 5 arcmin 
  # Resample to 5 arcmin
  rast_km_5min <- raster::resample(rast_km, SIMU_5min, method="bilinear")
  # It appears some values are slightly <0 because of interpolation. We set them to 0
  rast_km_5min[rast_km_5min < 0] <- 0
  
  ### 3. Calculate area of SIMU raster cells
  SIMU_area <- area(SIMU_5min)
  
  ### 4. Calculate water demand per SIMU raster cell
  # Calculate demand
  rast_SIMU_r <- rast_km_5min * SIMU_area
  
  ### 5. Aggregate water demand over SIMUs and link LUId
  rast_SIMU <- data.frame(zonal(rast_SIMU_r, SIMU_5min, 'sum')) %>%
    filter(zone !=0) %>%
    rename(SimUID = zone, value = sum) %>%
    inner_join(SIMU_LU) %>%
    dplyr::select(SimUID, LUId, value) %>%
    mutate(model = file_info$model,
           year = y,
           sector = file_info$sector,
           scenario = file_info$scenario,
           rcp = file_info$rcp)
  
  return(rast_SIMU)
}

### FUNCTION TO PROCESS NC FILE AND WRITE TO GDX
processWater_f <- function(file_info, period){
  
  # Downscale to SIMU level and create dataframe
  SIMU_df <- lapply(seq_along(period), downscale_f, file_info) %>%
    bind_rows() %>%
    mutate(SimUID = factor(SimUID),
           LUId = factor(LUId),
           model = factor(model),
           year = factor(year),
           sector = factor(sector),
           scenario = factor(scenario),
           rcp = factor(rcp)) %>%
    dplyr::select(SimUID, LUId, model, sector, scenario, year, value)
  
  
  # load required GAMS libraries (folder user specific)
  igdx("C:/GAMS/win64/24.4")
  
  # Create filename
  GDXfile <- file_info$target_filename
  
  # Extract levels
  level_file <- extractLevels_f(SIMU_df)
  
  # Create parameter file
  parameter_file <- GDXPara_f(factor2Number_f(SIMU_df), "demand", "water demand", unname(level_file))
  
  # Create set lists
  SIMUID <- GDXSet_f("SimUID", "SimUID", unname(level_file["SimUID"]))
  LUID <- GDXSet_f("LUId", "LUId", unname(level_file["LUId"]))
  MODEL <- GDXSet_f("model", "water model", unname(level_file["model"]))
  SECTOR <- GDXSet_f("sector", "water sector", unname(level_file["sector"]))
  SCENARIO <- GDXSet_f("scenario", "SSP scenario", unname(level_file["scenario"]))
  YEAR <- GDXSet_f("year", "year", unname(level_file["year"]))
  
  # Create GDX file
  print(file_info$target_filename)
  wgdx(file_info$full_target_filename, parameter_file, SIMUID, LUID, MODEL, SECTOR, SCENARIO, YEAR)
}

### UNZIP .gz files

# # Path with gz files
# gzPath <- file.path(dataPath, "\\Water_demand\\watergap\\wfas\\old")
# 
# zipFiles <- list.files(gzPath, pattern = ".gz", full.names = T)
# lapply(zipFiles, gunzip, skip=TRUE)


#########################################################################################
#'Procedure to downscale water demand scenarios from Wada et al. (2016)
#' 
#'Problem: Water demand is presented at 0.5 x 0.5 degree resolution while
#'while GLOBIOM SIMUs are defined at a higer resolution. Hence, we need a procedure to
#'to downscale the water demand to the SIMU level. We adopt the following procedure:
#' 
#'1. Calculate the areas of the 05 degree cells and use it to calculate water demand per km2.
#'2. Resample the 0.5 degree water demand/km2 map to a resolution of 5 arcmin so it is consistent with the 5 arcmin map of the SIMUs.
#'3. Calculate the area of the SIMU raster cells.
#'4. Calculate water demand per SIMU raster cell.
#'5. Aggregate over SIMUs to calculate water demand per SIMU
#'
#'THIS CAN BE IMPROVED BY adding covariates for population density and urban population (for industry)
# https://rdrr.io/rforge/ithir/man/dissever.html
# https://channel9.msdn.com/Events/useR-international-R-User-conference/useR2016/High-performance-climate-downscaling-in-R

#'########################################################################################

# Load required input
SIMU_LU <- read_csv(file.path(dataPath, "simu_lu/SimUIDLUID.csv"))
SIMU_5min <- raster(file.path(dataPath, "simu_raster_5min/rasti_simu_gr.tif"))

### PCRGLOBWB model
# Although it says monthly in the filename, data is already aggregated to annual values.
# Table with all information
pcrglob_info <- data.frame(full_filename = list.files(file.path(dataPath, "Water_demand/pcrglob/wfas"), pattern = "*.nc4", full.names = T),
                         filename = list.files(file.path(dataPath, "Water_demand/pcrglob/wfas"), pattern = "*.nc4", full.names = F)) %>%
  separate(filename, c("model", "rcp", "scenario", "sector", "time", "sy", "ey") , sep = "_", remove = F) %>%
  separate(ey, c("ey", "ext")) %>%
  mutate(sy = as.numeric(sy),
         sector = recode(sector, "PDomUse" = "dom_wc", "PDomWW" = "dom_ww", "PDomUseTech" = "dom_use_tech", 
                         "PDomWWTech" = "dom_ww_tech", "PIndWWTech" = "ind_ww_tech", "PIndUse" = "ind_wc", 
                         "PIndUseTech" = "ind_wc_tech", "PIndWW" = "ind_ww"),
         target_filename = gsub(".nc4", ".gdx", filename),
         full_target_filename = file.path(dataPath, paste0("Water_demand_simu/pcrglob/", target_filename)))

# Create GDX files
#lapply(c(1:nrow(pcrglob_info)), function(x) processWater_f(pcrglob_info[x,], c(2000:2050)))
lapply(c(1:10), function(x) processWater_f(pcrglob_info[x,], c(2000:2050)))

### WATERGAP model
# We take the new_Jan2015 values

# Table with all information
watergap_info <- data.frame(full_filename = list.files(file.path(dataPath, "Water_demand/watergap/wfas/new_Jan2015"), pattern = "*.nc", full.names = T),
                           filename = list.files(file.path(dataPath, "Water_demand/watergap/wfas/new_Jan2015"), pattern = "*.nc", full.names = F)) %>%
  separate(filename, c("model", "scenario", "rcp", "sector", "type", "time", "sy", "ey") , sep = "_", remove = F) %>%
  separate(ey, c("ey", "ext")) %>%
  mutate(sy = as.numeric(sy),
         sector = paste(sector, type, sep = "_"),
         target_filename = gsub(".nc", ".gdx", filename),
         full_target_filename = file.path(dataPath, paste0("Water_demand_simu/watergap/", target_filename))) %>%
  dplyr::select(-type)

# Rename files
file_info <- watergap_info[1,] 
file_info$full_target_filename

lapply(c(1:2), function(x) processWater_f(watergap_info[x,], c(2000:2002)))

###
