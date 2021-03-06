
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
source("Code/R2GDX.r")

# Function to extract data from raster
processWater_f <- function(file_info, period){
  
  # print file name
  print(file_info$filename)
  
  # Downscale to SIMU level and create dataframe
  SIMU_df <- lapply(seq_along(period), downscale_f, file_info) %>%
    bind_rows() %>%
    mutate(SimUID = factor(SimUID),
           LUId = factor(LUId),
           model = factor(model),
           year = factor(year),
           sector = factor(sector),
           ssp = factor(ssp),
           rcp = factor(rcp)) %>%
    dplyr::select(SimUID, LUId, model, sector, ssp, rcp, year, value)
  
  
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
  SSP <- GDXSet_f("SSP", "SSP scenario", unname(level_file["ssp"]))
  RCP <- GDXSet_f("RCP", "RCP scenario", unname(level_file["rcp"]))
  YEAR <- GDXSet_f("year", "year", unname(level_file["year"]))
  
  # Create GDX file
  print("Writing GDX file")
  wgdx(file_info$full_target_filename, parameter_file, SIMUID, LUID, MODEL, SECTOR, SSP, RCP, YEAR)
}




# Function to extract data from raster
downscale_f <- function(i, file_info){
  # Load stack
  stack <- stack(file_info$full_filename)
  # extract raster
  rast <- stack[[i]]
  # Set year
  y = file_info$sy + i -1
  print(y)
    
  ### 1. Ensure that water data has some raster resolution as simu grid. We rasterise to 0.08333 by scaling by 6.
  ### The original water raster has a resolution of 0.5 degrees/6 = 0.08333. 
  ### If we assume an even distribution we can divide by 36 (6x6) to reallocate the water over the new grid.
  # Rasterize to finer grid
  rast = disaggregate(rast, fact=6)
  
  # Divide by 36 to take average
  rast = rast/36
  
  ### 2. Ensure that simu and water raster have same extent by resampling
  # Set NA in SIMU raster to -1 as the water map has values for those locations and we want to know how much for checking
  SIMU_5min[is.na(SIMU_5min)] <- -1
  
  # Crop water raster to simu raster so the have the same extent
  rast = crop(rast, SIMU_5min)
  
  # Calculate sum of water over simus
  rast_SIMU <- data.frame(zonal(rast, SIMU_5min, 'sum')) %>%
    filter(!zone %in% c(-1, 0)) %>%
    rename(SimUID = zone, value = sum) %>%
    inner_join(SIMU_LU) %>%
    dplyr::select(SimUID, LUId, value) %>%
    mutate(model = file_info$model,
           year = y,
           sector = file_info$sector,
           ssp = file_info$ssp,
           rcp = file_info$rcp)
  
  return(rast_SIMU)
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
  separate(filename, c("model", "rcp", "ssp", "sector", "time", "sy", "ey") , sep = "_", remove = F) %>%
  separate(ey, c("ey", "ext")) %>%
  mutate(sy = as.numeric(sy),
         sector = recode(sector, "PDomUse" = "dom_wc", "PDomWW" = "dom_ww", "PDomUseTech" = "dom_use_tech", 
                         "PDomWWTech" = "dom_ww_tech", "PIndWWTech" = "ind_ww_tech", "PIndUse" = "ind_wc", 
                         "PIndUseTech" = "ind_wc_tech", "PIndWW" = "ind_ww"),
         target_filename = gsub(".nc4", "_simu.gdx", filename),
         full_target_filename = file.path(dataPath, paste0("Water_demand_simu/pcrglob/", target_filename)))

# Create GDX files
lapply(c(1:nrow(pcrglob_info)), function(x) processWater_f(pcrglob_info[x,], c(2000:2050)))
#lapply(c(1), function(x) processWater_f(pcrglob_info[x,], c(2020)))


### WATERGAP model
# We take the new_Jan2015 values
# We downscaled up to 2050. Downscaling up to can be done easily by: (1) removing the gsub call to change the filename from 2100 into 2050 and
# (2) change the period into 2005:2100.

# Table with all information
watergap_info <- data.frame(full_filename = list.files(file.path(dataPath, "Water_demand/watergap/wfas/new_Jan2015"), pattern = "*.nc", full.names = T),
                            filename = list.files(file.path(dataPath, "Water_demand/watergap/wfas/new_Jan2015"), pattern = "*.nc", full.names = F)) %>%
  separate(filename, c("model", "ssp", "rcp", "sector", "type", "time", "sy", "ey") , sep = "_", remove = F) %>%
  separate(ey, c("ey", "ext")) %>%
  mutate(sy = as.numeric(sy),
         sector = paste(sector, type, sep = "_"),
         target_filename = gsub(".nc", "_simu.gdx", filename),
         target_filename = gsub("2100", "2050", target_filename),
         full_target_filename = file.path(dataPath, paste0("Water_demand_simu/watergap/", target_filename))) %>%
  dplyr::select(-type)

# Create GDX files
#lapply(c(1:2), function(x) processWater_f(watergap_info[x,], c(2005:2006)))
lapply(c(1:nrow(watergap_info)), function(x) processWater_f(watergap_info[x,], c(2005:2050)))

