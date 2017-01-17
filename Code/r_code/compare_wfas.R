#'========================================================================================================================================
#' Project:  CCAFS South Asia
#' Subject:  Code to compare own calculated aggregates with country aggregates that come with data
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
p_load("WDI", "countrycode")


### SET WORKING DIRECTORY
wdPath <- "~/CCAFS_SouthAsia"
setwd(wdPath)

### SET DATAPATH
dataPath <- "P:\\globiom\\Projects\\Water\\CCAFS_SouthAsia\\Data"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### GET WORLD MAP
# Polygon map
worldmap <- map("world", fill = TRUE, plot = FALSE)
# Transform to polygon
worldmap_poly <- map2SpatialPolygons(worldmap, 
                                     IDs=sapply(strsplit(worldmap$names, ":"), "[", 1L), 
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))

# Raster map
worldmap_ras <- raster(file.path(dataPath, "global_raster/country.asc"))
crs(worldmap_ras) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
country_code <- read_csv((file.path(dataPath, "global_raster/country_code.csv")))

### COMPARE COUNTRY AGGREGATES OF PCRGLOBWB WITH OWN CALCULATED BOTH FROM RASTER AND SIMU DOWNSCALED
# Get data
pcrglobwb <- file.path(dataPath, "Water_demand\\pcrglob\\wfas\\pcrglobwb_rcp6p0_ssp2_PDomUse_monthly_2000_2050.nc4")
pcrglobwb2020 <- stack(pcrglobwb)[[21]] # Value for 2020

# Calculated from country raster
country_ag_ncdf <- data.frame(zonal(pcrglobwb2020, worldmap_ras, 'sum', na.rm=T)) %>%
  dplyr::rename(country_code = zone, ncdf = sum) %>%
  left_join(country_code,.) %>%
  mutate(ncdf = ncdf/1000)

# Calculated from simu
# Input
SIMU_LU <- read_csv(file.path(dataPath, "simu_lu/SimUIDLUID.csv"))
SIMU_5min <- raster(file.path(dataPath, "simu_raster_5min/rasti_simu_gr.tif"))
SIMU_5min <- projectRaster(SIMU_5min, worldmap_ras) # Add projection

### 1. Calculate water demand per km2
W <- pcrglobwb2020 
# Calculate area of cells
rast_area <- area(W)
# Combine with water demand and calculate demand/km2
rast_km <- W/rast_area
  
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

# Aggregate to countries
country_ag_simu <- data.frame(zonal(rast_SIMU_r, worldmap_ras, 'sum', na.rm=T)) %>%
  dplyr::rename(country_code = zone, simu = sum) %>%
  left_join(country_code,.) %>%
  mutate(simu = simu/1000) 

# Calculated from aggregate table
country_ag_org <- read_excel(file.path(dataPath, "Water_demand\\pcrglob\\wfas\\pcrglobwb_water_use_country_annual_statistics_domestic_km3year_ssp1-3.xlsx"), sheet = "PDomUse_ssp2", na = "-9999") %>%
  dplyr::select(`0`, `2020`) %>%
  dplyr::rename(country_code = `0`, original = `2020`) %>%
  left_join(country_code,.)

# Combine and compare
# SIMU downscaling and ncdf are the same. Excel original aggregate is different, probably because of different global map or procedure.
comp <- left_join(country_ag_org, country_ag_ncdf) %>%
  left_join(.,country_ag_simu)

ggplot(data = comp, aes(x = simu, y = ncdf)) +
  geom_abline(intercept = 0, slope=1)+
  geom_point()

ggplot(data = comp, aes(x = simu, y = original)) +
  geom_abline(intercept = 0, slope=1)+
  geom_point()

ggplot(data = comp, aes(x = original, y = ncdf)) +
  geom_abline(intercept = 0, slope=1)+
  geom_point()

rast_df <- data.frame(rasterToPoints(W))
sum(rast_df$X40177)/1000
sum(comp$simu, na.rm=T)
sum(comp$ncdf, na.rm=T)


### COMPARE OLD AND NEW DATA
wfas_old_raw <- file.path(dataPath, "Water_demand\\watergap\\wfas\\old\\watergap_ssp2_rcp6p0_dom_ww_annual_2005_2100.nc")
wfas_new_raw <- file.path(dataPath, "Water_demand\\watergap\\wfas\\new_Jan2015\\watergap_ssp2_rcp6p0_dom_ww_annual_2005_2100.nc")

### ADD NAMES
wfas_old  <- stack(wfas_old_raw)
names(wfas_old) <- c(2005:2100)

wfas_new  <- stack(wfas_new_raw)
names(wfas_new) <- c(2005:2100)

### SELECT 2010 map
wfas_old_2010 <- wfas_old[[6]]
wfas_new_2010 <- wfas_new[[6]]

#### COMPARE TOTAL WATER DEMAND PER COUNTRY
countries <- names(worldmap_poly)
pid <- sapply(slot(worldmap_poly, "polygons"), function(x) slot(x, "ID"))
p.df <- data.frame(ID=1:length(worldmap_poly), country =  pid)     

# Water demand by country
# NB using fun = sum in extract results in some NAs (e.g. for India) although there is data
# Possible na.rm is ignored.
wfas_old_c <- raster::extract(wfas_old_2010, worldmap_poly, method = "bilinear", df = TRUE) %>%
  group_by(ID) %>%
  summarize(wfas_old = sum(X2010, na.rm = TRUE)) %>%
  left_join(p.df,.) %>%
  mutate(wfas_old = wfas_old/1000000,
         iso3c = countrycode(country, "country.name", "iso3c"))


wfas_new_c <- raster::extract(wfas_new_2010, worldmap_poly, method = "bilinear", df = TRUE) %>%
  group_by(ID) %>%
  summarize(wfas_new = sum(X2010, na.rm = TRUE)) %>%
  left_join(p.df,.) %>%
  mutate(wfas_new = wfas_new/1000000,
         iso3c = countrycode(country, "country.name", "iso3c"))

### COMPARE WORLD TOTAL
# Differences probably caused by different global country masks
# From country aggregates
sum(wfas_old_c$wfas_old, na.rm = TRUE)
# Total of raw map
wfas_old_2010_df <- data.frame(rasterToPoints(wfas_old_2010))
sum(wfas_old_2010_df$X2010)/1000000

# From country aggregates
sum(wfas_new_c$wfas_new, na.rm = TRUE)
wfas_new_2010_df <- data.frame(rasterToPoints(wfas_new_2010))
# Total of raw map
sum(wfas_new_2010_df$X2010)/1000000

### COMPARE WITH COUNTRY AGGREGATES PRESENTED BY MODELS
# wfas new
wfas_new_c_org <- read_excel(file.path(dataPath, "Water_demand\\watergap\\wfas\\new_Jan2015\\WFaS_WaterUse_simple.xlsx"), sheet = "domestic") %>%
  filter(year == 2010, IdScen == 57) %>% # SSP2
  mutate(iso3c = countrycode(Country, "country.name", "iso3c")) %>%
  rename(wfas_new_org = `domestic water withdrawals [mio.m3]`)
sum(wfas_new_c_org$wfas_new_org, na.rm=T)

comp_wfas_new <- left_join(wfas_new_c_org, wfas_new_c)

ggplot(data = comp_wfas_new, aes(x = wfas_new, y = wfas_new_org)) +
  geom_abline(intercept = 0, slope=1)+
  geom_point()

### TEST SPATIAL DOWNSCALING AND COMPARE WITH RAW DATA
### LOAD TIF FILE WITH SIMU
SIMU_5min <- raster(file.path(dataPath, "simu_raster_5min/rasti_simu_gr.tif"))
W <- wfas_new_2010

### 1. Calculate water demand per km2
# Calculate area of cells
W_area <- area(W)

# Combine with water demand and calculate demand/km2
W_km <- W/W_area


### 2. Resample the 0.5 degree water demand/km2 map to a resolution of 5 arcmin 
# Resample to 5 arcmin
W_km_5min <- raster::resample(W_km, SIMU_5min, method="bilinear")

# It appears some values are slightly <0 because of interpolation. We set them to 0
W_km_5min[W_km_5min < 0] <- 0

### 3. Calculate area of SIMU raster cells
SIMU_area <- area(SIMU_5min)

### 4. Calculate water demand per SIMU raster cell
# Calculate demand
W_SIMU_r <- W_km_5min * SIMU_area
W_SIMU_r
levelplot(W_SIMU_r)
W_SIMU_r_check <- log(W_SIMU_r)
levelplot(W_SIMU_r_check) + layer(sp.polygons(worldmap_poly))

# Check which areas have zero values, meaning no water demand
W_check <- W_SIMU_r
W_check[W_check!=0] <- NA
levelplot(W_check) + layer(sp.polygons(worldmap_poly))

### 5. Aggregate water demand over SIMUs and link LUId

# Load LUId link file
SIMU_LU <- read_csv(file.path(dataPath, "simu_lu/SimUIDLUID.csv"))

# Aggregate over SIMUs
W_SIMU <- data.frame(zonal(W_SIMU_r, SIMU_5min, 'sum')) %>%
  filter(zone !=0) %>%
  rename(SimUID = zone, value = sum) %>%
  inner_join(SIMU_LU) %>%
  dplyr::select(SimUID, LUId, value)

#### COMPARE TOTAL WATER DEMAND PER COUNTRY
countries <- names(worldmap_poly)
pid <- sapply(slot(worldmap_poly, "polygons"), function(x) slot(x, "ID"))
p.df <- data.frame(ID=1:length(worldmap_poly), country =  pid)     

# Water demand from NCDF
# NB using fun = sum in extract resuls in some NAs (e.g. for India) although there is data
# Possible na.rm is ignored.
WD_ncdf <- raster::extract(W, worldmap_poly, method = "bilinear", df = TRUE) %>%
  group_by(ID) %>%
  #summarize(ncdf = sum(X2010, na.rm = TRUE)) %>%
  summarize(ncdf = sum(X40177, na.rm = TRUE)) %>%
  left_join(p.df,.) %>%
  mutate(ncdf = ncdf/1000000)

# Water demand from SIMU
# NB using fun = sum in extract resuls in some NAs (e.g. for India) although there is data
# Possible na.rm is ignored.
WD_simu <- raster::extract(W_SIMU_r, worldmap_poly, method = "bilinear", df = TRUE) %>%
  group_by(ID) %>%
  summarize(simu = sum(layer, na.rm = TRUE)) %>%
  left_join(p.df,.) %>%
  mutate(simu = simu/1000000)

# Compare
WD_comp <- left_join(WD_simu, WD_ncdf) %>%
  mutate(dif = simu/ncdf*100)

ggplot(data = WD_comp, aes(x = simu, y = ncdf)) +
  geom_abline(intercept = 0, slope=1)+
  geom_point()

# Compare world totals
WD_wld <- data.frame(rasterToPoints(W))
sum(WD_wld$X2010)/1000000
sum(WD_wld$X40177)/1000000
sum(WD_comp$simu, na.rm=T)
sum(WD_comp$ncdf, na.rm=T)

