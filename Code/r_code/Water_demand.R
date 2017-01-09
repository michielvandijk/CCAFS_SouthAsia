#'========================================================================================================================================
#' Project:  CCAFS South Asia
#' Subject:  Code to process netCDF files with water demand per SSP
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
BasePackages<- c("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer")
lapply(BasePackages, library, character.only = TRUE)
SpatialPackages<-c("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <- c("WDI", "R.utils")
lapply(AdditionalPackages, library, character.only = TRUE)

### SET WORKING DIRECTORY
wdPath<-"CCAFS_SouthAsia"
setwd(wdPath)

dataPath <- "P:\\globiom\\Projects\\Water\\CCAFS_SouthAsia\\Data"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### UNZIP .gz files

# # Path with gz files
# gzPath <- file.path(dataPath, "\\Water_demand\\watergap\\wfas\\old")
# 
# zipFiles <- list.files(gzPath, pattern = ".gz", full.names = T)
# lapply(zipFiles, gunzip, skip=TRUE)

### GET WORLD MAP
# Get map
worldmap <- map("world", fill = TRUE, plot = FALSE)

# Transform to polygon
worldmap_poly <- map2SpatialPolygons(worldmap, 
                                     IDs=sapply(strsplit(worldmap$names, ":"), "[", 1L), 
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))



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
#'########################################################################################


### COMBINE MULTIPLE BANDS (YEARS) IN RASTER STACK
ncdf_file <- file.path(dataPath, "\\Water_demand\\watergap\\wfas\\old\\watergap_ssp2_rcp6p0_dom_ww_annual_2005_2100.nc")
library(RNetCDF)
test <- open.nc(ncdf_file)
print.nc(test)
close.nc(test)

file  <- stack(ncdf_file)
names(file) <- c(2005:2100)
file

W <- file[[6]]
W
histogram(W)

# Visualise water map, take log as distribution is heavily skewed
# Most locations are in Green Land and the Northern part of the world but also some in African and Australia.
# Probably these are desert locations with no water demand
logW <- log(W)
levelplot(logW) + layer(sp.polygons(worldmap_poly))


### LOAD TIF FILE WITH SIMU
SIMU_5min <- raster(file.path(dataPath, "simu_raster_5min/rasti_simu_gr.tif"))

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
  summarize(ncdf = sum(X2010, na.rm = TRUE)) %>%
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

# check world total
WD_wld <- data.frame(rasterToPoints(W))
sum(WD_wld$X2010)/1000000

########################################################
#### Test India
# SIMUs in India
IND_map = getData('GADM', country="IND", level=0)
plot(IND_map)
IND_simu <- raster::extract(SIMU_5min, IND_map, df = TRUE)
IND_simu_WD <- 
names(IND_simu)

check_IND <-filter(WD_simu, ID == 101)
sum(check_IND$layer, na.rm=T)/1000000000


zoneCountryMap, df=T) %>% rename(Production = TZA_spam2005v2r0_production_maize_total) %>%
  group_by(ID) %>%
  summarize(Production = sum(Production, na.rm=T)) %>%
  left_join(., p.df)





################
# Possible approach is to calculate water demand per km2, allocate this to the SIMUs and multiply with their area.

target
target_AREA
plot(target_AREA)

WD_km = target/target_AREA
WD_km
plot(WD_km)

# Map values to SIMU pol
# Very slow!!
#http://gis.stackexchange.com/questions/130522/increasing-speed-of-crop-mask-extract-raster-by-many-polygons-in-r
SIMU_poly_wd <- raster::extract(WD_km, SIMU_poly, df = T, fun = mean)

# Change resolution
resamp_WD <- raster::resample(WD_km, SIMU_5min, method="bilinear")
resamp_WD

plot(SIMU_area)
SIMU_comb <- data.frame(rasterToPoints(stack(SIMU_5min, resamp_WD, SIMU_area)))

SIMU_comb2 <- filter(SIMU_comb, rasti_simu_gr != 0 & layer.1 !=0)

names(SIMU_comb)

# Sum values over 
temp <- data.frame(zonal(resamp_WD, SIMU_5min, 'sum'))

### LOAD SHP FILE WITH SIMU
shp = file.path(dataPath, "simu_poly/SimU_all.shp")
ogrListLayers(shp)
ogrInfo(shp, layer = "SimU_all")
SIMU_poly <- readOGR(shp, layer = "SimU_all")

### CREATE INDIA data set
IND_map = getData('GADM', country="IND", level=0)
plot(IND_map)

### 
# Crop and mask raster to IND
IND_raster <- mask(SIMU_5min, IND_map, updateNA=TRUE)
IND_raster <- crop(IND_raster, IND_map, updateNA=TRUE)
plot(IND_raster)

# Crop and mask poly to IND
IND_poly <- crop(SIMU_poly, IND_map, updateNA=TRUE)
plot(IND_poly) 



# GET TARGET FILE
target <- file[["X2005"]]
plot(target)
target
projection(target)
projection(SIMU_poly)
target_poly <- rasterToPolygons(target)

# Harmonize projections
proj4string(target) <- proj4string(SIMU_poly)
new_spdf <- raster::intersect(target_poly, SIMU_poly)
plot(new_spdf)
projection(new_spdf)
new_spdf
SIMU_poly
inter <- rgeos::intersect(target_poly, SIMU_poly)
plot(inter)

polygonValues(srdf, r, weights=TRUE)
test <- raster::resample(file, SIMU_5min, method = "bilinear")

test2 <- stack(test, SIMU_5min)
test2

check3 <- rasterToPoints(test2)

### Aggregate over countries
# Worldmap polygon
worldmap <- map("world", fill = TRUE, plot = FALSE)
map(worldmap)
worldmapPolys <- map2SpatialPolygons(worldmap, 
                                     IDs=sapply(strsplit(worldmap$names, ":"), "[", 1L), 
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))

# Compare raster and country map to see if projection is the same => OK
plot(fid)
plot(worldmapPolys, add = T)
p.df <- data.frame(ID=1:length(worldmapPolys), country = names(worldmapPolys)) 


# Calculate total (irrigated plus rainfed) production per zone
waterPerCountry <- fid %>%
  raster::extract(., worldmapPolys, df=T) %>%
  group_by(ID) %>%
  summarize(value = sum(Domestic.water.consumption, na.rm=T)) 
check <- left_join(p.df, waterPerCountry) %>%
  mutate(value = as.integer(value/1000000))

%>%
  left_join(., p.df)



library(RNetCDF)
fid <- open.nc(ncdf_file)
print.nc(fid); 
names(fid)
dat <- read.nc(fid)
close.nc(fid)

plot(fid)
; print.nc(fid); dat <- read.nc(fid); close.nc(fid)


### CALCULATE MEAN
# Function to calculate mean over set of years
mean_st_f <- function(stack, years) {
  stack_period <- stack[[years]]
  stack_mean <- mean(stack_period, na.rm = T)
}

# Periods
P2010 <- paste0("X", c(2005:2014))
P2020 <- paste0("X", c(2015:2024))
P2030 <- paste0("X", c(2025:2034))
P2020 <- paste0("X", c(2035:2044))
P2020 <- paste0("X", c(2045:2054))


raster2df_f <- function(rasterfile){
  TMP <- rasterfile %>% rasterToPoints %>% data.frame %>%
    dplyr::rename(lon=x, lat=y)
  return(TMP)
}