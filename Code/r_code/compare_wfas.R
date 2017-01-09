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
AdditionalPackages <- c("WDI", "R.utils", "countrycode")
lapply(AdditionalPackages, library, character.only = TRUE)

### SET WORKING DIRECTORY
wdPath <- "CCAFS_SouthAsia"
setwd(wdPath)

dataPath <- "P:\\globiom\\Projects\\Water\\CCAFS_SouthAsia\\Data"
dataPath <- "H:\\TEMP\\watergap"

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
wfasold <- file.path(dataPath, "wfas\\old\\watergap_ssp2_rcp6p0_dom_ww_annual_2005_2100.nc")
wfasnew <- file.path(dataPath, "wfas\\new_Jan2015\\watergap_ssp2_rcp6p0_dom_ww_annual_2005_2100.nc")

library(RNetCDF)

wfasold  <- stack(wfasold)
names(wfasold) <- c(2005:2100)
wfasnew  <- stack(wfasnew)
names(wfasnew) <- c(2005:2100)


W_old <- wfasold[[6]]
W_new <- wfasnew[[6]]

# Visualise water map, take log as distribution is heavily skewed
# Most locations are in Green Land and the Northern part of the world but also some in African and Australia.
# Probably these are desert locations with no water demand
logW_old <- log(W_old)
levelplot(logW_old) + layer(sp.polygons(worldmap_poly))
logW_new <- log(W_new)
levelplot(logW_new) + layer(sp.polygons(worldmap_poly))

#### COMPARE TOTAL WATER DEMAND PER COUNTRY
countries <- names(worldmap_poly)
pid <- sapply(slot(worldmap_poly, "polygons"), function(x) slot(x, "ID"))
p.df <- data.frame(ID=1:length(worldmap_poly), country =  pid)     

# Water demand by country
# NB using fun = sum in extract resuls in some NAs (e.g. for India) although there is data
# Possible na.rm is ignored.
WD_old <- raster::extract(W_old, worldmap_poly, method = "bilinear", df = TRUE) %>%
  group_by(ID) %>%
  summarize(w_old = sum(X2010, na.rm = TRUE)) %>%
  left_join(p.df,.) %>%
  mutate(w_old = w_old/1000000,
         iso3c = countrycode(country, "country.name", "iso3c"))
sum(WD_old$w_old, na.rm = TRUE)


WD_new <- raster::extract(W_new, worldmap_poly, method = "bilinear", df = TRUE) %>%
  group_by(ID) %>%
  summarize(w_new = sum(X2010, na.rm = TRUE)) %>%
  left_join(p.df,.) %>%
  mutate(w_new = w_new/1000000,
         iso3c = countrycode(country, "country.name", "iso3c"))
sum(WD_new$w_new, na.rm = TRUE)

# check world total
WD_wld_old <- data.frame(rasterToPoints(W_old))
sum(WD_wld_old$X2010)/1000000

WD_wld_new <- data.frame(rasterToPoints(W_new))
sum(WD_wld_new$X2010)/1000000

# Compare with Excel sheet
WFASxl <- read_excel(file.path(dataPath, "wfas\\new_Jan2015\\WFaS_WaterUse_simple.xlsx"), sheet = "domestic") %>%
  filter(year == 2010, IdScen == 57) %>%
  mutate(iso3c = countrycode(Country, "country.name", "iso3c"))
sum(WFASxl$"domestic water withdrawals [mio.m3]", na.rm=T)
comp <- left_join(WFASxl, WD_new)

ggplot(data = comp, aes(x = w_new, y = `domestic water withdrawals [mio.m3]`)) +
  geom_abline(intercept = 0, slope=1)+
  geom_point()
