#'========================================================================================================================================
#' Project:  CCAFS South Asia
#' Subject:  Create tables for paper
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("WDI", "countrycode", "FAOSTAT")

### SET WORKING DIRECTORY
library(rprojroot)
root <- find_root(is_rstudio_project)

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### WDI TABLE
# Load data
WDI_raw <- readRDS(file = file.path(root, "Cache/WDI_2017-01-31.rds"))

# Get meta data
WDI_meta <- as.data.frame(WDI_data[[1]]) 

# select last available year and link names
countries <- c("IND", "PAK", "BGD", "NPL", "LKA")

Tab_WDI <- WDI_raw %>% 
  filter(iso3c %in% countries) %>%
  mutate(SP.POP.TOTL = SP.POP.TOTL/1000000) %>%
  gather(indicator, value, -country, -iso3c, -iso2c, -year) %>%
  na.omit %>%
  group_by(iso3c, country, indicator) %>%
  mutate(value = value[year == max(year)],
         maxyear = max(year))%>%
  filter(year == maxyear) %>%
  left_join(., WDI_meta) %>%
  ungroup() %>%
  dplyr::select(country, name, value) %>%
  spread(country, value)

# Select final variables and reorder
Tab_WDI <- Tab_WDI[c(2,5,1,3,7,11,10,8,6),]


### KEY FIGURES AS WORLD TOTAL
# Tab_SA_rel <- WDI_raw %>% 
#   filter(iso3c %in% c(countries) | country %in% c("World")) %>%
  
