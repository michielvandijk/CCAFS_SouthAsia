#'========================================================================================================================================
#' Project:  CCAFS South Asia
#' Subject:  Download GADM maps
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
#p_load("WDI", "countrycode")


### SET WORKING DIRECTORY
wdPath <- "~/CCAFS_SouthAsia"
setwd(wdPath)

### SET DATAPATH

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### FUNCTIONS
# Obtain country coordinates for target country
GADM_f <- function(iso3c, lev=0, proj = "+proj=longlat +datum=WGS84"){
  dataPath <- paste("Data/Maps")
  if(!dir.exists(dataPath)) dir.create(dataPath)
  gadm = getData('GADM', country=iso3c, level=lev, path = dataPath)
  # change projection 
  projection <- proj
  country.sp <- spTransform(gadm, CRS(projection))
  return(country.sp)
}

# List of countries
# http://stackoverflow.com/questions/9950144/access-lapply-index-names-inside-fun
countries <- c("IND", "PAK", "BGD", "NPL", "LKA")
SA_adm0 <- sapply(countries, GADM_f, lev=0, simplify = FALSE, USE.NAMES = TRUE) # In this way list names are preserved, which is not the case with lapply


# Create map of all regions combined
# Function to rename map ID so there is not conflict
# http://stackoverflow.com/questions/5126745/gadm-maps-cross-country-comparision-graphics
ID_rename_f <- function(i ,countrylist) {
  name <- names(countrylist)[i]
  countrylist[[i]] <- spChFIDs(countrylist[[i]], paste(name, row.names(countrylist[[i]]), sep = "_"))
}

SA_adm0 <- lapply(seq_along(SA_adm0), ID_rename_f, SA_adm0)
SA_adm0 <- do.call(rbind, SA_adm0)
plot(SA_adm0)

saveRDS(SA_adm0, "Data/maps/SA_adm0.rds")

