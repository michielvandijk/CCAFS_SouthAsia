#'========================================================================================================================================
#' Project:  CCAFS South Asia
#' Subject:  Download basic info per country
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

### DOWNLOAD WDI DATA

# WDI data
# WDIsearch('total population')
WDI_raw <- WDI(country = "all", indicator = c("NY.GDP.PCAP.PP.CD", "SP.RUR.TOTL.ZS", "SP.POP.TOTL",
                                           "SI.POV.NAGP", "AG.LND.IRIG.AG.ZS", "AG.LND.AGRI.ZS",
                                           "ER.H2O.FWTL.ZS", "ER.H2O.FWAG.ZS", "SH.STA.MALN.ZS", 
                                             "SN.ITK.DEFC.ZS", "SL.AGR.EMPL.ZS", "SI.POV.NAHC",
                                           "NV.AGR.TOTL.ZS", "NY.GDP.MKTP.PP.KD"
                                         ),
                  start=1960, end=2016) %>%
  mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c")) 
 
saveRDS(WDI_raw, file = paste("Cache/WDI_", Sys.Date(), ".rds", sep=""))
