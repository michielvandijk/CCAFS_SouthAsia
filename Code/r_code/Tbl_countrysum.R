#'========================================================================================================================================
#' Project:  CCAFS South Asia
#' Subject:  Create table with basic info per country
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
BasePackages<- c("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer")
lapply(BasePackages, library, character.only = TRUE)
#SpatialPackages<-c("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
#lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <- c("WDI", "countrycode", "FAOSTAT")
lapply(AdditionalPackages, library, character.only = TRUE)


### SET WORKING DIRECTORY
library(rprojroot)
root <- find_root(is_rstudio_project)

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### DOWNLOAD WDI DATA

# WDI data
countries <- c("IND", "PAK", "BGD", "NPL", "LKA")


# WDI Macro variables
# WDIsearch('total population')
# WDI_raw <- WDI(country = "all", indicator = c("NY.GDP.PCAP.PP.CD", "SP.RUR.TOTL.ZS", "SP.POP.TOTL",
#                                           "SI.POV.NAGP", "AG.LND.IRIG.AG.ZS", "AG.LND.AGRI.ZS",
#                                           "ER.H2O.FWTL.ZS", "SH.STA.MALN.ZS", "SN.ITK.DEFC.ZS"
#                                         ),
#                  start=2000, end=2016) %>%
#  mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c")) %>%
#  filter(iso3c %in% countries)
 
# saveRDS(WDI_raw, file = paste("Cache/WDI_", Sys.Date(), ".rds", sep=""))
WDI_raw <- readRDS(file = file.path(root, "Cache/WDI_2016-12-14.rds"))

WDI_meta <- as.data.frame(WDI_data[[1]]) 

# select last available year and link names
Tbl_WDI <- WDI_raw %>% 
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

write_csv(Tbl_WDI, "Data/Tbl_WDI.csv")
