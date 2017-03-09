#'========================================================================================================================================
#' Project:  TOOLS
#' Subject:  Process FAOSTAT crop production data
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

### SET DATAPATH
dataPath <- "C:\\Users\\vandijkm\\DATA\\FAOSTAT_20170117"
GLOBIOMPath <- "H:\\MyDocuments\\Projects\\Global-to-local-GLOBIOM\\GLOBIOM"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### DOWNLOAD HISTORICAL FAOSTAT DATA
# To be added by means of package FAOSTAT

### PREPARE HISTORICAL DATA
FAOSTAT_raw <- read_csv(file.path(dataPath, "Production_Crops_E_All_Data_(Normalized).csv")) 

# countrycode package does not include all FAO codes so we use a table supplied by FAOSTAT
FAOSTAT_conc <- read_csv(file.path(dataPath, "FAOSTAT_data_3-8-2017.csv")) %>%
  select(fao = `Country Code`, iso3c = `ISO3 Code`) %>%
  unique

# GLOBIOM CROPS
GLOBIOM_ALLITEM <- read_excel(file.path(GLOBIOMPath, "Concordance/GLOBIOM_mappings.xlsx"), sheet = "ALLITEM")
GLOBIOM_FAOCROPS_PRODUCTMAP <- read_excel(file.path(GLOBIOMPath, "Concordance/GLOBIOM_mappings.xlsx"), sheet = "FAOCROPS_PRODUCTMAP")

# Conversion to dry matter
GLOBIOM_fm2dm <- read_excel(file.path(GLOBIOMPath, "Conversion/GLOBIOM_conversion.xlsx"), sheet = "fm2dm")


# ETH has two codes covering two linked periods. We recode to one code.
# We keep the world aggregate
FAOSTAT <- FAOSTAT_raw %>%
  select(fao = `Area Code`, region = Area, FAOCROPNAME = Item, variable = Element, year = Year, unit = Unit, value = Value) %>%
  mutate(fao = ifelse(fao == 62, 238, fao)) %>%
  left_join(.,FAOSTAT_conc) %>%
  mutate(iso3c = ifelse(fao == 5000, "WLD", iso3c)) %>%
  left_join(.,GLOBIOM_FAOCROPS_PRODUCTMAP) %>%
  filter(iso3c %in% c("IND", "BGD", "NPL", "LKA", "PAK"),
         ALLPRODUCT %in% GLOBIOM_ALLITEM$ALLPRODUCT,
         variable != "Seed") %>%
  select(iso3c, value, variable, year, ALLPRODUCT) %>%
  spread(variable, value) %>%
  rename(yield = Yield, area = `Area harvested`, production = Production) %>%
  left_join(.,GLOBIOM_fm2dm) %>%
  mutate(yield = yield/10000,
         yield_dm = yield*dm_conv) # from Hecto gram to tonnes

saveRDS(FAOSTAT, "Cache/FAOSTAT_production_crop_SA.rds")

         
  


  

