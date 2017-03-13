#'========================================================================================================================================
#' Project: CCAFS South Asia
#' Subject: Create maps a
#' Author:  Michiel van Dijk
#' Contact: michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("WDI", "countrycode", "gdxrrw")

### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

auxPath <- "C:\\Users\\vandijkm\\GLOBIOM\\Auxiliary"
dataPath <- "P:\\globiom\\Projects\\Water" 

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### LINK GAMS LIBRARIES
GAMSPath <- "C:\\GAMS\\win64\\24.4"
igdx(GAMSPath)

### LOAD SA MAP
SA_adm0 <- readRDS(file.path(root, "Data/Maps/SA_adm0.rds"))

# fortify map
SA_adm0_fo <- fortify(SA_adm0)


### PREPARE GLOBIOM MAP DATA
# Obtain GLOBIOM SIMU raster and link file
# CHECK FILE, more adf files
SIMU_map <- raster(file.path(dataPath, "CCAFS_SouthAsia/Data/simu_raster\\sta.adf"))

# projection seems the same but we harmonise to be sure
SA_adm <- spTransform(SA_adm0, CRS(proj4string(SIMU_map)))

# Read link variable
SimUIDLUID_map <- read_csv(file.path(dataPath, "CCAFS_SouthAsia/Data/simu_raster\\SimUIDLUID_map.csv"))

# Cut out Asia
SIMU_SA <- crop(SIMU_map, SA_adm0, updateNA=TRUE)
SIMU_SA <- mask(SIMU_SA, SA_adm0, updateNA=TRUE)
#plot(SIMU_SA)
#plot(SA_adm, add = T)

# Create dataframe and link LU 
raster2df_f <- function(rasterfile){
  TMP<-rasterfile %>% rasterToPoints %>% data.frame %>%
    dplyr::rename(lon=x, lat=y)
  return(TMP)
}

SIMU_SA_df <- raster2df_f(SIMU_SA) %>%
  rename(SimUID = sta) %>%
  left_join(., SimUIDLUID_map)


### GLOBIOM DATA PREPARATION
# Functon to load gdx files
gdx_load_f <- function(file, Symbol, names = NULL){
  file2 <- file.path(dataPath, paste0("CCAFS_SouthAsia\\Data\\GLOBIOM\\", file))
  print(file)
  df <- rgdx.param(file2, Symbol, names, compress = T)
  df$fileName <- file
  return(df)
}

# Read scenario definitions and add file name
scenDef <- read_excel(file.path(dataPath, "CCAFS_SouthAsia\\Data\\GLOBIOM\\scen_definitions.xlsx")) %>%
  mutate(fileName = paste0("output_SSPs_CCAFS_CC-", GDX_name, ".gdx"),
         scenName = trimws(scenName),
         climate_CC = trimws(climate_CC)) %>%
  dplyr::select(-SSP)

# Read gdx files
files <- list.files(file.path(dataPath, "CCAFS_SouthAsia\\Data\\GLOBIOM\\"), pattern = "output_SSPs_CCAFS_C")

### GRIDDED AREA DATA
# # Load gridded area data per scenario
# # Baseline scenarios are NoCC of NewUSA, Jugaad, PeoplePower, Precipice
# # Corresponding with GDX files # 0,1,3,4.gdx (see scenDef)
# colnames <- c("region","LUId","altClass", "slpClass","soilClass", "AEZClass", "crop", "areaProd", "scen", "allBioenScen", "scenName", "year", "value", "filename")
# 
# # 2000 map
# Area_raw <- bind_rows(lapply(files[c(1,2, 24,29)], gdx_load_f, "IRRACR_COMPARE", colnames)) %>%
#   left_join(., scenDef) %>%
#   mutate(year = as.numeric(as.character(year)),
#          LUId = as.character(LUId),
#          iso3c = countrycode(region, "country.name", "iso3c")) %>%
#   filter(iso3c %in% c("IND", "BGD", "NPL", "LKA", "PAK"),
#          year %in% c(2000, 2050)) %>%
#   droplevels()
# 
# saveRDS(Area_raw, "Cache/Area_raw.rds")
Area_raw <- readRDS(file.path(root, "Cache/Area_raw.rds"))

# Share of irrigated land
IRR_sh_raw <- Area_raw %>%
  group_by(iso3c, LUId, crop, areaProd, scenName, year) %>%
  summarize(
    value = sum(value, na.rm = T)) %>% # Aggregate LUId that are distributed over multiple countries and AEZs
  droplevels %>%
  ungroup %>%
  filter(areaProd %in% c("IRR_Area", "RNFD_Area")) %>%
  group_by(iso3c, LUId, areaProd, scenName, year) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  group_by(iso3c, LUId, scenName, year) %>%
  mutate(IRRsh = value/sum(value, na.rm = T)) %>% # Calculate share of IRR_Area
  filter(areaProd %in% c("IRR_Area"))

# Plot Irrigated area in 2000
IRR_sh_2000 <- IRR_sh_raw %>%
  filter(year == 2000, scenName == "PeoplePowerNoCC") %>%
  left_join(., SIMU_SA_df)

Map_IRRsh_2000 <- ggplot() +
  geom_raster(data = IRR_sh_2000, aes(y = lat, x = lon, fill = IRRsh)) + 
  geom_path(data = SA_adm0_fo, aes(y = lat, x = long, group = group), colour = "black") +
  coord_equal() +
  theme_bw() +
  labs(
    title = "Share of irrigated area in 2000",
    #subtitle = "check",
    #caption = "",
    x="", y="") +
  theme_classic() +
  theme(legend.key=element_blank(),
        line = element_blank(),
        axis.text = element_blank()) +
  scale_fill_gradientn(colours = rev(terrain.colors(10))) 

Map_IRRsh_2000

# Plot change in irrigated area for period 2000-2050 for different scenarios
IRR_sh_ch <- IRR_sh_raw %>%
  group_by(iso3c, LUId, scenName) %>%
  mutate(index = (IRRsh/IRRsh[year == 2000])-1) %>%
  filter(year == 2050) %>%
  left_join(., SIMU_SA_df)

Map_IRRsh_ch <- ggplot() +
  geom_raster(data = IRR_sh_ch, aes(y = lat, x = lon, fill = index)) + 
  #geom_path(data = SA_adm0_fo, aes(y = lat, x = long, group = group), colour = "black") +
  coord_equal() +
  theme_bw() +
  labs(
    title = "Change in share of irrigated area 2000/2050 by scenario",
    #subtitle = "check",
    #caption = "",
    x="", y="") +
  theme_bw() +
  theme(legend.key=element_blank(),
        line = element_blank(),
        axis.text = element_blank()) +
  scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  facet_wrap(~scenName) 

Map_IRRsh_ch


