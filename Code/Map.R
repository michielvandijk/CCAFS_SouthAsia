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


### PREPARE GLOBIOM MAP DATA
# Obtain GLOBIOM SIMU raster and link file
# CHECK FILE, more adf files
SIMU_map <- raster(file.path(dataPath, "CCAFS_SouthAsia/Data/simu_raster\\sta.adf"))
proj4string(SIMU_map)
proj4string(SA_adm0)

# projection seems the same but we harmonise to be sure
SA_adm <- spTransform(SA_adm0, CRS(proj4string(SIMU_map)))

# Read link variable
SimUIDLUID_map <- read_csv(file.path(dataPath, "CCAFS_SouthAsia/Data/simu_raster\\SimUIDLUID_map.csv"))

# Cut out Asia
SIMU_SA <- crop(SIMU_map, SA_adm0, updateNA=TRUE)
SIMU_SA <- mask(SIMU_SA, SA_adm0, updateNA=TRUE)
plot(SIMU_SA)
plot(SA_adm, add = T)

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
  file2 <- file.path(dataPath, paste0("CCAFS_SouthAsia\\gdx\\", file))
  print(file)
  df <- rgdx.param(file2, Symbol, names, compress = T)
  df$fileName <- file
  return(df)
}

# Read scenario definitions and add file name
scenDef <- read_excel(file.path(dataPath, "CCAFS_SouthAsia\\gdx\\scen_definitions.xlsx")) %>%
  mutate(fileName = paste0("output_SSPs_CCAFS_CC-", GDX_name, ".gdx"),
         scenName = trimws(scenName),
         climate_CC = trimws(climate_CC)) %>%
  dplyr::select(-SSP)

# Read gdx files
files <- list.files(file.path(dataPath, "CCAFS_SouthAsia\\gdx\\"), pattern = "output_SSPs_CCAFS_C")


### AREAPROD
colnames <- c("region","LUId","altClass", "slpClass","soilClass", "AEZClass", "crop", "areaProd", "scen", "allBioenScen", "scenName", "year", "value", "filename")

AreaProdGrid_raw <- bind_rows(lapply(files[1], gdx_load_f, "IRRACR_COMPARE", colnames)) %>%
  left_join(., scenDef) %>%
  mutate(year = as.numeric(as.character(year)),
         LUId = as.character(LUId),
         iso3c = countrycode(region, "country.name", "iso3c")) %>%
  filter(iso3c %in% countries)

# Share of irrigated land
IRR_sh <- AreaProdGrid_raw %>%
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
  mutate(IRRSh = value/sum(value, na.rm = T)) %>% # Calculate share of IRR_Area
  filter(areaProd %in% c("IRR_Area"))

# Plot Irrigated area
df <- inner_join(IRR_sh, SIMU_Asia_df) 


# Create map
Asia_ra <- fortify(Asia_pg)

# Map
Map_IRRsh <- ggplot() +
  geom_raster(data = df, aes(y = lat, x = lon, fill = IRRSh)) + 
  #geom_path(data = Asia_ra, aes(y = lat, x = long, group = group), colour = "black") +
  coord_equal() +
  theme_bw() +
  labs(
    title = "Share of irrigated area",
    #subtitle = "check",
    #caption = "",
    x="", y="") +
  theme_classic() +
  theme(legend.key=element_blank(),
        line = element_blank(),
        axis.text = element_blank()) +
  scale_fill_gradientn(colours = rev(terrain.colors(10))) + 
  facet_wrap(~year) 

#Map_IRRsh
