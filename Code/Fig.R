#'========================================================================================================================================
#' Project:  CCAFS South Asia
#' Subject:  Create figures
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
p_load("gdxrrw", "WDI", "countrycode")

### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

### DATAPATH
dataPath <- "P:\\globiom\\Projects\\Water\\CCAFS_SouthAsia" 

### SOURCE
source(file.path(root, "Code/Plot_f.r"))

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### LINK GAMS LIBRARIES
GAMSPath <- "C:\\GAMS\\win64\\24.4"
igdx(GAMSPath)

### GLOBIOM DATA PREPARATION
# Function to load gdx files
gdx_load_f <- function(file, Symbol, names = NULL){
  file2 <- file.path(dataPath, paste0("Data\\GLOBIOM\\AprilRuns\\", file))
  print(file)
  df <- rgdx.param(file2, Symbol, names, compress = T)
  df$fileName <- file
  return(df)
}

# Read scenario definitions and add file name
scenDef <- read_excel(file.path(dataPath, "Data\\GLOBIOM\\scen_definitions.xlsx")) %>%
  mutate(
         #fileName = paste0("output_SSPs_CCAFS_CC-", GDX_name, ".gdx"),
         fileName = paste0("output_SSPs_CCAFS_CC1_gw_dem_eff-", GDX_name, ".gdx"),
         scenName = trimws(scenName),
         climate_CC = trimws(climate_CC)) %>%
  dplyr::select(-SSP)

# Read gdx files
files <- list.files(file.path(dataPath, "Data\\GLOBIOM\\AprilRuns"), pattern = "output_SSPs_CCAFS_C")

### GET DATA
# Define colours, scenarios, etc
target_scen <- c("NewUSA", "Jugaad", "PeoplePower", "Precipice") #"UnstFlourish"
scen_col <- brewer.pal(5,"Set1")
target_crops <- c("Rice", "Whea", "SugC", "Mill", "Pota", "Corn", "Soya", "Cass", "Srgh") # "ChkP" not in simulation

# Pivot data
# CHECK: some data is presented for both India and IndiaReg, which should be the same
# colnames <- c("ind1","scen","GCM","SSP","crop", "ind2", "region", "year")
# Pivot <- bind_rows(lapply(files, gdx_load_f, "pivot", colnames)) %>%
#   left_join(., scenDef) %>%
#   filter(scen %in% target_scen) %>%
#   mutate(iso3c = countrycode(region, "country.name", "iso3c"),
#          year = as.numeric(as.character(year)),
#          iso3c = ifelse(region == "World", "WLD", iso3c),
#          iso3c = ifelse(region == "IndiaReg", "IREG", iso3c),
#          iso3c = ifelse(region == "RSAS", "RSA", iso3c),
#          scen = factor(scen, levels = c("Precipice", "Jugaad", "PeoplePower", "UnstFlourish", "NewUSA"))) %>%
#   filter(!is.na(iso3c)) %>%
#   droplevels()
# rm(colnames)
# 
# saveRDS(Pivot, "Cache/Pivot.rds")
Pivot <- readRDS(file.path(root, "Cache/Pivot.rds"))



### LOAD WDI HISTORICAL DATA
WDI_raw <- readRDS(file.path(root, "Cache/WDI_2017-03-08.rds"))

WDI <- filter(WDI_raw, iso3c %in% c("IND", "BGD", "NPL", "LKA", "PAK")) %>%
  rename(Population = SP.POP.TOTL, GDP = NY.GDP.MKTP.PP.KD) %>%
  mutate(Population = Population/1000000, GDP = GDP/1000000000) %>%
  dplyr::select(-iso2c) %>%
  gather(variable, value, -year, -iso3c, -country) %>%
  na.omit()

### GDP  
# Remove Bhutan because scenarios are not prepared
# Macro data for all scenarios is stored in each GDX file so here we use info from GDX file 0
# Simulation GDP is based on different PPP series. 
# We use the growth index to link the series so everything is expressed in WDI GDP PPP

# country
GDP_hist <- WDI %>%
  filter(variable == "GDP", year <= 2000) %>%
  mutate(country = countrycode(iso3c, "iso3c", "country.name"))

GDP_hist_base2000 <- GDP_hist %>%
  filter(year == 2000) %>%
  dplyr::rename(Base2000 = value) %>%
  select(-year, -country, -variable)

GDP_sim <- Pivot %>%
  filter(ind2 %in% c("GDP"), GDX_name == 0, iso3c %in% c("IND", "BGD", "NPL", "LKA", "PAK")) %>%
  select(iso3c, scen, year, value) %>%
  group_by(iso3c, scen) %>%
  mutate(index = value/value[year == 2000]) %>%
  left_join(., GDP_hist_base2000) %>%
  mutate(value = Base2000*index,
         country = countrycode(iso3c, "iso3c", "country.name"))

Fig_GDP <- ggplot(data = GDP_sim, aes(x = year, y = value, colour = scen)) +
  geom_line(data = GDP_hist, aes(x = year, y = value), colour = "black", size = 1.5) +
  geom_line( size = 1.5) +
  labs(
    title = "",
    #subtitle = "check",
    #caption = "",
    x = "" , y = "PPP (constant 2011 international $)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow=1)) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(1990, 2050, 10)) +
  scale_color_manual(name = "", 
                     values = scen_col) +
  facet_wrap(~country, scales = "free")

Fig_GDP

# SA
GDP_hist_SA <- GDP_hist %>%
  group_by(year) %>%
  summarize(value = sum(value))

GDP_sim_SA <- GDP_sim %>%
  group_by(year, scen) %>%
  summarize(value = sum(value))

Fig_GDP_SA <- ggplot(data = GDP_sim_SA, aes(x = year, y = value, colour = scen)) +
  geom_line(data = GDP_hist_SA, aes(x = year, y = value), colour = "black", size = 1.5) +
  geom_line( size = 1.5) +
  labs(
    title = "",
    #subtitle = "check",
    #caption = "",
    x = "" , y = "PPP (constant 2011 international $)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow=1)) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(1990, 2050, 10)) +
  scale_color_manual(name = "", 
                    values = scen_col) 
Fig_GDP_SA


### Population
# Simulation population has a slight offset in comparison to the WDI historical series. 
# We use the growth index to link the series

# Countries
POP_hist <- WDI %>%
  filter(variable == "Population", year <= 2000) %>%
  mutate(country = countrycode(iso3c, "iso3c", "country.name"))

POP_hist_base2000 <- POP_hist %>%
  filter(year == 2000) %>%
  dplyr::rename(Base2000 = value) %>%
  select(-year, -country, -variable)

POP_sim <- filter(Pivot, ind2 %in% c("Population"), GDX_name == 0, iso3c %in% c("IND", "BGD", "NPL", "LKA", "PAK")) %>%
  select(iso3c, scen, year, value) %>%
  group_by(iso3c, scen) %>%
  mutate(index = value/value[year == 2000]) %>%
  left_join(., POP_hist_base2000) %>%
  mutate(value = Base2000*index,
         country = countrycode(iso3c, "iso3c", "country.name"))

Fig_POP <- ggplot(data = POP_sim, aes(x = year, y = value, colour = scen)) +
  geom_line(data = POP_hist, aes(x = year, y = value), colour = "black", size = 1.5) +
  geom_line( size = 1.5) +
  labs(
    title = "",
    #subtitle = "check",
    #caption = "",
    x = "" , y = "million people") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow=1)) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(1960, 2050, 10)) +
  scale_color_manual(name = "", 
                     values = scen_col) +
  facet_wrap(~country, scales = "free")

Fig_POP


# SA
POP_hist_SA <- POP_hist %>%
  group_by(year) %>%
  summarize(value = sum(value))

POP_sim_SA <- POP_sim %>%
  group_by(year, scen) %>%
  summarize(value = sum(value))

Fig_POP_SA <- ggplot(data = POP_sim_SA, aes(x = year, y = value, colour = scen)) +
  geom_line(data = POP_hist_SA, aes(x = year, y = value), colour = "black", size = 1.5) +
  geom_line( size = 1.5) +
  labs(
    title = "",
    #subtitle = "check",
    #caption = "",
    x = "" , y = "Million people") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow=1)) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(1960, 2050, 10)) +
  scale_color_manual(name = "", 
                     values = scen_col) 
Fig_POP_SA


### Yield
# Load historical yield data from FAOSTAT
YLD_hist <- readRDS(file.path(root, "Cache/FAOSTAT_production_crop_SA.rds")) %>%
  filter(ALLPRODUCT %in% target_crops) %>%
  mutate(scen = "FAOSTAT") %>%
  ungroup()

# Aggregate to the SA level
AREA_base <- YLD_hist %>%
  filter(year == 2000) %>%
  select(iso3c, ALLPRODUCT, area_base = area) 

YLD_hist_SA <- YLD_hist %>%
  left_join(., AREA_base) %>%
  group_by(ALLPRODUCT, year) %>%
  summarise(yield_dm = sum(yield_dm*area_base, na.rm=T)/sum(area_base, na.rm = T))

# Prepare simulated data
YLD_sim <- filter(Pivot, ind1 %in% c("YIELD"), iso3c %in% c("IND", "BGD", "NPL", "LKA", "PAK")) %>%
  select(iso3c, scen, year, yield_dm = value, crop, ind2, GCM) %>%
  # group_by(iso3c, scen, crop, ind2, GCM) %>%
  # mutate(index = value/value[year == 2000]) %>%
  # ungroup() %>%
  # group_by(iso3c, scen, crop, ind2, year) %>%
  # mutate(max = max(value),
  #        min = min(value)) %>%
  filter(GCM == "NoCC") %>%
  mutate(ALLPRODUCT = recode(crop, 
                             Wheat = "Whea", 
                             Maize = "Corn",
                             SUGAR = "SugC",
                             Sorghum = "Srgh",
                             Millet = "Mill",
                             Cassava = "Cass",
                             Potatoes = "Pota",
                             `Sweet Potatoes` = "SwPo",
                             Groundnuts = "Gnut",
                             Soybeans = "Soya")) %>%
  filter(ALLPRODUCT %in% target_crops)

YLD_sim_SA <- YLD_sim  %>%
  left_join(., AREA_base) %>%
  group_by(ALLPRODUCT, year, scen, ind2) %>%
  summarise(yield_dm = sum(yield_dm*area_base, na.rm=T)/sum(area_base, na.rm = T))

# SA graph
# CHECK HOW TO DEAL WITH WEIGHTS OF IRR AND RAINFED => leads to the result that YLD irr <- YLD rainfed for Srgh
Fig_YLD_SA <- ggplot() +
  geom_line(data = YLD_hist_SA, aes(x = year, y = yield_dm), colour = "black", size = 1) +
  geom_line(data = YLD_sim_SA, aes(x = year, y = yield_dm, colour = scen, linetype = ind2), size = 1) +
  #geom_point(data = df, aes(x = year, y = value, colour = scen, shape = ind2), size = 1) +
  facet_wrap(~ ALLPRODUCT, scales = "free", ncol = 3) +
  labs(
    title = "",
    #subtitle = "check",
    #caption = "",
    x = "" , y = "tons/ha (dm)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(1960, 2050, 10), expand = c(0, 0)) +
  scale_linetype_manual(name = "",
                      values = c("solid", "twodash")) +
  scale_color_manual(name = "", 
                     values = c(scen_col)) +
  theme(aspect.ratio=0.7,
        legend.position = "bottom")

Fig_YLD_SA


### CALORIE CONSUMPTION
POP_hist <- POP_hist %>%
  select(iso3c, POP = value)

# Load historical data
CAL_hist <- read.csv(file.path(root, "Data/calcpcpd.csv")) %>%
  mutate(iso3c = countrycode(AreaCode, "fao", "iso3c")) %>%
  filter(iso3c %in% c("IND", "BGD", "NPL", "LKA", "PAK")) %>%
  select(iso3c, year = Year, value = Value) %>%
  filter(year <= 2000)

CAL_hist_SA <- CAL_hist %>%
  left_join(., POP_hist) %>%
  mutate(iso3c = ifelse(iso3c == "IND", "IREG", "RSA")) %>%
  group_by(year, iso3c) %>%
  summarize(value = sum(value*POP, na.rm = T)/sum(POP, na.rm = T))

CAL_hist_base <- filter(CAL_hist_SA, year == 2000) %>%
  dplyr::rename(Base2000 = value) %>%
  ungroup() %>%
  select(-year)


# Simulation
CAL <- filter(Pivot, ind1 == "pckal") %>%
  filter(scen %in% target_scen) %>%
  droplevels() %>%
  group_by(iso3c, scenName) %>%
  mutate(index = value/value[year == 2000])

# Rebase simulations 2000 to historical data (2000=100)
CAL <- CAL %>%
  left_join(., CAL_hist_base) %>%
  mutate(value = Base2000*index)

# Figures by region
# Total range of scenarios
range_tot <- bind_rows(
    CAL %>% 
      group_by(year, scen, iso3c) %>%
      summarize(value = max(value)),
    CAL %>% 
      group_by(year, scen, iso3c) %>%
      summarize(value = min(value)) %>%
      arrange(-year))
  
#  baseline
key_scen <- CAL %>%
    filter(GCM == "NoCC")

# Figures by region
Fig_CAL = ggplot() +
  geom_line(data = CAL_hist_SA, aes(x = year, y = value), colour = "black", size = 1) +
  geom_polygon(data = range_tot, aes(x = year, y = value, fill = scen), alpha = 0.3) +
  geom_line(data = key_scen, aes(x = year, y = value, colour = scen), size = 1.25) +
  #geom_line(data = CAL, aes(x = year, y = value, group = scenName, colour = scen)) +
  facet_wrap(~iso3c) +
  labs(
    title = "",
    #subtitle = "check",
    #caption = "",
    x = "" , y = "Cal/day/cap") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(1960, 2050, 10), expand = c(0, 0)) +
  scale_color_manual(name = "", 
                     values = c(scen_col)) +
  scale_fill_manual(name = "", 
                     values = c(scen_col)) +
  guides(colour = guide_legend(nrow=1)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="bottom",
        aspect.ratio=0.7) 

Fig_CAL




### Prices
# Prepare data
Prices <- filter(Pivot, ind1 == "RegPrice", ind2 == "Sup", crop == "Rice")

# Plot  
Fig_prices <- plot3_f(Prices, "Prices")
Fig_prices

### Production
# colnames <- c("reg","SSP","scen2", "scen" ,"year", "crop", "ind1" , "value")
# Prod <- bind_rows(lapply(files, gdx_load_f, "IRRACR_reg_COMPARE")) %>%
#   filter(ind1 %in% c("IRR_Production", "RNFD_Production"))
# rm(colnames)
# 
# 
# # Water use
# WaterUse <- bind_rows(lapply(files, gdx_load_f, "Water_reg_Compare"))
# 
# # production
# colnames <- c("reg","SSP","scen2", "scen" ,"year", "crop", "ind1" , "value")
# Prod <- bind_rows(lapply(files, gdx_load_f, "IRRACR_reg_COMPARE")) %>%
#   filter(ind1 %in% c("IRR_Production", "RNFD_Production"))
# rm(colnames)
# 
# # calorie production
# yldcals <- bind_rows(lapply(files, gdx_load_f, "yld_cals")) 
# 
# # supply table
# colnames <- c("crop", "reg", "level" ,"scen2", "scen3", "scen", "type",  "year",  "value")
# Supply <- bind_rows(lapply(files, gdx_load_f, "SupQuantity_Compare", colnames)) %>%
#   filter(ind1 %in% c("IRR_Production", "RNFD_Production"))
# rm(colnames)
# 


