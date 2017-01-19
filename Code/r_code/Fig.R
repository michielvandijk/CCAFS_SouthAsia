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

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### LINK GAMS LIBRARIES
GAMSPath <- "C:\\GAMS\\win64\\24.4"
igdx(GAMSPath)

### GLOBIOM DATA PREPARATION
# Functon to load gdx files
gdx_load_f <- function(file, Symbol, names = NULL){
  file2 <- file.path(dataPath, paste0("Data\\GLOBIOM\\", file))
  print(file)
  df <- rgdx.param(file2, Symbol, names, compress = T)
  df$fileName <- file
  return(df)
}

# Read scenario definitions and add file name
scenDef <- read_excel(file.path(dataPath, "Data\\GLOBIOM\\scen_definitions.xlsx")) %>%
  mutate(fileName = paste0("output_SSPs_CCAFS_CC-", GDX_name, ".gdx"),
         scenName = trimws(scenName),
         climate_CC = trimws(climate_CC)) %>%
  dplyr::select(-SSP)

# Read gdx files
files <- list.files(file.path(dataPath, "Data\\GLOBIOM"), pattern = "output_SSPs_CCAFS_C")

### GET DATA
## Pivot data
colnames <- c("ind1","scen","GCM","SSP","crop", "ind2", "region", "year")
Pivot <- bind_rows(lapply(files, gdx_load_f, "pivot", colnames)) %>%
  left_join(., scenDef) %>%
  mutate(year = as.numeric(as.character(year)),
         iso3c = countrycode(region, "country.name", "iso3c"))
rm(colnames)


# Plot function
plot1_f <- function(df) {
  title = unique(with(df, paste(ind1, crop, ind2, sep="_")))
  print(title)
  ggplot() +
    geom_line(data = df, aes(x = year, y = value, color = reg, group = reg))   +
    facet_wrap(~Scenname_GCM) +
    theme_bw() +
    labs(
      title = title,
      #subtitle = "check",
      #caption = "",
      x = "" , y = unique(df$ind1))
}

# Plot function
plot2_f <- function(df) {
  
  # Total range of scenarios
  range_tot <- bind_rows(
    df %>% 
      group_by(year, region) %>%
      summarize(value = max(value)),
    df %>% 
      group_by(year, region) %>%
      summarize(value = min(value)) %>%
      arrange(-year))
  
  # 90 percentile
  range_90 <- bind_rows(
    df %>% 
      group_by(year, region) %>%
      summarize(value = quantile(value, probs = c(0.1))),
    df %>% 
      group_by(year, region) %>%
      summarize(value = quantile(value, probs = c(0.9)))  %>%
      arrange(-year))
  
  #  Key scenarios
  key_scen <- df %>%
    filter(scenName %in% c("PeoplePowerNoCC"))
  
  title = unique(with(df, paste(ind1, crop, ind2, sep="_")))
  print(title)
  p = ggplot() + geom_polygon(data = range_tot, aes(x = year, y = value), alpha = 0.3, fill = "grey70")
  p = p + geom_polygon(data = range_90, aes(x = year, y = value), alpha = 0.3, fill = "grey50")
  p = p + geom_line(data = df, aes(x = year, y = value, group = scenName), color = "grey") 
  p = p + geom_line(data = key_scen, aes(x = year, y = value, colour = scenName), size = 1)
  p = p + theme_classic()
  p = p + scale_colour_discrete(guide = guide_legend(title = NULL))
  p = p + theme(legend.position = c(0.05, 0.8), legend.justification = c(0,0))
  p = p + labs(title = title, x = "", y = unique(df$ind1))
  p = p + facet_wrap(~region)
  p
}

plot3_f <- function(df, title) {
  
  # Total range of scenarios
  range_tot <- bind_rows(
    df %>% 
      group_by(year, region, scen) %>%
      summarize(value = max(value)),
    df %>% 
      group_by(year, region, scen) %>%
      summarize(value = min(value)) %>%
      arrange(-year))
  
  #  Key scenarios
  key_scen <- df %>%
    filter(scen %in% CCAFSscen, GCM == "NoCC")
  
  p = ggplot() + geom_polygon(data = range_tot, aes(x = year, y = value, group = scen, fill = scen), alpha = 0.3)
  p = p + geom_line(data = df, aes(x = year, y = value, group = scenName, colour = scen)) 
  p = p + geom_line(data = key_scen, aes(x = year, y = value, colour = scen), size = 1.25)
  p = p + theme_classic()
  p = p + scale_colour_discrete(guide = FALSE)
  p = p + scale_fill_discrete(guide = guide_legend(title = NULL))
  p = p + theme(legend.position = c(0.1, 0.7), legend.justification = c(0,0))
  p = p + labs(title = title, x = "", y = unique(df$ind1))
  p = p + facet_wrap(~region)
  p
}


### DOWNLOAD WDI HISTORICAL DATA
WDIsearch("GDP, PPP")
WDI_raw <- WDI(country="all", indicator=c("SP.POP.TOTL", "NY.GDP.MKTP.PP.KD"),
               start=1960, end=2016) %>%
  mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c")) %>%
  filter(!is.na(iso3c))

saveRDS(WDI, file = paste("Cache/WDI_", Sys.Date(), ".rds", sep=""))

WDI <- filter(WDI_raw, iso3c %in% c("IND", "BGD", "NPL", "LKA", "PAK")) %>%
  rename(Population = SP.POP.TOTL, GDP = NY.GDP.MKTP.PP.KD) %>%
  mutate(Population = Population/1000000, GDP = GDP/1000000000) %>%
  select(-iso2c) %>%
  gather(variable, value, -year, -iso3c, -country) %>%
  na.omit()

GDPPOP_hist <- WDI %>%
  group_by(year, variable) %>%
  summarize(value = sum(value, na.rm=T)) %>%
  rename(ind2 = variable)


### GDP  
# Remove Bhutan because scenarios are not prepared
# Macro data for all scenarios is stored in each GDX file so here we use info from GDX file 0
GDP_sim <- filter(Pivot, ind2 %in% c("GDP"), GDX_name == 0, region %in% c("Bangladesh", "India", "Nepal", "Pakistan" , "SriLanka")) %>%
  group_by(scen, ind2, year) %>%
  summarise(value = sum(value, na.rm=T)) %>%
  filter(scen %in% c("NewUSA", "Jugaad", "UnstFlourish", "PeoplePower", "Precipice"))

GDP_hist <- filter(GDPPOP_hist, ind2 == "GDP")

Fig_GDP <- ggplot(data = GDP_sim, aes(x = year, y = value, colour = scen)) +
  geom_line(data = GDP_hist, aes(x = year, y = value), colour = "black", size = 1.5) +
  geom_line( size = 1.5) +
  labs(
    title = "",
    #subtitle = "check",
    #caption = "",
    x = "" , y = "GDP") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow=1)) +
  scale_y_continuous(labels = comma) +
  scale_color_brewer(name = "", 
                     labels = c("New USA", "Jugaad" , "Unst Flourish", "People Power", " Precipice"), palette = "Spectral")

Fig_GDP


### Population
POP_sim <- filter(Pivot, ind2 %in% c("Population"), GDX_name == 0, region %in% c("Bangladesh", "India", "Nepal", "Pakistan" , "SriLanka")) %>%
  group_by(scen, ind2, year) %>%
  summarise(value = sum(value, na.rm=T)) %>%
  filter(scen %in% c("NewUSA", "Jugaad", "UnstFlourish", "PeoplePower", "Precipice"))

POP_hist <- filter(GDPPOP_hist, ind2 == "Population", year <= 2000)

Fig_POP <- ggplot(data = POP_sim, aes(x = year, y = value, colour = scen)) +
  geom_line(data = POP_hist, aes(x = year, y = value), colour = "black", size = 1.5) +
  geom_line( size = 1.5) +
  labs(
    title = "",
    #subtitle = "check",
    #caption = "",
    x = "" , y = "GDP") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow=1)) +
  scale_y_continuous(labels = comma) +
  scale_color_brewer(name = "", 
                     labels = c("New USA", "Jugaad" , "Unst Flourish", "People Power", " Precipice"), palette = "Spectral")

Fig_POP

### Yield
yld_sim <- filter(Pivot, ind1 %in% c("YIELD"), GDX_name == 0, region %in% c("Bangladesh", "India", "Nepal", "Pakistan" , "SriLanka")) %>%
  group_by(scen, crop, ind2, year) %>%
  summarise(value = mean(value, na.rm=T)) %>%
  filter(scen %in% c("NewUSA", "Jugaad", "UnstFlourish", "PeoplePower", "Precipice"))

Fig_yld <- ggplot(data = yld_sim, aes(x = year, y = value, colour = ind2)) +
  geom_line( size = 1.5) +
  facet_wrap(~crop, scales = "free", ncol = 3) +
  labs(
    title = "",
    #subtitle = "check",
    #caption = "",
    x = "" , y = "GDP") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow=1)) +
  scale_y_continuous(labels = comma) 

  #scale_color_brewer(name = "", 
  #                   labels = c("Rainfed", "Irrigated")) # , palette = "Spectral")

Fig_yld


### Kcals
# Prepare data
CCAFSscen <- c("NewUSA", "Jugaad", "PeoplePower", "Precipice") #"UnstFlourish"
kcals <- filter(Pivot, ind1 == "pckal") %>%
  filter(scen %in% CCAFSscen) %>%
  droplevels()

# Plot  
Fig_kcal <- plot3_f(kcals, "kcalories")

### Prices
# Prepare data
Prices <- filter(Pivot, ind1 == "RegPrice", ind2 == "Sup", crop == "Rice") %>%
  filter(scen %in% CCAFSscen) %>%
  droplevels()

# Plot  
Fig_prices <- plot3_f(Prices, "Prices")

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


