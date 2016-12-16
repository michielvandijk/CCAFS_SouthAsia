#'========================================================================================================================================
#' Project:  CCAFS South Asia
#' Subject:  Create plots
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
BasePackages<- c("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
lapply(BasePackages, library, character.only = TRUE)
#SpatialPackages<-c("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
#lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <- c("gdxrrw")
lapply(AdditionalPackages, library, character.only = TRUE)


### SET WORKING DIRECTORY
root <- find_root(is_rstudio_project)

dataPath <- "P:\\globiom\\Projects\\Water" 

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

### GET DATA
## Pivot data
colnames <- c("ind1","scen","GCM","SSP","crop", "ind2", "region", "year")
Pivot <- bind_rows(lapply(files, gdx_load_f, "pivot", colnames)) %>%
  left_join(., scenDef) %>%
  mutate(year = as.numeric(as.character(year)))
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

### # GDP AND POP 
# Remove Bhutan because scenarios are not prepared
# Macro data off all scenarios is stored in each GDX file so here we use info from GDX file 0

# Prepare data
GDPPOP <- filter(Pivot, ind2 %in% c("GDP", "Population"), GDX_name == 0, region != "Bhutan") %>%
  group_by(scen, region, ind2) %>%
  mutate(index = value/value[year==2010]) %>%
  filter(year == 2050) %>%
  ungroup() %>%
  group_by(region, ind2) %>%
  mutate(max = max(index),
         min = min(index)) %>%
  filter(((scen == "PeoplePower" & region != "World") | scen == "SSP2" & region == "World"), !(region %in% c("IndiaReg", "RSAS")))

# Bar plot
# http://stackoverflow.com/questions/31458536/increase-space-between-bars-in-ggplot
Fig_GDPPOP = ggplot(data = GDPPOP, aes(x = ind2, y = index, fill = ind2)) +
  #scale_fill_manual(values = c("green", "white")) +
  geom_bar(stat="identity", colour = "black", position = position_dodge(width = 0.1), width=1) + 
  geom_errorbar(aes(ymax = max, ymin = min), position = position_dodge(width = 0.1), width = 0.25) +
  facet_grid(~region) +
  labs(
    title = "",
    #subtitle = "check",
    #caption = "",
    x = "" , y = "Index (2010=1)") +
  theme_classic() +
  guides(fill = "none") +
  theme(panel.spacing = unit(0, "lines")) 

Fig_GDPPOP

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



