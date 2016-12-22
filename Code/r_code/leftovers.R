# Leftovers
# Map function
plot1_f <- function(df){
  print(unique(df$SPECIES))
  #title = unique(with(df, paste(AllScenYear, SPECIES, sep="_")))
  title = unique(df$SPECIES)
  
  ggplot() +
    geom_raster(data = df, aes(y = lat, x = lon, fill = value)) + 
    geom_path(data = Asia_ra, aes(y = lat, x = long, group = group), colour = "black") +
    coord_equal() +
    theme_bw() +
    labs(
      title = title,
      #subtitle = "check",
      #caption = "",
      x="", y="") +
    theme_classic() +
    theme(legend.key=element_blank(),
          line = element_blank(),
          axis.text = element_blank()) +
    scale_fill_gradientn(colours = rev(terrain.colors(10))) + 
    facet_wrap(~AllScenYear) 
}


# Make plots
p_test <- IIRA2 %>%
  group_by(SPECIES) %>%
  do(plots = plot1_f(.)) 

pdf(file = file.path(dataPath, paste0("CCAFS_SouthAsia/Graphs/p_test_", Sys.Date(), ".pdf")), width = 12, height = 8)
p_test$plots[1]
dev.off()


spread(areaProd, value) %>%
  
  %>%
  ungrou
mutate(IRRSh = value/sum(value, na.rm = T)) %>% # Calculate share of IRR_Area
  filter(IRRSh > 0.1)


%>%
  filter(areaProd == "IRR_Area") %>% # Filter out IRR_Area units
  group_by(iso3c, LUId, areaProd, scenName, year) %>%
  mutate(cropSh = value/sum(value, na.rm = T)) %>% # Calculate share of IRR_Area per crop
  summarize(m)

unique(AreaProdGrid$crop)

inner_join(SIMU_Asia_df,.) %>% 
  
  
  
  ###################
df <- filter(Pivot, crop == "Rice", ind1 == "YIELD", ind2 == "Total") %>%
  mutate(year = as.numeric(as.character(year)))
str(df)

p_pivot <- Pivot %>%
  group_by(ind1, crop, ind2, reg) %>%
  do(plots = plot3_f(.)) 

pdf(file = file.path(dataPath, paste0("CCAFS_SouthAsia/Graphs/p_pivot_", Sys.Date(), ".pdf")), width = 12, height = 8)
p_pivot$plots
dev.off()
#############################

### Kcals
#CHECK can/should we plot this by country?
kcals <- filter(Pivot, ind1 == "pckal") %>% droplevels()

plot3_f(kcals)

# Water use
WaterUse <- bind_rows(lapply(files, gdx_load_f, "Water_reg_Compare"))

# production
colnames <- c("reg","SSP","scen2", "scen" ,"year", "crop", "ind1" , "value")
Prod <- bind_rows(lapply(files, gdx_load_f, "IRRACR_reg_COMPARE")) %>%
  filter(ind1 %in% c("IRR_Production", "RNFD_Production"))
rm(colnames)

# calorie production
yldcals <- bind_rows(lapply(files, gdx_load_f, "yld_cals")) 

# supply table
colnames <- c("crop", "reg", "level" ,"scen2", "scen3", "scen", "type",  "year",  "value")
Supply <- bind_rows(lapply(files, gdx_load_f, "SupQuantity_Compare", colnames)) %>%
  filter(ind1 %in% c("IRR_Production", "RNFD_Production"))
rm(colnames)


  