#'========================================================================================================================================
#' Project:  CCAFS South Asia
#' Subject:  Plot functions
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

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
  range_tot <- df %>%
    group_by(year, iso3c, scen) %>%
    summarize(max = max(value),
              min = min(value))
  
  range_tot <- bind_rows(
    df %>% 
      group_by(year, scen, iso3c) %>%
      summarize(value = max(value)),
    df %>% 
      group_by(year, scen, iso3c) %>%
      summarize(value = min(value)) %>%
      arrange(-year))
  
  #  baseline
  key_scen <- df %>%
    filter(GCM == "NoCC")
  
  p = ggplot() 
  p = p + geom_polygon(data = range_tot, aes(x = year, y = value, fill = scen), alpha = 0.3)
  #p = p + geom_line(data = df, aes(x = year, y = value, group = scenName, colour = scen)) 
  p = p + geom_line(data = key_scen, aes(x = year, y = value, colour = scen), size = 1)
  p = p + labs(title = title, x = "", y = unique(df$ind1))
  p = p + facet_wrap(~iso3c)
  p = p + scale_color_manual(name = "", values = c(scen_col)) +
    scale_fill_manual(name = "", values = c(scen_col)) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks = seq(2000, 2050, 10), expand = c(0, 0)) +
    guides(colour = guide_legend(nrow=1)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position="bottom",
          aspect.ratio=0.7) 
  
  p
}
