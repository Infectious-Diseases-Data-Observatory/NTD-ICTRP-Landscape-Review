make_map = function(disease, burden_data){
  # centroid_data = centroids %>%
  #   group_by(ISO) %>% 
  #   slice(1) %>% 
  #   ungroup() %>% 
  #   rename("name" = "COUNTRY")
  
  centroid_data = CoordinateCleaner::countryref %>% 
    group_by(iso3) %>% 
    slice(1) %>% 
    ungroup() %>% 
    filter(type =="country") %>% 
    rename("longitude" = "centroid.lon", "latitude" = "centroid.lat",
           "ISO" = "iso2")
  
  data_04_13 = ictrp_split %>% 
    filter(str_detect(StandardisedCondition, disease),
           DATE_REGISTRATION >= ymd("2004-01-01") & DATE_REGISTRATION < ymd("2014-01-01")) %>% 
    group_by(COUNTRY) %>% 
    summarise(n_studies = n()) %>% 
    ungroup() %>% 
    filter(!is.na(COUNTRY)) %>% 
    full_join(burden_data, by = c("COUNTRY" = "alpha_3_code")) %>% 
    left_join(world_map, by = c("COUNTRY" = "alpha_3_code", "alpha_2_code", "income_group")) %>% 
    left_join(centroid_data, by = c("alpha_2_code" = "ISO"))
  
  data_14_23 = ictrp_split %>% 
    filter(str_detect(StandardisedCondition, disease),
           DATE_REGISTRATION >= ymd("2014-01-01") & DATE_REGISTRATION < ymd("2024-01-01")) %>% 
    group_by(COUNTRY) %>% 
    summarise(n_studies = n()) %>% 
    ungroup() %>% 
    filter(!is.na(COUNTRY)) %>% 
    full_join(burden_data, by = c("COUNTRY" = "alpha_3_code")) %>% 
    left_join(world_map, by = c("COUNTRY" = "alpha_3_code", "alpha_2_code", "income_group")) %>% 
    left_join(centroid_data, by = c("alpha_2_code" = "ISO"))
  
  plot_04_13 = ggplot() +
    geom_polygon(data = world_map %>% filter(alpha_3_code != "ATA"), 
                 aes(x = long, y = lat, group = group), 
                 colour = "black", fill = "gray") +
    labs(size = "Number of studies", fill = str_c("Mean annual DALYs per \nmillion population (2004-13)")) +
    theme(panel.background = element_rect(fill = "#FFFFFF"),
          plot.background = element_rect(fill = "#FFFFFF"),
          panel.grid = element_blank(),
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.key.height = unit(0.8, 'cm')) +
    geom_polygon(data_04_13 %>% 
                   filter(daly_mean_04_13_ppop > 0), mapping = aes(x = long, y = lat, 
                                                                   group = group, fill = daly_mean_04_13_ppop*1000000), 
                 alpha = 0.85, colour = "black") +
    geom_polygon(world_map %>% 
                   filter(alpha_3_code == "LSO"), mapping = aes(x = long, y = lat, 
                                                                group = group), 
                 alpha = 0.85, colour = "black", fill = "gray") +
    geom_point(data_04_13 %>% group_by(COUNTRY) %>% slice(1) %>% ungroup(), 
               mapping = aes(x = longitude, y = latitude, 
                                        size = as.integer(n_studies)), 
               pch = 18, color = "#D5573B", alpha = 0.85) +
    scale_size_continuous(breaks = scales::pretty_breaks(n=4), labels = label_number(accuracy = 1)) +
    scale_fill_viridis_c(breaks = scales::pretty_breaks(), na.value = "gray") +
    guides(
      fill = guide_colourbar(order = 1,reverse = FALSE),
      shape = guide_legend(order = 2)
    )   
  
  plot_14_23 = ggplot() +
    geom_polygon(data = world_map %>% filter(alpha_3_code != "ATA"), 
                 aes(x = long, y = lat, group = group), 
                 colour = "black", fill = "gray") +
    labs(size = "Number of studies", fill = str_c("Mean annual DALYs per \nmillion population (2014-23)")) +
    theme(panel.background = element_rect(fill = "#FFFFFF"),
          plot.background = element_rect(fill = "#FFFFFF"),
          panel.grid = element_blank(),
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.key.height = unit(0.8, 'cm')) +
    geom_polygon(data_14_23 %>% 
                   filter(daly_mean_14_23_ppop > 0), mapping = aes(x = long, y = lat, 
                                                                   group = group, fill = daly_mean_14_23_ppop*1000000), 
                 alpha = 0.85, colour = "black") +
    geom_polygon(world_map %>% 
                   filter(alpha_3_code %in% "LSO"), mapping = aes(x = long, y = lat, 
                                                                group = group), 
                 alpha = 0.85, colour = "black", fill = "gray")+
    geom_point(data_14_23 %>% group_by(COUNTRY) %>% slice(1) %>% ungroup(), 
               mapping = aes(x = longitude, y = latitude, 
                                         size = as.integer(n_studies)), 
               pch = 18, color = "#D5573B", alpha = 0.85) +
    scale_size_continuous(breaks = scales::pretty_breaks(n=4),labels = label_number(accuracy = 1)) +
    scale_fill_viridis_c(breaks = scales::pretty_breaks(), na.value = "gray") +
    guides(
      fill = guide_colourbar(order = 1,reverse = FALSE),
      shape = guide_legend(order = 2)
    )   
  
  plot_grid(plot_04_13, plot_14_23, ncol = 1)
}
