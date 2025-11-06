map_by_studies = function(disease, burden_data = NULL, year_from = 1998, year_until = 2024,
                          scale_factor){
  centroid_data = centroids %>%
    group_by(ISO) %>% 
    slice(1) %>% 
    ungroup()
  
  date_from = str_c(year_from, "-01-01") %>% 
    as_date()
  
  date_until = str_c(year_until, "-01-01") %>% 
    as_date()
  
  if(is.null(burden_data)){
    countries = ictrp_split %>% 
      filter(str_detect(StandardisedCondition, disease),
             DATE_REGISTRATION >= date_from & DATE_REGISTRATION < date_until) %>% 
      group_by(COUNTRY) %>% 
      summarise(n_studies = n()) %>% 
      ungroup() %>% 
      filter(!is.na(COUNTRY)) %>% 
      left_join(world_map, by = c("COUNTRY" = "alpha_3_code")) %>% 
      left_join(centroid_data, by = c("alpha_2_code" = "ISO"))
    
    plot = ggplot() +
      geom_polygon(data = world_map %>% filter(alpha_3_code != "ATA"), 
                   aes(x = long, y = lat, group = group), 
                   colour = "black", fill = "#E8E8E8") +
      labs(fill = "Number of Studies") +
      theme(panel.background = element_rect(fill = "#FFFFFF"),
            plot.background = element_rect(fill = "#FFFFFF"),
            panel.grid = element_blank(),
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 10),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            legend.key.height = unit(0.8, 'cm')) +
      geom_point(countries, mapping = aes(x = longitude, y = latitude, 
                                          color = n_studies), 
                 pch = 18, size = 3)  
  } else{
    countries = ictrp_split %>% 
      filter(str_detect(StandardisedCondition, disease),
             DATE_REGISTRATION >= date_from & DATE_REGISTRATION < date_until) %>% 
      group_by(COUNTRY) %>% 
      summarise(n_studies = n()) %>% 
      ungroup() %>% 
      filter(!is.na(COUNTRY)) %>% 
      full_join(burden_data, by = c("COUNTRY" = "alpha_3_code")) %>% 
      left_join(world_map, by = c("COUNTRY" = "alpha_3_code")) %>% 
      left_join(centroid_data, by = c("alpha_2_code.x" = "ISO"))
    
    if(year_from == 2014 & year_until == 2024){
      plot = ggplot() +
        geom_polygon(data = world_map %>% filter(alpha_3_code != "ATA"), 
                     aes(x = long, y = lat, group = group), 
                     colour = "black", fill = "gray") +
        labs(size = "Number of Studies", fill = str_c("Mean Annual DALYs per Million Population (2014-23)")) +
        theme(panel.background = element_rect(fill = "#FFFFFF"),
              plot.background = element_rect(fill = "#FFFFFF"),
              panel.grid = element_blank(),
              plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 10),
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              legend.key.height = unit(0.8, 'cm')) +
        geom_polygon(countries %>% 
                       filter(daly_mean_14_23_ppop > 0), mapping = aes(x = long, y = lat, 
                                              group = group, fill = daly_mean_14_23_ppop*1000000), 
                     alpha = 0.85, colour = "black") +
        # geom_point(countries, mapping = aes(x = longitude, y = latitude, 
        #                                     size = (n_studies*3)), 
        #            pch = 18, color = "black") +
        geom_point(countries, mapping = aes(x = longitude, y = latitude, 
                                            size = as.integer(n_studies)), 
                   pch = 18, color = "#D5573B") +
        scale_size_continuous(breaks = scales::pretty_breaks( )) +
        scale_fill_viridis_c(breaks = scales::pretty_breaks(), na.value = "gray") +
        guides(
          fill = guide_colourbar(order = 1,reverse = FALSE),
          shape = guide_legend(order = 2)
        )   
    }  else if(year_from == 2004 & year_until == 2014){
      plot = ggplot() +
        geom_polygon(data = world_map %>% filter(alpha_3_code != "ATA"), 
                     aes(x = long, y = lat, group = group), 
                     colour = "black", fill = "gray") +
        labs(size = "Number of Studies", fill = str_c("Mean Annual DALYs per Million Population (2004-13)")) +
        theme(panel.background = element_rect(fill = "#FFFFFF"),
              plot.background = element_rect(fill = "#FFFFFF"),
              panel.grid = element_blank(),
              plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 10),
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              legend.key.height = unit(0.8, 'cm')) +
        geom_polygon(countries %>% 
                       filter(daly_mean_04_13_ppop > 0), mapping = aes(x = long, y = lat, 
                                                                       group = group, fill = daly_mean_04_13_ppop*1000000), 
                     alpha = 0.85, colour = "black") +
        # geom_point(countries, mapping = aes(x = longitude, y = latitude, 
        #                                     size = (n_studies*3)), 
        #            pch = 18, color = "black") +
        geom_point(countries, mapping = aes(x = longitude, y = latitude, 
                                            size = as.integer(n_studies)), 
                   pch = 18, color = "#D5573B") +
        scale_size_continuous(breaks = scales::pretty_breaks( )) +
        scale_fill_viridis_c(breaks = scales::pretty_breaks(), na.value = "gray") +
        guides(
          fill = guide_colourbar(order = 1,reverse = FALSE),
          shape = guide_legend(order = 2)
        )   
    } else if(year_from == 2019 & year_until == 2024){
      plot = ggplot() +
        geom_polygon(data = world_map %>% filter(alpha_3_code != "ATA"), 
                     aes(x = long, y = lat, group = group), 
                     colour = "black", fill = "gray") +
        labs(size = "Number of Studies", fill = str_c("Mean Annual DALYs per Million Population (2019-23)")) +
        theme(panel.background = element_rect(fill = "#FFFFFF"),
              plot.background = element_rect(fill = "#FFFFFF"),
              panel.grid = element_blank(),
              plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 10),
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              legend.key.height = unit(0.8, 'cm')) +
        geom_polygon(countries %>% 
                       filter(daly_mean_19_23_ppop > 0), mapping = aes(x = long, y = lat, 
                                                                       group = group, fill = daly_mean_19_23_ppop*1000000), 
                     alpha = 0.85, colour = "black") +
        # geom_point(countries, mapping = aes(x = longitude, y = latitude, 
        #                                     size = (n_studies*3)), 
        #            pch = 18, color = "black") +
        geom_point(countries, mapping = aes(x = longitude, y = latitude, 
                                            size = as.integer(n_studies)), 
                   pch = 18, color = "#D5573B") +
        scale_size_continuous(breaks = scales::pretty_breaks( )) +
        scale_fill_viridis_c(breaks = scales::pretty_breaks(), na.value = "gray") +
        guides(
          fill = guide_colourbar(order = 1,reverse = FALSE),
          shape = guide_legend(order = 2)
        )   
    } else if(year_from == 2014 & year_until == 2019){
      plot = ggplot() +
        geom_polygon(data = world_map %>% filter(alpha_3_code != "ATA"), 
                     aes(x = long, y = lat, group = group), 
                     colour = "black", fill = "gray") +
        labs(size = "Number of Studies", fill = str_c("Mean Annual DALYs per Million Population (2014-18)")) +
        theme(panel.background = element_rect(fill = "#FFFFFF"),
              plot.background = element_rect(fill = "#FFFFFF"),
              panel.grid = element_blank(),
              plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 10),
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              legend.key.height = unit(0.8, 'cm')) +
        geom_polygon(countries %>% 
                       filter(daly_mean_14_18_ppop > 0), mapping = aes(x = long, y = lat, 
                                                                       group = group, fill = daly_mean_14_18_ppop*1000000), 
                     alpha = 0.85, colour = "black") +
        # geom_point(countries, mapping = aes(x = longitude, y = latitude, 
        #                                     size = (n_studies*3)), 
        #            pch = 18, color = "black") +
        geom_point(countries, mapping = aes(x = longitude, y = latitude, 
                                            size = as.integer(n_studies)), 
                   pch = 18, color = "#D5573B") +
        scale_size_continuous(breaks = scales::pretty_breaks( )) +
        scale_fill_viridis_c(breaks = scales::pretty_breaks(), na.value = "gray") +
        guides(
          fill = guide_colourbar(order = 1,reverse = FALSE),
          shape = guide_legend(order = 2)
        )   
    } else if(year_from == 2009 & year_until == 2014){
      plot = ggplot() +
        geom_polygon(data = world_map %>% filter(alpha_3_code != "ATA"), 
                     aes(x = long, y = lat, group = group), 
                     colour = "black", fill = "gray") +
        labs(size = "Number of Studies", fill = str_c("Mean Annual DALYs per Million Population (2009-13)")) +
        theme(panel.background = element_rect(fill = "#FFFFFF"),
              plot.background = element_rect(fill = "#FFFFFF"),
              panel.grid = element_blank(),
              plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 10),
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              legend.key.height = unit(0.8, 'cm')) +
        geom_polygon(countries %>% 
                       filter(daly_mean_09_13_ppop > 0), mapping = aes(x = long, y = lat, 
                                                                       group = group, fill = daly_mean_09_13_ppop*1000000), 
                     alpha = 0.85, colour = "black") +
        # geom_point(countries, mapping = aes(x = longitude, y = latitude, 
        #                                     size = (n_studies*3)), 
        #            pch = 18, color = "black") +
        geom_point(countries, mapping = aes(x = longitude, y = latitude, 
                                            size = as.integer(n_studies)), 
                   pch = 18, color = "#D5573B") +
        scale_size_continuous(breaks = scales::pretty_breaks( )) +
        scale_fill_viridis_c(breaks = scales::pretty_breaks(), na.value = "gray") +
        guides(
          fill = guide_colourbar(order = 1,reverse = FALSE),
          shape = guide_legend(order = 2)
        )   
    } else if(year_from == 2004 & year_until == 2009){
      plot = ggplot() +
        geom_polygon(data = world_map %>% filter(alpha_3_code != "ATA"), 
                     aes(x = long, y = lat, group = group), 
                     colour = "black", fill = "gray") +
        labs(size = "Number of Studies", fill = str_c("Mean Annual DALYs per Million Population (2004-08)")) +
        theme(panel.background = element_rect(fill = "#FFFFFF"),
              plot.background = element_rect(fill = "#FFFFFF"),
              panel.grid = element_blank(),
              plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 10),
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              legend.key.height = unit(0.8, 'cm')) +
        geom_polygon(countries %>% 
                       filter(daly_mean_04_08_ppop > 0), mapping = aes(x = long, y = lat, 
                                              group = group, fill = daly_mean_04_08_ppop*1000000), 
                     alpha = 0.85, colour = "black") +
        # geom_point(countries, mapping = aes(x = longitude, y = latitude, 
        #                                     size = (n_studies*3)), 
        #            pch = 18, color = "black") +
        geom_point(countries, mapping = aes(x = longitude, y = latitude, 
                                            size = as.integer(n_studies)), 
                   pch = 18, color = "#D5573B") +
        scale_size_continuous(breaks = scales::pretty_breaks( )) +
        scale_fill_viridis_c(breaks = scales::pretty_breaks(), na.value = "gray") +
        guides(
          fill = guide_colourbar(order = 1,reverse = FALSE),
          shape = guide_legend(order = 2)
        )   
    } else{
      plot = ggplot() +
        geom_polygon(data = world_map %>% filter(alpha_3_code != "ATA"), 
                     aes(x = long, y = lat, group = group), 
                     colour = "black", fill = "gray") +
        labs(size = "Number of Studies", fill = str_c("Mean Annual DALYs per Million Population (1999-2023)")) +
        theme(panel.background = element_rect(fill = "#FFFFFF"),
              plot.background = element_rect(fill = "#FFFFFF"),
              panel.grid = element_blank(),
              plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 10),
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              legend.key.height = unit(0.8, 'cm')) +
        geom_polygon(countries%>% 
                       filter(daly_mean_tot_ppop > 0), mapping = aes(x = long, y = lat, 
                                              group = group, fill = daly_mean_tot_ppop*1000000), 
                     alpha = 0.85, colour = "black") +
        scale_fill_viridis_c(breaks = scales::pretty_breaks(), na.value = "gray") + 
        # geom_point(countries, mapping = aes(x = longitude, y = latitude, 
        #                                     size = (n_studies*3)), 
        #            pch = 18, color = "black") +
        geom_point(countries, mapping = aes(x = longitude, y = latitude, 
                                            size = as.integer(n_studies)), 
                   pch = 18, color = "#D5573B") +
        scale_size_continuous(breaks = scales::pretty_breaks( )) +
        guides(
          fill = guide_colourbar(order = 1,reverse = FALSE),
          shape = guide_legend(order = 2)
        )   
    }
  }

  return(plot)
}
