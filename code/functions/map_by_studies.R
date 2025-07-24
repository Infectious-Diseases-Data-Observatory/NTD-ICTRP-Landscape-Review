map_by_studies = function(disease, death_data = NULL, year_from = 1998, scale_factor){
  centroid_data = centroids %>%
    group_by(ISO) %>% 
    slice(1) %>% 
    ungroup()
  
  date_from = str_c(year_from, "-01-01") %>% 
    as_date()
  
  if(is.null(death_data)){
    countries = ictrp_split %>% 
      filter(str_detect(StandardisedCondition, disease),
             DATE_REGISTRATION >= date_from) %>% 
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
      labs(fill = "Number of Studies",
           color = "Income Group") +
      theme(panel.background = element_rect(fill = "#FFFFFF"),
            plot.background = element_rect(fill = "#FFFFFF"),
            panel.grid = element_blank(),
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 10),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            legend.key.height = unit(0.8, 'cm')) +
      geom_polygon(countries, mapping = aes(x = long, y = lat, 
                                            group = group, fill = n_studies), 
                   alpha = 0.85, colour = "black") +
      scale_fill_gradient(high = colours_set3[10], low = colours_set3[5], 
                          breaks = scales::pretty_breaks(), na.value = "#e8e8e8") +
      geom_point(countries, mapping = aes(x = longitude, y = latitude, 
                                          color = income_group), 
                 pch = 18, size = 3)+
      scale_color_manual(values = c("#F28F3B",  "#2C5530", "#890620", "#F3FFB6"),
                         guide = "none")+
      guides(fill = guide_colourbar(order = 1, reverse = FALSE))    
  } else{
    countries = ictrp_split %>% 
      filter(str_detect(StandardisedCondition, disease),
             DATE_REGISTRATION >= date_from) %>% 
      group_by(COUNTRY) %>% 
      summarise(n_studies = n()) %>% 
      ungroup() %>% 
      filter(!is.na(COUNTRY)) %>% 
      full_join(death_data, by = c("COUNTRY" = "alpha_3_code")) %>% 
      left_join(world_map, by = c("COUNTRY" = "alpha_3_code")) %>% 
      left_join(centroid_data, by = c("alpha_2_code.x" = "ISO"))
    
    if(year_from == 2014){
      plot = ggplot() +
        geom_polygon(data = world_map %>% filter(alpha_3_code != "ATA"), 
                     aes(x = long, y = lat, group = group), 
                     colour = "black", fill = "#E8E8E8") +
        labs(fill = "Number of Studies",
             color = "Income Group", size = str_c("Total Deaths per Million Population")) +
        theme(panel.background = element_rect(fill = "#FFFFFF"),
              plot.background = element_rect(fill = "#FFFFFF"),
              panel.grid = element_blank(),
              plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 10),
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              legend.key.height = unit(0.8, 'cm')) +
        geom_polygon(countries, mapping = aes(x = long, y = lat, 
                                              group = group, fill = n_studies), 
                     alpha = 0.85, colour = "black") +
        scale_fill_gradient(high = colours_set3[10], low = colours_set3[5], 
                            breaks = scales::pretty_breaks(), na.value = "#e8e8e8") +
        geom_point(countries, mapping = aes(x = longitude, y = latitude, 
                                            color = income_group.x,
                                            size = death_2014_per_mill), 
                   pch = 18) +
        scale_color_manual(values = c("#F28F3B",  "#2C5530", "#890620", "#F3FFB6"),
                           guide = "none")+
        scale_size_continuous(breaks = scales::pretty_breaks()) +
        guides(
          fill = guide_colourbar(order = 1,reverse = FALSE),
          shape = guide_legend(order = 2)
        )   
    } else{
      plot = ggplot() +
        geom_polygon(data = world_map %>% filter(alpha_3_code != "ATA"), 
                     aes(x = long, y = lat, group = group), 
                     colour = "black", fill = "#E8E8E8") +
        labs(fill = "Number of Studies",
             color = "Income Group", size = str_c("Total Deaths per Million Population")) +
        theme(panel.background = element_rect(fill = "#FFFFFF"),
              plot.background = element_rect(fill = "#FFFFFF"),
              panel.grid = element_blank(),
              plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 10),
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              legend.key.height = unit(0.8, 'cm')) +
        geom_polygon(countries, mapping = aes(x = long, y = lat, 
                                              group = group, fill = n_studies), 
                     alpha = 0.85, colour = "black") +
        scale_fill_gradient(high = colours_set3[10], low = colours_set3[5], 
                            breaks = scales::pretty_breaks(), na.value = "#e8e8e8") +
        geom_point(countries, mapping = aes(x = longitude, y = latitude, 
                                            color = income_group.x,
                                            size = death_per_mill), 
                   pch = 18) +
        scale_color_manual(values = c("#F28F3B",  "#2C5530", "#890620", "#F3FFB6"),
                           guide = "none")+
        scale_size_continuous(breaks = scales::pretty_breaks()) +
        guides(
          fill = guide_colourbar(order = 1,reverse = FALSE),
          shape = guide_legend(order = 2)
        )   
    }
  }

  return(plot)
}
