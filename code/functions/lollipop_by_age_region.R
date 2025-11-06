lollipop_by_age_region = function(data, disease = "All"){
  if(disease != "All"){
    data <- data %>% 
      filter(str_detect(StandardisedCondition, disease))    
  } 
  
  dat_plot = data %>%  
    group_by(TrialID, WHO_Region, age_min, age_max) %>% 
    slice(1) %>% 
    ungroup() %>% 
    left_join(ages) %>% 
    mutate(age_min = as.numeric(str_sub(age_min, end = -2)),
           age_max = as.numeric(str_sub(age_max, end = -2))) %>%
    filter(!(is.na(age_min) & is.na(age_max))) %>% 
    arrange(desc(WHO_Region), desc(age_min), desc(age_max)) %>% 
    mutate(
      Trial_num = row_number())
  
  print(str_c("Unique studies: ", length(unique(dat_plot$TrialID)), "Total rows in plot: ", nrow(dat_plot)))
  
  plot = ggplot(
    dat_plot, 
    aes(
      y=Trial_num, 
      x=age_min, 
      xend=age_max))+
    xlab("Inclusion Age")+
    ylab("Studies")+
    labs(color = "Region")+
    theme(panel.background = element_rect(fill = "#FFFFFF"),
          plot.background = element_rect(fill = "#ffffff"),
          panel.grid = element_line(colour = "lightgray"),
          plot.title = element_text(size = 14),
          axis.text.y = element_blank(),
          plot.subtitle = element_text(size = 10),
          legend.text = element_text(size = 15),
          legend.key.width = unit(1, "cm")) +
    geom_segment(
      aes(x = age_min,
          xend=age_max,
          y=Trial_num, yend = Trial_num, colour = WHO_Region), lwd = 2.5) +
    scale_color_manual(values = colours_set3, drop = FALSE) +
    # scale_colour_viridis_d()+
    geom_point(aes(x = age_min, colour = WHO_Region))+
    geom_point(aes(x = age_max, colour = WHO_Region))+
    geom_vline(xintercept = 5, 
               color = colours_set3[12], 
               linetype=2, lwd = 1.5, alpha = 0.5)+
    geom_vline(xintercept = 15, 
               color = colours_set3[12], 
               linetype=2, lwd = 1.5, alpha = 0.5) +
    # Highlight when there is a min age but no max age
    geom_segment(
      data=dat_plot %>%  filter (is.na(age_max)),
      aes(
        x=age_min,
        xend=100,
        y=Trial_num, yend = Trial_num, color=WHO_Region), linetype=2, arrow=arrow(length=unit(0.1,"inches")), show.legend = FALSE) +
    # Highlight when there is a max age but no min age
    geom_segment(
      data=dat_plot %>%  filter (is.na(age_min)),
      aes(
        x=age_max,
        xend=0,
        y=Trial_num, yend = Trial_num, color=WHO_Region),linetype=2,arrow=arrow(length=unit(0.1,"inches")), show.legend = FALSE) +
    scale_x_continuous(breaks = c(0, 5, 15, 25, 50, 75, 100)) 
  
  return(plot)
}
