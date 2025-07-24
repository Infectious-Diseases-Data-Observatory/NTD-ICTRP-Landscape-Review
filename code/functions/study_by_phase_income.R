trial_by_map_prop_income = function(data, disease = "All"){
  if(disease != "All"){
    data <- data %>% 
      filter(str_detect(StandardisedCondition, disease))    
  }
  
  phase_income = data %>% 
    filter(CENTRE == "Single Country",
           !is.na(PHASE), 
           PHASE != "NOT APPLICABLE",
           STUDY_TYPE == "INTERVENTIONAL")  %>% 
    group_by(PHASE, income_group) %>% 
    summarise(N_Trials = n()) %>% 
    ungroup() %>% 
    mutate(prop_trials = N_Trials/sum(N_Trials)) %>% 
    arrange(desc(PHASE)) 
  
  ggplot(phase_income, aes(x = PHASE, y = N_Trials)) +
    geom_col( color = "black", fill = "#80B1D3") +
    coord_flip() +
    theme_classic() +
    scale_color_manual(values = colours_set3, drop = FALSE)+
    scale_fill_manual(values = colours_set3, drop = FALSE) +
    theme(legend.position = "none") +
    labs(y = "Number (Proportion) of Trials",
         x = "Trial Phase") +
    geom_text(data = phase_income %>% 
                filter(N_Trials >= max(N_Trials)-1),
              aes(label = str_c(N_Trials, "  (",round(prop_trials, 2)*100, "%)")), nudge_y = -1,
              color = "white", fontface = "bold")+
    geom_text(data = phase_income %>% 
                filter(!(N_Trials >= max(N_Trials)-1)), 
              aes(label = str_c(N_Trials, "  (",round(prop_trials, 2)*100, "%)")), nudge_y = 1,
              color = "black", fontface = "bold")+
    facet_wrap(vars(income_group), nrow = 2)+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 4))
}
