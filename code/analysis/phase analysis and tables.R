source("code/cleaning/Preamble.R")
source("code/functions/study_by_phase_income.R")

#-------------------------------------------------------------------------------
# annotate_figure(ggarrange(
#   trial_by_map_prop_income(ictrp_split, "Chagas Disease"),
#   trial_by_map_prop_income(ictrp_split, "Schistosomiasis"),
#   trial_by_map_prop_income(ictrp_split, "Soil-Transmitted Helminthiases"),
#   trial_by_map_prop_income(ictrp_split, "Visceral Leishmaniasis"),
#   labels = c("CD", "Sch", "STH", "VL"),
#   vjust = 1, hjust = c(-0.5, -0.5, -0.15, -.4))
# )

for (disease in c("Chagas Disease", "Schistosomiasis", 
                  "Soil-Transmitted Helminthiases", "Visceral Leishmaniasis")) {
  data <- ictrp_split %>% 
    filter(str_detect(StandardisedCondition, disease))  
  
  phase_income = data %>% 
    filter(CENTRE == "Single Country",
           !is.na(PHASE), 
           PHASE != "NOT APPLICABLE",
           STUDY_TYPE == "INTERVENTIONAL")  %>% 
    group_by(PHASE, income_group) %>% 
    summarise(N_Trials = n()) %>% 
    ungroup() %>% 
    mutate(prop_trials = N_Trials/sum(N_Trials),
           PHASE = factor(PHASE, levels = c("PHASE IV TRIAL", "PHASE III TRIAL",
                                            "PHASE II/III TRIAL", "PHASE II TRIAL",
                                            "PHASE I/II TRIAL", "PHASE I TRIAL"))) %>% 
    arrange(desc(PHASE)) 
  
  phase_plot = ggplot(phase_income, aes(x = PHASE, y = N_Trials)) +
    geom_col( color = "black", fill = "#80B1D3") +
    coord_flip() +
    theme_classic() +
    scale_color_manual(values = colours_set3, drop = FALSE)+
    scale_fill_manual(values = colours_set3, drop = FALSE) +
    theme(legend.position = "none") +
    labs(y = "",
         title = disease,
         x = "") +
    geom_text(data = phase_income %>% 
                filter(N_Trials >= max(N_Trials)-1),
              aes(label = str_c(N_Trials, "  (",round(prop_trials, 2)*100, "%)")), nudge_y = -1,
              color = "white", fontface = "bold")+
    geom_text(data = phase_income %>% 
                filter(!(N_Trials >= max(N_Trials)-1)), 
              aes(label = str_c(N_Trials, "  (",round(prop_trials, 2)*100, "%)")), nudge_y = 1,
              color = "black", fontface = "bold")+
    facet_wrap(vars(income_group), nrow = 2, drop = FALSE)+
    scale_x_discrete(drop = FALSE)+
    scale_y_continuous(breaks = scales::pretty_breaks(),
                       limits = c(0,10))
  
  if(disease == "Chagas Disease"){
    phase_plot = phase_plot +
      xlab("Phase") 
    
    phase_cd = phase_plot
  } else if(disease == "Schistosomiasis"){
    # phase_plot = phase_plot + theme(legend.position="bottom") 
    
    phase_sch = phase_plot
  } else if(disease == "Soil-Transmitted Helminthiases"){
    phase_plot = phase_plot +
      ylab("Number of Studies (Proportion)")+
      xlab("Phase")
    
    phase_sth = phase_plot
  } else if(disease == "Visceral Leishmaniasis"){
    phase_plot = phase_plot+
      ylab("Number of Studies (Proportion)")
    
    phase_vl = phase_plot
  }
}

plot_grid(phase_cd, 
          phase_sch,
          phase_sth,
          phase_vl,
          ncol = 2)


ggsave("phase_income.tiff", path = "paper/figures/", width = 18, height = 10.0)
#-------------------------------------------------------------------------------
table1(~ CENTRE + RECRUITMENT_STATUS + PHASE| StandardisedCondition, data = ictrp,
       topclass = "Rtable1-zebra")

table1(~ PHASE + PLACEBO + MASKING + RANDOMIZATION + `PRIMARY PRUPOSE` | StandardisedCondition, data = ictrp %>% filter(STUDY_TYPE == "INTERVENTIONAL"),
       topclass = "Rtable1-zebra")
