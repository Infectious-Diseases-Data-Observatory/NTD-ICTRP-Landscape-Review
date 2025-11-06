source("code/cleaning/Preamble.R")
# source("code/functions/lollipop_by_age_region.R")

#-------------------------------------------------------------------------------
# Cleaning age format
# ictrp_split$AGEMAX_YEARS = str_replace_all(ictrp_split$AGEMAX_YEARS, "72M", "6Y")
# ictrp_split$AGEMAX_YEARS = str_replace_all(ictrp_split$AGEMAX_YEARS, "6569D", "17Y")
# ictrp_split$AGEMAX_YEARS = str_replace_all(ictrp_split$AGEMAX_YEARS, "\\< ", "")
# ictrp_split$AGEMAX_YEARS = str_replace_all(ictrp_split$AGEMAX_YEARS, "\\<", "")
# ictrp_split$AGEMAX_YEARS = str_replace_all(ictrp_split$AGEMAX_YEARS, "48M", "4Y")
# ictrp_split$AGEMAX_YEARS = str_replace_all(ictrp_split$AGEMAX_YEARS, "59M", "5Y")
# 
# ictrp_split$AGEMIN_YEARS = str_replace_all(ictrp_split$AGEMIN_YEARS, "> ", "")
# ictrp_split$AGEMIN_YEARS = str_replace_all(ictrp_split$AGEMIN_YEARS, ">", "")
# ictrp_split$AGEMIN_YEARS = str_replace_all(ictrp_split$AGEMIN_YEARS, "12M", "1Y")
# ictrp_split$AGEMIN_YEARS = str_replace_all(ictrp_split$AGEMIN_YEARS, "1D", "0.002Y")
# ictrp_split$AGEMIN_YEARS = str_replace_all(ictrp_split$AGEMIN_YEARS, "3M", "0.25Y")
# ictrp_split$AGEMIN_YEARS = str_replace_all(ictrp_split$AGEMIN_YEARS, "6M", "0.5Y")
# ictrp_split$AGEMIN_YEARS = str_replace_all(ictrp_split$AGEMIN_YEARS, "730D", "2Y")
# 
# #-------------------------------------------------------------------------------
# # Plot age ranges
# annotate_figure(
#   ggarrange(lollipop_by_age_region(ictrp_split, disease = "Chagas Disease"),
#             lollipop_by_age_region(ictrp_split, disease = "Schistosomiasis"),
#             lollipop_by_age_region(ictrp_split, disease = "Soil-Transmitted Helminthiases"),
#             lollipop_by_age_region(ictrp_split, disease = "Visceral Leishmaniasis"),
#             labels = c("CD", "Sch", "STH", "VL"),
#             common.legend = TRUE, legend = "none", hjust = c(-0.1,-0.1,-0.1,-0.1))
# )


##########################
# table(ages$age_max, useNA = "ifany")

ages$age_min = str_replace_all(ages$age_min, "> ", "")
ages$age_min = str_replace_all(ages$age_min, ">", "")
ages$age_min = str_replace_all(ages$age_min, "12M", "1Y")
ages$age_min = str_replace_all(ages$age_min, "1D", "0.002Y")
ages$age_min = str_replace_all(ages$age_min, "3M", "0.25Y")
ages$age_min = str_replace_all(ages$age_min, "6M", "0.5Y")
ages$age_min = str_replace_all(ages$age_min, "730D", "2Y")

ages$age_max = str_replace_all(ages$age_max, "72M", "6Y")
ages$age_max = str_replace_all(ages$age_max, "6569D", "17Y")
ages$age_max = str_replace_all(ages$age_max, "\\< ", "")
ages$age_max = str_replace_all(ages$age_max, "\\<", "")
ages$age_max = str_replace_all(ages$age_max, "48M", "4Y")
ages$age_max = str_replace_all(ages$age_max, "59M", "5Y")

for (disease in c("Chagas Disease", "Schistosomiasis", 
                  "Soil-Transmitted Helminthiases", "Visceral Leishmaniasis")) {
  
  age_data = ictrp_split %>% 
    mutate(WHO_Region = factor(WHO_Region, levels = c("Africa", "Americas", 
                                                      "Eastern Mediterranean",
                                                      "Europe", "South-East Asia",
                                                      "Western Pacific", "Missing"))) %>% 
    filter(str_detect(StandardisedCondition, disease)) %>% 
    select(TrialID, WHO_Region) %>% 
    unique() %>% 
    left_join(ages, relationship = "many-to-many") %>% 
    mutate(min_age = as.numeric(str_sub(age_min, end = -2)),
           max_age = as.numeric(str_sub(age_max, end = -2))) %>%
    filter(!(age_min == "N/A" & age_max == "N/A"),
           !(age_min == "NOT SPECIFIED" & age_max == "NOT SPECIFIED"),
           !(age_min == "NOT APPLICABLE" & age_max == "NOT APPLICABLE")) %>% 
    group_by(WHO_Region,TrialID) %>%  
    arrange(desc(WHO_Region), desc(age_min), desc(age_max)) %>% 
    mutate(Trial_num = cur_group_id()) %>%
    ungroup()
  
  age_data[which(is.na(age_data$WHO_Region)), "WHO_Region"] = "Missing"
  
  print(str_c(disease, " unique studies: ", length(unique(age_data$TrialID)), ". Total rows in plot: ", nrow(age_data)))
  
  age_plot = ggplot(age_data, aes(y=Trial_num, yend = Trial_num, colour = WHO_Region,x=min_age, xend=max_age))+
    xlab("")+
    ylab("")+
    labs(color = "Region")+
    theme(panel.background = element_rect(fill = "#FFFFFF"),
          plot.background = element_rect(fill = "#ffffff"),
          panel.grid = element_line(colour = "lightgray"),
          # plot.title = element_text(size = 14),
          axis.text.y = element_blank(),
          # plot.subtitle = element_text(size = 10),
          legend.text = element_text(size = 12),
          # legend.position = "none",
          legend.key.width = unit(1, "cm")) +
    geom_segment(lwd = 2.5) +
    scale_color_manual(values = colours_set3, drop = FALSE) +
    # scale_colour_viridis_d()+
    geom_point(aes(x = min_age, colour = WHO_Region))+
    geom_point(aes(x = max_age, colour = WHO_Region))+
    geom_vline(xintercept = 5, color = colours_set3[12], 
               linetype = 2, lwd = 1.5, alpha = 0.5)+
    geom_vline(xintercept = 15, color = colours_set3[12], 
               linetype = 2, lwd = 1.5, alpha = 0.5) +
    # Highlight when there is a min age but no max age
    geom_segment(data = age_data %>% filter(is.na(max_age)),
                 aes(xend = 105), linetype = 2, 
                 arrow = arrow(length = unit(0.1, "inches")), show.legend = FALSE) +
    # Highlight when there is a max age but no min age
    geom_segment(data = age_data %>% filter (is.na(min_age)),
                 aes(xend = 0, x = max_age), linetype = 2,
                 arrow = arrow(length = unit(0.1, "inches")), show.legend = FALSE) +
    geom_segment(data = age_data %>% filter (is.na(min_age) & is.na(max_age)),
                 aes(x = 0, xend = 105), linetype = 2,
                 arrow = arrow(length = unit(0.1, "inches")), show.legend = FALSE) +
    scale_x_continuous(breaks = c(0, 5, 15, 25, 50, 75, 100))+
    # geom_hline(yintercept = c(3,79)) +
    scale_y_continuous(transform = "reverse") +
    labs(title = disease)
  
  if(disease == "Chagas Disease"){
    age_plot = age_plot +
      ylab("Studies") 
    
    age_cd = age_plot
  } else if(disease == "Schistosomiasis"){
    # age_plot = age_plot + theme(legend.position="bottom") 
    
    age_sch = age_plot
  } else if(disease == "Soil-Transmitted Helminthiases"){
    age_plot = age_plot +
      ylab("Studies")+
      xlab("Age")
    
    age_sth = age_plot
  } else if(disease == "Visceral Leishmaniasis"){
    age_plot = age_plot+
      xlab("Age")
    
    age_vl = age_plot
  }
}

plots = plot_grid(
  age_cd + theme(legend.position="none"), 
  age_sch + theme(legend.position="none"), 
  age_sth + theme(legend.position="none"), 
  age_vl + theme(legend.position="none"), 
  nrow = 2, ncol = 2
)
plots

legend = get_legend(age_vl)
legend
plot_grid(plots, legend, ncol = 2, rel_widths = c(0.6,0.1))

ggsave("ages.tiff", path = "paper/figures/", width = 15.6, height = 10.0)
