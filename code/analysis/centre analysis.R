source("code/cleaning/Preamble.R")

ggarrange(
  ggplot(ictrp_split %>% 
           filter(str_detect(StandardisedCondition, "Chagas"))%>% 
           left_join(world_income, by = c("COUNTRY" = "alpha_3_code")) %>% 
           filter(!is.na(COUNTRY)),
         aes(x = COUNTRY,pattern = CENTRE,
             fill = income_group.y, alpha = CENTRE))+
    geom_bar_pattern(position = "stack", 
                     aes(x = reorder(COUNTRY, as.numeric(income_group.y))),
                     colour = "black",
                     pattern_spacing = 0.025,
                     pattern_fill = "white",
                     pattern_colour = "black",
                     show.legend = FALSE)+
    scale_x_discrete(limits = rev)+
    scale_y_continuous(breaks = scales::breaks_pretty(), limits = c(0,40))+
    coord_flip()+
    theme(
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      panel.spacing.x = unit(0,"cm"),
      strip.background = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_line(colour = "#dadada"),
      axis.text.y = element_text(size = 8)  
    ) +
    scale_pattern_manual(name = "",
                         values = c(
                           "Missing" = "none",
                           "Single Country" = "none",
                           "Multi-Country" = "stripe"),
                         guide = guide_legend(reverse = TRUE)) +
    scale_alpha_manual(guide = "none",
                       values = c(0.65,1)) +
    scale_fill_manual(values = c(colours_set3[c(1,4,5,6)])) +
    labs(x = "Country",
         y = "Number of Studies",
         fill = ""),
  ggplot(ictrp_split %>% 
           filter(str_detect(StandardisedCondition, "Schisto")) %>% 
           left_join(world_income, by = c("COUNTRY" = "alpha_3_code")) %>% 
           filter(!is.na(COUNTRY)),
         aes(x = COUNTRY,pattern = CENTRE,
             fill = income_group.y, alpha = CENTRE)) +
    geom_bar_pattern(position = "stack", 
                     aes(x = reorder(COUNTRY, as.numeric(income_group.y))),
                     colour = "black",
                     pattern_spacing = 0.025,
                     pattern_fill = "white",
                     pattern_colour = "black",
                     show.legend = FALSE) +
    scale_x_discrete(limits = rev) +
    scale_y_continuous(breaks = scales::breaks_pretty(), limits = c(0,40)) +
    coord_flip() +
    theme(
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.spacing.x = unit(0,"cm"),
      strip.background = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_line(colour = "#dadada"),
      axis.text.y = element_text(size = 8)  
    ) +
    scale_pattern_manual(name = "",
                         values = c(
                           "Missing" = "none",
                           "Single Country" = "none",
                           "Multi-Country" = "stripe"),
                         guide = guide_legend(reverse = TRUE)) +
    scale_alpha_manual(guide = "none",
                       values = c(0.65,1)) +
    scale_fill_manual(values = c(colours_set3[c(1,4,5,6)])) +
    labs(x = "Country",
         y = "Number of Studies",
         fill = ""),
  ggplot(ictrp_split %>% 
           filter(str_detect(StandardisedCondition, "Soil")) %>% 
           left_join(world_income, by = c("COUNTRY" = "alpha_3_code")) %>% 
           filter(!is.na(COUNTRY)),
         aes(x = COUNTRY,pattern = CENTRE,
             fill = income_group.y, alpha = CENTRE)) +
    geom_bar_pattern(position = "stack", 
                     aes(x = reorder(COUNTRY, as.numeric(income_group.y))),
                     colour = "black",
                     pattern_spacing = 0.025,
                     pattern_fill = "white",
                     pattern_colour = "black",
                     show.legend = FALSE) +
    scale_x_discrete(limits = rev) +
    scale_y_continuous(breaks = scales::breaks_pretty(), limits = c(0,40)) +
    coord_flip()+
    theme(
      axis.ticks = element_blank(),
      panel.spacing.x = unit(0,"cm"),
      strip.background = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_line(colour = "#dadada") ,
      axis.text.y = element_text(size = 8) 
    )+
    scale_pattern_manual(name = "",
                         values = c(
                           "Missing" = "none",
                           "Single Country" = "none",
                           "Multi-Country" = "stripe"),
                         guide = guide_legend(reverse = TRUE)) +
    scale_alpha_manual(guide = "none",
                       values = c(0.65,1)) +
    scale_fill_manual(values = c(colours_set3[c(1,4,5,6)])) +
    labs(x = "Country",
         y = "Number of Studies",
         fill = ""),
  ggplot(ictrp_split %>% 
           filter(str_detect(StandardisedCondition, "Viscer")) %>% 
           left_join(world_income, by = c("COUNTRY" = "alpha_3_code")) %>% 
           filter(!is.na(COUNTRY)),
         aes(x = COUNTRY,pattern = CENTRE,
             fill = income_group.y, alpha = CENTRE)) +
    geom_bar_pattern(position = "stack", 
                     aes(x = reorder(COUNTRY, as.numeric(income_group.y))),
                     colour = "black",
                     pattern_spacing = 0.025,
                     pattern_fill = "white",
                     pattern_colour = "black",
                     show.legend = FALSE) +
    scale_x_discrete(limits = rev) +
    scale_y_continuous(breaks = scales::breaks_pretty(), limits = c(0,40)) +
    coord_flip() +
    theme(
      axis.ticks = element_blank(),
      axis.title.y = element_blank(),
      panel.spacing.x = unit(0,"cm"),
      strip.background = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_line(colour = "#dadada"),
      axis.text.y = element_text(size = 8) 
    ) +
    scale_pattern_manual(name = "",
                         values = c(
                           "Missing" = "none",
                           "Single Country" = "none",
                           "Multi-Country" = "stripe")) +
    scale_alpha_manual(guide = "none",
                       values = c(0.65,1)) +
    scale_fill_manual(values = c(colours_set3[c(1,4,5,6)])) +
    labs(x = "Country",
         y = "Number of Studies",
         fill = ""),
  labels = c("CD", "Sch", "STH", "VL"),
  vjust = c(1, 1, 0.5, 1), hjust = c(-0.3, 0.65, -0.05, -.3))
