source("code/cleaning/Preamble.R")

world_income[which(world_income$alpha_3_code == "ETH"),"income_group"] = "Low income"

plots = plot_grid(
  ggplot(ictrp_split %>% 
           filter(str_detect(StandardisedCondition, "Chagas"))%>% 
           left_join(world_income, by = c("COUNTRY" = "alpha_3_code")) %>% 
           filter(!is.na(COUNTRY)) %>% 
           left_join(display_names, c("COUNTRY" = "alpha_3_code")),
         aes(x = display_name,pattern = CENTRE,
             fill = income_group.y, alpha = CENTRE))+
    geom_bar_pattern(position = "stack", 
                     aes(x = reorder(display_name, as.numeric(income_group.y))),
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
         fill = "",
         title = "Chagas Disease"),
  ggplot(ictrp_split %>% 
           filter(str_detect(StandardisedCondition, "Schisto")) %>% 
           left_join(world_income, by = c("COUNTRY" = "alpha_3_code")) %>% 
           filter(!is.na(COUNTRY))%>% 
           left_join(display_names, c("COUNTRY" = "alpha_3_code")),
         aes(x = display_name,pattern = CENTRE,
             fill = income_group.y, alpha = CENTRE)) +
    geom_bar_pattern(position = "stack", 
                     aes(x = reorder(display_name, as.numeric(income_group.y))),
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
         fill = "",
         title = "Schistosomiasis"),
  ggplot(ictrp_split %>% 
           filter(str_detect(StandardisedCondition, "Soil")) %>% 
           left_join(world_income, by = c("COUNTRY" = "alpha_3_code")) %>% 
           filter(!is.na(COUNTRY))%>% 
           left_join(display_names, c("COUNTRY" = "alpha_3_code")),
         aes(x = display_name,pattern = CENTRE,
             fill = income_group.y, alpha = CENTRE)) +
    geom_bar_pattern(position = "stack", 
                     aes(x = reorder(display_name, as.numeric(income_group.y))),
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
         fill = "",
         title = "Soil-Transmitted Helminthiases"),
  ggplot(ictrp_split %>% 
           filter(str_detect(StandardisedCondition, "Viscer")) %>% 
           left_join(world_income, by = c("COUNTRY" = "alpha_3_code")) %>% 
           filter(!is.na(COUNTRY))%>% 
           left_join(display_names, c("COUNTRY" = "alpha_3_code")),
         aes(x = display_name,pattern = CENTRE,
             fill = income_group.y, alpha = CENTRE)) +
    geom_bar_pattern(position = "stack", 
                     aes(x = reorder(display_name, as.numeric(income_group.y))),
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
         fill = "",
         title = "Visceral Leishmaniasis"))

plots

legend = get_legend(ggplot(ictrp_split %>% 
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
                                       pattern_colour = "black") +
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
                           fill = "",
                           title = "Schistosomiasis"))
legend
plot_grid(plots, legend, ncol = 2, rel_widths = c(0.6,0.1))

ggsave("centre.tiff", path = "paper/figures/", width = 10, height = 8)
