source("code/cleaning/Preamble.R")

#-------------------------------------------------------------------------------
bubble_data = ictrp %>% 
  filter(CENTRE == "Single Country", STUDY_TYPE == "INTERVENTIONAL") %>%
  left_join(populations, by = c("COUNTRY" = "country_code")) %>% 
  select(-`country_name`) %>% 
  group_by(COUNTRY, `x2023`, income_group, StandardisedCondition) %>% 
  summarise(n_trials = n(),
            n_target_size = sum(`TARGET SIZE`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  rename("population" = "x2023")%>% 
  mutate(pop_mill = population/1000000, 
         pop_per_target = population/n_target_size, 
         pop_per_trial = population/n_trials, 
         target_per_trial = n_target_size/n_trials,
         trials_per_100000 = n_trials/(population/100000),
         targets_per_100000 = n_target_size/(population/100000),
         targets_per_10000 = n_target_size/(population/10000)) %>% 
  left_join(display_names, by = c("COUNTRY" = "alpha_3_code"))

#-------------------------------------------------------------------------------
# Schisto data 
df_sch = filter(bubble_data, str_detect(StandardisedCondition, "Schist"))%>% 
  group_by(COUNTRY, income_group, population, display_name) %>% 
  summarise(n_trials = sum(n_trials),
            n_target_size = sum(n_target_size, na.rm = TRUE)) %>% 
  mutate(pop_mill = population/1000000, 
         pop_per_target = population/n_target_size, 
         pop_per_trial = population/n_trials, 
         target_per_trial = n_target_size/n_trials,
         trials_per_100000 = n_trials/(population/100000),
         targets_per_100000 = n_target_size/(population/100000),
         targets_per_10000 = n_target_size/(population/10000)) %>% 
  group_by(income_group) %>% 
  mutate(total_ig = n()) %>% 
  ungroup() %>% 
  mutate(income_group = str_sub(income_group, end = -8),
         income_group = factor(income_group, levels = c("High", "Upper middle",
                                                        "Lower middle", "Low")))

access_sch = ggplot(df_sch, aes(x = income_group, y = targets_per_10000, fill = income_group))+ 
  stat_summary(fun = median, geom = "crossbar", show.legend = FALSE,
               linewidth = 0.75, color = "#FDB462", 
               mapping = aes(x = income_group, group = income_group))+
  geom_dotplot(binaxis ='y', stackdir = 'center', dotsize = 1.25,
               show.legend = FALSE, fill = "#80B1D3", colour = "#80B1D3")+
  labs(fill = "Income Group", x = "Income Group", 
       y = "Target Sample Size per 10,000",
       title = "Schistosomiasis")+
  scale_fill_manual(values = colours_set3[c(1,2,3,5)]) +
  geom_text(data = df_sch %>% group_by(income_group) %>% slice(1) %>% ungroup(),
            mapping = aes(x = income_group, y = -1.4), 
            label = str_c("n = ", (df_sch %>% 
                                     group_by(income_group) %>% 
                                     slice(1) %>% 
                                     ungroup())$total_ig))+
  theme(panel.background = element_rect(fill = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_line(colour = "lightgray"),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        axis.text = element_text(size =12),
        axis.title = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_x_discrete(drop = FALSE)+
  scale_y_continuous(breaks = scales::pretty_breaks())+
  geom_text_repel(df_sch %>% filter(targets_per_10000 >= 15), 
                  mapping = aes(label = display_name), size = 3,
                  nudge_x = .35, nudge_y = 1, segment.size = NA)

#-------------------------------------------------------------------------------
# Chagas Data 
df_cd = filter(bubble_data, str_detect(StandardisedCondition, "Chagas")) %>% 
  group_by(COUNTRY, income_group, population, display_name) %>% 
  summarise(n_trials = sum(n_trials),
            n_target_size = sum(n_target_size, na.rm = TRUE)) %>% 
  mutate(pop_mill = population/1000000, 
         pop_per_target = population/n_target_size, 
         pop_per_trial = population/n_trials, 
         target_per_trial = n_target_size/n_trials,
         trials_per_100000 = n_trials/(population/100000),
         targets_per_100000 = n_target_size/(population/100000),
         targets_per_10000 = n_target_size/(population/10000)) %>% 
  group_by(income_group) %>% 
  mutate(total_ig = n()) %>% 
  ungroup()%>% 
  mutate(income_group = str_sub(income_group, end = -8),
         income_group = factor(income_group, levels = c("High", "Upper middle",
                                                        "Lower middle", "Low")))

access_cd = ggplot(df_cd, aes(x = income_group, y = targets_per_10000, fill = income_group))+ 
  stat_summary(fun = median, geom = "crossbar", show.legend = FALSE,
               linewidth = 0.75, color = "#FDB462", 
               mapping = aes(x = income_group, group = income_group))+
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1.25, show.legend = FALSE, 
               fill = "#80B1D3", colour = "#80B1D3")+
  labs(fill = "Income Group", x = "Income Group", 
       y = "Target Sample Size per 10,000",
       title = "Chagas Disease")+
  scale_fill_manual(values = colours_set3[c(1,2,3,5)]) +
  geom_text(data = df_cd %>% 
              group_by(income_group) %>% 
              slice(1) %>% 
              ungroup(),
            mapping = aes(x = income_group, y = -0.2), 
            label = str_c("n = ", (df_cd %>% 
                                     group_by(income_group) %>% 
                                     slice(1) %>% 
                                     ungroup())$total_ig))+
  theme(panel.background = element_rect(fill = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_line(colour = "lightgray"),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        axis.text = element_text(size =12),
        axis.title = element_text(size = 12),
        axis.title.x = element_blank()) +
  coord_cartesian(ylim=c(-0.2, 2)) +
  scale_x_discrete(drop = FALSE)+
  scale_y_continuous(breaks = scales::pretty_breaks())+
  geom_text(df_cd, mapping = aes(x = (income_group), y = targets_per_10000, label = display_name), 
            nudge_x = 0.25, nudge_y = 0.08, size = 3)

#-------------------------------------------------------------------------------
# VL Data 
df_vl = filter(bubble_data, str_detect(StandardisedCondition, "Visceral")) %>%
  group_by(COUNTRY, income_group, population, display_name) %>% 
  summarise(n_trials = sum(n_trials),
            n_target_size = sum(n_target_size, na.rm = TRUE)) %>% 
  mutate(pop_mill = population/1000000, 
         pop_per_target = population/n_target_size, 
         pop_per_trial = population/n_trials, 
         target_per_trial = n_target_size/n_trials,
         trials_per_100000 = n_trials/(population/100000),
         targets_per_100000 = n_target_size/(population/100000),
         targets_per_10000 = n_target_size/(population/10000)) %>% 
  group_by(income_group) %>% 
  mutate(total_ig = n()) %>% 
  ungroup()%>% 
  mutate(income_group = str_sub(income_group, end = -8),
         income_group = factor(income_group, levels = c("High", "Upper middle",
                                                        "Lower middle", "Low")))

access_vl = ggplot(df_vl, aes(x = income_group, y = targets_per_10000, fill = income_group))+ 
  stat_summary(fun = median, geom = "crossbar", show.legend = FALSE,
               linewidth = 0.75, color = "#FDB462", 
               mapping = aes(x = income_group, group = income_group))+
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1.25, show.legend = FALSE, 
               fill = "#80B1D3", colour = "#80B1D3")+
  labs(fill = "Income Group", x = "Income Group", 
       y = "Target Sample Size per 10,000",
       title = "Visceral Leishmaniasis")+
  scale_fill_manual(values = colours_set3[c(1,2,3,5)]) +
  geom_text(data = df_vl %>%  
              group_by(income_group) %>% 
              slice(1) %>%
              ungroup(),
            mapping = aes(x = income_group, y = -0.01), 
            label = str_c("n = ", (df_vl %>% 
                                     group_by(income_group) %>% 
                                     slice(1) %>%
                                     ungroup())$total_ig))+
  theme(panel.background = element_rect(fill = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_line(colour = "lightgray"),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        axis.text = element_text(size =12),
        axis.title = element_text(size = 12),
        axis.title.y = element_blank()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  geom_text(df_vl, mapping = aes(x = (income_group ), y = targets_per_10000, label = display_name), 
            nudge_x = .25, nudge_y = 0.004, size = 3)

#-------------------------------------------------------------------------------
# STH data
df_sth = filter(bubble_data, str_detect(StandardisedCondition, "Soil")) %>% 
  group_by(COUNTRY, income_group, population, display_name) %>% 
  summarise(n_trials = sum(n_trials),
            n_target_size = sum(n_target_size, na.rm = TRUE)) %>% 
  mutate(pop_mill = population/1000000, 
         pop_per_target = population/n_target_size, 
         pop_per_trial = population/n_trials, 
         target_per_trial = n_target_size/n_trials,
         trials_per_100000 = n_trials/(population/100000),
         targets_per_100000 = n_target_size/(population/100000),
         targets_per_10000 = n_target_size/(population/10000)) %>% 
  group_by(income_group) %>% 
  mutate(total_ig = n()) %>% 
  ungroup() %>% 
  mutate(income_group = str_sub(income_group, end = -8),
         income_group = factor(income_group, levels = c("High", "Upper middle",
                                                        "Lower middle", "Low")))

access_sth = ggplot(df_sth, aes(x = income_group, y = targets_per_10000, group = income_group))+ 
  stat_summary(fun = median, geom = "crossbar",
               linewidth = 0.75, color="#FDB462",
               mapping = aes(x = income_group, group = income_group))+
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1.25, 
               show.legend = FALSE, fill = "#80B1D3", colour = "#80B1D3")+
  labs(fill = "Income Group", x = "Income Group", 
       y = "Target Sample Size per 10,000",
       title = "Soil-Transmitted Helminthiases")+
  scale_fill_manual(values = colours_set3[c(1,2,3,5)]) +
  geom_text(data = df_sth %>% 
              group_by(income_group) %>% 
              slice(1) %>%
              ungroup(),
            mapping = aes(x = income_group, y = -4), 
            label = str_c("n = ", (df_sth %>% 
                                     group_by(income_group) %>%
                                     slice(1) %>% 
                                     ungroup())$total_ig))+
  theme(panel.background = element_rect(fill = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_line(colour = "lightgray"),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        axis.text = element_text(size =12),
        axis.title = element_text(size = 12)) +
  scale_y_continuous(breaks =c(0,20,40,60,80,100))+
  geom_text(df_sth %>% 
              filter(targets_per_10000>=20 | COUNTRY == "ETH" | income_group== "High income"),
            mapping = aes(x = (income_group ), y = targets_per_10000, label = display_name), 
            nudge_x = .35, nudge_y = 4, size = 3)


plot_grid(access_cd,
          access_sch,
          access_sth,
          access_vl, align = "hv")

ggsave("access.tiff", path = "paper/figures/", width = 14.6, height = 11)
