source("code/cleaning/Preamble.R")

#-------------------------------------------------------------------------------
bubble_data = ictrp %>% 
  filter(CENTRE == "Single Country", STUDY_TYPE == "INTERVENTIONAL") %>%
  left_join(populations, by = c("COUNTRY" = "Country Code")) %>% 
  select(-`Country Name`) %>% 
  group_by(COUNTRY, `2023`, income_group, StandardisedCondition) %>% 
  summarise(n_trials = n(),
            n_target_size = sum(`TARGET SIZE`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  rename("population" = "2023")%>% 
  mutate(pop_mill = population/1000000, 
         pop_per_target = population/n_target_size, 
         pop_per_trial = population/n_trials, 
         target_per_trial = n_target_size/n_trials,
         trials_per_100000 = n_trials/(population/100000),
         targets_per_100000 = n_target_size/(population/100000),
         targets_per_10000 = n_target_size/(population/10000))

#-------------------------------------------------------------------------------
# Schisto data 
df_sch = filter(bubble_data, str_detect(StandardisedCondition, "Schist"))%>% 
  group_by(COUNTRY, income_group, population) %>% 
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
  ungroup() 

access_sch = ggplot(df_sch, aes(x = income_group, y = targets_per_10000, fill = income_group))+ 
  stat_summary(fun = median, geom = "crossbar", show.legend = FALSE,
               linewidth = 0.75, color = "#FDB462", 
               mapping = aes(x = income_group, group = income_group))+
  geom_dotplot(binaxis ='y', stackdir = 'center', dotsize = 1.25,
               show.legend = FALSE, fill = "#80B1D3", colour = "#80B1D3")+
  labs(fill = "Income Group", x = "Income Group", 
       y = "Target Sample Size per 10,000")+
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
  scale_y_continuous(breaks = scales::pretty_breaks())+
  geom_text_repel(df_sch %>% filter(targets_per_10000 >= 15), 
                  mapping = aes(x = (income_group), y = targets_per_10000, label = COUNTRY), 
                  nudge_x = .15, nudge_y = 1, segment.size = NA)

#-------------------------------------------------------------------------------
# Chagas Data 
df_cd = filter(bubble_data, str_detect(StandardisedCondition, "Chagas")) %>% 
  group_by(COUNTRY, income_group, population) %>% 
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
  ungroup()

access_cd = ggplot(df_cd, aes(x = income_group, y = targets_per_10000, fill = income_group))+ 
  stat_summary(fun = median, geom = "crossbar", show.legend = FALSE,
               linewidth = 0.75, color = "#FDB462", 
               mapping = aes(x = income_group, group = income_group))+
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1.25, show.legend = FALSE, 
               fill = "#80B1D3", colour = "#80B1D3")+
  labs(fill = "Income Group", x = "Income Group", 
       y = "Target Sample Size per 10,000")+
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
  scale_y_continuous(breaks = scales::pretty_breaks())+
  geom_text(df_cd, mapping = aes(x = (income_group), y = targets_per_10000, label = COUNTRY), nudge_x = .1, nudge_y = 0.08)

#-------------------------------------------------------------------------------
# VL Data 
df_vl = filter(bubble_data, str_detect(StandardisedCondition, "Visceral")) %>%
  group_by(income_group) %>% 
  mutate(total_ig = n()) %>% 
  ungroup()

access_vl = ggplot(df_vl, aes(x = income_group, y = targets_per_10000, fill = income_group))+ 
  stat_summary(fun = median, geom = "crossbar", show.legend = FALSE,
               linewidth = 0.75, color = "#FDB462", 
               mapping = aes(x = income_group, group = income_group))+
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1.25, show.legend = FALSE, 
               fill = "#80B1D3", colour = "#80B1D3")+
  labs(fill = "Income Group", x = "Income Group", 
       y = "Target Sample Size per 10,000")+
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
  geom_text(df_vl, mapping = aes(x = (income_group ), y = targets_per_10000, label = COUNTRY), nudge_x = .15, nudge_y = 0.004)

#-------------------------------------------------------------------------------
# STH data
df_sth = filter(bubble_data, str_detect(StandardisedCondition, "Soil")) %>% 
  group_by(COUNTRY, income_group, population) %>% 
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
  ungroup()

access_sth = ggplot(df_sth, aes(x = income_group, y = targets_per_10000, group = income_group))+ 
  stat_summary(fun = median, geom = "crossbar",
               linewidth = 0.75, color="#FDB462",
               mapping = aes(x = income_group, group = income_group))+
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1.25, 
               show.legend = FALSE, fill = "#80B1D3", colour = "#80B1D3")+
  labs(fill = "Income Group", x = "Income Group", 
       y = "Target Sample Size per 10,000")+
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
            mapping = aes(x = (income_group ), y = targets_per_10000, label = COUNTRY), 
            nudge_x = .15, nudge_y = 4)


ggarrange(access_cd,
          access_sch,
          access_sth,
          access_vl,
          labels = c("CD", "Sch", "STH", "VL"), vjust = 1)
