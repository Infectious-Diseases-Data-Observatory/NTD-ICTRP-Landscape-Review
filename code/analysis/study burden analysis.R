source("code/cleaning/Preamble.R")

ntd_disease = "Visceral Leishmaniasis"
burden_data = vl_burden
max_number = 20 # chagas,vl - 20, sch,sth - 30

tmp_04_13 <- ictrp_split %>% 
  filter(str_detect(StandardisedCondition, ntd_disease), #----------------------------
         DATE_REGISTRATION < ymd("2014-01-01") & DATE_REGISTRATION >= ymd("2004-01-01")) %>% #--------------
  group_by(COUNTRY) %>% 
  summarise(n_studies = n()) %>% 
  ungroup() %>% 
  filter(!is.na(COUNTRY))  %>% 
  full_join(burden_data, by = c("COUNTRY" = "alpha_3_code"))%>%
  left_join(display_names, by = c("COUNTRY" = "alpha_3_code")) %>% 
  arrange(desc(n_studies), desc(daly_mean_04_13_ppop)) %>% 
  mutate(n_studies = if_else(is.na(n_studies), 0, n_studies),
         rown = row_number()) #---------------------

tmp_14_23 <- ictrp_split %>% 
  filter(str_detect(StandardisedCondition, ntd_disease), #----------------------------
         DATE_REGISTRATION < ymd("2024-01-01") & DATE_REGISTRATION >= ymd("2014-01-01")) %>% #--------------
group_by(COUNTRY) %>% 
  summarise(n_studies = n()) %>% 
  ungroup() %>% 
  filter(!is.na(COUNTRY)) %>% 
  full_join(burden_data, by = c("COUNTRY" = "alpha_3_code"))%>%#---------------------
left_join(display_names, by = c("COUNTRY" = "alpha_3_code")) %>%   
  arrange(desc(n_studies), desc(daly_mean_14_23_ppop)) %>% 
  mutate(n_studies = if_else(is.na(n_studies), 0, n_studies),
         rown = row_number()) 


p1 = ggplot(tmp_04_13 %>% 
         filter(rown <= max_number), 
       aes(x = n_studies, y = reorder(display_name, -rown)))+
  geom_segment(aes(x=0, xend=n_studies), color="#2c728e", linewidth = 1.4) +
  geom_point(mapping = aes(size = daly_mean_04_13_ppop*1000000), color = "#450e5c") + #----------
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white"),
        legend.position = "none",
        plot.subtitle = element_text(hjust = 0.45, face = "bold")) +
  labs(x = "Number of studies (2004-13)", #---------------------------
       y = "Country",
       subtitle = "2004-13",
       size = "Mean Annual DALY per \nmillion population (2004-13)")+
  scale_size_continuous(limits = c(1,max(c(tmp_04_13$daly_mean_04_13_ppop*1000000, 
                                           tmp_14_23$daly_mean_14_23_ppop*1000000), na.rm = TRUE)),
                        breaks = breaks_pretty()) +
  scale_x_continuous(breaks = breaks_pretty(),
                     limits = c(0,max(c(tmp_04_13$n_studies, 
                                        tmp_14_23$n_studies), na.rm = TRUE))) +
  annotate("text", x = ceiling(max(c(tmp_04_13$n_studies, 
                                    tmp_14_23$n_studies), na.rm = TRUE)/2), y = 2.5,
           label = str_c(
    (tmp_04_13 %>% filter(daly_mean_04_13_ppop>0) %>% nrow()) - max_number,
    " additional countries with no studies & \nless than ",
    round(tmp_04_13[which(tmp_04_13$rown == max_number),"daly_mean_04_13_ppop"]*1000000,0),
    " DALYs \nper million population"), color = "#434d56")

p1

# p2_df = tmp_04_13 %>% 
#   filter(is.na(n_studies), daly_mean_04_13_ppop > 0) %>% 
#   mutate(n_studies = ifelse(is.na(n_studies), 0, n_studies))
#          
# p2 = ggplot(p2_df %>% 
#               arrange(desc(daly_mean_04_13_ppop)) %>% 
#               mutate(x = c(rep(1, floor(nrow(p2_df)/4)),
#                            rep(2, if_else(nrow(p2_df) %% 4 == 3, ceiling(nrow(p2_df)/4), floor(nrow(p2_df)/4))),
#                            rep(3, if_else(nrow(p2_df) %% 4 >= 2, ceiling(nrow(p2_df)/4), floor(nrow(p2_df)/4))),
#                            rep(4, ceiling(nrow(p2_df)/4)))) %>% 
#               group_by(x) %>% 
#               mutate(y = -row_number()) %>% 
#               ungroup(), 
#             aes(x = x, label = COUNTRY,
#                 y = y)) +
#   geom_point(mapping = aes(size = daly_mean_04_13_ppop*1000000), color = "#450e5c",
#              alpha = 0.6)+
#   theme_minimal() +
#   theme(axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid = element_blank(),
#         axis.text = element_blank(),
#         legend.position = "none",
#         panel.background = element_rect(fill = "white", colour = "white"),
#         plot.background = element_rect(fill = "white", colour = "white")) +
#   labs(size  = "Mean Annual DALY per \nmillion population (2004-13)",
#        title = "Countries with burden and no studies") +
#   geom_text(nudge_x = 0.3, size = 3)+
#   scale_size_continuous(limits = c(0,max(tmp_04_13$daly_mean_04_13_ppop*1000000, na.rm = TRUE)),
#                         breaks = breaks_pretty())+
#   scale_x_continuous(limits = c(0.8,4.5))
# 
# cowplot::plot_grid(p1, p2, nrow = 2)



p3 = ggplot(tmp_14_23 %>% 
              filter(rown <= max_number), 
            aes(x = n_studies, y = reorder(display_name, -rown)))+
  geom_segment(aes(x=0, xend=n_studies), color="#2c728e", linewidth = 1.4) +
  geom_point(mapping = aes(size = daly_mean_14_23_ppop*1000000), color = "#450e5c") + #----------
  # geom_point(data = tmp_14_23 %>% 
  #              filter(daly_mean_14_23_ppop==0,
  #                     rown <= max_number), aes(size = daly_mean_14_23_ppop*1000000), color = "#450e5c")
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white"),
        axis.title.y = element_blank(),
        plot.subtitle = element_text(hjust = 0.45, face = "bold")) +
  labs(x = "Number of studies (2014-23)", #---------------------------
       y = "",
       subtitle = "2014-23",
       size = "Mean Annual DALY per \nmillion population (within \nrespective decade)")+
  scale_size_continuous(limits = c(1,max(c(tmp_04_13$daly_mean_04_13_ppop*1000000, 
                                           tmp_14_23$daly_mean_14_23_ppop*1000000), na.rm = TRUE)),
                        breaks = breaks_pretty()) +
  scale_x_continuous(breaks = breaks_pretty(),
                     limits = c(0,max(c(tmp_04_13$n_studies, 
                                        tmp_14_23$n_studies), na.rm = TRUE)))+
  annotate("text", x = ceiling(max(c(tmp_04_13$n_studies, 
                                   tmp_14_23$n_studies), na.rm = TRUE)/2), y = 2.5,
           label = str_c(
             (tmp_14_23 %>% filter(daly_mean_14_23_ppop>0) %>% nrow()) - max_number,
             " additional countries with no studies & \nless than ",
             round(tmp_14_23[which(tmp_14_23$rown == max_number),"daly_mean_14_23_ppop"]*1000000,0),
             " DALYs \nper million population"), color = "#434d56")

p3

cowplot::plot_grid(p1, p3, nrow = 1, labels = ntd_disease, rel_widths = c(0.45, 0.55), vjust = 1, hjust = -0.1)

ggsave(str_c("lollipop_", ntd_disease, ".tiff"), path = "paper/figures/", width = 14.3, height = 8.2)

#-------------- vl -------------------------------------------------------------
ntd_disease = "Visceral Leishmaniasis"
burden_data = vl_burden
max_number = 20 # chagas,vl - 20, sch,sth - 30

tmp_04_13 <- ictrp_split %>% 
  filter(str_detect(StandardisedCondition, ntd_disease), #----------------------------
         DATE_REGISTRATION < ymd("2014-01-01") & DATE_REGISTRATION >= ymd("2004-01-01")) %>% #--------------
  group_by(COUNTRY) %>% 
  summarise(n_studies = n()) %>% 
  ungroup() %>% 
  filter(!is.na(COUNTRY))  %>% 
  full_join(burden_data, by = c("COUNTRY" = "alpha_3_code"))%>%
  left_join(display_names, by = c("COUNTRY" = "alpha_3_code")) %>% 
  arrange(desc(n_studies), desc(daly_mean_04_13_ppop)) %>% 
  mutate(n_studies = if_else(is.na(n_studies), 0, n_studies),
         rown = row_number()) #---------------------

tmp_14_23 <- ictrp_split %>% 
  filter(str_detect(StandardisedCondition, ntd_disease), #----------------------------
         DATE_REGISTRATION < ymd("2024-01-01") & DATE_REGISTRATION >= ymd("2014-01-01")) %>% #--------------
group_by(COUNTRY) %>% 
  summarise(n_studies = n()) %>% 
  ungroup() %>% 
  filter(!is.na(COUNTRY)) %>% 
  full_join(burden_data, by = c("COUNTRY" = "alpha_3_code"))%>%#---------------------
left_join(display_names, by = c("COUNTRY" = "alpha_3_code")) %>%   
  arrange(desc(n_studies), desc(daly_mean_14_23_ppop)) %>% 
  mutate(n_studies = if_else(is.na(n_studies), 0, n_studies),
         rown = row_number()) 


p1 = ggplot(tmp_04_13 %>% 
              filter(rown <= max_number), 
            aes(x = n_studies, y = reorder(display_name, -rown)))+
  geom_segment(aes(x=0, xend=n_studies), color="#2c728e", linewidth = 1.4) +
  geom_point(mapping = aes(size = daly_mean_04_13_ppop*1000000), color = "#450e5c") + #----------
theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white"),
        legend.position = "none",
        plot.subtitle = element_text(hjust = 0.45, face = "bold")) +
  labs(x = "Number of studies (2004-13)", #---------------------------
       y = "Country",
       subtitle = "2004-13",
       size = "Mean Annual DALY per \nmillion population (2004-13)")+
  scale_size_continuous(limits = c(1,max(c(tmp_04_13$daly_mean_04_13_ppop*1000000, 
                                           tmp_14_23$daly_mean_14_23_ppop*1000000), na.rm = TRUE)),
                        breaks = c(100, 500, 1000,10000),
                        range = c(2,14)) +
  scale_x_continuous(breaks = breaks_pretty(),
                     limits = c(0,max(c(tmp_04_13$n_studies, 
                                        tmp_14_23$n_studies), na.rm = TRUE))) +
  annotate("text", x = ceiling(max(c(tmp_04_13$n_studies, 
                                     tmp_14_23$n_studies), na.rm = TRUE)/2), y = 2.5,
           label = str_c(
             (tmp_04_13 %>% filter(daly_mean_04_13_ppop>0) %>% nrow()) - max_number,
             " additional countries with no studies & \nless than ",
             round(tmp_04_13[which(tmp_04_13$rown == max_number),"daly_mean_04_13_ppop"]*1000000,0),
             " DALYs \nper million population"), color = "#434d56")

p1

# p2_df = tmp_04_13 %>% 
#   filter(is.na(n_studies), daly_mean_04_13_ppop > 0) %>% 
#   mutate(n_studies = ifelse(is.na(n_studies), 0, n_studies))
#          
# p2 = ggplot(p2_df %>% 
#               arrange(desc(daly_mean_04_13_ppop)) %>% 
#               mutate(x = c(rep(1, floor(nrow(p2_df)/4)),
#                            rep(2, if_else(nrow(p2_df) %% 4 == 3, ceiling(nrow(p2_df)/4), floor(nrow(p2_df)/4))),
#                            rep(3, if_else(nrow(p2_df) %% 4 >= 2, ceiling(nrow(p2_df)/4), floor(nrow(p2_df)/4))),
#                            rep(4, ceiling(nrow(p2_df)/4)))) %>% 
#               group_by(x) %>% 
#               mutate(y = -row_number()) %>% 
#               ungroup(), 
#             aes(x = x, label = COUNTRY,
#                 y = y)) +
#   geom_point(mapping = aes(size = daly_mean_04_13_ppop*1000000), color = "#450e5c",
#              alpha = 0.6)+
#   theme_minimal() +
#   theme(axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid = element_blank(),
#         axis.text = element_blank(),
#         legend.position = "none",
#         panel.background = element_rect(fill = "white", colour = "white"),
#         plot.background = element_rect(fill = "white", colour = "white")) +
#   labs(size  = "Mean Annual DALY per \nmillion population (2004-13)",
#        title = "Countries with burden and no studies") +
#   geom_text(nudge_x = 0.3, size = 3)+
#   scale_size_continuous(limits = c(0,max(tmp_04_13$daly_mean_04_13_ppop*1000000, na.rm = TRUE)),
#                         breaks = breaks_pretty())+
#   scale_x_continuous(limits = c(0.8,4.5))
# 
# cowplot::plot_grid(p1, p2, nrow = 2)



p3 = ggplot(tmp_14_23 %>% 
              filter(rown <= max_number), 
            aes(x = n_studies, y = reorder(display_name, -rown)))+
  geom_segment(aes(x=0, xend=n_studies), color="#2c728e", linewidth = 1.4) +
  geom_point(mapping = aes(size = daly_mean_14_23_ppop*1000000), color = "#450e5c") + #----------
# geom_point(data = tmp_14_23 %>% 
#              filter(daly_mean_14_23_ppop==0,
#                     rown <= max_number), aes(size = daly_mean_14_23_ppop*1000000), color = "#450e5c")
theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white"),
        axis.title.y = element_blank(),
        plot.subtitle = element_text(hjust = 0.45, face = "bold")) +
  labs(x = "Number of studies (2014-23)", #---------------------------
       y = "",
       subtitle = "2014-23",
       size = "Mean Annual DALY per \nmillion population (within \nrespective decade)")+
  scale_size_continuous(limits = c(1,max(c(tmp_04_13$daly_mean_04_13_ppop*1000000, 
                                           tmp_14_23$daly_mean_14_23_ppop*1000000), na.rm = TRUE)),
                        breaks = c(100, 500, 1000,10000),
                        range = c(2,14)) +
  scale_x_continuous(breaks = breaks_pretty(),
                     limits = c(0,max(c(tmp_04_13$n_studies, 
                                        tmp_14_23$n_studies), na.rm = TRUE)))+
  annotate("text", x = ceiling(max(c(tmp_04_13$n_studies, 
                                     tmp_14_23$n_studies), na.rm = TRUE)/2), y = 2.5,
           label = str_c(
             (tmp_14_23 %>% filter(daly_mean_14_23_ppop>0) %>% nrow()) - max_number,
             " additional countries with no studies & \nless than ",
             round(tmp_14_23[which(tmp_14_23$rown == max_number),"daly_mean_14_23_ppop"]*1000000,0),
             " DALYs \nper million population"), color = "#434d56")

p3

cowplot::plot_grid(p1, p3, nrow = 1, labels = ntd_disease, rel_widths = c(0.45, 0.55), vjust = 1, hjust = -0.1)

ggsave(str_c("lollipop_", ntd_disease, ".tiff"), path = "paper/figures/", width = 14.3, height = 8.2)
##


cowplot::plot_grid(
  p1+scale_size_continuous(trans = "log10", 
                           limits = c(1,max(c(tmp_04_13$daly_mean_04_13_ppop*1000000, 
                                              tmp_14_23$daly_mean_14_23_ppop*1000000), 
                                            na.rm = TRUE))), 
  p3+scale_size_continuous(trans= "log10", 
                           limits = c(1,max(c(tmp_04_13$daly_mean_04_13_ppop*1000000,
                                              tmp_14_23$daly_mean_14_23_ppop*1000000), 
                                            na.rm = TRUE))), 
  nrow = 1, labels = ntd_disease, rel_widths = c(0.45, 0.55), vjust = 1, hjust = -0.1)

px = tmp_04_13 %>%
  select(COUNTRY, income_group, rown) %>% 
  rename("rown_04_13" = "rown") %>% 
  left_join((tmp_14_23 %>% select(COUNTRY, rown) %>% rename("rown_14_23" = "rown")), by = "COUNTRY") %>% 
  pivot_longer(cols = c(starts_with("rown"))) %>% 
  filter(value <= max_number) %>%  
  ggplot(aes(x = name, y = desc(value), group = COUNTRY, colour = income_group)) + 
    ggbump::geom_bump(linewidth = 1.5, show.legend = TRUE) +# , colour = "grey"
    theme_void() +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white")) +
  scale_y_continuous(expand = c(0,0.6), limits = c(-max_number, -1)) # cd: 0.6, sch: , sth:, vl: 

# px = px + 
#   ggbump::geom_bump(linewidth = 1.5, mapping = aes(colour = COUNTRY), data = tmp_04_13 %>%
#                       select(COUNTRY, rown) %>% 
#                       rename("rown_04_13" = "rown") %>% 
#                       left_join((tmp_14_23 %>% select(COUNTRY, rown) %>% rename("rown_14_23" = "rown")), by = "COUNTRY") %>% 
#                       pivot_longer(cols = c(starts_with("rown"))) %>% 
#                       filter(value <= max_number,
#                              COUNTRY %in% c("GHA", "GBR", "UGA"))) +
#   theme(legend.position = "none")+
#   scale_colour_viridis_d()

cowplot::plot_grid(p1, px, p3, nrow = 1, labels = ntd_disease, rel_widths = c(0.4, 0.1 ,0.5), align = "h", vjust = 1, hjust = -0.1)

ggsave(str_c("lollipop_", ntd_disease, "_bump.tiff"), path = "paper/figures/", width = 14.3, height = 8.2)

# p4_df = tmp_14_23 %>% 
#   filter(is.na(n_studies), daly_mean_14_23_ppop > 0) %>% 
#   mutate(n_studies = ifelse(is.na(n_studies), 0, n_studies))
# 
# p4 = ggplot(p4_df %>% 
#               arrange(desc(daly_mean_14_23_ppop)) %>% 
#               mutate(x = c(rep(1, floor(nrow(p4_df)/4)),
#                            rep(2, if_else(nrow(p4_df) %% 4 == 3, ceiling(nrow(p4_df)/4), floor(nrow(p4_df)/4))),
#                            rep(3, if_else(nrow(p4_df) %% 4 >= 2, ceiling(nrow(p4_df)/4), floor(nrow(p4_df)/4))),
#                            rep(4, ceiling(nrow(p4_df)/4)))) %>% 
#               group_by(x) %>% 
#               mutate(y = -row_number()) %>% 
#               ungroup(), 
#             aes(x = x, label = COUNTRY,
#                 y = y)) +
#   geom_point(mapping = aes(size = daly_mean_14_23_ppop*1000000), color = "#450e5c",
#              alpha = 0.6)+
#   theme_minimal() +
#   theme(axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid = element_blank(),
#         axis.text = element_blank(),
#         legend.position = "none",
#         panel.background = element_rect(fill = "white", colour = "white"),
#         plot.background = element_rect(fill = "white", colour = "white")) +
#   labs(size  = "Mean Annual DALY per \nmillion population (2014-23)",
#        title = "Countries with burden and no studies") +
#   geom_text(nudge_x = 0.3, size = 3)+
#   scale_size_continuous(limits = c(0,max(tmp_14_23$daly_mean_14_23_ppop*1000000, na.rm = TRUE)),
#                         breaks = breaks_pretty()) +
#   scale_x_continuous(limits = c(0.8,4.5))


#================================================================================
# 
# 
# 
# p2 
# p2 = ggplot(tmp %>% 
#               filter(is.na(n_studies), daly_mean_04_13_ppop > 0) %>% 
#               mutate(n_studies = ifelse(is.na(n_studies), 0, n_studies)), 
#             aes(x = n_studies, y = reorder(COUNTRY, daly_mean_04_13_ppop)))+
#   geom_segment(aes(x=0, xend=n_studies), color="#2c728e", linewidth = 1.4) +
#   geom_point(mapping = aes(size = daly_mean_04_13_ppop*1000000), color = "#450e5c") + #----------
# theme_minimal() +
#   theme(panel.background = element_rect(fill = "white", colour = "white"),
#         plot.background = element_rect(fill = "white", colour = "white")) +
#   labs(x = "Number of Chagas Studies (2004-13)", #---------------------------
#        y = "Country",
#        size = "Mean Annual DALY per Million Population (2014-23)")+
#   lims(x = c(0, 20))
# 
# cowplot::plot_grid(p1, p2, nrow = 2)
# 
# ggplot(tmp %>%
#          filter(daly_mean_04_13_ppop > 0) %>% #----------------------
#          mutate(n_studies = ifelse(is.na(n_studies), 0, n_studies)), 
#        aes(x = n_studies, y = reorder(COUNTRY, n_studies)))+
#   geom_segment(aes(x=0, xend=n_studies), color="#2c728e", size = 1.4) +
#   geom_point(mapping = aes(size = daly_mean_04_13_ppop*1000000), color = "#450e5c") + #-------
#   theme_minimal() +
#   labs(x = "Number of Studies",
#        y = "Country",
#        size = "Mean Annual DALY per Million Population (2004-13)") #--------------------
# 
# 
# p2 = ggplot(tmp %>% 
#          filter(is.na(n_studies)) %>% 
#          mutate(n_studies = 0), 
#        aes(x = n_studies, y = 1, size = daly_mean_04_13_ppop*1000000))+ #----------------
#   geom_jitter(color = "#450e5c", alpha = 0.4)+
#   theme_minimal() +
#   theme(axis.text = element_blank())+
#   labs(x = "",
#        y = "",
#        size = "Mean Annual DALY per Million Population (2004-13)")+ #--------------------
#   lims(x = c(-0.5, 13))
# 
# cowplot::plot_grid(p1, p2, nrow = 2)
# 
# ggsave("sch_burden_lollipop_04_13.tiff", path = "paper/figures/", width = 10.4, height = 8.21)
# 
# 
# 
# 
# ggplot(tmp %>% 
#          mutate(n_studies = ifelse(is.na(n_studies), 0, n_studies)),
#        aes(x = daly_mean_04_13_ppop, y = as.factor(n_studies))) +
#   ggridges::geom_density_ridges(fill = "#F1DCF9", colour = "#450e5c", scale = 1, stat = "binline")+
#   ggridges::theme_ridges()+
#   labs(x = "Mean Annual DALY Burden (2004-13)",
#        y = "Number of Studies")
# 
# ggplot(tmp %>% 
#          mutate(n_studies = ifelse(is.na(n_studies), 0, n_studies)),
#        aes(x = daly_mean_14_23_ppop, y = as.factor(n_studies))) +
#   ggridges::geom_density_ridges(fill = "#F1DCF9", colour = "#450e5c", scale = 1)+
#   ggridges::theme_ridges()+
#   labs(x = "Mean Annual DALY Burden (2014-23)",
#        y = "Number of Studies")
