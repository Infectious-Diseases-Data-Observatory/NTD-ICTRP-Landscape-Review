library(ggbump)
source("code/cleaning/Preamble.R")

display_names = read_csv("data/analysis/country_display_names.csv", show_col_types = FALSE) %>% 
  select(alpha_3_code, display_name)

rank_cd = read_csv("data/external/DALYs-CD.csv", show_col_types = FALSE) %>% 
  clean_burden_data(slice = FALSE) %>% 
  group_by(year) %>% 
  mutate(rank_all = rank(desc(val/pop_2023))) %>% 
  mutate(rank = if_else(rank_all > 10, 11L, rank_all)) %>% 
  ungroup() %>% 
  filter(
    rank <= 15,
    # year %in% c(2000, 2005, 2010, 2015, 2020)
    ) %>% 
  left_join(display_names)

rank_plot_cd = ggplot(rank_cd, aes(x = year, y = rank, group = alpha_3_code))+#, colour = alpha_3_code
  geom_bump(linewidth = 1.5, colour = "lightgray") +
  geom_bump(data = rank_cd %>% 
              filter(alpha_3_code %in% c("URY", "ECU", "COL", "SLV")),
            linewidth = 1.5, mapping = aes(color = alpha_3_code)) +
  geom_point(size= 4, colour = "lightgray")+
  geom_point(size= 2, colour = "white")+
  theme_classic()+
  theme(legend.position = "none",
        axis.line = element_blank(),
        axis.ticks.y = element_blank())+
  scale_colour_manual(values = colours_set3[1:10]) + 
  geom_text(data= rank_cd %>% filter(year == min(year), rank < 11L),
            mapping=  aes(x = year - 0.5, label = display_name),
            fontface = "bold", hjust = 1)+ 
  geom_text(data= rank_cd %>% filter(year == max(year), rank < 11L),
            mapping=  aes(x = year + 0.5, label = display_name),
            fontface = "bold", hjust = 0) +
  scale_y_reverse(breaks = c(1, 3, 5, 8, 10), limits = c(11,1))+
  scale_x_continuous(limits = c(1997,2023))+
  annotate("text", x = 1998, y = 11, label = "11th and\nbeyond")+
  labs(y = "Ranking of disease burden (Annual DALY per population)",
       x = "Year",
       title = "Chagas Disease")

rank_plot_cd

ggsave("burden_rank_CD.tiff", path = "paper/figures", width = 14, height = 6)


rank_sch = read_csv("data/external/DALYs-SCH.csv", show_col_types = FALSE) %>% 
  clean_burden_data(slice = FALSE) %>% 
  group_by(year) %>% 
  mutate(rank_all = rank(desc(val/pop_2023))) %>% 
  mutate(rank = if_else(rank_all > 10, 11L, rank_all)) %>% 
  ungroup() %>% 
  filter(
    rank <= 15,
    # year %in% c(2000, 2005, 2010, 2015, 2020)
  )  %>% 
  left_join(display_names)

rank_plot_sch =ggplot(rank_sch, aes(x = year, y = rank, group = alpha_3_code))+#, colour = alpha_3_code
  geom_bump(linewidth = 1.5, colour = "lightgray") +
  # geom_bump(linewidth = 1.5, colour = "black", data = rank_sch %>% filter(rank < 11)) +
  geom_bump(data = rank_sch %>% 
              filter(alpha_3_code %in% c("UGA", "SEN", "BEN", "KEN")),
            linewidth = 1.5, mapping = aes(color = alpha_3_code)) +
  geom_point(size= 4, colour = "lightgray")+
  geom_point(size= 2, colour = "white")+
  theme_classic()+
  theme(legend.position = "none",
        axis.line = element_blank(),
        axis.ticks.y = element_blank())+
  scale_colour_manual(values = colours_set3[1:10]) + 
  geom_text(data= rank_sch %>% filter(year == min(year), rank < 11L),
            mapping=  aes(x = year - 0.5, label = display_name),
            fontface = "bold", hjust = 1)+ 
  geom_text(data= rank_sch %>% filter(year == max(year), rank < 11L),
            mapping=  aes(x = year + 0.5, label = display_name),
            fontface = "bold", hjust = 0) +
  annotate("text", x = 1998, y = 11, label = "11th and\nbeyond")+
  scale_y_reverse(breaks = c(1, 3, 5, 8, 10), limits = c(11,1))+
  scale_x_continuous(limits = c(1997,2023))+
  labs(y = "Ranking of disease burden (Annual DALY per population)",
       x = "Year",
       title= "Schistosomiasis")

rank_plot_sch

ggsave("burden_rank_SCH.tiff", path = "paper/figures", width = 14, height = 6)


rank_sth = read_csv("data/external/DALYs-INF.csv", show_col_types = FALSE) %>% 
  clean_burden_data(slice = FALSE) %>% 
  group_by(year) %>% 
  mutate(rank_all = rank(desc(val/pop_2023))) %>% 
  mutate(rank = if_else(rank_all > 10, 11L, rank_all)) %>% 
  ungroup() %>% 
  filter(
    rank <= 15,
    # year %in% c(2000, 2005, 2010, 2015, 2020)
  )  %>% 
  left_join(display_names)

rank_plot_sth =ggplot(rank_sth, aes(x = year, y = rank, group = alpha_3_code))+#, colour = alpha_3_code
  geom_bump(linewidth = 1.5, colour = "lightgray") +
  # geom_bump(linewidth = 1.5, colour = "black", data = rank_sch %>% filter(rank < 11)) +
  geom_bump(data = rank_sth %>% 
              filter(alpha_3_code %in% c("NPL", "MMR", "SLE", "TZA", "PNG")),
            linewidth = 1.5, mapping = aes(color = alpha_3_code)) +
  geom_point(size= 4, colour = "lightgray")+
  geom_point(size= 2, colour = "white")+
  theme_classic()+
  theme(legend.position = "none",
        axis.line = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.ticks.y = element_blank())+
  scale_colour_manual(values = colours_set3[1:10]) + 
  geom_text(data= rank_sth %>% filter(year == min(year), rank < 11L),
            mapping=  aes(x = year - 0.5, label = display_name),
            fontface = "bold", hjust = 1)+ 
  geom_text(data= rank_sth %>% filter(year == max(year), rank < 11L),
            mapping=  aes(x = year + 0.5, label = display_name),
            fontface = "bold", hjust = 0) +
  annotate("text", x = 1998, y = 11, label = "11th and\nbeyond")+
  scale_y_reverse(breaks = c(1, 3, 5, 8, 10), limits = c(11,1))+
  scale_x_continuous(limits = c(1997,2023))+
  labs(y = "Ranking of disease burden (Annual DALY per population)",
       x = "Year",
       title = "Soil-Transmitted Helmithiases")

rank_plot_sth

ggsave("burden_rank_STH.tiff", path = "paper/figures", width = 14, height = 6)


rank_vl = read_csv("data/external/DALYs-VL.csv", show_col_types = FALSE) %>% 
  clean_burden_data(slice = FALSE) %>% 
  group_by(year) %>% 
  mutate(rank_all = rank(desc(val/pop_2023))) %>% 
  mutate(rank = if_else(rank_all > 10, 11L, rank_all)) %>% 
  ungroup() %>% 
  filter(
    rank <= 15,
    # year %in% c(2000, 2005, 2010, 2015, 2020)
  )  %>% 
  left_join(display_names)

rank_plot_vl =ggplot(rank_vl, aes(x = year, y = rank, group = alpha_3_code))+#, colour = alpha_3_code
  geom_bump(linewidth = 1.5, colour = "lightgray") +
  # geom_bump(linewidth = 1.5, colour = "black", data = rank_sch %>% filter(rank < 11)) +
  geom_bump(data = rank_vl %>% 
              filter(alpha_3_code %in% c("NPL", "BGD", "CAF", "NER", "DJI")),
            linewidth = 1.5, mapping = aes(color = alpha_3_code)) +
  geom_point(size= 4, colour = "lightgray")+
  geom_point(size= 2, colour = "white")+
  theme_classic()+
  theme(legend.position = "none",
        axis.line = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.ticks.y = element_blank())+
  scale_colour_manual(values = colours_set3[1:10]) + 
  geom_text(data= rank_vl %>% filter(year == min(year), rank < 11L),
            mapping=  aes(x = year - 0.5, label = display_name),
            fontface = "bold", hjust = 1)+ 
  geom_text(data= rank_vl %>% filter(year == max(year), rank < 11L),
            mapping=  aes(x = year + 0.5, label = display_name),
            fontface = "bold", hjust = 0) +
  annotate("text", x = 1998, y = 11, label = "11th and\nbeyond")+
  scale_y_reverse(breaks = c(1, 3, 5, 8, 10), limits = c(11,1))+
  scale_x_continuous(limits = c(1997,2023))+
  labs(y = "Ranking of disease burden (Annual DALY per population)",
       x = "Year",
       title = "Visceral Leishmaniasis")

rank_plot_vl
ggsave("burden_rank_VL.tiff", path = "paper/figures", width = 14, height = 6)

plot_grid(rank_plot_cd +
            xlab(""),
          rank_plot_sch +
            xlab("")+
            ylab(""),
          rank_plot_sth,
          rank_plot_vl +
            ylab(""),
          ncol = 2)

ggsave("burden_rank_all.tiff", path = "paper/figures", width = 14, height = 6)

