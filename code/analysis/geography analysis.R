source("code/cleaning/Preamble.R")
source("code/cleaning/clean_death_data.R")
source("code/functions/map_by_studies.R")

annotate_figure(
  ggarrange(map_by_studies("Chagas", year_from = 2014, death_data = cd_clean_deaths),
            map_by_studies("Schisto", year_from = 2014, death_data = sch_clean_deaths),
            map_by_studies("Soil", year_from = 2014, death_data = sth_clean_deaths),
            map_by_studies("Visceral", year_from = 2014, death_data = vl_clean_deaths),
            labels = c("CD", "Sch", "STH", "VL"), hjust = c(-0.5,1,-0.5,1), vjust = c(1,1,0,0))
) ##2000 x 1200

annotate_figure(
  ggarrange(map_by_studies("Chagas", year_from = 1998, death_data = cd_clean_deaths),
            map_by_studies("Schisto", year_from = 1998, death_data = sch_clean_deaths),
            map_by_studies("Soil", year_from = 1998, death_data = sth_clean_deaths),
            map_by_studies("Visceral", year_from = 1998, death_data = vl_clean_deaths),
            labels = c("CD", "Sch", "STH", "VL"), hjust = c(-0.5,1,-0.5,1), vjust = c(1,1,0,0))
) ##2000 x 1200
