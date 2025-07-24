cd_deaths <- read_csv("data/external/IHME-CD.csv", guess_max = Inf, show_col_types = FALSE)
sch_deaths <- read_csv("data/external/IHME-Sch.csv", guess_max = Inf, show_col_types = FALSE)
sth_deaths <- read_csv("data/external/IHME-INF.csv", guess_max = Inf, show_col_types = FALSE)
vl_deaths <- read_csv("data/external/IHME-VL.csv", guess_max = Inf, show_col_types = FALSE)

cd_deaths[which(cd_deaths$location_name == "Bolivia (Plurinational State of)"), "location_name"] = "Bolivia"

cd_clean_deaths = cd_deaths %>% 
  group_by(location_id, location_name) %>% 
  mutate(total_deaths = sum(val, na.rm = TRUE)) %>% 
  filter(year >= 2014) %>% 
  mutate(total_deaths_2014 = sum(val, na.rm = TRUE)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(location_id, location_name, total_deaths_2014, total_deaths) %>% 
  filter(total_deaths != 0) %>% 
  left_join(world_income, by = c("location_name" = "country")) %>% 
  left_join(populations, by = c("alpha_3_code" = "Country Code")) %>% 
  mutate(death_per_mill = total_deaths/`2023` * 1000000,
         death_2014_per_mill = total_deaths_2014/`2023` * 1000000)

sch_deaths[which(sch_deaths$location_name == "Iran (Islamic Republic of)"), "location_name"] = "Iran"
sch_deaths[which(sch_deaths$location_name == "Türkiye"), "location_name"] = "Turkey"
sch_deaths[which(sch_deaths$location_name == "Democratic Republic of the Congo"), "location_name"] = "Congo (the Democratic Republic of the)"
sch_deaths[which(sch_deaths$location_name == "United Republic of Tanzania"), "location_name"] = "Tanzania, United Republic of"
sch_deaths[which(sch_deaths$location_name == "Côte d'Ivoire"), "location_name"] = "Cote d'Ivoire"

sch_clean_deaths = sch_deaths %>% 
  group_by(location_id, location_name) %>% 
  mutate(total_deaths = sum(val, na.rm = TRUE)) %>% 
  filter(year >= 2014) %>% 
  mutate(total_deaths_2014 = sum(val, na.rm = TRUE)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(location_id, location_name, total_deaths_2014, total_deaths) %>% 
  filter(total_deaths != 0) %>% 
  left_join(world_income, by = c("location_name" = "country")) %>% 
  left_join(populations, by = c("alpha_3_code" = "Country Code")) %>% 
  mutate(death_per_mill = total_deaths/`2023` * 1000000,
         death_2014_per_mill = total_deaths_2014/`2023` * 1000000)

sth_deaths[which(sth_deaths$location_name == "Bolivia (Plurinational State of)"), "location_name"] = "Bolivia"
sth_deaths[which(sth_deaths$location_name == "Iran (Islamic Republic of)"), "location_name"] = "Iran"
sth_deaths[which(sth_deaths$location_name == "Türkiye"), "location_name"] = "Turkey"
sth_deaths[which(sth_deaths$location_name == "Democratic Republic of the Congo"), "location_name"] = "Congo (the Democratic Republic of the)"
sth_deaths[which(sth_deaths$location_name == "United Republic of Tanzania"), "location_name"] = "Tanzania, United Republic of"
sth_deaths[which(sth_deaths$location_name == "Côte d'Ivoire"), "location_name"] = "Cote d'Ivoire"
sth_deaths[which(sth_deaths$location_name == "Democratic People's Republic of Korea"), "location_name"] = "N. Korea"
sth_deaths[which(sth_deaths$location_name == "Republic of Korea"), "location_name"] = "S. Korea"
sth_deaths[which(sth_deaths$location_name == "Palestine"), "location_name"] = "Palestine, State of"

sth_clean_deaths = sth_deaths %>% 
  group_by(location_id, location_name) %>% 
  mutate(total_deaths = sum(val, na.rm = TRUE)) %>% 
  filter(year >= 2014) %>% 
  mutate(total_deaths_2014 = sum(val, na.rm = TRUE)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(location_id, location_name, total_deaths_2014, total_deaths) %>% 
  filter(total_deaths != 0) %>% 
  left_join(world_income, by = c("location_name" = "country")) %>% 
  left_join(populations, by = c("alpha_3_code" = "Country Code")) %>% 
  mutate(death_per_mill = total_deaths/`2023` * 1000000,
         death_2014_per_mill = total_deaths_2014/`2023` * 1000000)

vl_deaths[which(vl_deaths$location_name == "Bolivia (Plurinational State of)"), "location_name"] = "Bolivia"
vl_deaths[which(vl_deaths$location_name == "Iran (Islamic Republic of)"), "location_name"] = "Iran"
vl_deaths[which(vl_deaths$location_name == "Türkiye"), "location_name"] = "Turkey"
vl_deaths[which(vl_deaths$location_name == "Democratic Republic of the Congo"), "location_name"] = "Congo (the Democratic Republic of the)"
vl_deaths[which(vl_deaths$location_name == "Côte d'Ivoire"), "location_name"] = "Cote d'Ivoire"
vl_deaths[which(vl_deaths$location_name == "North Macedonia"), "location_name"] = "Republic of North Macedonia"
vl_deaths[which(vl_deaths$location_name == "Palestine"), "location_name"] = "Palestine, State of"

vl_clean_deaths = vl_deaths %>% 
  group_by(location_id, location_name) %>% 
  mutate(total_deaths = sum(val, na.rm = TRUE)) %>% 
  filter(year >= 2014) %>% 
  mutate(total_deaths_2014 = sum(val, na.rm = TRUE)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(location_id, location_name, total_deaths_2014, total_deaths) %>% 
  filter(total_deaths != 0) %>% 
  left_join(world_income, by = c("location_name" = "country")) %>% 
  left_join(populations, by = c("alpha_3_code" = "Country Code")) %>% 
  mutate(death_per_mill = total_deaths/`2023` * 1000000,
         death_2014_per_mill = total_deaths_2014/`2023` * 1000000)

rm(cd_deaths)
rm(sch_deaths)
rm(sth_deaths)
rm(vl_deaths)
