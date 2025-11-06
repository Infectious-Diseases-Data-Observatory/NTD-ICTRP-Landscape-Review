clean_burden_data <- function(data, slice = TRUE){
  data[which(data$location_id == 121), "location_name"] = "Bolivia (Plurinational State of)"
  data[which(data$location_id == 142), "location_name"] = "Iran (Islamic Republic of)"
  data[which(data$location_id == 155), "location_name"] = "Turkiye"
  data[which(data$location_id == 171), "location_name"] = "Congo (the Democratic Republic of the)"
  data[which(data$location_id == 189), "location_name"] = "Tanzania, the United Republic of"
  data[which(data$location_id == 205), "location_name"] = "Cote d'Ivoire"
  data[which(data$location_id == 7), "location_name"] = "Korea (the Democratic People's Republic of)"
  data[which(data$location_id == 68), "location_name"] = "Korea (the Republic of)"
  data[which(data$location_id == 149), "location_name"] = "Palestine, State of"
  data[which(data$location_id == 49), "location_name"] = "North Macedonia"
  data[which(data$location_id == 61), "location_name"] = "Moldova (the Republic of)" 
  data[which(data$location_id == 95), "location_name"] = "United Kingdom of Great Britain and Northern Ireland (the)" 
  data[which(data$location_id == 422), "location_name"] = "Virgin Islands (U.S.)" 
  data[which(data$location_id == 102), "location_name"] = "United States of America (the)"
  data[which(data$location_id == 522), "location_name"] = "Sudan (the)"
  data[which(data$location_id == 16), "location_name"] = "Philippines (the)"
  data[which(data$location_id == 213), "location_name"] = "Niger (the)"
  data[which(data$location_id == 89), "location_name"] = "Netherlands (Kingdom of the)"
  data[which(data$location_id == 12), "location_name"] = "Lao People's Democratic Republic (the)"
  data[which(data$location_id == 106), "location_name"] = "Bahamas (The)"
  data[which(data$location_id == 169), "location_name"] = "Central African Republic (the)"
  data[which(data$location_id == 176), "location_name"] = "Comoros (the)"
  data[which(data$location_id == 170), "location_name"] = "Congo (the)"
  data[which(data$location_id == 320), "location_name"] = "Cook Islands (the)"
  data[which(data$location_id == 111), "location_name"] = "Dominican Republic (the)"
  data[which(data$location_id == 206), "location_name"] = "Gambia (the)"
  data[which(data$location_id == 24), "location_name"] = "Marshall Islands (the)"
  data[which(data$location_id == 376), "location_name"] = "Northern Mariana Islands (the)"
  data[which(data$location_id == 62), "location_name"] = "Russian Federation (the)"
  data[which(data$location_id == 153), "location_name"] = "Syrian Arab Republic (the)"
  data[which(data$location_id == 156), "location_name"] = "United Arab Emirates (the)"
  
  if(slice == TRUE){
    data <- data %>% 
      select(location_id, location_name, year, val) %>% 
      group_by(location_id, location_name) %>% 
      mutate(daly_total = sum(val, na.rm = TRUE),
             daly_04_13 = sum(val[year >= 2004 & year < 2014], na.rm = TRUE),
             daly_14_21 = sum(val[year >= 2014], na.rm = TRUE),
             daly_med_tot = median(val, na.rm = TRUE),
             daly_med_04_13 = median(val[year >= 2004 & year < 2014], na.rm = TRUE),
             daly_med_14_21 = median(val[year >= 2014], na.rm = TRUE),
             daly_mean_tot = mean(val, na.rm = TRUE),
             daly_mean_04_13 = mean(val[year >= 2004 & year < 2014], na.rm = TRUE),
             daly_mean_14_21 = mean(val[year >= 2014], na.rm = TRUE),
             daly_mean_04_08 = mean(val[year >= 2004 & year < 2009], na.rm = TRUE),
             daly_mean_09_13 = mean(val[year >= 2009 & year < 2014], na.rm = TRUE),
             daly_mean_14_18 = mean(val[year >= 2014 & year < 2019], na.rm = TRUE),
             daly_mean_19_23 = mean(val[year >= 2019], na.rm = TRUE)) %>% 
      slice(1) %>% 
      ungroup() %>% 
      left_join(world_income, by = c("location_name" = "country")) %>% 
      left_join(populations, by = c("alpha_3_code" = "country_code")) %>% 
      rename("pop_2023" = "x2023",
             "pop_2013" = "x2013",
             "pop_2008" = "x2008",
             "pop_2013" = "x2013",
             "pop_2018" = "x2018") %>% 
      select(-year, -val, -numeric, -economy, -redcap_number, -country_name) %>% 
      mutate(daly_total_ppop = daly_total/pop_2023,
             daly_04_13_ppop = daly_04_13/pop_2013,
             daly_14_21_ppop = daly_14_21/pop_2023,
             daly_med_tot_ppop = daly_med_tot/pop_2023,
             daly_med_04_13_ppop = daly_med_04_13/pop_2013,
             daly_med_14_23_ppop = daly_med_14_21/pop_2023,
             daly_mean_tot_ppop = daly_mean_tot/pop_2023,
             daly_mean_04_13_ppop = daly_mean_04_13/pop_2013,
             daly_mean_14_23_ppop = daly_mean_14_21/pop_2023,
             daly_mean_04_08_ppop = daly_mean_04_08/pop_2008,
             daly_mean_09_13_ppop = daly_mean_09_13/pop_2013,
             daly_mean_14_18_ppop = daly_mean_14_18/pop_2018,
             daly_mean_19_23_ppop = daly_mean_19_23/pop_2023)    
  } else{
    data <- data %>% 
      select(location_id, location_name, year, val) %>% 
      group_by(location_id, location_name) %>% 
      mutate(daly_total = sum(val, na.rm = TRUE),
             daly_04_13 = sum(val[year >= 2004 & year < 2014], na.rm = TRUE),
             daly_14_21 = sum(val[year >= 2014], na.rm = TRUE),
             daly_med_tot = median(val, na.rm = TRUE),
             daly_med_04_13 = median(val[year >= 2004 & year < 2014], na.rm = TRUE),
             daly_med_14_21 = median(val[year >= 2014], na.rm = TRUE),
             daly_mean_tot = mean(val, na.rm = TRUE),
             daly_mean_04_13 = mean(val[year >= 2004 & year < 2014], na.rm = TRUE),
             daly_mean_14_21 = mean(val[year >= 2014], na.rm = TRUE),
             daly_mean_04_08 = mean(val[year >= 2004 & year < 2009], na.rm = TRUE),
             daly_mean_09_13 = mean(val[year >= 2009 & year < 2014], na.rm = TRUE),
             daly_mean_14_18 = mean(val[year >= 2014 & year < 2019], na.rm = TRUE),
             daly_mean_19_23 = mean(val[year >= 2019], na.rm = TRUE)) %>% 

      ungroup() %>% 
      left_join(world_income, by = c("location_name" = "country")) %>% 
      left_join(populations, by = c("alpha_3_code" = "country_code")) %>% 
      rename("pop_2023" = "x2023",
             "pop_2013" = "x2013",
             "pop_2008" = "x2008",
             "pop_2013" = "x2013",
             "pop_2018" = "x2018") %>% 
      select(-numeric, -economy, -redcap_number, -country_name) %>% 
      mutate(daly_total_ppop = daly_total/pop_2023,
             daly_04_13_ppop = daly_04_13/pop_2013,
             daly_14_21_ppop = daly_14_21/pop_2023,
             daly_med_tot_ppop = daly_med_tot/pop_2023,
             daly_med_04_13_ppop = daly_med_04_13/pop_2013,
             daly_med_14_23_ppop = daly_med_14_21/pop_2023,
             daly_mean_tot_ppop = daly_mean_tot/pop_2023,
             daly_mean_04_13_ppop = daly_mean_04_13/pop_2013,
             daly_mean_14_23_ppop = daly_mean_14_21/pop_2023,
             daly_mean_04_08_ppop = daly_mean_04_08/pop_2008,
             daly_mean_09_13_ppop = daly_mean_09_13/pop_2013,
             daly_mean_14_18_ppop = daly_mean_14_18/pop_2018,
             daly_mean_19_23_ppop = daly_mean_19_23/pop_2023)
  }

  
  return(data)
}
