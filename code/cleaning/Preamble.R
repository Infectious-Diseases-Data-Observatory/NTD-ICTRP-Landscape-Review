#-------------------------------------------------------------------------------
# Libraries
library(tidyverse)
library(admiral)
library(ggpubr)
library(ggrepel)
library(ggpattern)
library(readxl)
library(table1)
library(janitor)
library(worlddatr)

#-------------------------------------------------------------------------------
# Utility function
# `%!in%` = Negate(`%in%`)

#-------------------------------------------------------------------------------
# Centroid/centre of country coordinates
centroids <- read_csv("data/external/centroids.csv", show_col_types = FALSE)

# ICTRP Phase I curation
phase_I <- read_csv("data/curated/ICTRP_extract_all_NTD.csv",
                    show_col_types = FALSE, guess_max = Inf) %>% 
  select(TrialID, `TARGET SIZE`) 

# Data on country population
populations <- read_csv("data/external/populations.csv", skip = 4, show_col_types = FALSE, guess_max = Inf) %>% 
  select(`Country Name`, `Country Code`, `2023`)

# IDDO-Curated ICTRP data 
ictrp <- read_csv("data/curated/Conditions_17.csv", guess_max = Inf, show_col_types = FALSE) %>% 
  full_join(read_csv("data/curated/Country_20.csv", guess_max = Inf, show_col_types = FALSE), 
            by = "TrialID") %>% 
  full_join(read_csv("data/curated/Phase_5.csv", guess_max = Inf, show_col_types = FALSE), 
            by = "TrialID") %>% 
  full_join(read_csv("data/curated/Study Design_9.csv", guess_max = Inf, show_col_types = FALSE),
            by = "TrialID") %>% 
  full_join(read_csv("data/curated/Study Dates and Inclusion_0.csv", guess_max = Inf, show_col_types = FALSE), 
            by = "TrialID") %>%
  derive_vars_dtm(dtc = DATE_ENROLLEMENT, 
                  highest_imputation = "D", 
                  date_imputation = "first", 
                  new_vars_prefix = "DATE_ENROLLMENT_IMP_") %>%
  select(-DATE_ENROLLMENT_IMP_TMF) %>% 
  relocate(c(DATE_ENROLLMENT_IMP_DTM, DATE_ENROLLMENT_IMP_DTF), .after = DATE_ENROLLEMENT) %>% 
  mutate(DATE_ENROLLMENT_IMP_DTM = as.Date(DATE_ENROLLMENT_IMP_DTM),
         INCLUSION_AGEMIN_Aug2024 = str_to_upper(INCLUSION_AGEMIN_Aug2024),
         INCLUSION_AGEMAX_Aug2024 = str_to_upper(INCLUSION_AGEMAX_Aug2024),
         GAGE = NA,
         AGEMIN_YEARS = INCLUSION_AGEMIN_Aug2024,
         AGEMAX_YEARS = INCLUSION_AGEMAX_Aug2024,
         PHASE = factor(PHASE, levels = c("PHASE IV TRIAL", "PHASE III TRIAL",
                                          "PHASE II/III TRIAL", "PHASE II TRIAL",
                                          "PHASE I/II TRIAL", "PHASE I TRIAL", "NOT APPLICABLE")),
         CENTRE = if_else(str_detect(COUNTRY, "\\|"), "Multi-Country", "Single Country") 
         ) %>% 
  left_join(world_income, by = c("COUNTRY" = "alpha_3_code")) %>% 
  select(-country, -economy) %>% 
  left_join(phase_I)

# WHO regions data
who_regions <- read_csv("data/external/who-regions.csv", guess_max = Inf, show_col_types = FALSE) %>% 
  select(-Year) %>% 
  rename("WHO_Region" = "WHO region") 
#-------------------------------------------------------------------------------
# Data cleaning
ictrp[which(is.na(ictrp$CENTRE)), "CENTRE"] <- "Missing"

ictrp[which(ictrp$TrialID == "RBR-5n4htp"),"PHASE"] <- "PHASE I/II TRIAL"
ictrp[which(ictrp$TrialID == "CTRI/2018/06/014579"),"PHASE"] <- "PHASE IV TRIAL"

label(ictrp$StandardisedCondition) <- "Disease"

ictrp_split = ictrp %>% 
  separate_rows(COUNTRY) %>% 
  left_join(who_regions, by = c(COUNTRY = "Code")) %>% 
  mutate(WHO_Region  = as.factor(WHO_Region))

#-------------------------------------------------------------------------------
# colour palettes
colours_set3 = c(
  "#BEBADA",
  "#FFED6F",
  "#B3DE69",
  "#FDB462",
  "#80B1D3",
  "#FB8072",
  "#8DD3C7",
  "#FCCDE5",
  "#E5C494",
  "#BC80BD",
  "#81BD80",
  "black")
