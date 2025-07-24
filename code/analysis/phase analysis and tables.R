source("code/cleaning/Preamble.R")
source("code/functions/study_by_phase_income.R")

#-------------------------------------------------------------------------------
annotate_figure(ggarrange(
  trial_by_map_prop_income(ictrp_split, "Chagas Disease"),
  trial_by_map_prop_income(ictrp_split, "Schistosomiasis"),
  trial_by_map_prop_income(ictrp_split, "Soil-Transmitted Helminthiases"),
  trial_by_map_prop_income(ictrp_split, "Visceral Leishmaniasis"),
  labels = c("CD", "Sch", "STH", "VL"),
  vjust = 1, hjust = c(-0.5, -0.5, -0.15, -.4))
)

#-------------------------------------------------------------------------------
table1(~ CENTRE + RECRUITMENT_STATUS + PHASE| StandardisedCondition, data = ictrp,
       topclass = "Rtable1-zebra")

table1(~ PHASE + PLACEBO + MASKING + RANDOMIZATION + `PRIMARY PRUPOSE` | StandardisedCondition, data = ictrp %>% filter(STUDY_TYPE == "INTERVENTIONAL"),
       topclass = "Rtable1-zebra")
