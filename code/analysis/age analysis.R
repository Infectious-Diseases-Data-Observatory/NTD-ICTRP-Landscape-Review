source("code/cleaning/Preamble.R")
source("code/functions/lollipop_by_age_region.R")

#-------------------------------------------------------------------------------
# Cleaning age format
ictrp_split$AGEMAX_YEARS = str_replace_all(ictrp_split$AGEMAX_YEARS, "72M", "6Y")
ictrp_split$AGEMAX_YEARS = str_replace_all(ictrp_split$AGEMAX_YEARS, "6569D", "17Y")
ictrp_split$AGEMAX_YEARS = str_replace_all(ictrp_split$AGEMAX_YEARS, "\\< ", "")
ictrp_split$AGEMAX_YEARS = str_replace_all(ictrp_split$AGEMAX_YEARS, "\\<", "")
ictrp_split$AGEMAX_YEARS = str_replace_all(ictrp_split$AGEMAX_YEARS, "48M", "4Y")
ictrp_split$AGEMAX_YEARS = str_replace_all(ictrp_split$AGEMAX_YEARS, "59M", "5Y")

ictrp_split$AGEMIN_YEARS = str_replace_all(ictrp_split$AGEMIN_YEARS, "> ", "")
ictrp_split$AGEMIN_YEARS = str_replace_all(ictrp_split$AGEMIN_YEARS, ">", "")
ictrp_split$AGEMIN_YEARS = str_replace_all(ictrp_split$AGEMIN_YEARS, "12M", "1Y")
ictrp_split$AGEMIN_YEARS = str_replace_all(ictrp_split$AGEMIN_YEARS, "1D", "0.002Y")
ictrp_split$AGEMIN_YEARS = str_replace_all(ictrp_split$AGEMIN_YEARS, "3M", "0.25Y")
ictrp_split$AGEMIN_YEARS = str_replace_all(ictrp_split$AGEMIN_YEARS, "6M", "0.5Y")
ictrp_split$AGEMIN_YEARS = str_replace_all(ictrp_split$AGEMIN_YEARS, "730D", "2Y")

#-------------------------------------------------------------------------------
# Plot age ranges
annotate_figure(
  ggarrange(lollipop_by_age_region(ictrp_split, disease = "Chagas Disease"),
            lollipop_by_age_region(ictrp_split, disease = "Schistosomiasis"),
            lollipop_by_age_region(ictrp_split, disease = "Soil-Transmitted Helminthiases"),
            lollipop_by_age_region(ictrp_split, disease = "Visceral Leishmaniasis"),
            labels = c("CD", "Sch", "STH", "VL"),
            common.legend = TRUE, legend = "none", hjust = c(-0.1,-0.1,-0.1,-0.1))
)
