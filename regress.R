# Jonah Simon
# run regressions and format nicely
library(stargazer)

# Generate models
# to factor: v006, v007, v024, v025, v116, 190, ml0
# DHS varlist: caseid, midx, v001, v006, v007, v012, v024, v025, v113, v115, v116, v119, v127, v128, v129, v136, v137, v149, v152, v161, v190, ml0, v201, v218, v717, b8, h22

small_model <- lm(h22~deforestation0YearLag + deforestation1YearLag + deforestation2YearLag + deforestation3YearLag + treeCover2000, base)
middle_model <- lm(h22~deforestation0YearLag + deforestation1YearLag + deforestation2YearLag + deforestation3YearLag + treeCover2000 + factor(v006) + factor(v007), base)
full_model <- lm(h22~deforestation0YearLag + deforestation1YearLag + deforestation2YearLag + deforestation3YearLag + treeCover2000 + factor(v006) + factor(v007) + v012 + factor(v024) + factor(v025) + v113 + v115 + factor(v116) + factor(v116) + v119 + v127 + v128 + v129 + v136 + v137 + v149 + v152 + v161 + factor(v190) + factor(ml0) + v201 + v218 + v717 + b8, base)

#summary(small_model)
stargazer(small_model, middle_model, full_model, column.labels = c("Basic", "Partial", "Full"), omit = c("treeCover2000", "v006", "v007", "v012", "v024", "v025", "v113", "v115", "v116", "v119", "v127", "v128", "v129", "v136", "v137", "v149", "v152", "v161", "v190", "ml0", "v201", "v218", "v717", "b8"), omit.labels = c("Year Fixed Effects","Seasonality", "HH and indiv controls","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a"), omit.yes.no = c("No", "Yes"), type = "latex")



#Old
#full_model <- lm(h22~.-caseid - midx - v001, base.d)