# Jonah Simon
# run regressions and format nicely
library(stargazer)


# test dummy vars
#c("v024","v025","v113","v116","v119","v127","v128","v129","v137","v190","ml0")
full_model <- lm(h22~defLag0 + defLag1 + defLag2 + defLag3 + treeCover2000  + factor(v024) + factor(v025) + factor(v113) + factor(v116) + factor(v119) + factor(v127) + factor(v128) + factor(v129) + factor(v137) + factor(v190) + factor(ml0), base)
# Generate models
#Full model
#full_model <- lm(h22~.-caseid - midx - v001, base.d)
middle_model <- lm(h22~defLag0 + defLag1 + defLag2 + defLag3 + treeCover2000, base)
small_model <- lm(h22~defLag0 + defLag1 + defLag2 + defLag3, base)
#summary(small_model)
stargazer(full_model, middle_model, small_model, column.labels = c("Large", "Middle", "Small"), omit = c("v024","v025","v113","v116","v119","v127","v128","v129","v137","v190","ml0"), omit.labels = c("HH vars?","a","a","a","a","a","a","a","a","a","a"), omit.yes.no = c("No", "Yes"), type = "text")
