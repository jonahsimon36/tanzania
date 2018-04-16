# Jonah Simon
# run regressions and format nicely
library(stargazer)

# Generate models
#Full model
full_model <- lm(h22~.-caseid - midx - v001, base.d)
summary(full_model)
