# Jonah Simon
# run regressions and format nicely
library(stargazer)
library(reshape2)

# Generate models
# to factor: v006, v007, v024, v025, v116, 190, ml0
# DHS varlist: caseid, midx, v001, v006, v007, v012, v024, v025, v113, v115, v116, v119, v127, v128, v129, v136, v137, v149, v152, v161, v190, ml0, v201, v218, v717, b8, h22

small_model <- lm(h22~deforestation0YearLag + deforestation1YearLag + deforestation2YearLag + deforestation3YearLag + treeCover2000, base)
middle_model <- lm(h22~deforestation0YearLag + deforestation1YearLag + deforestation2YearLag + deforestation3YearLag + treeCover2000 + factor(v006) + factor(v007), base)
full_model <- lm(h22~deforestation0YearLag + deforestation1YearLag + deforestation2YearLag + deforestation3YearLag + treeCover2000 + factor(v006) + factor(v007) + v012 + factor(v024) + factor(v025) + v113 + v115 + factor(v116) + factor(v116) + v119 + v127 + v128 + v129 + v136 + v137 + v149 + v152 + v161 + factor(v190) + factor(ml0) + v201 + v218 + v717 + b8, base)

summary(full_model)
stargazer(small_model, middle_model, full_model, column.labels = c("Basic", "Partial", "Full"), omit = c("treeCover2000", "v006", "v007", "v012", "v024", "v025", "v113", "v115", "v116", "v119", "v127", "v128", "v129", "v136", "v137", "v149", "v152", "v161", "v190", "ml0", "v201", "v218", "v717", "b8"), omit.labels = c("Year Fixed Effects","Seasonality", "HH and indiv controls","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a"), omit.yes.no = c("No", "Yes"), type = "latex")

#Create plots for poster
#base %>% ggplot(aes(x = deforestation0YearLag, y = h22)) + geom_point() + stat_smooth(method = "lm", col = "red")

#Create density plots
def0NoFev <- filter(select(base, h22, deforestation0YearLag), h22 == 0)
def0Fev <- filter(select(base, h22, deforestation0YearLag), h22 == 1)

base %>% ggplot(aes(x = deforestation0YearLag)) + geom_density()

def0NoFev%>% ggplot(aes(x = deforestation0YearLag)) + labs(title = "No Fev") + geom_density()
def0Fev%>% ggplot(aes(x = deforestation0YearLag)) + labs(title = "Fev") + geom_density()

d0nf <- melt(def0NoFev$deforestation0YearLag)

ggplot(melt(data.frame(def0NoFev$deforestation0YearLag,def0Fev$deforestation0YearLag)), mapping = aes (fill = variable, x = value)) + geom_density (alpha = .5)
####
basePlot <- base %>% mutate(h22 = factor(h22))
basePlot %>% ggplot(aes(x = deforestation0YearLag), size = 10) + xlab("Deforestation in Survey Year") + labs(title = "Survey Year Deforestation by Malaria", subtitle = "Probability density curves") + scale_color_discrete(name="", labels = c("Malaria", "No Malaria")) + geom_density(aes(color=h22))

def0Fev <- basePlot %>% select(h22,deforestation0YearLag, deforestation1YearLag)

tapply(basePlot$deforestation0YearLag, basePlot$h22, describe)
X <- split(def0Fev, basePlot$h22)

stargazer(data.frame(X[1]), type = "latex")
stargazer(data.frame(X[2]), type = "latex")
#Old
#full_model <- lm(h22~.-caseid - midx - v001, base.d)