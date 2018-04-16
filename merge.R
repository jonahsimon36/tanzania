# Jonah Simon
# Merge together dataframes

library(dummies)

# first construct base for 2010, then will add 2015/16 later
base10 <- select(children2010, caseid, midx, v001, v007, v024, v025, v113, v116, v119, v127, v128, v129, v137, v190, ml0, h22)
base10 <- na.omit(base10)
base10 <- filter(base10, h22 < 8)
#base10 <- dummy.data.frame(data = as.data.frame(base10), sep = "_", names = c("v024","v025","v113","v116","v119","v127","v128","v129","v137","v190","ml0","h22"))

# join loss year data to base 10
for (year in c(7,8,9,10)){
  assign(sprintf("DHS2010_LossYear%s", year), mutate(get(sprintf("DHS2010_LossYear%s", year)), cluster = as.numeric(substr(get(sprintf("DHS2010_LossYear%s", year))$DHSID, 14-2, 14))))
  potato <- sprintf("defLag%s", 10 - year)
  assign(sprintf("DHS2010_LossYear%s", year), mutate(get(sprintf("DHS2010_LossYear%s", year)), !!potato := COUNT/102023)) # divide COUNT by 102093 to get percentage of cell deforested
  assign(sprintf("DHS2010_LossYear%s", year), select(get(sprintf("DHS2010_LossYear%s", year)),cluster,sprintf("defLag%s", 10 - year)))
  base10<- left_join(base10,get(sprintf("DHS2010_LossYear%s", year)), by = c("v001" = "cluster"))
}

# join tree cover to base 10
treeCover2010 <- mutate(treeCover2010, cluster = as.numeric(substr(treeCover2010$DHSID, 14-2, 14)))
treeCover2010 <- rename(treeCover2010, treeCover2000)
base10 <- left_join(base10, select(treeCover2010, cluster, treeCover2000), by = c("v001" = "cluster"))

# construct base15
base15 <- select(children2015, caseid, midx, v001, v007, v024, v025, v113, v116, v119, v127, v128, v129, v137, v190, ml0, h22)
base15 <- na.omit(base15)
base15 <- filter(base15, h22 < 8)
#base15 <- dummy.data.frame(data = as.data.frame(base15), sep = "_", names = c("v024","v025","v113","v116","v119","v127","v128","v129","v137","v190","ml0","h22"))

# join loss year data to base 15
for (year in c(13,14,15,16)){
  assign(sprintf("DHS2015_LossYear%s", year), mutate(get(sprintf("DHS2015_LossYear%s", year)), cluster = as.numeric(substr(get(sprintf("DHS2015_LossYear%s", year))$DHSID, 14-2, 14))))
  potato <- sprintf("defLag%s", 16 - year)
  assign(sprintf("DHS2015_LossYear%s", year), mutate(get(sprintf("DHS2015_LossYear%s", year)), !!potato := COUNT/102023)) # divide COUNT by 102093 to get percentage of cell deforested
  assign(sprintf("DHS2015_LossYear%s", year), select(get(sprintf("DHS2015_LossYear%s", year)),cluster,sprintf("defLag%s", 16 - year)))
  base15<- left_join(base15,get(sprintf("DHS2015_LossYear%s", year)), by = c("v001" = "cluster"))
}

# join tree cover to base 10
treeCover2015 <- mutate(treeCover2015, cluster = as.numeric(substr(treeCover2015$DHSID, 14-2, 14)))
treeCover2015 <- rename(treeCover2015, treeCover2000)
base15 <- left_join(base15, select(treeCover2015, cluster, treeCover2000), by = c("v001" = "cluster"))


# Bind 2010 and 2015 dataframes
base <- bind_rows(base10, base15)
# Change NA values to 0
base <- base%>%mutate_all(funs(ifelse(is.na(.), 0, .)))

# Create factors for dummy variables, NO MORE Factor
#base[, c("v024","v025","v113","v116","v119","v127","v128","v129","v137","v190","ml0","h22")] <- lapply(base[, c("v024","v025","v113","v116","v119","v127","v128","v129","v137","v190","ml0","h22")], factor)
base.d <- dummy.data.frame(as.data.frame(base),names = c("v007","v024","v025","v113","v116","v119","v127","v128","v129","v137","v190","ml0"), sep = ".")


# run combined regression
#summary(lm(h22~.-caseid - midx - v001, base.d))

# #lm(h22 ~ ., data = base)
# 
# # Try everything on 2010 dataframe as test
# # Change 2010 NA values to 0
# base10 <- base10%>%mutate_all(funs(ifelse(is.na(.), 0, .)))
# 
# # Create factors for 2010 dummy variables, NOT DOING IT THIS WAY ANY MORE, don't need factors
# #base10[, c("v024","v025","v113","v116","v119","v127","v128","v129","v137","v190","ml0","h22")] <- lapply(base10[, c("v024","v025","v113","v116","v119","v127","v128","v129","v137","v190","ml0","h22")], factor)
# 
# # run 2010 regression, really slow currently
# #lm(h22 ~ ., data = base10)
# 
# b10d <- dummy.data.frame(as.data.frame(base10),names = c("v007","v024","v025","v113","v116","v119","v127","v128","v129","v137","v190","ml0"), sep = ".")
# summary(lm(h22~.-caseid - midx - v001, b10d))
# 
# # create sample to regress faster
# base10_sample <- base10[sample(nrow(base10),1000),]
# #base10_sample <- base10_sample %>% select(v025, v127, defLag0, defLag1, defLag2, defLag3, h22)
# #base10_sample[,c("v025","v127")] <- factor(base10_sample$v025)
# #b10sd <- dummy.data.frame(as.data.frame(base10_sample),names = c("v025","v127"), sep = ".")
# b10sd <- dummy.data.frame(as.data.frame(base10_sample),names = c("v007","v024","v025","v113","v116","v119","v127","v128","v129","v137","v190","ml0"), sep = ".")
# summary(lm(h22~.-caseid - midx - v001, b10sd))
# #b10SampleModel <- model.matrix(h22~., base10_sample)
# #base10_sample_model <- model.matrix(h22 ~ ., base10_sample)
# #summary(lm(base10_sample_model))
# #summary(lm(h22 ~ defLag0 + defLag1 + defLag2 + defLag3, data = base10_sample))
