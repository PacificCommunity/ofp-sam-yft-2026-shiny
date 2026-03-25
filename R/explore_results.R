load("../app/data/fishery_map.RData")
load("../app/data/lfits_dat.RData")
load("../app/data/ll_tab_data.RData")
load("../app/data/move_coef.RData")
load("../app/data/other_data.RData")
load("../app/data/oto_dat.RData")
load("../app/data/tag_data.RData")
load("../app/data/wfits_dat.RData")

# 1 Length comps
lf <- split(lfits_dat, lfits_dat$model)
sapply(lf, function(x) weighted.mean(x$length, x$pred))
# 01a_Online_Repo 02b_Increase_Specs 02c_2278e
#        63.34249           63.33749  63.33749
sapply(lf, function(x) sum((x$obs-x$pred)^2)) / 1e6
# 01a_Online_Repo 02b_Increase_Specs 02c_2278e
#        20612.51           20686.57  20686.57

# 2 Likelihood
ll_tab_dat
# Model              Npar    ObjFun      CPUE    Length    Weight      Age     Tags Penalties     Gradient
# 01a_Online_Repo    1901 -748630.0   732.909 -154972.9 -610341.2 2479.577 12143.94  1327.725 0.0000895235
# 02b_Increase_Specs 1901 -750509.8 -1150.885        NA        NA 2476.183 12109.20        NA 0.0022942431
# 02c_2278e          1901 -750509.8 -1150.885        NA        NA 2476.183 12109.20        NA 0.0022942431

# 3 Movement
aggregate(value~model, move_coef, var)
# model              value
# 01a_Online_Repo    0.1198070
# 02b_Increase_Specs 0.1202888
# 02c_2278e          0.1202888

# 4 CPUE
cpue <- split(cpue_dat, cpue_dat$model)
aggregate(cpue_pred~model, cpue_dat, mean)
# model              cpue_pred
# 01a_Online_Repo    1.211511
# 02b_Increase_Specs 1.210996
# 02c_2278e          1.210996
sapply(cpue, function(x) sum((x$cpue_obs-x$cpue_pred)^2))
# 01a_Online_Repo 02b_Increase_Specs 02c_2278e
#        167.1575           167.9387  167.9387

# 5 Biomass
aggregate(SB~model, biomass_dat, mean)
# model              SB
# 01a_Online_Repo    1337049
# 02b_Increase_Specs 1354204
# 02c_2278e          1354204

# 6 Weight comps
wf <- split(wfits_dat, wfits_dat$model)
sapply(wf, function(x) weighted.mean(x$length, x$pred))
#   01a_Online_Repo 02b_Increase_Specs 02c_2278e
#          32.89025           32.88274  32.88274
sapply(wf, function(x) sum((x$obs-x$pred)^2)) / 1e6
#  01a_Online_Repo 02b_Increase_Specs 02c_2278e
#         5994.565           5981.447  5981.447
