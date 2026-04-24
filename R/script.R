library(condor)
library(FLR4MFCL)
library(TAF)

load("../app/data/ll_tab_data.RData")
load("../app/data/other_data.RData")
logfiles <- dir("~/x/yft/2026/model_runs/stepwise", pattern="[0-9].*\\.log",
                full=TRUE, recursive=TRUE)[3:23]
condor <- condor_dir(local.dir="~/x/yft/2026/model_runs/stepwise")
folders <- dir("~/x/yft/2026/model_runs/stepwise", full=TRUE)
parfiles <- sapply(folders, finalPar)

# General dimensions
model <- ll_tab_dat$Model[3:9]
model <- substring(model, 5)
five <- 3:9
four <- 10:16
three <- 17:23

# Objective function
objfun <- data.frame(row.names=model,
                     five=ll_tab_dat$ObjFun[five],
                     four=ll_tab_dat$ObjFun[four],
                     three=ll_tab_dat$ObjFun[three])
objfun <- round(objfun - min(objfun), 3)

# Maximum gradient
gradient <- data.frame(row.names=model,
                       five=ll_tab_dat$Gradient[five],
                       four=ll_tab_dat$Gradient[four],
                       three=ll_tab_dat$Gradient[three])
gradient <- round(gradient, 5)

# Depletion
depletion <- data.frame(row.names=model,
                        five=status_tab_dat$"Final SB/SBF0recent"[five],
                        four=status_tab_dat$"Final SB/SBF0recent"[four],
                        three=status_tab_dat$"Final SB/SBF0recent"[three])
depletion <- round(depletion, 2)

# Run time
runtime <- data.frame(row.names=model,
                      five=condor$runtime[five],
                      four=condor$runtime[four],
                      three=condor$runtime[three])

# L1 parameter
par <- list()
for(i in seq_along(parfiles)){
  message(basename(folders[i]))
  par[[i]] <- read.MFCLBiol(parfiles[i], first.yr=1952)
}
est <- sapply(par, function(x) growth(x)["Lmin", "est"])
L1 <- data.frame(row.names=model,
                 five=est[five],
                 four=est[four],
                 three=est[three])
L1 <- round(L1, 1)
