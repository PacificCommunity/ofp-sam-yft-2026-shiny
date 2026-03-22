library(FLR4MFCL)

dir.create("png", showWarnings=FALSE)

# Read depletion from Shiny
load("../app/data/other_data.RData")
dep <- as.data.frame(biomass_dat)
dep <- dep[dep$model == "15_Diag2023",]
names(dep)[names(dep) == "area"] <- "region"
dep <- dep[dep$region %in% as.character(1:5),]
names(dep)[names(dep) == "SBSBF0"] <- "depletion"
dep$model <- dep$SB <- dep$SBF0 <- NULL
dep <- type.convert(dep, as.is=TRUE)
rownames(dep) <- NULL

# 1  Current depletion by region (Shiny)
dep.cur.reg <- aggregate(depletion~region, dep, sum, subset=year==2021)
round(dep.cur.reg, 2)
setNames(round(dep.cur.reg$depletion, 2), 1:5)

png("png/depletion.png", width=1600, height=1600, res=200)
barplot(depletion~region, dep.cur.reg, main="2021")
dev.off()

# 2  Recent trend in region 2
round(dep[dep$region==2 & dep$year>=2010,], 2)
