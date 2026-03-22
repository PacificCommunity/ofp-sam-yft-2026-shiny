load("../app/data/other_data.RData")

# Read CPUE from Shiny
cpue <- as.data.frame(cpue_dat)

# Format CPUE table
cpue <- cpue[cpue$model == "15_Diag2023",]
cpue$area <- cpue$fishery - 32
names(cpue)[names(cpue) == "area"] <- "region"
names(cpue)[names(cpue) == "season"] <- "quarter"
names(cpue)[names(cpue) == "cpue_obs"] <- "index"
cpue$model <- cpue$age <- cpue$fishery <- cpue$iter <- NULL
cpue$cpue_pred <- cpue$ts <- cpue$diff <- cpue$scale_diff <- NULL
cpue <- type.convert(cpue, as.is=TRUE)
rownames(cpue) <- NULL

# Average over quarters
cpue.yr <- aggregate(index~year+region, cpue, mean)
# xyplot(index~year|factor(region), cpue.yr, layout=c(1,5),
#        scale=list(y="free"), as.table=TRUE, pch=16, cex=0.2)

# Current year
cpue.cur <- cpue.yr[cpue.yr$year == 2021,]
cpue.cur$year <- NULL
rownames(cpue.cur) <- NULL
cpue.cur$prop <- 100 * proportions(cpue.cur$index)
round(cpue.cur[-2])

# NorthSouth
ns <- sum(cpue.cur$prop[c(1,5)])
eq <- sum(cpue.cur$prop[c(2,3,4)])
png("png/cpue.png")
barplot(c(ns, eq), names=c("NorthSouth", "Equatorial"),
        ylab="Proportion of CPUE (%)")
dev.off()
