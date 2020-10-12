# note survival analysis 1

library(MASS)

# Step 1: Load Data ----
data(gehan)
View(gehan)


# Step 2: Check unique event time ----
# https://cran.r-project.org/web/packages/survival/index.html
cut.points <- unique(gehan$time[gehan$cens == 1])
cat(cut.points[order(cut.points)])
# 1 2 3 4 5 6 7 8 10 11 12 13 15 16 17 22 23


# Step 3: Create CP format dataframe ----

# install.packages("survival")
# 
# or
# 
# library(devtools)
# devtools::install_git("therneau/survival")

library(survival)

gehan2 <- survSplit(data = gehan, 
                    cut = cut.points,
                    start = "start",
                    end = "time",
                    event = "cens")
colnames(gehan2)[4] <- "stop"
View(gehan2)
