# note survival analysis 2
library(MASS)
library(survival)
library(survminer)

# 1 ----
data(gehan)

# 2 ----
surv.obj <- Surv(time = gehan$time, event = gehan$cens)

# 3 ----
ge.sf <- survfit(surv.obj ~ treat, data = gehan)

# 4 ----
ge.sf.df <- surv_summary(ge.sf, data = gehan)
View(ge.sf.df)

# 5 ----
ggsurvplot(
    fit = ge.sf,
    data = gehan,
    conf.int = T,
    pval = T,
    risk.table = T,
    cumevents = T,
    cumcensor = T,
    ggtheme = ggplot2::theme_light(),
    tables.height = 0.15
)
