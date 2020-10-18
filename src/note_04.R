# note survival analysis 4

# Step 1: Load Data ----
library(tidyverse)
gehan <- read_csv("./input/gehan-wbc.csv", 
                  col_types = cols(treat = col_character())) %>% 
    mutate(treat = ifelse(treat == "6-MP", "6MP", "control"))


# Step 2: Create a survival object ----
library(survival)
surv.obj <- Surv(time = gehan$time, event = gehan$cens)


# Step 3: Fits a Cox proportional hazards regression model ----
coxph(surv.obj ~ treat + logWBC, 
      data = gehan, 
      method = "efron")

# Step 4: Interaction term ----
coxph(surv.obj ~ treat + logWBC + treat:logWBC, 
      data = gehan,
      method = "efron")

# Step 5: Likelihood Ratio Test ----
m1 <- coxph(surv.obj ~ treat + logWBC, 
            data = gehan,
            method = "efron")
m2 <- coxph(surv.obj ~ treat + logWBC + treat:logWBC, 
            data = gehan,
            method = "efron")

anova(m1, m2)


# Step 6: Summary ----
summary(m1)


# Step 7: Draw Predicted Survival Function ----
library(survminer)

ggsurvplot(
  fit = survfit(m1, conf.int = 0.95),
  data = gehan,
  conf.int = T,
  ggtheme = ggplot2::theme_light(),
)


# Step 8: Draw Another Predicted Survival Function ----
new.data <- data.frame(
  treat = c("control", "6MP"),
  logWBC = rep(mean(gehan$logWBC), 2)
)

ggsurvplot(
  fit = survfit(m1,newdata = new.data, conf.int = 0.95),
  data = gehan,
  conf.int = T,
  legend.labs = c("control", "6MP"),
  ggtheme = ggplot2::theme_light(),
)
