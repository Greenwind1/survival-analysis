# note survival analysis 5
# Ref: https://rpubs.com/kaz_yos/kleinbaum-ph-assumption

# Step 0: Set Environments ----
library(tidyverse)
library(survival)
# library(ggsurvfit)

source(file = "utility/environments.R")
source(file = "utility/helper_functions.R")


# Step 1: Load Data ----
# censoring status 1=censored, 2=dead
cancer <- survival::cancer %>% 
    filter(!is.na(ph.ecog) & ph.ecog < 2) %>% 
    mutate(
        status = ifelse(status == 1, yes = 0, no = 1), 
        sex = factor(sex, levels = c(1, 2), labels = c("M", "F")), 
        ph.ecog = as.factor(ph.ecog)
    )


# Step 2: Plot 1 ----
# For interval censored data, the status indicator is 
# 0 = right censored
# 1 = event at time
# 2 = left censored
# 3 = interval censored
surv.obj <- Surv(time = cancer$time, event = cancer$status, type = "right")
sf.obj <- survfit(surv.obj ~ sex, data = cancer)
# sf.obj <- ggsurvfit::survfit2(surv.obj ~ sex, data = cancer)

ggplot() + 
    geom_point(aes(x = time, y = log.m.log), 
               data = tibble(
                   time = sf.obj$time[1:sf.obj$strata[1]], 
                   log.m.log = log(-log(sf.obj$surv + 1e-16))[1:sf.obj$strata[1]]
               ), color = col.bmc.blue, size = 1, alpha = 0.5
    ) + 
    geom_step(aes(x = time, y = log.m.log), 
              data = tibble(
                  time = sf.obj$time[1:sf.obj$strata[1]], 
                  log.m.log = log(-log(sf.obj$surv + 1e-16))[1:sf.obj$strata[1]]
              ), color = col.bmc.blue, size = 0.5
    ) + 
    geom_point(aes(x = time, y = log.m.log), 
               data = tibble(
                   time = sf.obj$time[
                       (sf.obj$strata[1] + 1):length(sf.obj$time)
                   ], 
                   log.m.log = log(-log(sf.obj$surv + 1e-16))[
                       (sf.obj$strata[1] + 1):length(sf.obj$time)
                   ]
               ), color = col.rm, size = 1, alpha = 0.5
    ) + 
    geom_step(aes(x = time, y = log.m.log), 
              data = tibble(
                  time = sf.obj$time[
                      (sf.obj$strata[1] + 1):length(sf.obj$time)
                  ], 
                  log.m.log = log(-log(sf.obj$surv + 1e-16))[
                      (sf.obj$strata[1] + 1):length(sf.obj$time)
                  ]
              ), color = col.rm, size = 0.5
    ) + 
    labs(x = "Time", y = "Log-Log Kaplan Meier Survival Curve") + 
    annotate(geom = "text", x = 500, y = 0.85, label = "Men", 
             family = font.base, color = col.bmc.blue, size = 5) + 
    annotate(geom = "text", x = 550, y = -0.75, label = "Women", 
             family = font.base, color = col.rm, size = 5) + 
    theme(
        axis.title = element_text(size = 12)
    )

ggsave("rf-surv/fig/note_05-1.png", width = 5, height = 3.5, dpi = 300)


# Step 4: Plot 2 ----
surv.obj <- Surv(time = cancer$time, event = cancer$status, type = "right")
sf.obj <- ggsurvfit::survfit2(surv.obj ~ ph.ecog, data = cancer)

ggplot() + 
    geom_point(aes(x = time, y = log.m.log), 
               data = tibble(
                   time = sf.obj$time[1:sf.obj$strata[1]], 
                   log.m.log = log(-log(sf.obj$surv + 1e-16))[1:sf.obj$strata[1]]
               ), color = col.plos.pink, size = 1, alpha = 0.5
    ) + 
    geom_step(aes(x = time, y = log.m.log), 
              data = tibble(
                  time = sf.obj$time[1:sf.obj$strata[1]], 
                  log.m.log = log(-log(sf.obj$surv + 1e-16))[1:sf.obj$strata[1]]
              ), color = col.plos.pink, size = 0.5
    ) + 
    geom_point(aes(x = time, y = log.m.log), 
               data = tibble(
                   time = sf.obj$time[
                       (sf.obj$strata[1] + 1):length(sf.obj$time)
                   ], 
                   log.m.log = log(-log(sf.obj$surv + 1e-16))[
                       (sf.obj$strata[1] + 1):length(sf.obj$time)
                   ]
               ), color = col.plos.yellow, size = 1, alpha = 0.5
    ) + 
    geom_step(aes(x = time, y = log.m.log), 
              data = tibble(
                  time = sf.obj$time[
                      (sf.obj$strata[1] + 1):length(sf.obj$time)
                  ], 
                  log.m.log = log(-log(sf.obj$surv + 1e-16))[
                      (sf.obj$strata[1] + 1):length(sf.obj$time)
                  ]
              ), color = col.plos.yellow, size = 0.5
    ) + 
    labs(x = "Time", y = "Log-Log Kaplan Meier Survival Curve") + 
    annotate(geom = "text", x = 650, y = -0.75, label = "ECOG performance = 0", 
             family = font.base, color = col.plos.pink, size = 5) + 
    annotate(geom = "text", x = 350, y = 0.75, label = "ECOG performance = 1", 
             family = font.base, color = col.plos.yellow, size = 5) + 
    theme(
        axis.title = element_text(size = 12)
    )

ggsave("rf-surv/fig/note_05-2.png", width = 5, height = 3.5, dpi = 300)
