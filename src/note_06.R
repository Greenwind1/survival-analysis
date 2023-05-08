# note: survival analysis 6

# 0: Set Environments ----
library(tidyverse)
library(survival)
library(survminer)
source(file = "utility/environments.R")

# 1: Load Data ----
cancer <- survival::cancer %>% 
    filter(!is.na(ph.ecog) & ph.ecog < 2) %>% 
    mutate(
        status = ifelse(status == 1, yes = 0, no = 1), 
        sex = factor(sex, levels = c(1, 2), labels = c("M", "F")), 
        ph.ecog = as.factor(ph.ecog)
    )


# 2: Create a survival object ----
surv.obj <- Surv(time = cancer$time, event = cancer$status)


# 3-1: Fit a Cox PH model using a sex variable ----
cox.m.1 <- coxph(formula = surv.obj ~ sex, 
                 data = cancer,
                 ties = "efron")


# 3-2: Calculate and plot residuals ----
# residuals = residuals.coxph {survival}
# Calculates martingale, deviance, score or Schoenfeld residuals 
# for a Cox proportional hazards model.
cox.m.1.res <- residuals(cox.m.1, type = "schoenfeld")
cat(sum(cox.m.1.res))
cox.m.1.res.df <- tibble(time = names(cox.m.1.res), sex = cox.m.1.res) %>% 
    mutate(time = as.numeric(time))
p1 <- ggplot(data = cox.m.1.res.df, aes(x = time, y = sex)) + 
    geom_point(color = col.plos.pink, size = 1, alpha = 0.75) + 
    geom_smooth(method = "loess", span = 1, se = FALSE, size = 0.5, color = col.os) +
    labs(y = "Schoenfeld residuals for sex variable")
p1
ggsave("rf-surv/fig/shoenfeld-residuals_1.jpg", dpi = 300)


cox.m.1.res.scaled <- residuals(cox.m.1, type = "scaledsch")
p1.s <- ggcoxzph(
    fit = cox.zph(fit = cox.m.1), 
    resid = TRUE, se = TRUE, df = 3, nsmo = length(cox.m.1.res), 
    var = "sex",
    point.col = col.plos.pink, point.size = 1, point.shape = 19, point.alpha = 0.75,
    caption = NULL, 
    ggtheme = theme(
        title = element_blank(),
        axis.title = element_text(face = font.face, color = col.os),
        axis.text = element_text(face = font.face, color = col.os),
        panel.grid.major = element_line(linewidth = 0.25),
        panel.grid.minor = element_blank()
    ), 
    # ylab = "Scaled Schoenfeld residuals"
)
attr(p1.s, "global_pval") <- NULL
p1.s
ggsave("rf-surv/fig/scaled-shoenfeld-residuals_1.jpg", dpi = 300)
