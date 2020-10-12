# note survival analysis 3
library(MASS)
library(survival)
library(survminer)
library(tidyverse)

# Step 1 ----
data(gehan)
gehan <- gehan %>% 
    mutate(treat = ifelse(treat == "6-MP", "6MP", "control"))

# Step 2 ----
surv.obj <- Surv(time = gehan$time, event = gehan$cens)

# Step 3 ----
ge.sf <- survfit(surv.obj ~ treat, data = gehan)

# Step 4 ----
ge.sf.df <- surv_summary(ge.sf, data = gehan)
View(ge.sf.df)

# Step 5 ----
ge.table <- ge.sf.df %>% 
    select(time, n.risk, n.event, n.censor, treat) %>% 
    pivot_wider(names_from = treat, 
                values_from = c(n.risk, n.event, n.censor)) %>% 
    arrange(time) %>% 
    fill(starts_with("n.risk"), .direction = "up") %>% 
    fill(starts_with("n.risk"), .direction = "down") %>% 
    replace_na(list(n.event_6MP = 0, n.event_control = 0, 
                    n.censor_6MP = 0, n.censor_control = 0)) %>% 
    mutate(n.event = n.event_6MP + n.event_control,
           n.risk = n.risk_6MP + n.risk_control)
View(ge.table)

# Step 6 ----
ge.table <- ge.table %>% 
    mutate(
        n.event.expected_6MP = n.event * n.risk_6MP / n.risk,
        n.event.expected_control = n.event * n.risk_control / n.risk
        ) %>% 
    mutate(
        o_e_6MP = n.event_6MP - n.event.expected_6MP,
        o_e_control = n.event_control - n.event.expected_control,
        var = n.risk_6MP * n.risk_control * n.event * (n.risk - n.event) /
            n.risk ^ 2 / (n.risk - 1)
    )

# Step 7 ----
sum(ge.table$o_e_6MP) ^ 2 / sum(ge.table$var)
# 16.79294
pchisq(16.79, df = 1, lower.tail = FALSE)
# 4.175275e-05

# Step 8 ----
t2.1 <- sum(ge.table$o_e_6MP) ^ 2 / sum(ge.table$n.event.expected_6MP)
t2.2 <- sum(ge.table$o_e_control) ^ 2 / sum(ge.table$n.event.expected_control)
t2.1 + t2.2
# 15.23285
pchisq(15.23, df = 1, lower.tail = FALSE)
# 9.517935e-05

# Step 9 ----
survdiff(surv.obj ~ treat, data = gehan)

