install.packages("survival")
install.packages("survminer")

library(survival)
library(survminer)


# Fit a Cox PH model from hfdata
cox <- coxph(Surv(fu_time, death) ~ ethnicgroup, data = hfdata) 
summary(cox)
