
library(survival)
library(flexsurv)

survreg_obj <- survreg(Surv(futime, fustat) ~ 1, #ecog.ps + rx, 
        ovarian, 
        dist='exponential')
flexsurv_obj <- flexsurvreg(Surv(futime, fustat) ~ 1, #ecog.ps + rx, 
            data = ovarian, 
            dist='exponential')


# Raw estimate
# 0.000769
sum(ovarian$fustat) / sum(ovarian$futime)

# Survreg is the inverse rate
# 1299
exp(coef(survreg_obj))

# Flexsurv is the rate of the event
# 0.000769
exp(coef(flexsurv_obj))

