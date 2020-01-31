# Section 3.3.3 Overdispersion
# GLM with Negative Binomial random component
library(MASS)
summary(glm.nb(satell ~ width, data=crab))

# Section 3.3.5 Count Regression for Rate Data
tab3_4 = read.table("Table3_4.txt")
names(tab3_4) = c("year","km","train","y")
attach(tab3_4)
nyears = year - 1975

# EDA
plot(nyears, y/km); lines(lowess(nyears, y/km), col=2) # smoothing lines in the boundary are not reliable
plot(nyears, log(y/km)); lines(lowess(nyears, log(y/km)), col=2)

# Fit GLMs
pois.obj = glm(y ~ offset(log(km)) + nyears, family=poisson)
# OR glm(y ~ nyears, data=tab3_4, family=poisson, offset=log(km))
summary(pois.obj)
nb.obj = glm.nb(y ~ offset(log(km)) + nyears, data=tab3_4) # fit a negative binomial model, which gives a slightly lower AIC.
summary(nb.obj)
AIC(pois.obj, nb.obj) # Model Selection: Choose the model with lower AIC
abline(coef(pois.obj), col=3) # poisson fit in green line
abline(coef(nb.obj), col=4) # negative binomial fit in blue line

rate = y/km; glm(rate ~ nyears, family=poisson) # THIS IS NOT WORKING! 'rate' is not integer-valued.
detach(tab3_4)