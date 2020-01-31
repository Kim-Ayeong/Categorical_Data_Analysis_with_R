## Section 7.1 LOGLINEAR MODELS 
## Example: Alcohol, Cigarette, and Marijuana Use for Hight School Seniors
(A = rep(c("Yes", "No"), c(4,4)))
(C = rep(rep(c("Yes", "No"),c(2,2)), 2))
(M = rep(c("Yes", "No"), 4))
freq = c(911, 538, 44, 456, 3, 43, 2, 279)
#library(MASS)
#fit = loglm(freq ~ (A + C + M)^2)
#summary(fit)
fit = glm(freq ~ (A + C + M)^2, family=poisson)
summary(fit) # Table 7.6
predict(fit, type="response") # Table 7.4