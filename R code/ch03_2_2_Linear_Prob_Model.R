# Table 3.1: Linear Prob Model
snoring = rep(c(0,2,4,5),c(24+1355,35+603,21+192,30+224))
y = rep(c(1,0,1,0,1,0,1,0),c(24,1355,35,603,21,192,30,224)) 
glm.obj = glm(y ~ snoring, family=binomial(link="identity"))
summary(glm.obj)

# or you can specify y as a two-column matrix with the columns giving the numbers of successes and failures
snoring = c(0,2,4,5)
y = matrix(c(24,35,21,30,1355,603,192,224), nrow=4)
glm.obj = glm(y ~ snoring, family=binomial(link="identity"))
summary(glm.obj) # We get the equivalent results for the regression coefficients

# try some other link functions
summary(glm(y ~ snoring, family=binomial(link="logit")))
summary(glm(y ~ snoring, family=binomial(link="probit")))

