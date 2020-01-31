## Section 3.3 Poisson Regression or Poisson Loglinear Model
## Section 3.3.2 Example: Female Horseshoe Crabs and their Satellites 
crab = read.table("crab.dat") # The 'crab.dat' file should be in the working directory
names(crab) = c("color","spine","width","satell","weight")

# To draw Fig. 3.4 and (one similar to) 3.5
attach(crab)
plot(width, satell)
plot(width, jitter(satell)) 
lines(lowess(width, satell), col=2)

# Fit the Poisson loglinear model
obj.log = glm(satell ~ width, family = poisson(link="log"))
summary(obj.log)

# Fit the GLM with Poisson random component and identity link
obj.iden = glm(satell ~ width, data=crab, family = poisson(link="identity")) # Fail to find MLE
start.values = coef(glm(satell ~ width, data=crab, family = gaussian)) # To get the initial values
obj.iden = glm(satell ~ width, data=crab, family = poisson(link="identity"), start=start.values)
summary(obj.iden)

# Fig. 3.6
vec = order(width)
lines(width[vec], obj.log$fitted.values[vec], col=4) # blue line for log link. 
lines(width[vec], obj.iden$fitted.values[vec], col=3) # green line for identity link
detach(crab)
