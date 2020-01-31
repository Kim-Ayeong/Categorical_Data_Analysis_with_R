# We want to draw Fig. 4.2 and Fig. 4.3 in the same graph
crab = read.table("crab.dat") # The 'crab.dat' file should be in the working directory
names(crab) = c("color","spine","width","satell","weight")
attach(crab)
y = as.numeric(satell > 0)
plot(jitter(width), y, yaxp=c(0,1,1), xlab="Width", ylab="Presence of Satellites")
lines(lowess(width, y), col=4)
width.center = seq(22.75, 29.75)
sample.prop = c(0.36, 0.29, 0.61, 0.54, 0.68, 0.83, 0.83, 1)
points(width.center, sample.prop, pch=19, col=4)
obj.glm = glm(y ~ width, family=binomial)
(EL50 = -coef(obj.glm)[1]/coef(obj.glm)[2]) # median effective level
pihat = predict(obj.glm, type="response")
ord = order(width)
lines(width[ord], pihat[ord], col=2)
# How to draw the 95% confidence band?
etahat = predict(obj.glm, type="link", se.fit=TRUE)
eta.upper = etahat$fit + 2*etahat$se.fit; eta.lower = etahat$fit - 2*etahat$se.fit
pi.upper = 1/(1 + exp(-eta.upper)); pi.lower = 1/(1 + exp(-eta.lower))
matlines(width[ord], cbind(pi.upper[ord], pi.lower[ord]), col=2, lty=2)