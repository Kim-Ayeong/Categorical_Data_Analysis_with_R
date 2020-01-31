## Section 5.1 Model Selection
crab = read.table("crab.dat") # The 'crab.dat' file should be in the working directory
names(crab) = c("color","spine","width","satell","weight")
str(crab) # color, spine have been read as integer-type varibles
crab$color = as.factor(crab$color); crab$spine = as.factor(crab$spine)
crab$y = as.numeric(crab$satell > 0)
crab$weight = crab$weight/1000
## 5.1.2: multicolliarity diagnostics
obj = glm(y ~ . - satell, crab, family=binomial)
summary(obj) # Table 5.1. Caution: The first category of a factor is the baseline category in R
install.packages("car"); library(car); vif(lm(y ~ . -satell, crab)) # for vif
install.packages("olsrr"); library(olsrr); ols_coll_diag(lm(y ~ . -satell, crab)) # to print out vif and the condition index
## 5.1.3 ~ 5.1.5: backward selection
step(glm(y ~ ( . - satell - weight)^2, crab, family=binomial), direction="backward")
crab$dark = (crab$color != "5") # the level names of color are 2, 3, 4, 5
summary(glm(y ~ dark + width, crab, family=binomial))
obj.final = glm(y ~ dark + width, crab, family=binomial)
AIC(obj.final)
## 5.1.6: Classification Tables; Table 5.3
obj.widthcolor = glm(y ~ color + width, crab, family=binomial)
phat = predict(obj.widthcolor, type="response") # estimates of the probabilities
yhat = as.numeric(phat > 0.5)
table(crab$y, yhat) # somewhat different from Table 5.3
yhat = as.numeric(phat > mean(crab$y))
table(crab$y, yhat)
## 5.1.7: ROC curves and AUC
install.packages("ROCR"); library(ROCR)
obj.prediction = prediction(phat, y)
   # To see the components, type 'slotNames(obj.prediction)'.
   # 'obj.prediction' is an object of S4 class, so that different from a list. 
roc.perf = performance(obj.prediction, measure = "tpr", x.measure = "fpr")
plot(roc.perf, colorize=T) # ROC Curve. The curve is colorized according to cutoff points. 
abline(a=0, b=1, lty=2)
auc.perf = performance(obj.prediction, measure = "auc")
auc.perf@y.values # AUC (Area Under the ROC Curve) is about 0.771 with width and color 
