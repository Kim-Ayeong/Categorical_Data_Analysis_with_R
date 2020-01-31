# Problem 3.20
#dfr = read.table("Table3_9.txt", header=T)
dfr = scan(what=list(age="", smoke="", personyears=0, cdeaths=0))
35-44 Nonsmokers 18793 2
45-54 Nonsmokers 10673 12
55-64 Nonsmokers 5710 28
65-74 Nonsmokers 2585 28
75-84 Nonsmokers 1462 31
35-44 Smokers 52407 32
45-54 Smokers 43248 104
55-64 Smokers 28612 206
65-74 Smokers 12663 186
75-84 Smokers 5317 102

# [a]
dfr = as.data.frame(dfr)
attach(dfr)
dfr.nonsmokers = subset(dfr, smoke=="Nonsmokers")
dfr.smokers = subset(dfr, smoke=="Smokers")
rates.nonsmokers = with(dfr.nonsmokers, cdeaths/(personyears*1000))
rates.smokers = with(dfr.smokers, cdeaths/(personyears*1000))
age.score = seq(40,80,by=10)
plot(age.score, rates.nonsmokers/rates.smokers, type="b", xlab="Age", ylab="ratio of death rates" )
# [b] and [d]
obj1 = glm(cdeaths ~ age + smoke, family=poisson(), data=dfr, offset=log(personyears))
# [c] and [d]
age.variable = c(age.score, age.score) # age for nonsmokers and smokers
obj2 = glm(cdeaths ~ age + smoke + age.variable:smoke, family=poisson(), data=dfr, offset=log(personyears))
AIC(obj1, obj2) # The model with the quantitative interaction term of age and smoking is better
LRT = deviance(obj1) - deviance(obj2); df.LRT = obj1$df.residual - obj2$df.residual; 1 - pchisq(LRT, df=df.LRT) 
   # The quantitative interaction term of age and smoking is significant
detach(dfr)
