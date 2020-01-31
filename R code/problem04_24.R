# Problem 4.24
dfr = read.csv("Table4_19.csv")
obj = glm(Y ~ D + T, data=dfr, family=binomial)
summary(obj)
predict(obj, newdata=data.frame(D=c(63,25), T=c(0,0)), type="response")
  # print out the estimated probabilities at the specified values of D and T
