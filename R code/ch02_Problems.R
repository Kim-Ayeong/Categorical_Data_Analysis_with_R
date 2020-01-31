# Problem 2.13: Difference of Proportions and Odds Ratio
A = array(c(509,398,116,104), dim=c(2,2), dimnames=list(Gender=c("F","M"), 
          Belief=c("Yes","No/Undecided"))) # You can use 'matrix' instead of 'array' for two-way tables
library(epiR) # You need to install it first
mosaicplot(A, shade=TRUE, main="Belief in Afterlife")
epi.2by2(A, method="cross.sectional", conf.level=0.9)

# Problem 2.19: Chi-squared Test and Studentized Residuals
A = array(c(871,302,444,80,873,43), dim=c(2,3), dimnames=list(Race=c("W","B"), 
            Party=c("Dem","Indep","Rep"))) 
mosaicplot(t(A), shade=TRUE, main="Party Identification by Race")
(chisq.obj = chisq.test(A))
chisq.obj$expected
chisq.obj$stdres

# Problem 2.27: Ordinal Data
A = array(c(9,44,13,10,11,52,23,22,9,41,12,27), dim=c(4,3), 
  dimnames=list(Aspir=c("some high","high grad","some college","college grad"), 
                Income=c("Low","Mid","High"))) 
A = t(A)
(chisq.obj = chisq.test(A))
chisq.obj$stdres
library(vcdExtra) # You need to install it first
CMHtest(A, rscores=1:3, cscores=1:4) # You need to perform a sensitivity analysis

# Problem 2.30, 2.31: Fisher's Exact Test
A = array(c(21,15,2,3), dim=c(2,2), dimnames=list(Therapy=c("Surgery","Radiation"), 
          Cancer=c("Controlled","Not Controlled"))) 
fisher.test(A, alternative = "greater") # one-sided p-value
fisher.test(A) # two-sided p-value
x = A[1,1];  m = sum(A[,1]); n = sum(A[,2]); k = sum(A[1,]) 
0.5*dhyper(x, m, n, k) + (1 - phyper(x, m, n, k)) # one-sided mid p-value

