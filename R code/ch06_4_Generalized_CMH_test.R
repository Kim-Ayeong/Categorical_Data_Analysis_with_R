# Table 6.12 Job Satisfaction
freq = c(1,3,11,2,2,3,17,3,0,1,8,5,0,2,4,2,1,1,2,1,0,3,5,1,0,0,7,3,0,1,9,6)
(freq.mat = matrix(freq, nrow=8, byrow=T))
gender = c(rep("female",sum(freq.mat[1:4,])), rep("male",sum(freq.mat[5:8,])))
income = c()
for (i in 1:4) income = c(income, rep(i, sum(freq.mat[i,])))
for (i in 1:4) income = c(income, rep(i, sum(freq.mat[i + 4,])))
income = factor(income, labels=c("<5000","5000-15000","15000-25000",">25000"))
jobsatis = c()
for (i in 1:8) jobsatis = c(jobsatis, rep(1:4, freq.mat[i,1:4]))
jobsatis = factor(jobsatis, labels=c("VeryDis","AlittleSatis","ModSatis","VerySatis"))
tab6.12 = table(income, jobsatis, gender)
#mantelhaen.test(tab6.12) # Alt Hypo: General Association in Table 6.13
library(vcdExtra)
CMHtest(tab6.12, rscores=c(3,10,20,35), cscores=c(1,3,4,5), overall=T) # Table 6.13
