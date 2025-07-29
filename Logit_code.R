rm(list=ls())
setwd("D:/College Studies/Dissertation/MAIN/Data")
data=read.csv("Survey Data Cleaned for R.csv")
#data
attach(data)
colnames(data)

#Summing 3,4 and 5 for negative emotions
ne=ifelse(negative.emotions>=3,1,0)
ne=as.factor(ne)

re.time=as.factor(Avg.time)
re.time <- relevel(re.time, ref = "Less than 1 hour")

re.dayt=as.factor(Time.of.day)
re.dayt<- relevel(re.dayt, ref = "Morning")

re.soc=as.factor(conform.to.social.norms)
re.soc<- relevel(re.soc, ref = "No")

fit=glm(as.factor(ne)~as.factor(re.time)+as.factor(re.dayt)+as.factor(distracted.when.busy)+
as.factor(FOMO)+as.factor(compare)+as.factor(re.soc),family=binomial("logit"))
summary(fit)

library(glmtoolbox)
gvif(fit)

pi_hat_L=predict(fit,type="response")
thr=0.4505884

Yi_hat_L=ifelse(pi_hat_L>thr,1,0)
Yi_hat_L

Y=ne

#Confusion matrix using threshold 
conf_mat_L=table(Y,Yi_hat_L)
conf_mat_L

> 70/(70+19)
[1] 0.7865169
> 23/(23+60)
[1] 0.2771084

library("ROCit")
roc_info=rocit(score=pi_hat_L,Y)
roc_info
plot(roc_info)
summary(roc_info)
locator(1)
opt_cutoff <- roc_info$Cutoff[which.max(roc_info$TPR*(1-roc_info$FPR))]
print(opt_cutoff)


max(roc_info$TPR*(1-roc_info$FPR))
roc_info$TPR[84]
roc_info$FPR[84]
