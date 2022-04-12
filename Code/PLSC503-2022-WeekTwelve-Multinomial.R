##########################################
# Code for PLSC 503 - Spring 2022
#
# Regression models for nominal-level 
# outcomes... (this week, the topic of
# ordinal-response models was covered by
# Ilayda Onder).
#
##########################################
# Packages, etc. (install as necessary):

library(RCurl)
library(MASS)
library(mlogit)
library(nnet)
library(VGAM)
library(MNLpred)
library(aod)
library(car)
library(ggplot2)
library(scales)
library(margins)

# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

# setwd():
#
setwd("~/Dropbox (Personal)/PLSC 503/Notes")
#####################################################
# Multinomial logit, etc.

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2022-git/master/Data/Election1992small.csv")
nes92<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(nes92)

nes92.mlogit<-vglm(presvote~partyid, multinomial, nes92)
summary(nes92.mlogit)

Bush.nes92.mlogit<-vglm(formula=presvote~partyid, 
                        family=multinomial(refLevel=1),data=nes92) 
summary(Bush.nes92.mlogit)

Clinton.nes92.mlogit<-vglm(formula=presvote~partyid,
                           family=multinomial(refLevel=2),data=nes92)
summary(Clinton.nes92.mlogit)

# Conditional logit...

colnames(nes92)<-c("caseid","presvote","partyid","FT.Bush","FT.Clinton","FT.Perot")
nes92$PVote<-factor(nes92$presvote,labels=c("Bush","Clinton","Perot"))
head(nes92)

nes92CL<-mlogit.data(nes92,shape="wide",choice="PVote",varying=4:6)
head(nes92CL,6)

# Conditional logistic regression:

nes92.clogit<-mlogit(PVote~FT|partyid,data=nes92CL)
summary(nes92.clogit)

# Interpretation part (with more predictors...):

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2022-git/master/Data/Election1992.csv")
BigNES92<-read.csv(text=temp, header=TRUE)
rm(temp)

NES.MNL<-vglm(presvote~partyid+age+white+female,data=BigNES92,
              multinomial(refLevel=1))
summaryvglm(NES.MNL)

wald.test(b=c(t(coef(NES.MNL))),Sigma=vcov(NES.MNL),Terms=c(5,6))
wald.test(b=c(t(coef(NES.MNL))),Sigma=vcov(NES.MNL),Terms=c(1,3,5,7,9))

# Marginal effects, via -margins-...
#
# Refit model using -multinom-:

BigNES92$PresVote<-cut(BigNES92$presvote,3,labels=c("Bush","Clinton","Perot"))
BigNES92$White<-ifelse(BigNES92$white=="White",1,0) # numeric
MNL.alt<-multinom(PresVote~partyid+age+White+female,data=BigNES92,
                  Hess=TRUE)
summary(marginal_effects(MNL.alt))


# Hats, yo:

PickBush<-ifelse(fitted.values(NES.MNL)[,1]>fitted.values(NES.MNL)[,2] 
                 & fitted.values(NES.MNL)[,1]>fitted.values(NES.MNL)[,3], 1,0)
PickWJC<-ifelse(fitted.values(NES.MNL)[,2]>fitted.values(NES.MNL)[,1] 
                & fitted.values(NES.MNL)[,2]>fitted.values(NES.MNL)[,3], 2, 0)
PickHRP<-ifelse(fitted.values(NES.MNL)[,3]>fitted.values(NES.MNL)[,1] 
                & fitted.values(NES.MNL)[,3]>fitted.values(NES.MNL)[,2], 3, 0)
OutHat<-PickBush+PickWJC+PickHRP
table(BigNES92$presvote,OutHat)

# Odds ratios:

mnl.or <- function(model) { 
  coeffs <- c(t(coef(NES.MNL))) 
  lci <- exp(coeffs - 1.96 * diag(vcov(NES.MNL))^0.5) 
  or <- exp(coeffs) 
  uci <- exp(coeffs + 1.96* diag(vcov(NES.MNL))^0.5) 
  lreg.or <- cbind(lci, or, uci) 
  lreg.or 
} 

mnl.or(NES.MNL)

# In-sample predictions:

hats<-as.data.frame(fitted.values(NES.MNL))
names(hats)<-c("Bush","Clinton","Perot")
attach(hats)

pdf("InSampleRScatterplotMatrix.pdf",8,7)
spm(~Bush+Clinton+Perot,pch=20,plot.points=TRUE,
    diagonal="histogram",col=c("black","grey"))
dev.off()

pdf("InSampleMNLPredProbsR.pdf",8,6)
par(mfrow=c(1,3))
plot(BigNES92$partyid,Bush,xlab="Party ID")
plot(BigNES92$partyid,Clinton,xlab="Party ID")
plot(BigNES92$partyid,Perot,xlab="Party ID")
par(mfrow=c(1,1))
dev.off()

# Predicted probabilities using -MNLpred-...
#
# Recall the -multinom- estimates:

summary(MNL.alt)

# Predictions:

Hats<-mnl_pred_ova(model=MNL.alt,data=BigNES92,
                   x="partyid",by=0.1,seed=7222009,nsim=500)

# Plotting predicted probabilities & CIs
# (via ggplot; can also be done easily with 
# base R):

cand.labs <- c("Bush", "Clinton", "Perot")
names(cand.labs) <- c("1", "2", "3")

pdf("MNLPredictedProbabilities.pdf",8,6)
ggplot(data=Hats$plotdata,aes(x=partyid,y=mean,
            ymin=lower,ymax=upper)) +
  geom_ribbon(alpha = 0.1) +
  geom_line() + theme_bw() +
  facet_wrap(PresVote~.,scales="fixed",
             labeller=labeller(presvote=cand.labs)) +
  scale_x_continuous(breaks=1:7) +
  labs(y = "Predicted Probabilities",x = "Party Identification")
dev.off()

# Plotting first differences for the FEMALE variable:

FDF<-mnl_fd2_ova(model=MNL.alt,data=BigNES92,x="White",
                 value1=min(BigNES92$White),
                 value2=max(BigNES92$White),nsim=500)

pdf("MNLFirstDifferences.pdf",7,5)
ggplot(FDF$plotdata_fd,aes(categories, y=mean,
                           ymin=lower,max=upper)) +
  geom_pointrange() + geom_hline(yintercept=0) +
  scale_y_continuous(name="First Difference: White") +
  labs(x = "Candidate") + theme_bw()
dev.off()

# Conditional logit example:

nes92.clogit<-mlogit(PVote~FT|partyid,data=nes92CL)
summary(nes92.clogit)

# In-sample predictions:

CLhats<-predict(nes92.clogit,nes92CL)

pdf("InSampleCLHatsR.pdf",7,6)
plot(nes92$FT.Bush,CLhats[,1],pch=19,
     col=rgb(100,0,0,100,maxColorValue=255),
     xlab="Feeling Thermometer",
     ylab="Predicted Probability")
points(nes92$FT.Clinton+runif(nrow(CLhats),-1,1),
       CLhats[,2],pch=4,col=rgb(0,0,100,100,maxColorValue=255))
points(nes92$FT.Perot+runif(nrow(CLhats),-1,1),
       CLhats[,3],pch=17,col=rgb(0,100,0,50,maxColorValue=255))
lines(lowess(nes92$FT.Bush,CLhats[,1]),lwd=2,col="red")
lines(lowess(nes92$FT.Clinton,CLhats[,2]),lwd=2,col="blue")
lines(lowess(nes92$FT.Perot,CLhats[,3]),lwd=2,col="darkgreen")
legend("topleft",bty="n",c("Bush","Clinton","Perot"),
       col=c("red","blue","darkgreen"),pch=c(19,4,17))
dev.off()

# \fin (ordered-response code is in a separate file...)