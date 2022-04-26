###############################################
# PLSC 503 -- Spring 2022
#
# Generalized Linear Models (last day!)
###############################################
# Set working directory as necessary:

setwd("~/Dropbox (Personal)/PLSC 503")

# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 6) # show fewer decimal places

# Packages:

library(RCurl)
library(foreign)
library(readr)

######################################
# Toy example

X<-c(1,1,2,2,3,3,4,4,5,5)
Y<-c(0,2,1,3,2,4,3,5,4,6)

linmod<-lm(Y~X)
summary(linmod)
linglm<-glm(Y~X,family="gaussian")
summary(linglm)

# 2008 NES Data

NES08<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2022-git/main/Data/NES2008.csv")
summary(NES08[,4:16])

pdf("Notes/PolKnowledge.pdf",5,4)
par(mar=c(4,4,2,2))
bp<-barplot(xtabs(~NES08$knowledge),pch=19,lcolor="grey",
         ylab="Frequency",ylim=c(0,400),
         xlab="Knowledge Score")
dev.off()

nes08.binom<-glm(cbind(knowledge,4-knowledge)~age+female+white+
                   oftenvote+conservative+prayfreq+heterosexual+married+
                   yrsofschool+income,data=NES08,family=binomial)
summary(nes08.binom)

# Some predictions things...
#
# In-sample predictions:

XBhats<-predict(nes08.binom,type="link")
Yhats<-predict(nes08.binom,type="response")

# Plot:

pdf("Notes/GLM-predictions.pdf",7,6)
par(mar=c(4,4,2,2))
scatter.smooth(NES08$knowledge,Yhats,pch=20,ylab="Predicted Score",
     xlab="Actual Score",lpars=list(col="red",lwd=3))
dev.off()

# fin