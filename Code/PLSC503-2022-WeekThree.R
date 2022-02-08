###################################################
# PLSC 503 -- Spring 2022
#
# Code for Week Three ("Multivariate Regression")
###################################################
# setwd() here...
#
# setwd(~/Whatever)
#
# Packages:

library(RCurl) # <- install packages as necessary
library(car) 
library(psych)
library(stargazer)
library(lmtest)
library(plotrix)
library(dplyr)
library(dotwhisker)

#######################################
# STUPID REGRESSION TRICKS
#######################################

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2022-git/master/Data/africa2001.csv")
africa<-read.csv(text=temp, header=TRUE)
summary(africa)

fit<-with(africa, lm(adrate~muslperc))
summary(fit)

# First figure

SEs<-predict(fit,interval="confidence")
Sort<-order(africa$muslperc)

pdf("Notes/SRTFig1.pdf",6,5)
plot(africa$muslperc, africa$adrate, 
     xlab="Muslim Percentage of the Population", 
     ylab="HIV Prevalence Rate",pch=16) 
abline(fit,lwd=3)
lines(sort(africa$muslperc),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(africa$muslperc),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

# Add 10:

africa$muslplusten<-africa$muslperc+10
fit2<-with(africa, lm(adrate~muslplusten,data=africa))
summary(fit2)

SEs<-predict(fit2,interval="confidence")

pdf("Notes/SRTFig2.pdf",6,5)
plot(africa$muslplusten, africa$adrate, 
     xlab="Muslim Percentage of the Population + 10", 
     ylab="HIV Prevalence Rate",pch=16)
abline(fit2,lwd=3)
lines(sort(africa$muslplusten),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(africa$muslplusten),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

# Multiply Y times -314:

africa$screwyrate<-africa$adrate*(-314)
fit3<-with(africa, lm(screwyrate~muslperc))
summary(fit3)

SEs<-predict(fit3,interval="confidence")

pdf("Notes/SRTFig3.pdf",6,5)
plot(africa$muslperc, africa$screwyrate, 
     xlab="Muslim Percentage of the Population", 
     ylab="HIV Prevalence Rate times -314",pch=16,
     ylim=c(-13000,1000))
abline(fit3,lwd=3)
lines(sort(africa$muslperc),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(africa$muslperc),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

# Reversing the scales:

africa$nonmuslimpct <- 100 - africa$muslperc
africa$noninfected <- 100 - africa$adrate
fit4<-lm(noninfected~nonmuslimpct,data=africa)
summary(fit4)

SEs<-predict(fit4,interval="confidence")
Sort2 <- order(africa$nonmuslimpct)

pdf("Notes/SRTFig4.pdf",6,5)
plot(africa$nonmuslimpct, africa$noninfected, 
     xlab="Non-Muslim Percentage of the Population", 
     ylab="HIV Non-Prevalence Rate",pch=16,
     ylim=c(60,111))
abline(fit4,lwd=3)
lines(sort(africa$nonmuslimpct),SEs[Sort2,2],col="red",lwd=2,lty=2)
lines(sort(africa$nonmuslimpct),SEs[Sort2,3],col="red",lwd=2,lty=2)
dev.off()

# Centering X:

africa$muslcenter<-africa$muslperc - mean(africa$muslperc, na.rm=TRUE)
fit5<-lm(adrate~muslcenter,data=africa)
summary(fit5)
mean(africa$adrate)

SEs<-predict(fit5,interval="confidence")

pdf("Notes/SRTFig5.pdf",6,5)
plot(africa$muslcenter, africa$adrate, 
     xlab="Centered Muslim Percentage of the Population", 
     ylab="HIV Prevalence Rate",pch=16,
     ylim=c(-10,40))
abline(fit5,lwd=3)
lines(sort(africa$muslcenter),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(africa$muslcenter),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

# Rescaling X for interpretability:

fit6<-lm(adrate~population,data=africa)
summary(fit6)

africa$popmil<-africa$population / 1000000
fit7<-lm(adrate~popmil,data=africa)
summary(fit7)

# Dichotomous X and t-tests:

fit8<-lm(adrate~subsaharan,data=africa)
summary(fit8)

with(africa,
     t.test(adrate~subsaharan, var.equal=TRUE))

# Reporting:

fit<-lm(adrate~muslperc, data=africa)
summary.lm(fit)

# Easy LaTeX table using *stargazer*:

stargazer(fit,
          type="latex",
          title="OLS Regression Model of HIV/AIDS Rates in Africa, 2001",
          dep.var.caption="",
          dep.var.labels="Model I",
          digits=2,
          no.space=TRUE,
          intercept.top=TRUE,intercept.bottom=FALSE,
          covariate.labels=c("(Constant)","
                             Muslim Percentage of the Population"))


################################################################
# MULTIVARIATE REGRESSION...
################################################################
# Added variable plot:

library(RCurl)

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2022-git/master/Data/CountryData2000.csv")
Data <- read.csv(text = url) # read the country-level data
rm(url)

Data<-na.omit(Data[c("infantmortalityperK","DPTpct","healthexpGDP")])

fit<-lm(infantmortalityperK~DPTpct,data=Data)
aux<-lm(healthexpGDP~DPTpct,data=Data)

# Plot:

pdf("AVPlot.pdf",7,6)
plot(aux$residuals,fit$residuals,pch=19,
     xlab="Health Expenditures | DPT Rates: Residuals", 
     ylab="Infant Mortality | DPT Rates: Residuals")
abline(lm(fit$residuals~aux$residuals),lwd=3)
dev.off()

# Using avPlots from -car-:

fit2<-lm(infantmortalityperK~DPTpct+healthexpGDP,data=Data)
avPlots(fit2,~healthexpGDP)

#####################
# Toy example (N=4):

Y<-c(4,-2,9,-5)
X1<-c(200,120,430,110)
X2<-c(-17,32,-29,25)
data<-cbind(Y,X1,X2)
scatterplotMatrix(data)

cor(data)

fit<-lm(Y~X1+X2)
summary(fit)

########################################

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2022-git/master/Data/africa2001.csv")
Data<-read.csv(text=temp, header=TRUE)
Data<-with(Data, data.frame(adrate,polity,
                            subsaharan=as.numeric(as.factor(subsaharan))-1,
                            muslperc,literacy))
summary(Data)
cor(Data)

# Scatterplot matrix:

scatterplotMatrix(Data)

# Linerar model...

model<-lm(adrate~polity+subsaharan+muslperc+literacy,data=Data)
summary(model)

options(digits=4)
vcov(model)

# Linear hypothesis (F) tests...

modelsmall<-lm(adrate~muslperc+literacy,data=Data)
waldtest(model,modelsmall)  # from -lmtest- package

# Or:

linearHypothesis(model,"muslperc=0.1") # from -car-

linearHypothesis(model,"literacy=muslperc")

# Confidence ellipse

confidenceEllipse(model=model,which.coef=c(4,5),
                  xlab="Muslim Percentage",ylab="Literacy")
abline(h=0,v=0,lty=2)

# Predicted values:

hats<-fitted(model)

# Or, alternatively:

fitted<-predict(model,se.fit=TRUE, interval=c("confidence"))

# Plotted:

scatterplot(model$fitted~Data$adrate,log="x",smooth=FALSE,boxplots=FALSE,
            reg.line=FALSE,xlab="Observed HIV Rate",ylab="Predicted HIV Rate",
            pch=16,cex=2)

# Or:

plotCI(Data$adrate,model$fitted,uiw=(1.96*(fitted$se.fit)),
       log="x",xlab="Observed HIV Rate",ylab="Predicted HIV Rate")
lines(lowess(Data$adrate,Data$adrate),lwd=2)

########
# Presentation...
#
# Re-fit three models:

M1<-lm(adrate~polity+subsaharan+muslperc+literacy,data=Data)
M2<-lm(adrate~polity+subsaharan+muslperc,data=Data)
M3<-lm(adrate~polity+subsaharan+literacy,data=Data)

# A (default) table:

stargazer(M1,M2,M3)

# A dot-whisker (or "ladder") plot of the un-rescaled
# coefficients:

pdf("ExampleDotWhisker.pdf",6,5)
dwplot(list(M1,M2,M3),
    vline=geom_vline(xintercept = 0,
              colour = "black",linetype = 2),
    vars_order=c("polity","subsaharan","muslperc","literacy")) %>%
    relabel_predictors(c(polity="POLITY",
                            subsaharan="Sub-Saharan",
                            muslperc="Muslim Percentage",
                            literacy="Literacy Rate")) +
        theme_classic() +
        xlab("Coefficient Estimate") +
        scale_color_hue(labels=c('Full Model','w/o Literacy','w/o Muslim Pct.'))
dev.off()        

# A rescaled-by-two-standard-deviations dot-whisker plot:

pdf("BetterDotWhisker.pdf",6,5)
dwplot(list(M1,M2,M3),by_2sd = TRUE,
       vline=geom_vline(xintercept = 0,
                        colour = "black",linetype = 2),
       vars_order=c("polity","subsaharan","muslperc","literacy")) %>%
        relabel_predictors(c(polity="POLITY",
                             subsaharan="Sub-Saharan",
                             muslperc="Muslim Percentage",
                             literacy="Literacy Rate")) +
        theme_classic() +
        xlab("Coefficient Estimate") +
        scale_color_hue(labels=c('Full Model','w/o Literacy','w/o Muslim Pct.'))
        dev.off()  




