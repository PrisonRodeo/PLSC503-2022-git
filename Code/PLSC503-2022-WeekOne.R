##############################################
# PLSC 503 week one slides (2022 remix).
#
# Infant mortality, etc. + bivariate regression.
#
# Be sure to setwd() to the right space...
#
# e.g.:
# setwd("~/Dropbox (Personal)/PLSC 503/Notes/")
  
library(RCurl)
library(car)
# install.packages("psych") <- Install psych package, 
                            #  if necessary
library(psych)

################################################
# Conceptual stuff...
#
# Read data from the github repo:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2022-git/master/Data/CountryData2000.csv")
IR2000<-read.csv(text=temp, header=TRUE)
rm(temp)

# I offer no warranty that any of this is the most 
# efficient way to do what I'm doing... all graphics
# are using base R graphics functionality, with
# the exception of the scatterplot matrices, which use
# the -car- package.
  
# Figure 1

pdf("IMbyLE.pdf",7,7) # make a PDF
par(cex=1.2) # make the things bigger
par(mar=c(4,4,2,2)) # plot margins
with(IR2000, plot(lifeexpectancy,infantmortalityperK, # draw the plot
             pch="",xlab="Life Expectancy at Birth",
             ylab="Infant Mortality Per 1000 Live Births"))
par(cex=0.8) # make the things smaller
# Add names:
with(IR2000, text(lifeexpectancy,infantmortalityperK,label=WBcode))
# add a linear-fit line
with(IR2000, abline(lm(infantmortalityperK~lifeexpectancy),lwd=3))
dev.off() # turn off the PDF-maker


# Figure 2 ("residual" plot):
#
# Make a smöl dataframe...

small<-data.frame(infantmortalityperK,lifeexpectancy,WBcode)
small<-small[is.na(small$lifeexpectancy)==FALSE,]
small<-small[is.na(small$infantmortalityperK)==FALSE,]

# regression: 

IMLE.fit<-lm(small$infantmortalityperK~small$lifeexpectancy)

# residuals:

IMLE.res<-resid(IMLE.fit)

# Plot!:

pdf("IMbyLEresids.pdf",7,7) # make a PDF
par(cex=1.2)
par(mar=c(4,4,2,2))
with(IR2000, plot(small$lifeexpectancy,IMLE.res,pch="",
     xlab="Life Expectancy at Birth",
     ylab="Residuals (Observed minus Expected)"))
par(cex=0.8)
with(IR2000, text(small$lifeexpectancy,IMLE.res,
     label=small$WBcode))
abline(h=0,lwd=3)
dev.off() # turn off the PDF-maker

# Figure 3...

pdf("IMbyFertility.pdf",7,7) # make a PDF
par(mar=c(4,4,2,2)) # margins
par(cex=1.2) #symbol size
with(IR2000, plot(fertility,infantmortalityperK,pch="",xlab="Fertility Rate (Births Per Woman)",
       ylab="Infant Mortality Per 1000 Live Births"))
par(cex=0.8)
with(IR2000, text(fertility,infantmortalityperK,label=WBcode))
with(IR2000, abline(lm(infantmortalityperK~fertility),lwd=3))
dev.off()

# Figure 4...

pdf("IMbyGDP.pdf",7,7) # make a PDF
par(mar=c(4,4,2,2)) # margins
par(cex=1.2)
with(IR2000, plot(NEWrgdpch,infantmortalityperK,pch="",xlab="Real GDP per capita",
       ylab="Infant Mortality Per 1000 Live Births"))
par(cex=0.8)
with(IR2000, text(NEWrgdpch,infantmortalityperK,label=WBcode))
with(IR2000, abline(lm(infantmortalityperK~NEWrgdpch),lwd=3))
with(IR2000, lines(lowess(infantmortalityperK~NEWrgdpch,span=0.1,iter=1),
      lwd=3,col="red",lty=2))
dev.off()

# Figure 5...

pdf("lnIMbylnGDP.pdf",7,7) # make a PDF
par(mar=c(4,4,2,2)) # margins
par(cex=1.2)
with(IR2000, plot(log(NEWrgdpch),log(infantmortalityperK),pch="",xlab="Logged Real GDP per 
       Capita",ylab="Logged Infant Mortality Per 1000 Live Births"))
par(cex=0.8)
with(IR2000, text(log(NEWrgdpch),log(infantmortalityperK),label=WBcode))
with(IR2000, abline(lm(log(infantmortalityperK)~log(NEWrgdpch)),lwd=3))
with(IR2000, lines(lowess(log(infantmortalityperK)~log(NEWrgdpch)),
      lwd=3,lty=2,col="red"))
dev.off()

# Figure 6...

pdf("IMbyPOLITY.pdf",7,7) # make a PDF
par(mar=c(4,4,2,2)) # margins
par(cex=1.2)
with(IR2000, plot(polity2,infantmortalityperK,pch="",xlab="POLITY IV score",ylab="Infant 
       Mortality Per 1000 Live Births"))
par(cex=0.8)
with(IR2000, text(polity2,infantmortalityperK,label=WBcode))
with(IR2000, abline(lm(infantmortalityperK~polity2),lwd=3))
with(IR2000, lines(lowess(infantmortalityperK~polity2),lwd=3,
             lty=2,col="red"))
dev.off()

# Figure 7...

IR2000$Rich<-as.factor(IR2000$NEWrgdpch>median(IR2000$NEWrgdpch,na.rm=TRUE))

pdf("IMbyPOLITYandGDP.pdf",7,7) # make a PDF
par(mar=c(4,4,2,2)) # margins
par(cex=1.2)
scatterplot(infantmortalityperK~polity2|Rich,IR2000,cex=1.5,
            cex.axis=1.5,cex.lab=1.3,boxplots="",xlab="POLITY IV", 
            ylab="Infant Mortality per 1000 Live Births",
            pch=c(16,17),legend.coords="topleft",grid=FALSE,
            reg.line=FALSE,lwd=c(3,3),col=c(1,2),lty=c(3,4))
dev.off()

# Figure 8
  
pdf("HealthScatterMatrix.pdf",7,7) # make a PDF
par(mar=c(4,4,2,2)) # margins
with(IR2000, scatterplotMatrix(~infantmortalityperK+fertility+lifeexpectancy+measlespct+DPTpct,
   var.labels=list("Infant Mortality","Fertility","Life Expectancy","Measles Immunizations",
   "DPT Immunizations"),use=c("complete.obs")))
dev.off()

# Figure 9
#
# Build an additive index of standardized variables:

IMS<-with(IR2000, (infantmortalityperK-mean(infantmortalityperK,na.rm=TRUE))/sd(infantmortalityperK,
        na.rm=TRUE))
FertS<-with(IR2000, (fertility-mean(fertility,na.rm=TRUE))/sd(fertility,na.rm=TRUE))
LES<-with(IR2000, (lifeexpectancy-mean(lifeexpectancy,na.rm=TRUE))/sd(lifeexpectancy,na.rm=TRUE))
MIS<-with(IR2000, (measlespct-mean(measlespct,na.rm=TRUE))/sd(measlespct,na.rm=TRUE))
DPTS<-with(IR2000, (DPTpct-mean(DPTpct,na.rm=TRUE))/sd(DPTpct,na.rm=TRUE))
IR2000$Health<-(-IMS-FertS+LES+MIS+DPTS)/5

# plot:
pdf("HealthScatterMatrixII.pdf",7,7) # make a PDF
par(mar=c(4,4,2,2)) # margins
with(IR2000, scatterplotMatrix(~infantmortalityperK+fertility+lifeexpectancy+measlespct+DPTpct+Health,
      var.labels=list("Infant Mortality","Fertility","Life Expectancy","Measles Immunizations",
      "DPT Immunizations","Additive Index")))
dev.off()

# That's all for now.
##############################################