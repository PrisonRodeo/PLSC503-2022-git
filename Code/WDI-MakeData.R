#########################################################
# PLSC 503 (2022)
#
# Regression class, basically...
#
# Code to grab & create data from the World Bank's
# _World Development Indicators_. It uses the excellent
# -WDI- package; see the -help- file for that 
# package for more information.
#
# NOTE: This code can take a hot second to
# run, depending on how fast the World Bank's
# API is operating on any given day...
#
# Packages (install as necessary):

library(RCurl)
library(readr)
library(data.table)
library(countrycode)
library(WDI)

# Also, be sure to set a working directory in here
# someplace, a la:
#
# setwd("~/Dropbox (Personal)/PLSC 503")
#
# or whatever.
#######################################################
# Get the relevant data / indicators (add variables
# as you wish...):

wdi<-WDI(country="all",
   indicator=c("LandArea"="AG.LND.TOTL.K2", # Land area (sq. km)
  "ArablePercent"="AG.LND.ARBL.ZS", # Arable Land (% of total land area)
  "Population"="SP.POP.TOTL", # Popluation (in, like, people)
  "PopGrowth"="SP.POP.GROW", # Population Growth (%)
  "RuralPopulation"="SP.RUR.TOTL.ZS", # Rural Population (% of total)
  "UrbanPopulation"="SP.URB.TOTL.IN.ZS", # Urban Population (% of total)
  "BirthRatePer1K"="SP.DYN.CBRT.IN", # Birth Rate (births per 1K people)
  "FertilityRate"="SP.DYN.TFRT.IN", # Fertility Rate (births per woman)
  "PrimarySchoolAge"="SE.PRM.AGES", # Primary school starting age (years)
  "LifeExpectancy"="SP.DYN.LE00.IN", # Life Expectancy at birth (years)
  "AgeDepRatioOld"="SP.POP.DPND.OL", # Age Dependency Ratio (old), % working age population
  "CO2Emissions"="EN.ATM.CO2E.PC", # CO2 Emissions (metric tons per capita)
  "GDP"="NY.GDP.MKTP.KD", # GDP, constant 2010 $US
  "GDPPerCapita"="NY.GDP.PCAP.KD", # GDP per capita (constant 2010 $US)
  "GDPPerCapGrowth"="NY.GDP.PCAP.KD.ZG", # GDP Per Capita Growth (%)
  "Inflation"="FP.CPI.TOTL.ZG", # Inflation (CPI, annual %)
  "TotalTrade"="NE.TRD.GNFS.ZS", # Total trade, % of GDP
  "Exports"="NE.EXP.GNFS.ZS", # Exports, % of GDP
  "Imports"="NE.IMP.GNFS.ZS", # Imports, % of GDP
  "FDIIn"="BX.KLT.DINV.WD.GD.ZS", # FDI in, % of GDP
  "AgriEmployment"="SL.AGR.EMPL.ZS", # Percent of total employment in agriculture
  "NetAidReceived"="DT.ODA.ALLD.KD", # Net official dev. aid received (constant 2018 $US)
  "MobileCellSubscriptions"="IT.CEL.SETS.P2", # Mobile / cellular subscriptions per 100 people
  "NaturalResourceRents"="NY.GDP.TOTL.RT.ZS", # Total natural resource rents (% of GDP)
  "MilitaryExpenditures"="MS.MIL.XPND.GD.ZS", # Military expenditures, % of GDP
  "GovtExpenditures"="NE.CON.GOVT.ZS", # Government Expenditures, % of GDP
  "PublicEdExpend"="SE.XPD.EDUC.ZS", # Public expenditure on education (% of GDP)
  "PublicHealthExpend"="SH.XPD.HLTH.ZS", # Public expenditure on health (% of GDP)
  "HIVDeaths"="SH.DYN.AIDS.DH", # Deaths due to HIV/AIDS (UNAIDS estimate)
  "WomenBusLawIndex"="SG.LAW.INDX", # Women Business & the Law Index Score
  "PaidParentalLeave"="SH.PAR.LEVE.AL")) # Paid Parental Leave (0=no,1=yes)

# Remove aggregated units (e.g., "World," "Arab World," etc.):

wdi$ISO3<-countrycode(wdi$iso2c,origin="iso2c",destination="iso3c")
wdi<-wdi[is.na(wdi$ISO3)==FALSE,]

# Rename Year:

wdi$Year<-wdi$year
wdi$year<-NULL

# Delete ISO2:

wdi$iso2c<-NULL

# Create a "region" variable

wdi$Region<-countrycode(wdi$ISO3,origin="iso3c",destination="region")

# Put ISO3 + Year + Region at the front of the data:

nc<-ncol(wdi)
sb<-seq(nc-2,nc)
se<-seq(1,(nc-3))
wdi<-wdi[,c(sb,se)]
rm(nc,sb,se)

# Output the file:

write.csv(wdi,"Data/WDI-2022.csv",row.names=FALSE) # <- change as needed

# /fin