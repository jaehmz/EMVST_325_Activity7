# James Maisano

# Installing packages
install.packages(c("dplyr","ggplot2","olsrr","PerformanceAnalytics"))
library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)

# Reading in activity 7 data
ghg <- read.csv("/cloud/project/activity07/Deemer_GHG_Data.csv")

# log transform methane fluxes
ghg$log.ch4 <- log(ghg$ch4+1)
# log transform age
ghg$log.age <- log(ghg$age)
# log transform DIP
ghg$log.DIP <- log(ghg$DIP+1)
# log transform percipitation
ghg$log.precip <- log(ghg$precipitation)

# printing out unique regions
unique(ghg$Region)

# binary variable for boreal region
ghg$BorealV <- ifelse(ghg$Region == "Boreal",1,0)
# binary variable for tropical region
ghg$TropicalV <- ifelse(ghg$Region == "Tropical",1,0)
