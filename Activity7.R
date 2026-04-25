# James Maisano

# Installing packages
install.packages(c("dplyr","ggplot2","olsrr","PerformanceAnalytics","lubridate","forecast"))
library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)
library(lubridate)
library(forecast)

# In Class Activity Ch 6 ----
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
# binary variable for alpine region
ghg$AlpineV <- ifelse(ghg$Alpine == "yes",1,0)
# binary variable for known hydropower
ghg$HydroV <- ifelse(ghg$hydropower == "yes",1,0)

# multiple regression
# creates a model object
mod.full <- lm(log.ch4 ~ airTemp+
                 log.age+mean.depth+
                 log.DIP+
                 log.precip+ BorealV, data=ghg) #uses the data argument to specify dataframe

res.full <- rstandard(mod.full)
fit.full <- fitted.values(mod.full)

# qq plot
qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)

# shapiro-wilks test
shapiro.test(res.full)

plot(fit.full,res.full, pch=19, col="grey50")
abline(h=0)

# isolate continuous model variables into data frame:

reg.data <- data.frame(ghg$airTemp,
                       ghg$log.age,ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip)

# make a correlation matrix 
chart.Correlation(reg.data, histogram=TRUE, pch=19)

# run stepwise
full.step <- ols_step_forward_aic(mod.full)
# view table
full.step 

# check full model
full.step$model
# plot AIC over time
plot(full.step )

# prediction with interval for predicting a point
predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="prediction")

# look at prediction with 95% confidence interval of the mean

predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="confidence")

# Activity 7 Questions ----
# Question 1
# See PDF
# Question 2
# See PDF
# Question 3
# Improvement on Regression
mod.improve <- lm(log.ch4 ~ airTemp+surface.area+Residence.Time..days.+
                 log.age+mean.depth+
                 log.DIP+
                 log.precip+ BorealV, data=ghg)
# Analyze regression
summary(mod.improve)

# Checking Assumptions
res.improve <- rstandard(mod.improve)
fit.improve <- fitted.values(mod.improve)
# Normality
# qq plot
qqnorm(res.improve, pch=19, col="grey50")
qqline(res.improve)

# shapiro-wilks test
shapiro.test(res.improve)

# Checking Residuals 
plot(fit.improve,res.improve, pch=19, col="grey50")
abline(h=0)

# Checking multicollinearity
# isolate continuous model variables into data frame:

reg.data.improve <- data.frame(ghg$airTemp,
                       ghg$log.age,ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip,
                       ghg$surface.area,
                       ghg$Residence.Time..days.)

# make a correlation matrix 
chart.Correlation(reg.data.improve, histogram=TRUE, pch=19)

# Question 4
# See PDF
# In class Activity Ch 7 ----
# Reading in the data
ETdat <- read.csv("/cloud/project/activity07/ETdata.csv")

unique(ETdat$crop)

# average fields for each month for almonds
almond <- ETdat %>% # ET data
  filter(crop == "Almonds") %>% # only use almond fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

# visualize the data
ggplot(almond, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# almond ET time series
almond_ts <- ts(almond$ET.in, # data
                start = c(2016,1), #start year 2016, month 1
                #first number is unit of time and second is observations within a unit
                frequency= 12) # frequency of observations in a unit

# decompose almond ET time series
almond_dec <- decompose(almond_ts)
# plot decomposition
plot(almond_dec)

almondTrend <- almond_dec$trend
almondSeason <- almond_dec$seasonal

acf(na.omit(almond_ts), # remove missing data
    lag.max = 24) # look at 2 years (24 months)

pacf.plot <- pacf(na.omit(almond_ts))

almond_y <- na.omit(almond_ts)
model1 <- arima(almond_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model1

model4 <- arima(almond_y , # data 
                order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model4
# calculate fit
AR_fit1 <- almond_y - residuals(model1) 
AR_fit4 <- almond_y - residuals(model4)
#plot data
plot(almond_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")

newAlmond <- forecast(model4)
newAlmond

#make dataframe for plotting
newAlmondF <- data.frame(newAlmond)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newAlmondF$dateF <- ymd(paste(years,"/",month,"/",1))

# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = almond, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(almond$date[1]),newAlmondF$dateF[24])+  # Plotting original data
  geom_line(data = newAlmondF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newAlmondF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

# Homework Questions ----
# Question 1
# Transforming co2 data
co2.trans <- 1/(co2+1000)

# co2 Regression
ghg$co2.trans <- 1 / (ghg$co2 + 1000)
# Runoff 
ghg$log.runoff <- log(ghg$runoff + 1)  
# Precipitation
ghg$log.precip <- log(ghg$precipitation) 
# Age
ghg$log.age    <- log(ghg$age)

# multivariate regression
mod.co2 <- lm(co2.trans ~ log.runoff + log.precip + airTemp + log.age,
              data = ghg)
summary(mod.co2)

# checking the regression model fit assumptions 
res.co2 <- rstandard(mod.co2)
fit.co2 <- fitted.values(mod.co2)

# Normality (qq plot)
par(mfrow = c(1, 3))

qqnorm(res.co2, pch = 19, col = "grey50")
qqline(res.co2)

# Shapiro Wilks test
shapiro.test(res.co2)

# checking resdiuals
plot(fit.co2,res.co2, pch=19, col="grey50")
abline(h=0)

# Checking multicollinearity
reg.data.co2 <- data.frame(
  Log.Runoff  = ghg$log.runoff,
  Log.Precip  = ghg$log.precip,
  AirTemp     = ghg$airTemp,
  Log.Age     = ghg$log.age
)
chart.Correlation(reg.data.co2, histogram = TRUE, pch = 19)

# Question 2
# See PDF

# Question 3
# average each crop to help decompose
# almond (already done)
almond <- ETdat %>%
  filter(crop == "Almonds") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))

almond_ts <- ts(almond$ET.in, start = c(2016,1), frequency = 12)
almond_dec <- decompose(almond_ts)
plot(almond_dec)

# Pistachios
pist <- ETdat %>%
  filter(crop == "Pistachios") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))

pist_ts <- ts(pist$ET.in, start = c(2016,1), frequency = 12)
pist_dec <- decompose(pist_ts)
plot(pist_dec)

# Idle
fallow <- ETdat %>%
  filter(crop == "Fallow/Idle Cropland") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))

fallow_ts <- ts(fallow$ET.in, start = c(2016,1), frequency = 12)
fallow_dec <- decompose(fallow_ts)
plot(fallow_dec)

# corn
corn <- ETdat %>%
  filter(crop == "Corn") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))

corn_ts <- ts(corn$ET.in, start = c(2016,1), frequency = 12)
corn_dec <- decompose(corn_ts)
plot(corn_dec)
# combining all 5 crops in one graph
crops_5 <- c("Almonds","Pistachios","Fallow/Idle Cropland",
             "Corn","Grapes (Table/Raisin)")
# All cropps 
ET_all <- ETdat %>%
  filter(crop %in% crops_5) %>%
  group_by(date, crop) %>%
  summarise(ET.mean = mean(Ensemble.ET, na.rm = TRUE), .groups = "drop") %>%
  mutate(date = ymd(date))

ggplot(ET_all, aes(x = date, y = ET.mean, color = crop)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c("Almonds"               = "brown",
                                "Pistachios"            = "olivedrab4",
                                "Fallow/Idle Cropland"  = "grey50",
                                "Corn"                  = "goldenrod2",
                                "Grapes (Table/Raisin)" = "mediumpurple3")) +
  labs(title = "Monthly Evapotranspiration by Crop Type (2016–2021)", x= "Date", y = "Evapotranspiration (mm/day)", color  = "Crop") + theme_classic(base_size = 12) + theme(legend.position = "bottom")

# Question 4
# Pistachio
# Already done copying for readability
pist <- ETdat %>%
  filter(crop == "Pistachios") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))

ggplot(pist, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")
# pistachio ET time series
pist_ts <- ts(pist$ET.in,
              start = c(2016,1),
              frequency= 12)
# decompose pistachio ET time series
pist_dec <- decompose(pist_ts)
# plot decomposition
plot(pist_dec)

# plot PACF to help identify AR order
pacf.plot <- pacf(na.omit(pist_ts))

# remove missing data
pist_y <- na.omit(pist_ts)

# fit AR(1) model
model1 <- arima(pist_y,
                order = c(1,0,0))
model1

# fit AR(4) model
model4 <- arima(pist_y,
                order = c(4,0,0))
model4

# calculate fit for both models
AR_fit1 <- pist_y - residuals(model1)
AR_fit4 <- pist_y - residuals(model4)
# plot data and model fits to compare AR(1) and AR(4)
plot(pist_y)
points(AR_fit1, type = "l", col = "darkgreen", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "lightgreen", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2),
       col=c("darkgreen", "green4","lightgreen"),
       bty="n")

# forecast future ET using AR(4) model
newPist <- forecast(model4)
newPist
# make dataframe for plotting
newPistF <- data.frame(newPist)

# set up dates for forecast period
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newPistF$dateF <- ymd(paste(years,"/",month,"/",1))

# plot historical data and forecast with 95% prediction interval
ggplot() +
  geom_line(data = pist, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(pist$date[1]),newPistF$dateF[24])+
  geom_line(data = newPistF, aes(x = dateF, y = Point.Forecast),
            col="green4") +
  geom_ribbon(data=newPistF,
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

# Fallow Cropland

# filter data for fallow/idle cropland and average fields for each month
fallow <- ETdat %>%
  filter(crop == "Fallow/Idle Cropland") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))

# visualize the data
ggplot(fallow, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# fallow ET time series
fallow_ts <- ts(fallow$ET.in,
                start = c(2016,1),
                frequency= 12)

# decompose fallow ET time series
fallow_dec <- decompose(fallow_ts)
# plot decomposition
plot(fallow_dec)

# plot ACF to examine autocorrelation structure
acf(na.omit(fallow_ts),
    lag.max = 24)

# plot PACF to help identify AR order
pacf.plot <- pacf(na.omit(fallow_ts))

# remove missing data
fallow_y <- na.omit(fallow_ts)
# fit AR(1) model
model1 <- arima(fallow_y,
                order = c(1,0,0))
model1

# fit AR(4) model
model4 <- arima(fallow_y,
                order = c(4,0,0))
model4

# calculate fit for both models
AR_fit1 <- fallow_y - residuals(model1)
AR_fit4 <- fallow_y - residuals(model4)
# plot data and model fits to compare AR(1) and AR(4)
plot(fallow_y)
points(AR_fit1, type = "l", col = "grey", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgrey", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2),
       col=c("grey", "lightgrey","darkgrey"),
       bty="n")

# forecast future ET using AR(4) model
newFallow <- forecast(model4)
newFallow

# make dataframe for plotting
newFallowF <- data.frame(newFallow)
# set up dates for forecast period
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newFallowF$dateF <- ymd(paste(years,"/",month,"/",1))

# plot historical data and forecast with 95% prediction interval
ggplot() +
  geom_line(data = fallow, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(fallow$date[1]),newFallowF$dateF[24])+
  geom_line(data = newFallowF, aes(x = dateF, y = Point.Forecast),
            col="grey") +
  geom_ribbon(data=newFallowF,
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")