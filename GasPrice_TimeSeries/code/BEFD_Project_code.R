#  Explaining Gas Price Dynamics in Italy and Turkey

#  Università di Padova
#  Department of Mathematics
#  Business, Economic and Financial Data Project

#  Iaroslav Tkachenko - badge nr. 2141893
#  Degirmenci Kubra Deniz - badge nr. 2145075
#  Lazzari Tommaso - badge nr. 2159017
############################################################################################

#to properly run the following code, be sure to set the directory 
#to .../BEFD_project/BEFD_Project_data

#1.     Libraries
############################################################################################
library(readxl)
library(lmtest) 
library(forecast)
library(fpp2) 
library(gam)
library(corrplot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(GGally)
#library(fpp2) links to the book 
#"Forecasting: Principles and Practice" by Rob J. Hyndman and George Athanasopoulos, 
#containing all the functions and the datasets used for the practical 
#examples and exercises in the book
############################################################################################





#2.     Data
############################################################################################

###############2.1. GAS PRICES
GAS <- read_excel("nrg_pc_202__custom_18451811_page_spreadsheet.xlsx", sheet="Sheet 1", range= "A12:AL49", col_names=TRUE, na=":")
GAS<- t(GAS[2:37,])
colnames(GAS) <- GAS[1,]
GAS <- GAS[-1,]
GAS <- ts(GAS, start=c(2007,1), frequency=2)
#the variable GAS is now a multivariate bi-annual time series
#source: Eurostat, last update 15/10/2025
#Gas prices for household consumers - bi-annual data (from 2007 onwards)
#Product: Natural GAS
#Energy Consumptions: Consumption from 20 GJ to 199 GJ - band D2
#Unit of measure: Kilowatt-hour
#Taxes: Excluding taxes and levies
#Currency: Euro
#Start = 1st semester 2007
#End = 1st semester 2025
#Frequency = 2
#24 timestamps
#36 countries: 
#   01. European Union - 27 countries (from 2020)
#   02. Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015, EA20-2023)
#   03. Belgium
#   04. Bulgaria
#   05. Czechia
#   06. Denmark
#   07. Germany
#   08. Estonia
#   09. Ireland
#   10. Greece
#   11. Spain
#   12. France
#   13. Croatia
#   14. Italy
#   15. Latvia
#   16. Lithuania
#   17. Luxembourg
#   18. Hungary
#   19. Netherlands
#   20. Austria
#   21. Poland
#   22. Portugal
#   23. Romania
#   24. Slovenia
#   25. Slovakia
#   26. Sweden
#   27. Liechtenstein
#   28. United Kingdom
#   29. Bosnia and Herzegovina
#   30. Moldova
#   31. North Macedonia
#   32. Georgia
#   33. Albania
#   34. Serbia
#   35. Türkiye
#   36. Ukraine

###############2.2. TEMPERATURE ANOMALIES
temp_anomalies_data <- read.csv("temperature_anomalies_EU.csv", header = FALSE, sep=";")
#Source: National Centers for Environmental Information, NCEI, https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/global/time-series
#Query:
#   Region: Europe
#   Surface: Land
#   Parameter: Average Temperature Anomaly
#   Time Scale: 1-Month
#   Month: All months
#   Start Year: 2007
#   End Year: 2025
temp_anomalies <- temp_anomalies_data[5:226, 2] # Extract time series of temperature anomalies
temp_anomalies <- as.numeric(temp_anomalies)
groups <- ceiling(seq_along(temp_anomalies) / 6) # Biannual averages
biannual_temp_anomalies <- tapply(temp_anomalies, groups, mean, na.rm = TRUE)
biannual_temp_anomalies <- ts(as.numeric(biannual_temp_anomalies), frequency=2, start=c(2007,1))
#The variable biannual_temp_anomalies is now a bi-annual time series matching length, starting and ending dates
#of the GAS time series

###############2.3. BRENT 
#The term BRENT refers to a type of crude oil extracted mainly in the North Sea.
#It is one of the most important benchmarks for determining global oil prices
#and is used as a reference for about two-thirds of the world's oil transactions
#Ga prices are historically correlated with oil due to long-term indexation
BRENT_data <- read_excel("BRENT_price.xls", sheet = 2,range = "B4:B465",col_names = FALSE)
#Source: U.S. Energy Information Administration (EIA)
#Source key: RBRTE
#Dataset: Europe Brent Spot Price (FOB)
#Release Date: November 19, 2025
#Unit of measure: Dollars per barrel
#Start Date: May 1987
#End Date: Oct 2025
#Frequency: Monthly
BRENT <- as.numeric(BRENT_data[[1]]) # Extract time series of BRENT prices
BRENT <- BRENT[237:458] #we only keep the information corresponding to the same interval of our GAS prices time series: january 2007, june 2025
groups <- ceiling(seq_along(BRENT) / 6) #Biannual averages
biannual_BRENT <- tapply(BRENT, groups, mean, na.rm=TRUE)
biannual_BRENT <- ts(as.numeric(biannual_BRENT), frequency=2, start=c(2007,1))
#The variable biannual_temp_BRENT is now a bi-annual time series matching length, starting and ending dates
#of the GAS time series

###############2.4. HENRYHUB
#The HENRY hub natural gas spot price is a benchmark for North American natural gas prices.
#This price is determined by the supply and demand for natural gas at a major pipeline hub in Erath, Louisiana, 
#and is used for spot market trades of natural gas for immediate delivery.
HENRYHUB_data <- read_excel("HenryHub_price.xls", sheet = 2,range = "B4:B349",col_names = FALSE)
#Source: U.S. Energy Information Administration (EIA)
#Dataset: Henry Hub Natural Gas Spot Price 
#Release Date: November 19, 2025
#Unit of measure: Dollars per Milion Btu
#Start Date: Jan 1997
#End Date: Oct 2025
#Frequency: Monthly
HENRYHUB <- as.numeric(HENRYHUB_data[[1]]) # Extract time series of BRENT prices
HENRYHUB <- HENRYHUB[121:342] #we only keep the information corresponding to the same interval of our GAS prices time series: january 2007, june 2025
groups <- ceiling(seq_along(HENRYHUB) / 6) #Biannual averages
biannual_HENRYHUB <- tapply(HENRYHUB, groups, mean, na.rm=TRUE)
biannual_HENRYHUB <- ts(as.numeric(biannual_HENRYHUB), frequency=2, start=c(2007,1))

############################################################################################





#3.     Preliminary Analysis
############################################################################################
#subset of the data, only containing Italy's GAS prices for household consumers
ITA_GAS <- ts(as.numeric(GAS[,"Italy"]), frequency=2, start=c(2007,1))
plot(ITA_GAS, ylab="Euro", main="Gas price - Italy")
#only looking at the time series plot we can say that the trend can be easily
#divided into two main components: a first one stationary that goes up to 2021
#and an extreme positive shock followed by what seems to be the start of a decreasing trend
acf(as.numeric(ITA_GAS), main = "GAS prices - Italy")
#Statistically significant autocorrelation at lags 1, 2, 3 and 4
#The autocorrelation function is decreasing
#The decrease is non-linear: autocorrelations at lags 2 and 4 are higher than they would be if the decrease was linear

#subset of the data, only containing Turkey's GAS prices for household consumers
#   !!!the first lement of this ts is NA!!!
TUR_GAS <- ts(as.numeric(GAS[,"Türkiye"]), frequency=2, start=c(2007,1))
plot(TUR_GAS, ylab="Euro", main="Gas prices - Türkiye")
#by the plot we can see a strongly decreasing trend with high volatility, highlighted by great peaks 
#along all the life of the time series
acf(as.numeric(TUR_GAS[-1]), main = "GAS prices - Türkiye")
#Statistically significant autocorrelation at lags 1, 2, 3, 4, 5 and 6
#The autocorrelation function is decreasing

autoplot(GAS[,c("Italy", "Türkiye")], ylab="Euro", main="Gas prices")

covariates <- data.frame(ITA_GAS[2:37], TUR_GAS[2:37], biannual_BRENT[2:37], biannual_HENRYHUB[2:37], biannual_temp_anomalies[2:37])
colnames(covariates) <- c("ITA gas price", "TUR gas price", "Brent", "HenryHub", "T anomalies")
corrplot(cor(covariates))
cor(covariates)
#for what concerns the Italian gas prices time series we notice
#an high positive correlation with the temperature anomalies time series
#a negative correlation with the Turkish gas prices time series
#and a slightly positive correlation with the Brent prices time series

#about the Turkish gas prices time series we can see
#a high negative correlation with the temperature anomalies time series (of the same magnitude of the italian's correlation, but of opposite sign)
#a positive correlation with HernyHub and Brent prices
############################################################################################





#4.     Linear Model
############################################################################################

###############4.1. ITA

#4.1.1. time-features approach
#we first start from time-derived features, such as trend and seasonality
plot(ITA_GAS, ylab="Euro", main = "Gas prices - Italy")
fit_linear_GAS_ITA <- tslm(ITA_GAS ~ trend + season)
summary(fit_linear_GAS_ITA) #Adjusted R_squared : 0.5051
lines(fitted(fit_linear_GAS_ITA), col=2)

#or, to take into account the shock that can be seen from the time series plot 
#that starts the first semester of 2021
trend <- seq(1:length(ITA_GAS))
seasonal_dummy <-rep(c(0,1),19)[1:37]
shock_dummy_ITA_GAS <- c(rep(0,30), rep(1,7))
ITA_GAS_linear_time <- lm(ITA_GAS ~ trend + seasonal_dummy + shock_dummy_ITA_GAS)
summary(ITA_GAS_linear_time) #the trend is not even significant #Adjusted R_squared : 0.8397
#we can try to divide it into two parts, to try to capture the behaviour described in the preliminary analysis
trend1_ITA_GAS <- c(seq(1:30),rep(0,7))
trend2_ITA_GAS <- c(rep(0,31),c(32:37))
ITA_GAS_linear_time <- lm(ITA_GAS ~  trend1_ITA_GAS + trend2_ITA_GAS +seasonal_dummy + shock_dummy_ITA_GAS)
summary(ITA_GAS_linear_time) #in this case they become slighlty significant, with a p-value close to 0.05 #Adjusted R_squared : 0.8538
AIC(ITA_GAS_linear_time) #-235.5184

#the model overall has great explanatory performances, obtaining an adjusted R_squared = 0.8538 and a AIC = -235.5184
#but as we can see from the acf functions
#the residuals are not indipendent (breaking the assumptions of the linear model)
tsdisplay(residuals(ITA_GAS_linear_time), main="Italy - Gas prices, linear model with time-derived features residuals") 

#4.1.2.
#now, we try to model the gas prices on the basis of other external covariates
#first, we try a linear model with just the covariate with higher correlation
ITA_GAS_linear_cov <- lm(ITA_GAS ~ biannual_temp_anomalies)
summary(ITA_GAS_linear_cov) #Adjusted R squared : 0.2798
AIC(ITA_GAS_linear_cov) # -179.1954
#let's try to add another covariate
ITA_GAS_linear_cov <- lm(ITA_GAS ~ biannual_temp_anomalies + biannual_BRENT)
summary(ITA_GAS_linear_cov) #Adjusted R squared : 0.3719
AIC(ITA_GAS_linear_cov) # -183.3296
#we remove the intercept which is not significant
ITA_GAS_linear_cov <- lm(ITA_GAS ~ -1 +biannual_temp_anomalies + biannual_BRENT)
summary(ITA_GAS_linear_cov) #Adjusted R squared : 0.9174
AIC(ITA_GAS_linear_cov) #AIC -184.8352
#we try to add the shock dummy variable
ITA_GAS_linear_cov <- lm(ITA_GAS ~ -1 +biannual_temp_anomalies + biannual_BRENT + shock_dummy_ITA_GAS)
summary(ITA_GAS_linear_cov) #Adjusted R squared : 0.957
AIC(ITA_GAS_linear_cov) #AIC -208.0825

#once more, the acf functions display some slighlty significant lags in the residuals
#time series, which break the linear model assumptions
tsdisplay(residuals(ITA_GAS_linear_cov), main = "Italy - Gas prices, linear model with external features residuals") 

{
  plot(ITA_GAS, ylab = "Euro", main = "ITALY - Gas price", lwd=2)
  lines(ts(fitted(ITA_GAS_linear_time), frequency = 2, start = c(2007, 1)), 
        col = 2, lty = "dotdash", lwd = 2)
  lines(ts(fitted(ITA_GAS_linear_cov), frequency = 2, start = c(2007, 1)), 
        col = 4, lty = "dotdash", lwd = 2)
  legend("topleft",
         legend = c("Linear model with time-derived features",
                    "Linear model with external features"),
         col = c(2, 4),
         lty = "dotdash",
         lwd = 2,
         bty = "n")
}



###############4.2.TUR
TUR_GAS <- ts(TUR_GAS[2:37], frequency=2, start=c(2007,2), end=c(2025,1))
plot(TUR_GAS, ylab="Euro", main="Turkey - Gas prices")

#4.2.1. time-features approach
#we first start from time derived features, such as trend, shocks and seasonality
fit_linear_GAS_TUR <- tslm(TUR_GAS ~ trend + season)
summary(fit_linear_GAS_TUR) #Adjusted R_squared : 0.5679
AIC(fit_linear_GAS_TUR) #AIC: -285.5279
lines(fitted(fit_linear_GAS_TUR), col=2)

#or, to take into account the shock that can be seen from the time series plot 
#from 2012 to 2016
shock_dummy_TUR_GAS <- c(rep(0,10),c(11:18), rep(0,18))
trend_TUR <- trend[2:37]
seasonal_dummy_TUR <- seasonal_dummy[2:37]
TUR_GAS_linear_time <- lm(TUR_GAS ~ trend_TUR + seasonal_dummy_TUR + shock_dummy_TUR_GAS)
summary(TUR_GAS_linear_time) #the seasonal dummy is not significant; Adjusted R_squared : 0.7395
AIC(TUR_GAS_linear_time) #AIC: -302.8568
#we remove the non-significant seasonal dummy
TUR_GAS_linear_time <- lm(TUR_GAS ~ trend_TUR + shock_dummy_TUR_GAS)
summary(TUR_GAS_linear_time) #Adjusted R_squared : 0.7398
AIC(TUR_GAS_linear_time) #AIC: -303.7896

#4.2.2.now, we try to add other external covariates
#let's add the most correlated covariate to the Turkish gas prices, given by the correlation matrix in the preliminary analysis
TUR_GAS_linear_cov <- lm(TUR_GAS ~ trend_TUR + shock_dummy_TUR_GAS+ biannual_temp_anomalies[2:37])
summary(TUR_GAS_linear_cov) #Adjusted R_squared : 0.7414
AIC(TUR_GAS_linear_cov) #AIC: -303.1095

plot(TUR_GAS, ylab="Euro", main="TURKEY - Gas price", lwd=2)
lines(ts(fitted(TUR_GAS_linear_cov), frequency=2, start=c(2007,2), end=c(2025,1)), col=3, lwd=2, lty="dotdash")

tsdisplay(residuals(TUR_GAS_linear_cov), main = "Turkey - Gas prices, linear model residuals") 
#one more, the linear model assumptions are not satisfied: from the ACF functions 
#we can see that the residuals of the linear model are not independent
############################################################################################





#5.     Arima
############################################################################################
###############5.1. ITA
tsdisplay(ITA_GAS, main="Italy - Gas prices")
#from the ts plot, we do not see a clear trend, moreover
#we have a sinusoidal behaviour in the PACF and a high peak in the ACF for the first (non seasonal) lag
#moreover we can see that the first and the second seasonal lags in the ACF are significant.

#Given the behaviour of the ACF and the PACF for what concerns the non-seasonal components
#that shows a sinusoidal behaviour in the PACF and one high peak in the ACF, we start modelling
#an ARIMA(0,0,1)
fit_arima1_ITA_GAS <- Arima(ITA_GAS, order=c(0,0,1))
summary(fit_arima1_ITA_GAS) #AIC -176.2
res_arima1_ITA_GAS <- residuals(fit_arima1_ITA_GAS)
tsdisplay(res_arima1_ITA_GAS, main="Italy - Gas prices, ARIMA(0,0,1) residuals")
#we now can see that the non-seasonal lags are all non significant
#for what concerns the seasonal component, we have that in the ACF we have an exponential decrease in the seasonal lags
#and in the PACF we have a peak at the first seasonal lag

#to model the seasonal component. First of all we differenciate
fit_arima2_ITA_GAS <- Arima(ITA_GAS, order=c(0,0,1), seasonal=c(0,1,0))
summary(fit_arima2_ITA_GAS) #AIC -208.23
res_arima2_ITA_GAS <- residuals(fit_arima2_ITA_GAS)
tsdisplay(res_arima2_ITA_GAS, main="Italy - Gas prices, SARIMA(0,0,1)(0,1,0)_2 residuals") 
#the autocorrelation is non significant at all lags
#for both the seasonal and the non-seasonal component

#ITA_GAS -> SARIMA(0,0,1)(0,1,0)_2
plot(ITA_GAS, ylab="Euro", main="ITALY - Gas prices, SARIMA(0,0,1)(0,1,0)_2")
lines(ts(fitted(fit_arima2_ITA_GAS), frequency=2, start=c(2007,1), end=c(2025,1)), col=5, lty="dotdash", lwd=2)


###############5.2. TUR
tsdisplay(TUR_GAS, main="Turkey - Gas prices")
#we can see a strong decreasing trend and
#we have an exponential decay in the ACF for both the seasonal and the non seasonal lags
#and one strong peak in the PACF in correspondance of the first lag

#Given this information, we startdifferentitating the time series
fit_arima1_TUR_GAS <-Arima(TUR_GAS, order=c(0,1,0))
summary(fit_arima1_TUR_GAS) #AIC : -280.99
res_arima1_TUR_GAS <- residuals(fit_arima1_TUR_GAS)
tsdisplay(res_arima1_TUR_GAS, main="Turkey - Gas prices, ARIMA(0,1,0) residuals")
#we can now see that in both the ACF and the PACF the only signifcant lags are the first seasonal

fit_arima2_TUR_GAS <-Arima(TUR_GAS, order=c(0,1,0), seasonal=c(0,0,1))
summary(fit_arima2_TUR_GAS) #AIC : -285.02
res_arima2_TUR_GAS <- residuals(fit_arima2_TUR_GAS)
tsdisplay(res_arima2_TUR_GAS, main="Turkey - Gas prices, ARIMA(0,1,0)(0,0,1)_2 residuals") #the autocorrelation is non significant at all lags
#for both the seasonal and the non-seasonal component

#just to be sure, since the ACF and the PACF of the residuals of the first model, were
#showing that in both the ACF and the PACF the only signifcant lags are the first seasonal
#we also try 
fit_arima3_TUR_GAS <-Arima(TUR_GAS, order=c(0,1,0), seasonal=c(1,0,0))
summary(fit_arima3_TUR_GAS) #AIC : -287.11
res_arima3_TUR_GAS <- residuals(fit_arima3_TUR_GAS)
tsdisplay(res_arima3_TUR_GAS, main="Turkey - Gas prices, SARIMA(0,1,0)(1,0,0)_2 residuals)") #even in this case the autocorrelation is non significant at all lags
#for both the seasonal and the non-seasonal component
#but, if we look at the AIC, we can see that this third model is the best one in terms of the information criterion

#TUR_GAS -> SARIMA(0,1,0)(1,0,0)_2
plot(TUR_GAS, ylab="Euro", main="TURKEY - Gas prices, SARIMA(0,1,0)(1,0,0)_2", lwd=2)
lines(ts(fitted(fit_arima3_TUR_GAS), frequency=2, start=c(2007,2), end=c(2025,1)), col=6, lwd=2, lty="dotdash")
############################################################################################





#6.     ARIMAX
############################################################################################
###############6.1. ITA
#we can start from the linear model previosuly estimated (external regressors)
summary(ITA_GAS_linear_cov)
cov_df_ITA = as.matrix(data.frame(biannual_temp_anomalies, biannual_BRENT, shock_dummy_ITA_GAS))
ITA_GAS_armax_cov <- Arima(ITA_GAS, xreg=cov_df_ITA, include.mean=FALSE)
summary(ITA_GAS_armax_cov) #AIC = -208.08
tsdisplay(residuals(ITA_GAS_armax_cov), main="Italy - Gas prices, linear model residuals")
#we can see from the plots that
#the ACF has the first and the fifth lags which are significant
#the PACF has significant peak only at lag 1
ITA_GAS_armax_cov <- Arima(ITA_GAS, xreg=cov_df_ITA, order=c(1,0,0), include.mean=FALSE)
summary(ITA_GAS_armax_cov) #AIC = -213.85
tsdisplay(residuals(ITA_GAS_armax_cov), main="Italy - Gas prices, ARIMAX residuals")

plot(ITA_GAS, ylab="Euro", main="ITALY - Gas prices, ARIMAX")
lines(ts(fitted(ITA_GAS_armax_cov), frequency=2, start=c(2007,1), end=c(2025,1)), col=7, lty="dotdash", lwd=2)

#or the other one (time-derived features)
summary(ITA_GAS_linear_time)
cov_df_ITA <- as.matrix(data.frame(trend1_ITA_GAS, trend2_ITA_GAS, seasonal_dummy, shock_dummy_ITA_GAS))
ITA_GAS_armax_time <- Arima(ITA_GAS, xreg=cov_df_ITA)
summary(ITA_GAS_armax_time) #AIC = -235.52
tsdisplay(residuals(ITA_GAS_armax_time), main="Italy - Gas prices, linear model residuals")

ITA_GAS_armax_time <- Arima(ITA_GAS, xreg=cov_df_ITA, seasonal=c(1,0,0))
summary(ITA_GAS_armax_time) #AIC = -241.45
tsdisplay(residuals(ITA_GAS_armax_time)) #white noise

plot(ITA_GAS, ylab="Euro", main="ITALY - Gas prices, ARIMAX")
lines(ts(fitted(ITA_GAS_armax_time), frequency=2, start=c(2007,1), end=c(2025,1)), col=13, lty="dotdash", lwd=2)

###############6.2. TUR
#as done for italian gas prices time series, we start from the last linear model estimated
summary(TUR_GAS_linear_cov)
cov_df_TUR <- as.matrix(data.frame(trend_TUR, shock_dummy_TUR_GAS, biannual_temp_anomalies[2:37]))
TUR_GAS_armax_cov <- Arima(TUR_GAS, xreg=cov_df_TUR)
summary(TUR_GAS_armax_cov) #AIC = -303.11
tsdisplay(residuals(TUR_GAS_armax_cov), main="Turkey - Gas prices, linear model residuals")
#we can see from the plots that
#for both the ACF and the PACF we can see that only the first seasonal lag is significant
TUR_GAS_armax_cov <- Arima(TUR_GAS, xreg=cov_df_TUR, seasonal=c(1,0,0))
summary(TUR_GAS_armax_cov) #AIC = -308.98
tsdisplay(residuals(TUR_GAS_armax_cov), main="Turkey - Gas prices, ARIMAX residuals") #white noise

plot(TUR_GAS, ylab="Euro", main="TURKEY - Gas prices, ARIMAX", lwd=2)
lines(ts(fitted(TUR_GAS_armax_cov), frequency=2, start=c(2007,2), end=c(2025,1)), col=10, lwd=2, lty="dotdash")
############################################################################################





#7.     GAM
############################################################################################
###############7.1. ITA
ITA_GAS_gam_time <- gam(ITA_GAS~ s(trend1_ITA_GAS) + s(trend2_ITA_GAS) + seasonal_dummy + shock_dummy_ITA_GAS)
summary(ITA_GAS_gam_time)
AIC(ITA_GAS_gam_time) #AIC: -255.3992
#the linear model with same covariates has
AIC(ITA_GAS_linear_time) #AIC: -235.5184
ITA_GAS_gam_time <- gam(ITA_GAS~ lo(trend1_ITA_GAS) + s(trend2_ITA_GAS) + seasonal_dummy + shock_dummy_ITA_GAS)
AIC(ITA_GAS_gam_time) #AIC = -256.9777
ITA_GAS_gam_time <- gam(ITA_GAS~ lo(trend1_ITA_GAS) + lo(trend2_ITA_GAS) + seasonal_dummy + shock_dummy_ITA_GAS)
AIC(ITA_GAS_gam_time) #AIC = -261.9375

plot(ITA_GAS, ylab="Euro", main="ITALY - Gas prices, GAM")
lines(ts(fitted(ITA_GAS_gam_time), frequency=2, start=c(2007,1), end=c(2025,1)), col=14, lty="dotdash", lwd=2)
#let's try and see if the residuals of this model have some structure and eventually how to manage it
res_ITA_GAS_gam_time <- residuals(ITA_GAS_gam_time)
tsdisplay(res_ITA_GAS_gam_time, main="ITALY - Gas prices, GAM model residuals") #white noise

###############7.2. TUR
summary(TUR_GAS_linear_cov)
TUR_GAS_gam_cov <- gam(TUR_GAS ~ s(trend_TUR) + shock_dummy_TUR_GAS + s(biannual_temp_anomalies[2:37])) 
AIC(TUR_GAS_gam_cov) #AIC = -299.2067
#the linear model with same covariates has
AIC(TUR_GAS_linear_cov) #AIC = -303.1095
TUR_GAS_gam_cov <- gam(TUR_GAS ~ lo(trend_TUR) + shock_dummy_TUR_GAS + s(biannual_temp_anomalies[2:37])) 
AIC(TUR_GAS_gam_cov) #AIC = -299.5006
TUR_GAS_gam_cov <- gam(TUR_GAS ~ lo(trend_TUR) + shock_dummy_TUR_GAS + lo(biannual_temp_anomalies[2:37])) 
AIC(TUR_GAS_gam_cov) #AIC = -298.5139
#we are not getting better results with respect to the linear model, both the original linear model and the arimax with same covariates
#had better AIC values
############################################################################################





# 8.     Model comparison tables
#######################################################################

#MSE of fitted model function
get_mse <- function(model) {
  mean(residuals(model)^2, na.rm = TRUE)
}

#R2 of fitted model function
get_r2 <- function(model, y) {
  y <- as.numeric(y)
  fitted_vals <- as.numeric(fitted(model))
  
  ss_res <- sum((y - fitted_vals)^2)
  ss_tot <- sum((y - mean(y))^2)
  
  r2 <- 1 - ss_res/ss_tot
  return(r2)
}

#Adjusted R2 function
get_adj_r2 <- function(model, y, k) {
  # k = number of estimated parameters
  n <- length(y)
  r2 <- get_r2(model, y)
  adj <- 1 - (1 - r2) * (n - 1) / (n - k - 1)
  return(adj)
}

###############8.1. ITA
ITA_models_list <- list(
  "linear model (1)" = ITA_GAS_linear_time,
  "linear model (2)" = ITA_GAS_linear_cov,
  "SARIMA(0,0,1)(0,1,0)_2" = fit_arima2_ITA_GAS,
  "linear model (2) + ARMA(1,0)" = ITA_GAS_armax_cov,
  "linear model (1) + SARIMA(0,0,0)(1,0,0)_2" = ITA_GAS_armax_time,
  "GAM" = ITA_GAS_gam_time
)

ITA_results <- data.frame(
  Model = names(ITA_models_list),
  AIC = NA, MSE = NA, R2 = NA, Adj_R2 = NA
)

for (i in seq_along(ITA_models_list)) {
  mod <- ITA_models_list[[i]]
  ITA_results$AIC[i] <- AIC(mod)
  ITA_results$MSE[i] <- get_mse(mod)
  ITA_results$R2[i]  <- get_r2(mod, ITA_GAS)
  # Count effective parameters (approximate: #coef)
  k <- length(coef(mod))
  ITA_results$Adj_R2[i] <- get_adj_r2(mod, ITA_GAS, k)
}

print(ITA_results)


###############8.2. TUR
TUR_models_list <- list(
  "linear model" = TUR_GAS_linear_cov,
  "SARIMA(0,1,0)(1,0,0)_2" = fit_arima3_TUR_GAS,
  "linear model + SARIMA(0,0,0)(1,0,0)_2" = TUR_GAS_armax_cov,
  "GAM" = TUR_GAS_gam_cov
)

TUR_results <- data.frame(
  Model = names(TUR_models_list),
  AIC = NA, MSE = NA, R2 = NA, Adj_R2 = NA
)

for (i in seq_along(TUR_models_list)) {
  mod <- TUR_models_list[[i]]
  TUR_results$AIC[i] <- AIC(mod)
  TUR_results$MSE[i] <- get_mse(mod)
  TUR_results$R2[i]  <- get_r2(mod, TUR_GAS)
  k <- length(coef(mod))
  TUR_results$Adj_R2[i] <- get_adj_r2(mod, TUR_GAS, k)
}

print(TUR_results)
############################################################################################


#9.     Plots
############################################################################################

#correlogram time series and covariates
cov_df <- data.frame(
  ITA = as.numeric(ITA_GAS[2:37]),
  TUR = as.numeric(TUR_GAS[2:37]),
  Brent = as.numeric(biannual_BRENT[2:37]),
  HenryHub = as.numeric(biannual_HENRYHUB[2:37]),
  Temp = as.numeric(biannual_temp_anomalies[2:37])
)

ggpairs(cov_df,
        title = "Scatterplot Matrix of Gas Prices and Covariates")

#ITA - fitted linear model 1
df_plot <- data.frame(
  time = time(ITA_GAS),
  ITA = as.numeric(ITA_GAS),
  Fitted = as.numeric(fitted(ITA_GAS_linear_time))
)

ggplot(df_plot, aes(x = time)) +
  geom_line(aes(y = ITA, color = "Observed"), size = 1.2) +
  geom_line(aes(y = Fitted, color = "Fitted"), 
            size = 1.1, linetype = "dotdash") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Linear Model with Time-Derived Features – Italy Gas Prices",
    x = "Year",
    y = "Euro",
    color = NULL
  ) +
  scale_color_manual(values = c(
    "Observed" = "#1b4f72",   # Blue
    "Fitted"   = "#d35400"    # Red/Orange
  )) +
  scale_x_continuous(breaks = seq(2007, 2025, 2)) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "top",
    legend.text = element_text(size = 12)
  )

#ITA - fitted linear model 2
df_plot2 <- data.frame(
  time = time(ITA_GAS),
  ITA = as.numeric(ITA_GAS),
  Fitted = as.numeric(fitted(ITA_GAS_linear_cov))
)

ggplot(df_plot2, aes(x = time)) +
  geom_line(aes(y = ITA, color = "Observed"), size = 1.2) +
  geom_line(aes(y = Fitted, color = "Fitted"),
            size = 1.1, linetype = "dotdash") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Linear Model with External Features – Italy Gas Prices",
    x = "Year",
    y = "Euro",
    color = NULL
  ) +
  scale_color_manual(values = c(
    "Observed" = "#1b4f72",   
    "Fitted"   = "#d35400"    
  )) +
  scale_x_continuous(breaks = seq(2007, 2025, 2)) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "top",
    legend.text = element_text(size = 12)
  )

#TUR - fitted linear model
df_plot3 <- data.frame(
  time = time(TUR_GAS),
  TUR = as.numeric(TUR_GAS),
  Fitted = as.numeric(fitted(TUR_GAS_linear_cov))
)

ggplot(df_plot3, aes(x = time)) +
  geom_line(aes(y = TUR, color = "Observed"), size = 1.2) +
  geom_line(aes(y = Fitted, color = "Fitted"),
            size = 1.1, linetype = "dotdash") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Linear Model – Turkey Gas Prices",
    x = "Year",
    y = "Euro",
    color = NULL
  ) +
  scale_color_manual(values = c(
    "Observed" = "#1b4f72",   
    "Fitted"   = "#d35400"    
  )) +
  scale_x_continuous(breaks = seq(2007, 2025, 2)) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "top",
    legend.text = element_text(size = 12)
  )


#ITA - fitted ARIMA
df_plot4 <- data.frame(
  time = time(ITA_GAS),
  ITA = as.numeric(ITA_GAS),
  Fitted = as.numeric(fitted(fit_arima2_ITA_GAS))
)

ggplot(df_plot4, aes(x = time)) +
  geom_line(aes(y = ITA, color = "Observed"), size = 1.2) +
  geom_line(aes(y = Fitted, color = "Fitted"),
            size = 1.1, linetype = "dotdash") +
  theme_minimal(base_size = 14) +
  labs(
    title = "SARIMA(0,0,1)(0,1,0)_2 – Italy Gas Prices",
    x = "Year",
    y = "Euro",
    color = NULL
  ) +
  scale_color_manual(values = c(
    "Observed" = "#1b4f72",   
    "Fitted"   = "#d35400"    
  )) +
  scale_x_continuous(breaks = seq(2007, 2025, 2)) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "top",
    legend.text = element_text(size = 12)
  )

#TUR - fitted ARIMA
df_plot5 <- data.frame(
  time = time(TUR_GAS),
  TUR = as.numeric(TUR_GAS),
  Fitted = as.numeric(fitted(fit_arima3_TUR_GAS))
)

ggplot(df_plot5, aes(x = time)) +
  geom_line(aes(y = TUR, color = "Observed"), size = 1.2) +
  geom_line(aes(y = Fitted, color = "Fitted"),
            size = 1.1, linetype = "dotdash") +
  theme_minimal(base_size = 14) +
  labs(
    title = "SARIMA(0,1,0)(1,0,0)_2 – Turkey Gas Prices",
    x = "Year",
    y = "Euro",
    color = NULL
  ) +
  scale_color_manual(values = c(
    "Observed" = "#1b4f72",   
    "Fitted"   = "#d35400"    
  )) +
  scale_x_continuous(breaks = seq(2007, 2025, 2)) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "top",
    legend.text = element_text(size = 12)
  )

#ITA - fitted ARIMAX
df_plot6 <- data.frame(
  time = time(ITA_GAS),
  ITA = as.numeric(ITA_GAS),
  Fitted = as.numeric(fitted(ITA_GAS_armax_time))
)

ggplot(df_plot6, aes(x = time)) +
  geom_line(aes(y = ITA, color = "Observed"), size = 1.2) +
  geom_line(aes(y = Fitted, color = "Fitted"),
            size = 1.1, linetype = "dotdash") +
  theme_minimal(base_size = 14) +
  labs(
    title = "ARIMAX – Italy Gas Prices",
    x = "Year",
    y = "Euro",
    color = NULL
  ) +
  scale_color_manual(values = c(
    "Observed" = "#1b4f72",   
    "Fitted"   = "#d35400"    
  )) +
  scale_x_continuous(breaks = seq(2007, 2025, 2)) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "top",
    legend.text = element_text(size = 12)
  )

#TUR - fitted ARIMAX
df_plot7 <- data.frame(
  time = time(TUR_GAS),
  TUR = as.numeric(TUR_GAS),
  Fitted = as.numeric(fitted(TUR_GAS_armax_cov))
)

ggplot(df_plot7, aes(x = time)) +
  geom_line(aes(y = TUR, color = "Observed"), size = 1.2) +
  geom_line(aes(y = Fitted, color = "Fitted"),
            size = 1.1, linetype = "dotdash") +
  theme_minimal(base_size = 14) +
  labs(
    title = "ARIMAX – Turkey Gas Prices",
    x = "Year",
    y = "Euro",
    color = NULL
  ) +
  scale_color_manual(values = c(
    "Observed" = "#1b4f72",   
    "Fitted"   = "#d35400"    
  )) +
  scale_x_continuous(breaks = seq(2007, 2025, 2)) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "top",
    legend.text = element_text(size = 12)
  )

#ITA - fitted GAM 
df_plot8 <- data.frame(
  time = time(ITA_GAS),
  ITA = as.numeric(ITA_GAS),
  Fitted = as.numeric(fitted(ITA_GAS_gam_time))
)

ggplot(df_plot8, aes(x = time)) +
  geom_line(aes(y = ITA, color = "Observed"), size = 1.2) +
  geom_line(aes(y = Fitted, color = "Fitted"),
            size = 1.1, linetype = "dotdash") +
  theme_minimal(base_size = 14) +
  labs(
    title = "GAM – Italy Gas Prices",
    x = "Year",
    y = "Euro",
    color = NULL
  ) +
  scale_color_manual(values = c(
    "Observed" = "#1b4f72",   
    "Fitted"   = "#d35400"    
  )) +
  scale_x_continuous(breaks = seq(2007, 2025, 2)) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "top",
    legend.text = element_text(size = 12)
  )
