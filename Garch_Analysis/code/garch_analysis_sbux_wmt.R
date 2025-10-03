{
  library(quantmod)
  library(astsa)
  library(readxl)
  library(tidyverse)
  library(fImport)
  library(fBasics)
  library(rugarch)
  library(roll)
  library(distr)
  library(sandwich)
}


#------------------------------------------------------------------------------------
#### Point 1 ###
#------------------------------------------------------------------------------------

# Historical series of adjusted closing prices for Starbucks (SBUX.O) and Walmart (WMT)
# were downloaded from the Refinitiv data provider

# Data preparation
{
  dati <- read_excel("Price_History_SBUX.O_WMT.xlsx")
  # Convert to xts object
  colnames(dati) <- c("Date", "SBUX.O", "WMT")
  close=xts(dati[,-1], order.by=dati$Date, frequency = "daily")
  # Allows date-based indexing
  head(close)
  # Log-returns
  a=dim(dati)
  N=a[2]
  T=a[1]
  p=log(dati[,2:N])
  r=p[2:T,]-p[1:T-1,]
  r=xts(r, order.by=dati$Date[-1], frequency = "daily")
  # Convert to percentage log-returns
  r=r*100 
}
head(r)

#------------------------------------------------------------------------------------
#### Point 2 ###
#------------------------------------------------------------------------------------

# STARBUCKS

# Exploratory analysis
basicStats(r$SBUX.O) # excess kurtosis and slight skewness

# Empirical and statistical evidence of non-normal distribution of log-returns
{
  hist(r$SBUX.O,breaks=100,freq=F,main="Empirical distribution of Starbucks returns",xlab="")
  curve(dnorm(x, mean(r$SBUX.O),sd(r$SBUX.O)), col=2, add=TRUE, lwd=2) 
}
jarqueberaTest(r$SBUX.O)
# Data lead to rejection of normality hypothesis

{
  par(mfrow=c(3,1))
  # Daily returns
  plot(r$SBUX.O,main="Starbucks daily returns")
  # Volatility proxy
  plot(abs(r$SBUX.O), main="Absolute returns Starbucks")
  # Variance proxy
  plot(r$SBUX.O^2, main="Squared returns Starbucks")
  par(mfrow=c(1,1))
}


# Given graphical evidence of volatility clustering,
# Perform Lagrange Multiplier ARCH (LM-ARCH) tests
# to statistically assess heteroskedasticity in the series
# and Ljung-Box (LB) test.

# ARCH effect test on mean-deviation returns
{
  y<-as.timeSeries(r$SBUX.O-mean(r$SBUX.O))
  y2<-y^2
  # Consider 1, 2 and 3 lags for the LM test
  y2L1<-lag(y2,k=1)
  y2L2<-lag(y2,k=2)
  y2L3<-lag(y2,k=3)
  # Three test statistics and corresponding p-values:
  #       lag=1
  out1<-lm(y2~y2L1) # auxiliary regression
  sum1<-summary(out1)
  LMARCH1<-sum1$r.squared*(T-2)
  pchisq(LMARCH1,1,lower.tail=FALSE)
  #       lag=2
  out1<-lm(y2~y2L1+y2L2)
  sum1<-summary(out1)
  LMARCH1<-sum1$r.squared*(T-2)
  pchisq(LMARCH1,2,lower.tail=FALSE)
  #       lag=3
  out1<-lm(y2~y2L1+y2L2+y2L3)
  sum1<-summary(out1)
  LMARCH1<-sum1$r.squared*(T-2)
  pchisq(LMARCH1,3,lower.tail=FALSE)
}
# Reject the null hypothesis of homoskedasticity
# Confirms presence of ARCH effects for each lag considered

# Ljung-Box test (stats) on squared returns for lags 5 and 10
Box.test(as.numeric(y2),lag=5,type="Ljung-Box")
Box.test(as.numeric(y2),lag=10,type="Ljung-Box")
# Confirms serial correlation in the squared returns series

# Prepare data for model fitting and testing
{
  dati$Date <- as.Date(dati$Date)
  test_size <- length(which(dati$Date>as.Date("2023-03-31"))) # test set size (post-31/03/23 observations)
  train_size <- dim(r)[1] - test_size # train set size (observations up to 31/03/23)
  train.SBUX.O <- r$SBUX.O[1:train_size] # train set
  test.SBUX.O <- tail(r$SBUX.O, test_size) # test set
}


# Fit GARCH(1,1) model with normal innovations to log-returns
{
  spec.garch11.norm.SBUX <-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                                      mean.model=list(armaOrder=c(0,0),include.mean=TRUE), 
                                      distribution.model="norm")
  fit.garch11.norm.SBUX <- ugarchfit(spec.garch11.norm.SBUX, train.SBUX.O$SBUX.O)
  
  show(fit.garch11.norm.SBUX) 
}

# Diagnostics on standardized residuals
# Standardized residuals
{
  resst0.SBUX<-residuals(fit.garch11.norm.SBUX, standardize=TRUE)
  colnames(resst0.SBUX)<-"res.s"
  plot(resst0.SBUX,main="Standardized residuals GARCH(1,1) - SBUX") 
}
# Diagnostic analysis: check for remaining ARCH effects and correct innovation distribution
{
  # Squared residuals
  y2.SBUX<-resst0.SBUX^2
  # Consider 1, 2 and 3 lags for LM test on residuals
  y2L1.SBUX<-lag(y2.SBUX,k=1)
  y2L2.SBUX<-lag(y2.SBUX,k=2)
  y2L3.SBUX<-lag(y2.SBUX,k=3)
  # Three test statistics and corresponding p-values
  out1.SBUX<-lm(y2.SBUX~y2L1.SBUX)
  sum1.SBUX<-summary(out1.SBUX)
  LMARCH1.SBUX<-sum1.SBUX$r.squared*(T-2)
  pchisq(LMARCH1.SBUX,1,lower.tail=FALSE) # H0
  
  out1.SBUX<-lm(y2.SBUX~y2L1.SBUX+y2L2.SBUX)
  sum1.SBUX<-summary(out1.SBUX)
  LMARCH1.SBUX<-sum1.SBUX$r.squared*(T-2)
  pchisq(LMARCH1.SBUX,2,lower.tail=FALSE) # H0
  
  out1.SBUX<-lm(y2.SBUX~y2L1.SBUX+y2L2.SBUX+y2L3.SBUX)
  sum1.SBUX<-summary(out1.SBUX)
  LMARCH1.SBUX<-sum1.SBUX$r.squared*(T-2)
  pchisq(LMARCH1.SBUX,3,lower.tail=FALSE) # H0
}

# Accept the null hypothesis of homoskedasticity: no heteroskedasticity in standardized residuals

# Innovation distribution
plot(fit.garch11.norm.SBUX,which=8)
# Q-Q plot
plot(fit.garch11.norm.SBUX,which=9)

# Graphically, standardized residuals do not follow a normal distribution:
# empirical distribution has heavier tails than the normal

signbias(fit.garch11.norm.SBUX)
# Sign bias terms are not significant

#------------------------------------------------------------------------------------
#### WALMART ###
#------------------------------------------------------------------------------------

# Exploratory analysis
basicStats(r$WMT) # excess kurtosis and slight skewness

# Empirical and statistical evidence of non-normal distribution of log-returns
{
  hist(r$WMT, breaks=100, freq=F, main="Empirical distribution of Walmart returns", xlab="")
  curve(dnorm(x, mean(r$WMT), sd(r$WMT)), col=2, add=TRUE, lwd=2) 
}
jarqueberaTest(r$WMT)  # Data leads to rejection of normality hypothesis

{
  par(mfrow=c(3,1))
  # Daily returns
  plot(r$WMT, main="Walmart daily returns")
  # Volatility proxy
  plot(abs(r$WMT), main="Absolute returns Walmart")
  # Variance proxy
  plot(r$WMT^2, main="Squared returns Walmart")
  par(mfrow=c(1,1)) 
}

# Given graphical evidence of volatility clustering,
# perform Lagrange Multiplier ARCH (LM-ARCH) test
# and Ljung-Box test to assess heteroskedasticity and serial correlation

{
  y.WMT <- as.timeSeries(r$WMT - mean(r$WMT))
  y2.WMT <- y.WMT^2
  # Consider 1, 2, and 3 lags for LM test
  y2L1.WMT <- lag(y2.WMT, k=1)
  y2L2.WMT <- lag(y2.WMT, k=2)
  y2L3.WMT <- lag(y2.WMT, k=3)
  # LM test statistics and p-values
  out1.WMT <- lm(y2.WMT ~ y2L1.WMT)
  sum1.WMT <- summary(out1.WMT)
  LMARCH1.WMT <- sum1.WMT$r.squared * (T-2)
  pchisq(LMARCH1.WMT, 1, lower.tail=FALSE)
  
  out1.WMT <- lm(y2.WMT ~ y2L1.WMT + y2L2.WMT)
  sum1.WMT <- summary(out1.WMT)
  LMARCH1.WMT <- sum1.WMT$r.squared * (T-2)
  pchisq(LMARCH1.WMT, 2, lower.tail=FALSE)
  
  out1.WMT <- lm(y2.WMT ~ y2L1.WMT + y2L2.WMT + y2L3.WMT)
  sum1.WMT <- summary(out1.WMT)
  LMARCH1.WMT <- sum1.WMT$r.squared * (T-2)
  pchisq(LMARCH1.WMT, 3, lower.tail=FALSE)
  # Reject null hypothesis: confirms ARCH effects in squared returns 
}

# Ljung-Box test on squared returns
Box.test(as.numeric(y2.WMT), lag=5, type="Ljung-Box")
Box.test(as.numeric(y2.WMT), lag=10, type="Ljung-Box")
# Confirms serial correlation in squared return series

# Prepare data for modeling
{
  train.WMT <- r$WMT[1:train_size]
  test.WMT <- tail(r$WMT, test_size) 
}

# Fit GARCH(1,1) model with normal distribution
{
  spec.garch11.norm.WMT <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                                      mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                                      distribution.model="norm")
  fit.garch11.norm.WMT <- ugarchfit(spec.garch11.norm.WMT, train.WMT$WMT)
  show(fit.garch11.norm.WMT) 
}

# Residual diagnostics
{
  resst0.WMT <- residuals(fit.garch11.norm.WMT, standardize=TRUE)
  colnames(resst0.WMT) <- "res.s"
  plot(resst0.WMT, main="Standardized residuals GARCH(1,1) - WMT") 
}

# LM test on squared residuals
{
  y2.WMT <- resst0.WMT^2
  y2L1.WMT <- lag(y2.WMT, k=1)
  y2L2.WMT <- lag(y2.WMT, k=2)
  y2L3.WMT <- lag(y2.WMT, k=3)
  
  out1.WMT <- lm(y2.WMT ~ y2L1.WMT)
  sum1.WMT <- summary(out1.WMT)
  LMARCH1.WMT <- sum1.WMT$r.squared * (T-2)
  pchisq(LMARCH1.WMT, 1, lower.tail=FALSE)
  
  out1.WMT <- lm(y2.WMT ~ y2L1.WMT + y2L2.WMT)
  sum1.WMT <- summary(out1.WMT)
  LMARCH1.WMT <- sum1.WMT$r.squared * (T-2)
  pchisq(LMARCH1.WMT, 2, lower.tail=FALSE)
  
  out1.WMT <- lm(y2.WMT ~ y2L1.WMT + y2L2.WMT + y2L3.WMT)
  sum1.WMT <- summary(out1.WMT)
  LMARCH1.WMT <- sum1.WMT$r.squared * (T-2)
  pchisq(LMARCH1.WMT, 3, lower.tail=FALSE) 
}
# Accept null hypothesis: no ARCH effects in standardized residuals

# Innovation distribution
{
  plot(fit.garch11.norm.WMT, which=8)
  # Q-Q plot
  plot(fit.garch11.norm.WMT, which=9) 
}
# Residuals deviate from normal distribution

signbias(fit.garch11.norm.WMT)
# No significant sign bias detected

#------------------------------------------------------------------------------------
#### Point 3 ###
#------------------------------------------------------------------------------------

# STARBUCKS

# Specify asymmetric GARCH models with normal innovations for SBUX.O
#EGARCH(1,1)
{
  spec.egarch11.norm.SBUX <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                                        mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                                        distribution.model="norm")
  fit.egarch11.norm.SBUX <- ugarchfit(spec.egarch11.norm.SBUX, train.SBUX.O$SBUX.O)
  show(fit.egarch11.norm.SBUX)
}
#GJR-GARCH(1,1)
{
  spec.gjrgarch11.norm.SBUX <- ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                                          mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                                          distribution.model="norm")
  fit.gjrgarch11.norm.SBUX <- ugarchfit(spec.gjrgarch11.norm.SBUX, train.SBUX.O$SBUX.O)
  show(fit.gjrgarch11.norm.SBUX)
}

{
  spec.aparch11.norm.SBUX <- ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)),
                                        mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                                        distribution.model="norm")
  fit.aparch11.norm.SBUX <- ugarchfit(spec.aparch11.norm.SBUX, train.SBUX.O$SBUX.O)
  show(fit.aparch11.norm.SBUX) 
}

# Graphical comparison of variances from different models
{
  sigma0.SBUX <- sigma(fit.garch11.norm.SBUX)
  sigma1.SBUX <- sigma(fit.egarch11.norm.SBUX)
  sigma2.SBUX <- sigma(fit.gjrgarch11.norm.SBUX)
  sigma3.SBUX <- sigma(fit.aparch11.norm.SBUX)
  par(mfrow=c(2,2))
  plot(as.numeric(sigma0.SBUX), type="l", ylab="", xlab="", main="GARCH(1,1)")
  plot(as.numeric(sigma1.SBUX), type="l", ylab="", xlab="", main="EGARCH(1,1)")
  plot(as.numeric(sigma2.SBUX), type="l", ylab="", xlab="", main="GJR-GARCH(1,1)")
  plot(as.numeric(sigma3.SBUX), type="l", ylab="", xlab="", main="APARCH(1,1)") 
}

# News Impact Curve comparison
{
  ni0.SBUX <- newsimpact(z=NULL, fit.garch11.norm.SBUX)
  ni1.SBUX <- newsimpact(z=NULL, fit.egarch11.norm.SBUX)
  ni2.SBUX <- newsimpact(z=NULL, fit.gjrgarch11.norm.SBUX)
  ni3.SBUX <- newsimpact(z=NULL, fit.aparch11.norm.SBUX)
  plot(ni0.SBUX$zx, ni0.SBUX$zy, ylab=ni0.SBUX$yexpr, xlab=ni0.SBUX$xexpr, type="l", main="GARCH")
  plot(ni1.SBUX$zx, ni1.SBUX$zy, ylab=ni1.SBUX$yexpr, xlab=ni1.SBUX$xexpr, type="l", main="EGARCH")
  plot(ni2.SBUX$zx, ni2.SBUX$zy, ylab=ni2.SBUX$yexpr, xlab=ni2.SBUX$xexpr, type="l", main="GJR")
  plot(ni3.SBUX$zx, ni3.SBUX$zy, ylab=ni3.SBUX$yexpr, xlab=ni3.SBUX$xexpr, type="l", main="APARCH")
  par(mfrow=c(1,1)) 
}

# Specify asymmetric GARCH models with Student-t innovations for SBUX.O
#GARCH(1,1)
{
  spec.garch11.std.SBUX <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                                      mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                                      distribution.model="std")
  fit.garch11.std.SBUX <- ugarchfit(spec.garch11.std.SBUX, train.SBUX.O$SBUX.O)
  show(fit.garch11.std.SBUX) 
}
#EGARCH(1,1)
{
  spec.egarch11.std.SBUX <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                                       mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                                       distribution.model="std")
  fit.egarch11.std.SBUX <- ugarchfit(spec.egarch11.std.SBUX, train.SBUX.O$SBUX.O)
  show(fit.egarch11.std.SBUX)
}
#GJR-GARCH(1,1)
{
  spec.gjrgarch11.std.SBUX <- ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                                         mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                                         distribution.model="std")
  fit.gjrgarch11.std.SBUX <- ugarchfit(spec.gjrgarch11.std.SBUX, train.SBUX.O$SBUX.O)
  show(fit.gjrgarch11.std.SBUX) 
}
#APARCH(1,1)
{
  spec.aparch11.std.SBUX <- ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)),
                                       mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                                       distribution.model="std")
  fit.aparch11.std.SBUX <- ugarchfit(spec.aparch11.std.SBUX, train.SBUX.O$SBUX.O)
  show(fit.aparch11.std.SBUX)
}

# Compare innovation distributions for GARCH(1,1) with normal vs student-t
{
  plot(fit.garch11.std.SBUX, which=9)
  plot(fit.garch11.std.SBUX, which=8) 
}
# Graphical evidence shows better fit with Student-t innovations

# Compare model variances
{
  sigma0.SBUX <- sigma(fit.garch11.std.SBUX)
  sigma1.SBUX <- sigma(fit.egarch11.std.SBUX)
  sigma2.SBUX <- sigma(fit.gjrgarch11.std.SBUX)
  sigma3.SBUX <- sigma(fit.aparch11.std.SBUX)
  par(mfrow=c(2,2))
  plot(as.numeric(sigma0.SBUX), type="l", ylab="", xlab="", main="GARCH(1,1) std")
  plot(as.numeric(sigma1.SBUX), type="l", ylab="", xlab="", main="EGARCH(1,1) std")
  plot(as.numeric(sigma2.SBUX), type="l", ylab="", xlab="", main="GJR-GARCH(1,1) std")
  plot(as.numeric(sigma3.SBUX), type="l", ylab="", xlab="", main="APARCH(1,1) std") 
}

# News Impact Curve comparison for Student-t models
{
  ni0.SBUX <- newsimpact(z=NULL, fit.garch11.std.SBUX)
  ni1.SBUX <- newsimpact(z=NULL, fit.egarch11.std.SBUX)
  ni2.SBUX <- newsimpact(z=NULL, fit.gjrgarch11.std.SBUX)
  ni3.SBUX <- newsimpact(z=NULL, fit.aparch11.std.SBUX)
  plot(ni0.SBUX$zx, ni0.SBUX$zy, ylab=ni0.SBUX$yexpr, xlab=ni0.SBUX$xexpr, type="l", main="GARCH")
  plot(ni1.SBUX$zx, ni1.SBUX$zy, ylab=ni1.SBUX$yexpr, xlab=ni1.SBUX$xexpr, type="l", main="EGARCH")
  plot(ni2.SBUX$zx, ni2.SBUX$zy, ylab=ni2.SBUX$yexpr, xlab=ni2.SBUX$xexpr, type="l", main="GJR")
  plot(ni3.SBUX$zx, ni3.SBUX$zy, ylab=ni3.SBUX$yexpr, xlab=ni3.SBUX$xexpr, type="l", main="APARCH")
  par(mfrow=c(1,1)) 
}

# Compare models using information criteria
{
  ICall <- cbind(infocriteria(fit.garch11.norm.SBUX)*(T-1),
                 infocriteria(fit.egarch11.norm.SBUX)*(T-1),
                 infocriteria(fit.gjrgarch11.norm.SBUX)*(T-1),
                 infocriteria(fit.aparch11.norm.SBUX)*(T-1),
                 infocriteria(fit.garch11.std.SBUX)*(T-1),
                 infocriteria(fit.egarch11.std.SBUX)*(T-1),
                 infocriteria(fit.gjrgarch11.std.SBUX)*(T-1),
                 infocriteria(fit.aparch11.std.SBUX)*(T-1))
  colnames(ICall) <- c("GARCH-norm", "EGARCH-norm", "GJR-norm", "APARCH-norm",
                       "GARCH-std", "EGARCH-std", "GJR-std", "APARCH-std")
  show(ICall)
}

# Identify best model based on information criteria
{
  # AIC
  names(which.min(ICall[1,]))
  # BIC
  names(which.min(ICall[2,]))
  # Shibata
  names(which.min(ICall[3,]))
  # Hannan-Quinn
  names(which.min(ICall[4,])) 
}
# According to all four information criteria, the best model for SBUX.O
# based on data up to March 2023 is the APARCH(1,1) model with Student-t innovations

# WALMART

# Specify asymmetric GARCH models with normal innovations for WMT
#EGARCH(1,1)
{
  spec.egarch11.norm.WMT <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                                       mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                                       distribution.model="norm")
  fit.egarch11.norm.WMT <- ugarchfit(spec.egarch11.norm.WMT, train.WMT$WMT)
  show(fit.egarch11.norm.WMT) 
}
#GJR-GARCH(1,1)
{
  spec.gjrgarch11.norm.WMT <- ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                                         mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                                         distribution.model="norm")
  fit.gjrgarch11.norm.WMT <- ugarchfit(spec.gjrgarch11.norm.WMT, train.WMT$WMT)
  show(fit.gjrgarch11.norm.WMT) 
}
#APARCH(1,1)
{
  spec.aparch11.norm.WMT <- ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)),
                                       mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                                       distribution.model="norm")
  fit.aparch11.norm.WMT <- ugarchfit(spec.aparch11.norm.WMT, train.WMT$WMT)
  show(fit.aparch11.norm.WMT) 
}

# Graphical comparison of variances from different models
{
  sigma0.WMT <- sigma(fit.garch11.norm.WMT)
  sigma1.WMT <- sigma(fit.egarch11.norm.WMT)
  sigma2.WMT <- sigma(fit.gjrgarch11.norm.WMT)
  sigma3.WMT <- sigma(fit.aparch11.norm.WMT)
  par(mfrow=c(2,2))
  plot(as.numeric(sigma0.WMT), type="l", ylab="", xlab="", main="GARCH(1,1)")
  plot(as.numeric(sigma1.WMT), type="l", ylab="", xlab="", main="EGARCH(1,1)")
  plot(as.numeric(sigma2.WMT), type="l", ylab="", xlab="", main="GJR-GARCH(1,1)")
  plot(as.numeric(sigma3.WMT), type="l", ylab="", xlab="", main="APARCH(1,1)") 
}

# News Impact Curve comparison
{
  ni0.WMT <- newsimpact(z=NULL, fit.garch11.norm.WMT)
  ni1.WMT <- newsimpact(z=NULL, fit.egarch11.norm.WMT)
  ni2.WMT <- newsimpact(z=NULL, fit.gjrgarch11.norm.WMT)
  ni3.WMT <- newsimpact(z=NULL, fit.aparch11.norm.WMT)
  plot(ni0.WMT$zx, ni0.WMT$zy, ylab=ni0.WMT$yexpr, xlab=ni0.WMT$xexpr, type="l", main="GARCH")
  plot(ni1.WMT$zx, ni1.WMT$zy, ylab=ni1.WMT$yexpr, xlab=ni1.WMT$xexpr, type="l", main="EGARCH")
  plot(ni2.WMT$zx, ni2.WMT$zy, ylab=ni2.WMT$yexpr, xlab=ni2.WMT$xexpr, type="l", main="GJR")
  plot(ni3.WMT$zx, ni3.WMT$zy, ylab=ni3.WMT$yexpr, xlab=ni3.WMT$xexpr, type="l", main="APARCH")
  par(mfrow=c(1,1)) 
}

# Specify asymmetric GARCH models with Student-t innovations for WMT
#GARCH(1,1)
{
  spec.garch11.std.WMT <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                                     mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                                     distribution.model="std")
  fit.garch11.std.WMT <- ugarchfit(spec.garch11.std.WMT, train.WMT$WMT)
  show(fit.garch11.std.WMT)
}
#EGARCH(1,1)
{
  spec.egarch11.std.WMT <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                                      mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                                      distribution.model="std")
  fit.egarch11.std.WMT <- ugarchfit(spec.egarch11.std.WMT, train.WMT$WMT)
  show(fit.egarch11.std.WMT)
}
#GJR-GARCH(1,1)
{
  spec.gjrgarch11.std.WMT <- ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                                        mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                                        distribution.model="std")
  fit.gjrgarch11.std.WMT <- ugarchfit(spec.gjrgarch11.std.WMT, train.WMT$WMT)
  show(fit.gjrgarch11.std.WMT)
}
#APARCH(1,1)
{
  spec.aparch11.std.WMT <- ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)),
                                      mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                                      distribution.model="std")
  fit.aparch11.std.WMT <- ugarchfit(spec.aparch11.std.WMT, train.WMT$WMT)
  show(fit.aparch11.std.WMT)
}

# Compare innovation distributions for GARCH(1,1) with Student-t
{
  plot(fit.garch11.std.WMT, which=9)
  plot(fit.garch11.std.WMT, which=8) 
}
# Graphical evidence shows better fit with Student-t innovations

# Compare model variances
{
  sigma0.WMT <- sigma(fit.garch11.std.WMT)
  sigma1.WMT <- sigma(fit.egarch11.std.WMT)
  sigma2.WMT <- sigma(fit.gjrgarch11.std.WMT)
  sigma3.WMT <- sigma(fit.aparch11.std.WMT)
  par(mfrow=c(2,2))
  plot(as.numeric(sigma0.WMT), type="l", ylab="", xlab="", main="GARCH(1,1) std")
  plot(as.numeric(sigma1.WMT), type="l", ylab="", xlab="", main="EGARCH(1,1) std")
  plot(as.numeric(sigma2.WMT), type="l", ylab="", xlab="", main="GJR-GARCH(1,1) std")
  plot(as.numeric(sigma3.WMT), type="l", ylab="", xlab="", main="APARCH(1,1) std") 
}

# News Impact Curve comparison for Student-t models
{
  ni0.WMT <- newsimpact(z=NULL, fit.garch11.std.WMT)
  ni1.WMT <- newsimpact(z=NULL, fit.egarch11.std.WMT)
  ni2.WMT <- newsimpact(z=NULL, fit.gjrgarch11.std.WMT)
  ni3.WMT <- newsimpact(z=NULL, fit.aparch11.std.WMT)
  plot(ni0.WMT$zx, ni0.WMT$zy, ylab=ni0.WMT$yexpr, xlab=ni0.WMT$xexpr, type="l", main="GARCH")
  plot(ni1.WMT$zx, ni1.WMT$zy, ylab=ni1.WMT$yexpr, xlab=ni1.WMT$xexpr, type="l", main="EGARCH")
  plot(ni2.WMT$zx, ni2.WMT$zy, ylab=ni2.WMT$yexpr, xlab=ni2.WMT$xexpr, type="l", main="GJR")
  plot(ni3.WMT$zx, ni3.WMT$zy, ylab=ni3.WMT$yexpr, xlab=ni3.WMT$xexpr, type="l", main="APARCH")
  par(mfrow=c(1,1))
}

# Compare models using information criteria for WMT
{
  ICall <- cbind(infocriteria(fit.garch11.norm.WMT)*(T-1),
                 infocriteria(fit.egarch11.norm.WMT)*(T-1),
                 infocriteria(fit.gjrgarch11.norm.WMT)*(T-1),
                 infocriteria(fit.aparch11.norm.WMT)*(T-1),
                 infocriteria(fit.garch11.std.WMT)*(T-1),
                 infocriteria(fit.egarch11.std.WMT)*(T-1),
                 infocriteria(fit.gjrgarch11.std.WMT)*(T-1),
                 infocriteria(fit.aparch11.std.WMT)*(T-1))
  colnames(ICall) <- c("GARCH-norm", "EGARCH-norm", "GJR-norm", "APARCH-norm",
                       "GARCH-std", "EGARCH-std", "GJR-std", "APARCH-std")
  show(ICall) 
}

# Identify best model based on information criteria
{
  # AIC
  names(which.min(ICall[1,]))
  # BIC
  names(which.min(ICall[2,]))
  # Shibata
  names(which.min(ICall[3,]))
  # Hannan-Quinn
  names(which.min(ICall[4,])) 
}

# Depending on the criterion, the best model varies. However, the APARCH(1,1) model
# with Student-t innovations consistently performs well across multiple criteria.
# Also, models with Student-t innovations generally fit the data better than those with normal innovations.

#------------------------------------------------------------------------------------
#### Point 4 ###
#------------------------------------------------------------------------------------

# STARBUCKS

# Prepare demeaned return series
ret.SBX <- r$SBUX.O - mean(r$SBUX.O)

# Define model specifications for forecasting
model_specs_SBX <- list(
  garch11_norm = spec.garch11.norm.SBUX,
  egarch11_norm = spec.egarch11.norm.SBUX,
  gjrgarch11_norm = spec.gjrgarch11.norm.SBUX,
  aparch11_norm = spec.aparch11.norm.SBUX,
  garch11_std = spec.garch11.std.SBUX,
  egarch11_std = spec.egarch11.std.SBUX,
  gjrgarch11_std = spec.gjrgarch11.std.SBUX,
  aparch11_std = spec.aparch11.std.SBUX
)


# Perform 1-step-ahead rolling forecasts
forecast_results_SBX <- lapply(model_specs_SBX, function(spec) {
  ugarchroll(spec, data = ret.SBX, forecast.length = test_size, refit.window = "recursive")
})

# Plot:
# - solid grey line: actual data
# - dashed lines: forecasts from each model
# Colors 1:8 correspond to models as in `forecast_data_SBX`
{
  # Extract sigma values and convert to data.frame
  forecast_data_SBX <- lapply(forecast_results_SBX, function(x) as.data.frame(x)$Sigma)
  names(forecast_data_SBX) <- names(model_specs_SBX)
  
  # Combine all forecasted sigmas and actual absolute returns for plotting
  forecast_matrix_SBX <- do.call(cbind, forecast_data_SBX)
  forecast_matrix_SBX <- cbind(forecast_matrix_SBX, tail(abs(ret.SBX$SBUX.O), test_size))
  
  
  matplot(forecast_matrix_SBX, type="l", pch=1, col=c(1:8, "grey"), lty=c(rep(2,8),1), lwd=c(rep(2,8),1))
  title("Actual data vs static forecasts - SBUX")
  legend("topleft", c("garch11-norm", "egarch11-norm","gjrarch11-norm","aparch11-norm",
                      "garch11-std","egarch11-std","gjrarch11-std","aparch11-std","actual"), 
         col=c(1:8, "grey"), lty=c(rep(2,8),1), lwd=c(rep(2,8),1), ncol=2)
}


# Diebold-Mariano test
{
  # Actual realized values (as volatility proxy)
  rf <- as.data.frame(forecast_results_SBX$garch11_norm)$Realized
  
  # Loss functions (squared forecast error of sigma)
  loss_functions_SBX <- lapply(forecast_data_SBX, function(sigma) (sigma^2 - rf^2))
  
  # Compute pairwise differences between loss functions
  diff_names <- combn(names(loss_functions_SBX), 2, simplify=FALSE)
  diff_list <- lapply(diff_names, function(pair) loss_functions_SBX[[pair[1]]] - loss_functions_SBX[[pair[2]]])
  names(diff_list) <- sapply(diff_names, function(pair) paste(pair, collapse = "_vs_"))
  
  # Compute Newey-West robust standard errors and DM statistics
  m <- floor(0.75 * (NROW(rf))^(1/3))
  x1 <- rep(1, NROW(rf))
  DM_SBUX <- matrix(NA, nrow=8, ncol=8)
  colnames(DM_SBUX) <- rownames(DM_SBUX) <- names(forecast_data_SBX)
  
  for (k in seq_along(diff_list)) {
    pair <- strsplit(names(diff_list)[k], "_vs_")[[1]]
    d <- diff_list[[k]]
    nw_se <- sqrt(NeweyWest(lm(d ~ x1 - 1), lag=m, prewhite=0))
    stat <- mean(d) / nw_se
    i <- match(pair[1], names(forecast_data_SBX))
    j <- match(pair[2], names(forecast_data_SBX))
    DM_SBUX[i, j] <- stat
    DM_SBUX[j, i] <- -stat
  }
}
show(DM_SBUX)

# Table interpretation:
# - Positive value: model in row has higher loss
# - Negative value: model in row has better predictive performance
# - Absolute values > 1.64 are statistically significant at 5%
# - NA on the diagonal

# APARCH(1,1)-std significantly outperforms models with normal innovations and GJRGARCH(1,1)-std

# WALMART

# Prepare demeaned return series
ret.WMT <- r$WMT - mean(r$WMT)

# Define model specifications for forecasting
model_specs_WMT <- list(
  garch11_norm = spec.garch11.norm.WMT,
  egarch11_norm = spec.egarch11.norm.WMT,
  gjrgarch11_norm = spec.gjrgarch11.norm.WMT,
  aparch11_norm = spec.aparch11.norm.WMT,
  garch11_std = spec.garch11.std.WMT,
  egarch11_std = spec.egarch11.std.WMT,
  gjrgarch11_std = spec.gjrgarch11.std.WMT,
  aparch11_std = spec.aparch11.std.WMT
)

# Perform 1-step-ahead rolling forecasts
forecast_results_WMT <- lapply(model_specs_WMT, function(spec) {
  ugarchroll(spec, data = ret.WMT, forecast.length = test_size, refit.window = "recursive")
})

# Plot:
# - solid grey line: actual data
# - dashed lines: forecasts from each model
# Colors 1:8 correspond to models as in `forecast_data_WMT`
{
  # Extract sigma values and convert to data.frame
  forecast_data_WMT <- lapply(forecast_results_WMT, function(x) as.data.frame(x)$Sigma)
  names(forecast_data_WMT) <- names(model_specs_WMT)
  
  # Combine all forecasted sigmas and actual absolute returns for plotting
  forecast_matrix_WMT <- do.call(cbind, forecast_data_WMT)
  forecast_matrix_WMT <- cbind(forecast_matrix_WMT, tail(abs(ret.WMT$WMT), test_size))
  
  matplot(forecast_matrix_WMT, type="l", pch=1, col=c(1:8, "grey"), lty=c(rep(2,8),1), lwd=c(rep(2,8),1))
  title("Actual data vs static forecasts - WMT")
  legend("topleft", c("garch11-norm", "egarch11-norm","gjrarch11-norm","aparch11-norm",
                      "garch11-std","egarch11-std","gjrarch11-std","aparch11-std","actual"), 
         col=c(1:8, "grey"), lty=c(rep(2,8),1), lwd=c(rep(2,8),1), ncol=2)
  
}

# Diebold-Mariano test
{
  # Actual realized values (as volatility proxy)
  rf <- as.data.frame(forecast_results_WMT$garch11_norm)$Realized
  
  # Loss functions (squared forecast error of sigma)
  loss_functions_WMT <- lapply(forecast_data_WMT, function(sigma) (sigma^2 - rf^2))
  
  # Compute pairwise differences between loss functions
  diff_names <- combn(names(loss_functions_WMT), 2, simplify=FALSE)
  diff_list <- lapply(diff_names, function(pair) loss_functions_WMT[[pair[1]]] - loss_functions_WMT[[pair[2]]])
  names(diff_list) <- sapply(diff_names, function(pair) paste(pair, collapse = "_vs_"))
  
  # Compute Newey-West robust standard errors and DM statistics
  DM_WMT <- matrix(NA, nrow=8, ncol=8)
  colnames(DM_WMT) <- rownames(DM_WMT) <- names(forecast_data_WMT)
  
  for (k in seq_along(diff_list)) {
    pair <- strsplit(names(diff_list)[k], "_vs_")[[1]]
    d <- diff_list[[k]]
    nw_se <- sqrt(NeweyWest(lm(d ~ x1 - 1), lag=m, prewhite=0))
    stat <- mean(d) / nw_se
    i <- match(pair[1], names(forecast_data_WMT))
    j <- match(pair[2], names(forecast_data_WMT))
    DM_WMT[i, j] <- stat
    DM_WMT[j, i] <- -stat
  }
}
show(DM_WMT)

# Table interpretation:
# - Positive value: model in row has higher loss
# - Negative value: model in row has better predictive performance
# - Absolute values > 1.64 are statistically significant at 5%
# - NA on the diagonal

# APARCH(1,1)-norm appears to be among the best models for WMT

#------------------------------------------------------------------------------------
#### Point 5 ###
#------------------------------------------------------------------------------------

# STARBUCKS

# Selected model for SBUX: APARCH(1,1) - std

show(spec.aparch11.std.SBUX)
{
  fit.SBUX <- ugarchfit(spec.aparch11.std.SBUX, r$SBUX.O)
  sigma.SBUX <- sigma(fit.SBUX)
  mu.SBUX <- rep(mean(r$SBUX.O), length(r$SBUX.O))
}
r.std.SBUX <- (r$SBUX.O - mu.SBUX) / sigma.SBUX

# Selected model for WMT: APARCH(1,1) - norm

show(spec.aparch11.norm.WMT)
{
  fit.WMT <- ugarchfit(spec.aparch11.norm.WMT, r$WMT)
  sigma.WMT <- sigma(fit.WMT)
  mu.WMT <- rep(mean(r$SBUX.O), length(r$WMT)) 
}
r.std.WMT <- (r$WMT - mu.WMT) / sigma.WMT

# Rolling correlation between the two series using a 62-day window
{
  len62 <- length(r.std.SBUX) - 61
  corr_62 <- rep(0, len62)
  for (j in 1:len62) {
    corr_62[j] <- cor(r.std.SBUX[j:(j+61)], r.std.WMT[j:(j+61)], use = "all.obs")
  } 
}

# Rolling correlation using a 252-day window
{
  len252 <- length(r.std.SBUX) - 251
  corr_252 <- rep(0, len252)
  for (j in 1:len252) {
    corr_252[j] <- cor(r.std.SBUX[j:(j+251)], r.std.WMT[j:(j+251)], use = "all.obs")
  } 
}

{
  par(mfrow = c(2,1))
  plot(corr_62, type = "l", main = "Rolling correlation between standardized residuals - 62-day window", ylab = "correlation")
  plot(corr_252, type = "l", main = "Rolling correlation between standardized residuals - 252-day window", ylab = "correlation")
  par(mfrow = c(1,1)) 
}

# Based on the graphical analysis,
# the rolling correlations between standardized residuals of the two stocks are not constant