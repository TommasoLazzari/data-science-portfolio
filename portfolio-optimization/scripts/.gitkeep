#libraries
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
  library(fBasics) 
  library ( PortfolioAnalytics )
  library ( DEoptim )
  library ( ROI )
  require ( ROI.plugin.quadprog )
  require ( ROI.plugin.glpk )
}

#------------------------------------------------------------------------------------
#### Point 1 ###
#------------------------------------------------------------------------------------

# Energy Sector:
#   SHELL
#   OMV
#   ORLEN
#   SUBSEA 7
#   REPSOL YPF

# Technology Sector:
#   AIXTRON (XET)
#   ALTEN
#   SAGE GROUP
#   ASM INTERNATIONAL
#   SOITEC

# Financials Sector:
#   SWEDBANK A
#   AEGON
#   ALLIANZ (XET)
#   ASSICURAZIONI GENERALI
#   AVIVA

# Real Estate Sector:
#   BRITISH LAND
#   CASTELLUM
#   COFINIMMO
#   COVIVIO
#   DERWENT LONDON

# Load the price dataset from Excel
dati <- read_xlsx("DatiHW3.xlsx", sheet = 1, range = "A4:WC299")

#Extract price time series for selected stocks and format by period (S1, S2, S3)
{
  dati <- dati[3:295, ]
  dati <- rename(dati, "Date" = "Name")
  dati$Date <- as.integer(dati$Date)
  dati$Date <- as.Date(dati$Date, origin = "1899-12-30")
  
  prezzi.S1 <- data.frame(
    dati$Date,
    dati$`SHELL - TOT RETURN IND`, dati$`OMV - TOT RETURN IND`,
    dati$`ORLEN - TOT RETURN IND`, dati$`SUBSEA 7 - TOT RETURN IND`, dati$`REPSOL YPF - TOT RETURN IND`,
    dati$`AIXTRON (XET) - TOT RETURN IND`, dati$`ALTEN - TOT RETURN IND`, dati$`SAGE GROUP - TOT RETURN IND`,
    dati$`ASM INTERNATIONAL - TOT RETURN IND`, dati$`SOITEC - TOT RETURN IND`,
    dati$`SWEDBANK A - TOT RETURN IND`, dati$`AEGON - TOT RETURN IND`,
    dati$`ALLIANZ (XET) - TOT RETURN IND`, dati$`ASSICURAZIONI GENERALI - TOT RETURN IND`,
    dati$`AVIVA - TOT RETURN IND`, dati$`BRITISH LAND - TOT RETURN IND`, dati$`CASTELLUM - TOT RETURN IND`,
    dati$`COFINIMMO - TOT RETURN IND`, dati$`COVIVIO - TOT RETURN IND`,
    dati$`DERWENT LONDON - TOT RETURN IND`
  )
  
  colnames(prezzi.S1) <- c("Date", "SHELL", "OMV", "ORLEN", "SUBSEA7", "REPSOL",
                           "AIXTRON", "ALTEN", "SAGE_GROUP", "ASM_INTERNATIONAL", "SOITEC",
                           "SWEDBANK_A", "AEGON", "ALLIANZ", "ASSICURAZIONI_GENERALI", "AVIVA",
                           "BRITISH_LAND", "CASTELLUM", "COFINIMMO", "COVIVIO", "DERWENT_LONDON")
  
  for (col in 2:21) {
    prezzi.S1[, col] <- as.numeric(prezzi.S1[, col])
  }
  
  close <- xts(prezzi.S1[, -1], order.by = prezzi.S1$Date, frequency = "monthly")
  
  # Divide into sub-periods
  prezzi.S2 <- prezzi.S1[1:205, ]
  prezzi.S3 <- prezzi.S1[206:289, ]
}

#Calculate log returns from price data for each period
{
  N <- dim(prezzi.S1)[2]
  T.S1 <- dim(prezzi.S1)[1]
  T.S2 <- dim(prezzi.S2)[1]
  T.S3 <- dim(prezzi.S3)[1]
  
  p.S1 <- log(prezzi.S1[, 2:N])
  p.S2 <- log(prezzi.S2[, 2:N])
  p.S3 <- log(prezzi.S3[, 2:N])
  
  r.S1 <- p.S1[2:T.S1, ] - p.S1[1:(T.S1 - 1), ]
  T.S1 <- T.S1 - 1
  r.S2 <- p.S2[2:T.S2, ] - p.S2[1:(T.S2 - 1), ]
  T.S2 <- T.S2 - 1
  r.S3 <- p.S3[2:T.S3, ] - p.S3[1:(T.S3 - 1), ]
  T.S3 <- T.S3 - 1
  
  N <- N - 1
  r.S1 <- r.S1 * 100
  r.S2 <- r.S2 * 100
  r.S3 <- r.S3 * 100
}

#Calculate mean returns and risk metrics (variance and correlation) in percentage terms
{
  # Monthly average returns
  m.S1 <- as.data.frame(colMeans(r.S1))
  m.S2 <- as.data.frame(colMeans(r.S2))
  m.S3 <- as.data.frame(colMeans(r.S3))
  
  # Vectors of monthly average returns
  mS.S1 <- m.S1[1:N, 1]
  mS.S2 <- m.S2[1:N, 1]
  mS.S3 <- m.S3[1:N, 1]
  
  # Covariance matrices
  V.S1 <- var(r.S1)
  V.S2 <- var(r.S2)
  V.S3 <- var(r.S3)
  
  VS.S1 <- V.S1[1:N, 1:N]
  VS.S2 <- V.S2[1:N, 1:N]
  VS.S3 <- V.S3[1:N, 1:N]
  
  # Correlation matrices
  CR.S1 <- cor(r.S1)[1:N, 1:N]
  CR.S2 <- cor(r.S2)[1:N, 1:N]
  CR.S3 <- cor(r.S3)[1:N, 1:N]
}

# Visualize input data for Efficient Frontier

# Plot bar charts of average monthly returns for S1, S2 and S3
{
  par(mfrow = c(3, 1))
  barplot(mS.S1, ylab = "Avg. Monthly Returns (%)", xlab = "Stocks", main = "Period S1")
  barplot(mS.S2, ylab = "Avg. Monthly Returns (%)", xlab = "Stocks", main = "Period S2")
  barplot(mS.S3, ylab = "Avg. Monthly Returns (%)", xlab = "Stocks", main = "Period S3")
  par(mfrow = c(1, 1))
}

# Plot bar charts of monthly variances for S1, S2 and S3
{
  par(mfrow = c(3, 1))
  barplot(diag(VS.S1), ylab = "Monthly Variance", xlab = "Stocks", main = "Period S1", ylim = c(0, 350))
  barplot(diag(VS.S2), ylab = "Monthly Variance", xlab = "Stocks", main = "Period S2", ylim = c(0, 350))
  barplot(diag(VS.S3), ylab = "Monthly Variance", xlab = "Stocks", main = "Period S3", ylim = c(0, 350))
  par(mfrow = c(1, 1))
}

# Visualize covariance matrices using heatmaps for each period
heatmap(VS.S1, Rowv = NA, Colv = NA, symm = TRUE, main = "Covariance Matrix - Period S1")
heatmap(VS.S2, Rowv = NA, Colv = NA, symm = TRUE, main = "Covariance Matrix - Period S2")
heatmap(VS.S3, Rowv = NA, Colv = NA, symm = TRUE, main = "Covariance Matrix - Period S3")

# Visualize correlation matrices using heatmaps for each period
heatmap(CR.S1, Rowv = NA, Colv = NA, symm = TRUE, main = "Correlation Matrix - Period S1")
heatmap(CR.S2, Rowv = NA, Colv = NA, symm = TRUE, main = "Correlation Matrix - Period S2")
heatmap(CR.S3, Rowv = NA, Colv = NA, symm = TRUE, main = "Correlation Matrix - Period S3")

#------------------------------------------------------------------------------------
#### Point 2 ###
#------------------------------------------------------------------------------------

# Build vector of ones and calculate scalars A, B, C, and Delta for each sample period
{
  iv <- matrix(1, nrow = N, ncol = 1)  # Vector of 1s
  
  # Scalars for the Efficient Frontier
  A.S1 <- t(mS.S1) %*% inv(VS.S1) %*% mS.S1
  A.S2 <- t(mS.S2) %*% inv(VS.S2) %*% mS.S2
  A.S3 <- t(mS.S3) %*% inv(VS.S3) %*% mS.S3
  
  B.S1 <- t(iv) %*% inv(VS.S1) %*% mS.S1
  B.S2 <- t(iv) %*% inv(VS.S2) %*% mS.S2
  B.S3 <- t(iv) %*% inv(VS.S3) %*% mS.S3
  
  C.S1 <- t(iv) %*% inv(VS.S1) %*% iv
  C.S2 <- t(iv) %*% inv(VS.S2) %*% iv
  C.S3 <- t(iv) %*% inv(VS.S3) %*% iv
  
  De.S1 <- A.S1 * C.S1 - B.S1^2
  De.S2 <- A.S2 * C.S2 - B.S2^2
  De.S3 <- A.S3 * C.S3 - B.S3^2
}

# Define the range of target returns (from minimum to 4x maximum) for each period
{
  l.S1 <- floor(min(mS.S1 * 100)) / 100
  l.S2 <- floor(min(mS.S2 * 100)) / 100
  l.S3 <- floor(min(mS.S3 * 100)) / 100
  
  u.S1 <- floor(max(mS.S1 * 100)) / 100 * 4
  u.S2 <- floor(max(mS.S2 * 100)) / 100 * 4
  u.S3 <- floor(max(mS.S3 * 100)) / 100 * 4
  
  ro.S1 <- as.matrix(seq(l.S1, u.S1, by = 0.05))
  ro.S2 <- as.matrix(seq(l.S2, u.S2, by = 0.05))
  ro.S3 <- as.matrix(seq(l.S3, u.S3, by = 0.05))
  
  M.S1 <- dim(ro.S1)
  M.S2 <- dim(ro.S2)
  M.S3 <- dim(ro.S3)
}

## Plot the efficient frontier (standard deviation vs. return) for periods S1, S2, and S3
{
  par(mfrow = c(1, 3))
  
  s.S1 <- sqrt(matrix(C.S1 / De.S1, M.S1[1], 1) * ro.S1^2 - 
                 2 * matrix(B.S1 / De.S1, M.S1[1], 1) * ro.S1 + 
                 matrix(A.S1 / De.S1, M.S1[1], 1))
  s.S2 <- sqrt(matrix(C.S2 / De.S2, M.S2[1], 1) * ro.S2^2 - 
                 2 * matrix(B.S2 / De.S2, M.S2[1], 1) * ro.S2 + 
                 matrix(A.S2 / De.S2, M.S2[1], 1))
  s.S3 <- sqrt(matrix(C.S3 / De.S3, M.S3[1], 1) * ro.S3^2 - 
                 2 * matrix(B.S3 / De.S3, M.S3[1], 1) * ro.S3 + 
                 matrix(A.S3 / De.S3, M.S3[1], 1))
  
  plot(s.S1, ro.S1, xlab = "Risk", ylab = "Return", type = "l", main = "Efficient Frontier - S1")
  plot(s.S2, ro.S2, xlab = "Risk", ylab = "Return", type = "l", main = "Efficient Frontier - S2")
  plot(s.S3, ro.S3, xlab = "Risk", ylab = "Return", type = "l", main = "Efficient Frontier - S3")
  par(mfrow = c(1, 1))
}

# Compute weights, returns, and risks of Maximum Trade-Off portfolios for each period
{
  wMT.S1 <- inv(VS.S1) %*% mS.S1
  wMT.S2 <- inv(VS.S2) %*% mS.S2
  wMT.S3 <- inv(VS.S3) %*% mS.S3
  
  # Normalize weights
  wMT.S1 <- wMT.S1 / sum(wMT.S1)
  wMT.S2 <- wMT.S2 / sum(wMT.S2)
  wMT.S3 <- wMT.S3 / sum(wMT.S3)
  
  # Expected returns
  rMT.S1 <- t(wMT.S1) %*% mS.S1
  rMT.S2 <- t(wMT.S2) %*% mS.S2
  rMT.S3 <- t(wMT.S3) %*% mS.S3
  
  # Portfolio risks (variances)
  sMT.S1 <- t(wMT.S1) %*% VS.S1 %*% wMT.S1
  sMT.S2 <- t(wMT.S2) %*% VS.S2 %*% wMT.S2
  sMT.S3 <- t(wMT.S3) %*% VS.S3 %*% wMT.S3
}

# Visualize asset weights in Maximum Trade-Off portfolios (S1, S2, S3)
barplot(t(wMT.S1), las = 2, main = "Weights of Maximum Trade-Off Portfolio - S1")
barplot(t(wMT.S2), las = 2, main = "Weights of Maximum Trade-Off Portfolio - S2")
barplot(t(wMT.S3), las = 2, main = "Weights of Maximum Trade-Off Portfolio - S3")

# Compute weights, returns, and risks of Global Minimum Variance portfolios for each period
{
  wGMV.S1 <- inv(VS.S1) %*% iv
  wGMV.S2 <- inv(VS.S2) %*% iv
  wGMV.S3 <- inv(VS.S3) %*% iv
  
  wGMV.S1 <- wGMV.S1 / sum(wGMV.S1)
  wGMV.S2 <- wGMV.S2 / sum(wGMV.S2)
  wGMV.S3 <- wGMV.S3 / sum(wGMV.S3)
  
  rGMV.S1 <- t(wGMV.S1) %*% mS.S1
  rGMV.S2 <- t(wGMV.S2) %*% mS.S2
  rGMV.S3 <- t(wGMV.S3) %*% mS.S3
  
  sGMV.S1 <- t(wGMV.S1) %*% VS.S1 %*% wGMV.S1
  sGMV.S2 <- t(wGMV.S2) %*% VS.S2 %*% wGMV.S2
  sGMV.S3 <- t(wGMV.S3) %*% VS.S3 %*% wGMV.S3
}

# Visualize asset weights in Global Minimum Variance portfolios (S1, S2, S3)
barplot(t(wGMV.S1), las = 2, main = "Weights of Global Minimum Variance Portfolio - S1")
barplot(t(wGMV.S2), las = 2, main = "Weights of Global Minimum Variance Portfolio - S2")
barplot(t(wGMV.S3), las = 2, main = "Weights of Global Minimum Variance Portfolio - S3")

# Overlay efficient frontier with Max Trade-Off, Min Variance, and Market portfolios
{
  par(mfrow = c(1, 3))
  plot(s.S1, ro.S1, xlab = "Risk", ylab = "Return", type = "l", main = "S1")
  points(sqrt(sMT.S1), rMT.S1, pch = 19, col = "blue", lwd = 5)
  points(sqrt(sGMV.S1), rGMV.S1, pch = 19, col = "orange", lwd = 5)
  legend("topleft", legend = c("Max Trade-Off", "Global Min Variance"), col = c("blue", "orange"), pch = 1)
  
  plot(s.S2, ro.S2, xlab = "Risk", ylab = "Return", type = "l", main = "S2")
  points(sqrt(sMT.S2), rMT.S2, pch = 19, col = "blue", lwd = 5)
  points(sqrt(sGMV.S2), rGMV.S2, pch = 19, col = "orange", lwd = 5)
  
  plot(s.S3, ro.S3, xlab = "Risk", ylab = "Return", type = "l", main = "S3")
  points(sqrt(sMT.S3), rMT.S3, pch = 19, col = "blue", lwd = 5)
  points(sqrt(sGMV.S3), rGMV.S3, pch = 19, col = "orange", lwd = 5)
  par(mfrow = c(1, 1))
}

# Load market portfolio price data
prezzi.mercato.S1 <- read_xlsx("DatiHW3.xlsx", sheet = 2, range = "A19:B299", col_names = FALSE)

# Format market price data
{
  colnames(prezzi.mercato.S1) <- c("Date", "Prices")
  prezzi.mercato.S1$Date <- as.Date(prezzi.mercato.S1$Date)
  
  prezzi.mercato.S2 <- prezzi.mercato.S1[1:205, ]
  prezzi.mercato.S3 <- prezzi.mercato.S1[206:289, ]
}

# Compute market log-returns
{
  r.mercato.S1 <- data.frame(prezzi.mercato.S1$Date[2:281], diff(prezzi.mercato.S1$Prices))
  colnames(r.mercato.S1) <- c("Date", "Returns")
  
  r.mercato.S2 <- data.frame(prezzi.mercato.S2$Date[2:205], diff(prezzi.mercato.S2$Prices))
  colnames(r.mercato.S2) <- c("Date", "Returns")
  
  r.mercato.S3 <- data.frame(prezzi.mercato.S3$Date[2:84], diff(prezzi.mercato.S3$Prices))
  colnames(r.mercato.S3) <- c("Date", "Returns")
}

# Compute market return and risk for each period
{
  r.market.S1 <- mean(r.mercato.S1$Returns)
  r.market.S2 <- mean(r.mercato.S2$Returns)
  r.market.S3 <- mean(r.mercato.S3$Returns, na.rm = TRUE)
  
  s.market.S1 <- sd(r.mercato.S1$Returns)
  s.market.S2 <- sd(r.mercato.S2$Returns)
  s.market.S3 <- sd(r.mercato.S3$Returns, na.rm = TRUE)
}

# Plot Efficient Frontiers with Market Portfolio
{
  par(mfrow = c(1, 3))
  
  plot(s.S1, ro.S1, xlab = "Risk", ylab = "Return", type = "l", main = "S1")
  points(sqrt(sMT.S1), rMT.S1, pch = 19, col = "blue", lwd = 5)
  points(sqrt(sGMV.S1), rGMV.S1, pch = 19, col = "orange", lwd = 5)
  points(s.market.S1, r.market.S1, pch = 19, col = "violet", lwd = 5)
  legend("topleft", legend = c("Max Trade-Off", "Min Variance", "Market"), col = c("blue", "orange", "violet"), pch = 1)
  
  plot(s.S2, ro.S2, xlab = "Risk", ylab = "Return", type = "l", main = "S2")
  points(sqrt(sMT.S2), rMT.S2, pch = 19, col = "blue", lwd = 5)
  points(sqrt(sGMV.S2), rGMV.S2, pch = 19, col = "orange", lwd = 5)
  points(s.market.S2, r.market.S2, pch = 19, col = "violet", lwd = 5)
  
  plot(s.S3, ro.S3, xlab = "Risk", ylab = "Return", type = "l", main = "S3")
  points(sqrt(sMT.S3), rMT.S3, pch = 19, col = "blue", lwd = 5)
  points(sqrt(sGMV.S3), rGMV.S3, pch = 19, col = "orange", lwd = 5)
  points(s.market.S3, r.market.S3, pch = 19, col = "violet", lwd = 5)
  
  par(mfrow = c(1, 1))
}

#------------------------------------------------------------------------------------
#### Point 3 ###
#------------------------------------------------------------------------------------

# Convert log-returns to xts format for portfolio optimization (S1, S2, S3)
{
  r.S1 <- xts(r.S1, order.by = prezzi.S1$Date[-1], frequency="monthly")
  r.S2 <- xts(r.S2, order.by = prezzi.S2$Date[-1], frequency="monthly")
  r.S3 <- xts(r.S3, order.by = prezzi.S3$Date[-1], frequency="monthly")
}

# Specify portfolio constraints (unconstrained vs. long-only) for each period
{
  # Unconstrained portfolios
  unc.portf.S1 <- portfolio.spec(assets=colnames(r.S1))
  unc.portf.S2 <- portfolio.spec(assets=colnames(r.S2))
  unc.portf.S3 <- portfolio.spec(assets=colnames(r.S3))
  unc.portf.S1 <- add.constraint(portfolio=unc.portf.S1, type="full_investment")
  unc.portf.S2 <- add.constraint(portfolio=unc.portf.S2, type="full_investment")
  unc.portf.S3 <- add.constraint(portfolio=unc.portf.S3, type="full_investment")
  # Constrained portfolios (only positive weights)
  lo.portf.S1 <- add.constraint(portfolio=unc.portf.S1 ,type="long_only")
  lo.portf.S2 <- add.constraint(portfolio=unc.portf.S2 ,type="long_only")
  lo.portf.S3 <- add.constraint(portfolio=unc.portf.S3 ,type="long_only")
  portf.list.S1 <- combine.portfolios(list( unc.portf.S1, lo.portf.S1 ) )
  portf.list.S2 <- combine.portfolios(list( unc.portf.S2, lo.portf.S2 ) )
  portf.list.S3 <- combine.portfolios(list( unc.portf.S3, lo.portf.S3 ) )
}

# Plot both unconstrained and long-only efficient frontiers side by side for each of the three periods
{
  par(mfrow=c(1,3))
  legend.labels <- c("Unconstrained", "Positive Weights Only")
  chart.EfficientFrontierOverlay(R=r.S1 , portfolio_list = portf.list.S1 , type ="mean-StdDev",
                                 match.col="StdDev", legend.loc="topleft",legend.labels= legend.labels, 
                                 cex.legend=0.6,labels.assets=FALSE, pch.assets=18, main="Efficient Frontier - S1")
  chart.EfficientFrontierOverlay(R=r.S2 , portfolio_list = portf.list.S2 , type ="mean-StdDev",
                                 match.col="StdDev", legend.loc="topleft",legend.labels= legend.labels, 
                                 cex.legend=0.6,labels.assets=FALSE, pch.assets=18, main="Efficient Frontier - S2")
  chart.EfficientFrontierOverlay(R=r.S3 , portfolio_list = portf.list.S3 , type ="mean-StdDev",
                                 match.col="StdDev", legend.loc="topleft",legend.labels= legend.labels, 
                                 cex.legend=0.6,labels.assets=FALSE, pch.assets=18, main="Efficient Frontier - S3")
  par(mfrow=c(1,1))
}

# Create efficient frontiers under the long-only (positive weights) constraint
fe.vinc.S1 <- create.EfficientFrontier(R=r.S1, portfolio = lo.portf.S1, type="mean-StdDev")
fe.vinc.S2 <- create.EfficientFrontier(R=r.S2, portfolio = lo.portf.S2, type="mean-StdDev")
fe.vinc.S3 <- create.EfficientFrontier(R=r.S3, portfolio = lo.portf.S3, type="mean-StdDev")

# Identify Global Minimum Variance portfolios with long-only constraint
# With positive weight constraint for the three considered periods
{
  # Standard deviation
  sd.gmv.vinc.S1 <- min(fe.vinc.S1$frontier[,2])
  sd.gmv.vinc.S2 <- min(fe.vinc.S2$frontier[,2])
  sd.gmv.vinc.S3 <- min(fe.vinc.S3$frontier[,2])
  # Return
  mu.gmv.vinc.S1 <- fe.vinc.S1$frontier[fe.vinc.S1$frontier[,2]==sd.gmv.vinc.S1,1]
  mu.gmv.vinc.S2 <- fe.vinc.S2$frontier[fe.vinc.S2$frontier[,2]==sd.gmv.vinc.S2,1]
  mu.gmv.vinc.S3 <- fe.vinc.S3$frontier[fe.vinc.S3$frontier[,2]==sd.gmv.vinc.S3,1]
}

# Identify Maximum Trade-Off portfolios with long-only constraint
# With positive weight constraint for the three considered periods
{
  # Find the point on the efficient frontier where the return/risk ratio is the highest
  id.S1 <- which.max(fe.vinc.S1$frontier[,1]/fe.vinc.S1$frontier[,2])
  id.S2 <- which.max(fe.vinc.S2$frontier[,1]/fe.vinc.S2$frontier[,2])
  id.S3 <- which.max(fe.vinc.S3$frontier[,1]/fe.vinc.S3$frontier[,2])
  # Standard deviation
  sd.to.vinc.S1 <- fe.vinc.S1$frontier[id.S1,2]
  sd.to.vinc.S2 <- fe.vinc.S2$frontier[id.S2,2]
  sd.to.vinc.S3 <- fe.vinc.S3$frontier[id.S3,2]
  # Return
  mu.to.vinc.S1 <- fe.vinc.S1$frontier[id.S1,1]
  mu.to.vinc.S2 <- fe.vinc.S2$frontier[id.S2,1]
  mu.to.vinc.S3 <- fe.vinc.S3$frontier[id.S3,1]
}

# Plot of the two efficient frontiers for each of the three periods
{
  par(mfrow=c(1,3))
  legend.labels <- c("Unconstrained", "Positive Weights Only")
  chart.EfficientFrontierOverlay(R=r.S1 , portfolio_list = portf.list.S1 , type ="mean-StdDev",
                                 match.col="StdDev", legend.loc="topleft",legend.labels= legend.labels, 
                                 cex.legend=0.6,labels.assets=FALSE, pch.assets=18, main="Efficient Frontier - S1")
  points(sd.to.vinc.S1,mu.to.vinc.S1,pch=19,col="blue",bg="blue", lwd=5)
  points(sd.gmv.vinc.S1,mu.gmv.vinc.S1,pch=19,col="orange",bg="orange", lwd=5)
  legend("bottomright", legend=c("Maximum Trade-off", "Global Minimum Variance"), col=c("blue","orange"), pch=1)
  chart.EfficientFrontierOverlay(R=r.S2 , portfolio_list = portf.list.S2 , type ="mean-StdDev",
                                 match.col="StdDev", legend.loc="topleft",legend.labels= legend.labels, 
                                 cex.legend=0.6,labels.assets=FALSE, pch.assets=18, main="Efficient Frontier - S2")
  points(sd.to.vinc.S2,mu.to.vinc.S2,pch=19,col="blue",bg="blue", lwd=5)
  points(sd.gmv.vinc.S2,mu.gmv.vinc.S2,pch=19,col="orange",bg="orange", lwd=5)
  legend("bottomright", legend=c("Maximum Trade-off", "Global Minimum Variance"), col=c("blue","orange"), pch=1)
  chart.EfficientFrontierOverlay(R=r.S3 , portfolio_list = portf.list.S3 , type ="mean-StdDev",
                                 match.col="StdDev", legend.loc="topleft",legend.labels= legend.labels, 
                                 cex.legend=0.6,labels.assets=FALSE, pch.assets=18, main="Efficient Frontier - S3")
  points(sd.to.vinc.S3,mu.to.vinc.S3,pch=19,col="blue",bg="blue", lwd=5)
  points(sd.gmv.vinc.S3,mu.gmv.vinc.S3,pch=19,col="orange",bg="orange", lwd=5)
  legend("bottomright", legend=c("Maximum Trade-off", "Global Minimum Variance"), col=c("blue","orange"), pch=1)
  par(mfrow=c(1,1))
}

# Extract asset weights for constrained portfolios (GMV and Max Trade-Off)
{
  # Global minimum variance
  w.gmv.vinc.S1 <- fe.vinc.S1$frontier[1,4:23]
  w.gmv.vinc.S2 <- fe.vinc.S2$frontier[1,4:23]
  w.gmv.vinc.S3 <- fe.vinc.S3$frontier[1,4:23]
  # Maximum trade-off
  w.to.vinc.S1 <- fe.vinc.S1$frontier[id.S1,4:23]
  w.to.vinc.S2 <- fe.vinc.S2$frontier[id.S2,4:23]
  w.to.vinc.S3 <- fe.vinc.S3$frontier[id.S3,4:23]
}

# Compare GMV portfolio weights across constrained and unconstrained settings
# for the three considered periods
{
  par(mfrow=c(2,3))
  barplot(t(wGMV.S1), las=2, main="S1 - Unconstrained")
  barplot(t(wGMV.S2), las=2, main="S2 - Unconstrained")
  barplot(t(wGMV.S3), las=2, main="S3 - Unconstrained")
  barplot(t(w.gmv.vinc.S1), las=2, main="S1 - Constrained")
  barplot(t(w.gmv.vinc.S2), las=2, main="S2 - Constrained")
  barplot(t(w.gmv.vinc.S3), las=2, main="S3 - Constrained")
}

# Compare Max Trade-Off portfolio weights across constrained and unconstrained settings
# for the three considered periods
{
  par(mfrow=c(2,3))
  barplot(t(wMT.S1), las=2, main="S1 - Unconstrained")
  barplot(t(wMT.S2), las=2, main="S2 - Unconstrained")
  barplot(t(wMT.S3), las=2, main="S3 - Unconstrained")
  barplot(t(w.to.vinc.S1), las=2, main="S1 - Constrained")
  barplot(t(w.to.vinc.S2), las=2, main="S2 - Constrained")
  barplot(t(w.to.vinc.S3), las=2, main="S3 - Constrained")
}

#------------------------------------------------------------------------------------
#### Point 4 ###
#------------------------------------------------------------------------------------
# Load and process the risk-free rate (3M Euribor) from Excel
{
  risk.free <- read_xlsx("DatiHW3.xlsx",sheet=3, range="A4:B299")
  risk.free <- risk.free[3:295,]
  risk.free <- rename(risk.free, "Date" = "Name")
  risk.free$Date <- as.integer(risk.free$Date)
  risk.free$Date <- as.Date(risk.free$Date, origin = "1899-12-30")
  risk.free <- data.frame(risk.free$Date, risk.free$`EBF EURIBOR 3M DELAYED - OFFERED RATE`)
  colnames(risk.free) <- c("Date", "Risk-Free Rate")
  risk.free[,2] <- as.numeric(risk.free[,2])
  rf.ss = xts(risk.free[,-1], order.by = risk.free$Date, frequency = "monthly")
  rf.ss <- rf.ss / 100 
  # Replace negative risk-free rates with 0.48% annually, as per project guidelines
  rf.ss[which(rf.ss < 0)] <- 0.0048
  # Convert annualized rates to monthly values
  rf.ss <- rf.ss / 12
  # Select the latest available risk-free rate for each period (S1, S2, S3)
  rf.S1 <- as.numeric(rf.ss[293])
  rf.S2 <- as.numeric(rf.ss[205])
  rf.S3 <- as.numeric(rf.ss[289])
}

{
  ro1.S1 <- as.matrix(seq(rf.S1, u.S1, by = 0.1)) # New min and max sequence
  ro1.S2 <- as.matrix(seq(rf.S2, u.S2, by = 0.1))
  ro1.S3 <- as.matrix(seq(rf.S3, u.S3, by = 0.1))
  sh.S1 <- sqrt(A.S1 - 2 * B.S1 * rf.S1 + C.S1 * rf.S1 * rf.S1)
  sh.S2 <- sqrt(A.S2 - 2 * B.S2 * rf.S2 + C.S2 * rf.S2 * rf.S2)
  sh.S3 <- sqrt(A.S3 - 2 * B.S3 * rf.S3 + C.S3 * rf.S3 * rf.S3)
  s1.S1 <- (ro1.S1 - rf.S1) %*% (sh.S1^(-1))
  s1.S2 <- (ro1.S2 - rf.S2) %*% (sh.S2^(-1))
  s1.S3 <- (ro1.S3 - rf.S3) %*% (sh.S3^(-1))
}

# Compute weights, returns, and risk of the tangency portfolio for each period
{
  # Weights
  wT.S1 <- inv(VS.S1) %*% (mS.S1 - rf.S1) 
  wT.S2 <- inv(VS.S2) %*% (mS.S2 - rf.S2)
  wT.S3 <- inv(VS.S3) %*% (mS.S3 - rf.S3)
  wT.S1 <- wT.S1 / sum(wT.S1)
  wT.S2 <- wT.S2 / sum(wT.S2)
  wT.S3 <- wT.S3 / sum(wT.S3)
  # Return
  rT.S1 <- t(wT.S1) %*% mS.S1
  rT.S2 <- t(wT.S2) %*% mS.S2
  rT.S3 <- t(wT.S3) %*% mS.S3
  # Risk
  sT.S1 <- t(wT.S1) %*% VS.S1 %*% wT.S1
  sT.S2 <- t(wT.S2) %*% VS.S2 %*% wT.S2
  sT.S3 <- t(wT.S3) %*% VS.S3 %*% wT.S3
}

# Plot efficient frontiers with risk-free asset and highlight tangency portfolio for the three periods considered
{
  par(mfrow=c(1,3))
  plot(s1.S1, ro1.S1, type="l", col="red", xlab="Risk", ylab="Return", main="S1")
  lines(s.S1, ro.S1, xlab="Risk", ylab="Return", type="l")
  points(sqrt(sT.S1), rT.S1, pch=19, col="black", bg="black", lwd=4)
  legend("topleft", legend=c("EF with rf", "EF without rf", "Tangency Portfolio"), lty=c(1,1,NA), pch=c(NA,NA,1), col=c("red","black","black"))
  
  plot(s1.S2, ro1.S2, type="l", col="red", xlab="Risk", ylab="Return", main="S2")
  lines(s.S2, ro.S2, xlab="Risk", ylab="Return", type="l")
  points(sqrt(sT.S2), rT.S2, pch=19, col="black", bg="black", lwd=4)
  
  plot(s1.S3, ro1.S3, type="l", col="red", xlab="Risk", ylab="Return", main="S3")
  lines(s.S3, ro.S3, xlab="Risk", ylab="Return", type="l")
  points(sqrt(sT.S3), rT.S3, pch=19, col="black", bg="black", lwd=4)
  
  par(mfrow=c(1,1))
}

# Bar plot of asset weights in tangency portfolio for S1, S2, and S3
{
  par(mfrow=c(3,1))
  barplot(t(wT.S1), las=2, main="Tangency Portfolio Weights - S1")
  barplot(t(wT.S2), las=2, main="Tangency Portfolio Weights - S2")
  barplot(t(wT.S3), las=2, main="Tangency Portfolio Weights - S3")
  par(mfrow=c(1,1))
}

# Define investor risk aversion levels: 3 (high), 1 (medium), 0.1 (low) as per assignment
{
  ra1 <- 3
  ra2 <- 1
  ra3 <- 0.1
}

# Compute optimal portfolios based on different risk aversion levels (without risk-free)
{
  # Weights
  wa1.S1 <- wMT.S1 %*% (B.S1 / ra1) - wGMV.S1 %*% ((B.S1 - ra1) / ra1)
  wa1.S2 <- wMT.S2 %*% (B.S2 / ra1) - wGMV.S2 %*% ((B.S2 - ra1) / ra1)
  wa1.S3 <- wMT.S3 %*% (B.S3 / ra1) - wGMV.S3 %*% ((B.S3 - ra1) / ra1)
  wa2.S1 <- wMT.S1 %*% (B.S1 / ra2) - wGMV.S1 %*% ((B.S1 - ra2) / ra2)
  wa2.S2 <- wMT.S2 %*% (B.S2 / ra2) - wGMV.S2 %*% ((B.S2 - ra2) / ra2)
  wa2.S3 <- wMT.S3 %*% (B.S3 / ra2) - wGMV.S3 %*% ((B.S3 - ra2) / ra2)
  wa3.S1 <- wMT.S1 %*% (B.S1 / ra3) - wGMV.S1 %*% ((B.S1 - ra3) / ra3)
  wa3.S2 <- wMT.S2 %*% (B.S2 / ra3) - wGMV.S2 %*% ((B.S2 - ra3) / ra3)
  wa3.S3 <- wMT.S3 %*% (B.S3 / ra3) - wGMV.S3 %*% ((B.S3 - ra3) / ra3)
  
  # Returns
  rp1.S1 <- t(wa1.S1) %*% mS.S1
  rp1.S2 <- t(wa1.S2) %*% mS.S2
  rp1.S3 <- t(wa1.S3) %*% mS.S3
  rp2.S1 <- t(wa2.S1) %*% mS.S1
  rp2.S2 <- t(wa2.S2) %*% mS.S2
  rp2.S3 <- t(wa2.S3) %*% mS.S3
  rp3.S1 <- t(wa3.S1) %*% mS.S1
  rp3.S2 <- t(wa3.S2) %*% mS.S2
  rp3.S3 <- t(wa3.S3) %*% mS.S3
  
  # Risk
  sp1.S1 <- t(wa1.S1) %*% VS.S1 %*% wa1.S1
  sp1.S2 <- t(wa1.S2) %*% VS.S2 %*% wa1.S2
  sp1.S3 <- t(wa1.S3) %*% VS.S3 %*% wa1.S3
  sp2.S1 <- t(wa2.S1) %*% VS.S1 %*% wa2.S1
  sp2.S2 <- t(wa2.S2) %*% VS.S2 %*% wa2.S2
  sp2.S3 <- t(wa2.S3) %*% VS.S3 %*% wa2.S3
  sp3.S1 <- t(wa3.S1) %*% VS.S1 %*% wa3.S1
  sp3.S2 <- t(wa3.S2) %*% VS.S2 %*% wa3.S2
  sp3.S3 <- t(wa3.S3) %*% VS.S3 %*% wa3.S3
}

# Plot optimal portfolios on efficient frontier for different risk aversion levels (no risk-free)
{
  par(mfrow=c(1,3))
  plot(s.S1, ro.S1, xlab="Risk", ylab="Return", type="l", main="S1")
  points(sqrt(sp1.S1), rp1.S1, pch=19, col="blue", bg="blue", lwd=4)
  points(sqrt(sp2.S1), rp2.S1, pch=19, col="brown", bg="brown", lwd=4)
  points(sqrt(sp3.S1), rp3.S1, pch=19, col="orange", bg="orange", lwd=4)
  legend("topleft", col=c("blue", "brown", "orange"), 
         legend=c("risk aversion 3", "risk aversion 1", "risk aversion 0.1"),
         pch=rep(1,3))
  
  plot(s.S2, ro.S2, xlab="Risk", ylab="Return", type="l", main="S2")
  points(sqrt(sp1.S2), rp1.S2, pch=19, col="blue", bg="blue", lwd=4)
  points(sqrt(sp2.S2), rp2.S2, pch=19, col="brown", bg="brown", lwd=4)
  points(sqrt(sp3.S2), rp3.S2, pch=19, col="orange", bg="orange", lwd=4)
  
  plot(s.S3, ro.S3, xlab="Risk", ylab="Return", type="l", main="S3")
  points(sqrt(sp1.S3), rp1.S3, pch=19, col="blue", bg="blue", lwd=4)
  points(sqrt(sp2.S3), rp2.S3, pch=19, col="brown", bg="brown", lwd=4)
  points(sqrt(sp3.S3), rp3.S3, pch=19, col="orange", bg="orange", lwd=4)
  
  par(mfrow=c(1,1))
}

# Compute weights of risky assets in optimal portfolios with a risk-free asset and risk aversion
# in the three considered periods
{
  w.ot.ra1.S1 <- as.numeric((B.S1 - C.S1 * rf.S1) / ra1) * wMT.S1 # ra==3
  w.ot.ra2.S1 <- as.numeric((B.S1 - C.S1 * rf.S1) / ra2) * wMT.S1 # ra==1
  w.ot.ra3.S1 <- as.numeric((B.S1 - C.S1 * rf.S1) / ra3) * wMT.S1 # ra==0.1
  w.ot.ra1.S2 <- as.numeric((B.S2 - C.S2 * rf.S2) / ra1) * wMT.S2
  w.ot.ra2.S2 <- as.numeric((B.S2 - C.S2 * rf.S2) / ra2) * wMT.S2
  w.ot.ra3.S2 <- as.numeric((B.S2 - C.S2 * rf.S2) / ra3) * wMT.S2
  w.ot.ra1.S3 <- as.numeric((B.S3 - C.S3 * rf.S3) / ra1) * wMT.S3
  w.ot.ra2.S3 <- as.numeric((B.S3 - C.S3 * rf.S3) / ra2) * wMT.S3
  w.ot.ra3.S3 <- as.numeric((B.S3 - C.S3 * rf.S3) / ra3) * wMT.S3
}

  # Calculate expected return and standard deviation for optimal portfolios with risk-free assete and risk aversion
# in the three considered periods
{
  r.ra1.S1.ot <- as.numeric(rT.S1 * sum(w.ot.ra1.S1))
  r.ra2.S1.ot <- as.numeric(rT.S1 * sum(w.ot.ra2.S1))
  r.ra3.S1.ot <- as.numeric(rT.S1 * sum(w.ot.ra3.S1))
  r.ra1.S2.ot <- as.numeric(rT.S2 * sum(w.ot.ra1.S2))
  r.ra2.S2.ot <- as.numeric(rT.S2 * sum(w.ot.ra2.S2))
  r.ra3.S2.ot <- as.numeric(rT.S2 * sum(w.ot.ra3.S2))
  r.ra1.S3.ot <- as.numeric(rT.S3 * sum(w.ot.ra1.S3))
  r.ra2.S3.ot <- as.numeric(rT.S3 * sum(w.ot.ra2.S3))
  r.ra3.S3.ot <- as.numeric(rT.S3 * sum(w.ot.ra3.S3))
  
  s.ra1.S1.ot <- sqrt(sum(w.ot.ra1.S1) * VS.S1 %*% w.ot.ra1.S1)
  s.ra2.S1.ot <- sqrt(sum(w.ot.ra2.S1) * VS.S1 %*% w.ot.ra2.S1)
  s.ra3.S1.ot <- sqrt(sum(w.ot.ra3.S1) * VS.S1 %*% w.ot.ra3.S1)
  s.ra1.S2.ot <- sqrt(sum(w.ot.ra1.S2) * VS.S2 %*% w.ot.ra1.S2)
  s.ra2.S2.ot <- sqrt(sum(w.ot.ra2.S2) * VS.S2 %*% w.ot.ra2.S2)
  s.ra3.S2.ot <- sqrt(sum(w.ot.ra3.S2) * VS.S2 %*% w.ot.ra3.S2)
  s.ra1.S3.ot <- sqrt(sum(w.ot.ra1.S3) * VS.S3 %*% w.ot.ra1.S3)
  s.ra2.S3.ot <- sqrt(sum(w.ot.ra2.S3) * VS.S3 %*% w.ot.ra2.S3)
  s.ra3.S3.ot <- sqrt(sum(w.ot.ra3.S3) * VS.S3 %*% w.ot.ra3.S3)
}

#------------------------------------------------------------------------------------
#### Point 5 ###
#------------------------------------------------------------------------------------
# Significance of the Sharpe Ratio on the Tangency Portfolio for the Three Periods Considered
{
  shT.S1 <- (rT.S1 - rf.S1) / sqrt(sT.S1)
  shT.S2 <- (rT.S2 - rf.S2) / sqrt(sT.S2)
  shT.S3 <- (rT.S3 - rf.S3) / sqrt(sT.S3)
  varshT.S1 <- (1 / T.S1) * (1 + 0.5 * shT.S1^2)  
  varshT.S2 <- (1 / T.S2) * (1 + 0.5 * shT.S2^2)
  varshT.S3 <- (1 / T.S3) * (1 + 0.5 * shT.S3^2)
  tshT.S1 <- shT.S1 / sqrt(varshT.S1)
  tshT.S2 <- shT.S2 / sqrt(varshT.S2)
  tshT.S3 <- shT.S3 / sqrt(varshT.S3)
  stat.test <- cbind(tshT.S1, tshT.S2, tshT.S3)
  rownames(stat.test) <- c("Sharpe")
  colnames(stat.test) <- c("S1", "S2", "S3")
  stat.test
}
# All the test statistics for different groups are in absolute values greater than 1.96, so we reject
# the null hypothesis of the nullity of the Sharpe ratio. This means that the slope of the efficient frontier
# is significant.

# Significance of the Sharpe Ratio on the Global Minimum Variance Portfolio for the Three Periods Considered
{
  shGMV.S1 <- (rGMV.S1 - rf.S1) / sqrt(sGMV.S1)
  shGMV.S2 <- (rGMV.S2 - rf.S2) / sqrt(sGMV.S2)
  shGMV.S3 <- (rGMV.S3 - rf.S3) / sqrt(sGMV.S3)
  varshGMV.S1 <- (1 / T.S1) * (1 + 0.5 * shGMV.S1^2)  
  varshGMV.S2 <- (1 / T.S2) * (1 + 0.5 * shGMV.S2^2)
  varshGMV.S3 <- (1 / T.S3) * (1 + 0.5 * shGMV.S3^2)
  tshGMV.S1 <- shGMV.S1 / sqrt(varshGMV.S1)
  tshGMV.S2 <- shGMV.S2 / sqrt(varshGMV.S2)
  tshGMV.S3 <- shGMV.S3 / sqrt(varshGMV.S3)
  stat.test <- cbind(tshGMV.S1, tshGMV.S2, tshGMV.S3)
  rownames(stat.test) <- c("Sharpe")
  colnames(stat.test) <- c("S1", "S2", "S3")
  stat.test
}
# For the sample containing all data and the sample containing the last 6 years of data, we are inclined to accept
# the null hypothesis of the nullity of the Sharpe ratio. However, for the data sample up until 2016, we reject 
# the null hypothesis (H0).

#------------------------------------------------------------------------------------
#### Point 6 ###
#------------------------------------------------------------------------------------
# Covariance matrices of the assets divided by the three periods and the 4 sectors
{
  # Energy Sector
  VS.En.S1 <- VS.S1[1:5,1:5]
  VS.En.S2 <- VS.S2[1:5,1:5] 
  VS.En.S3 <- VS.S3[1:5,1:5]
  # Technology Sector
  VS.Te.S1 <- VS.S1[6:10,6:10]
  VS.Te.S2 <- VS.S2[6:10,6:10] 
  VS.Te.S3 <- VS.S3[6:10,6:10]
  # Financials Sector
  VS.Fi.S1 <- VS.S1[11:15,11:15]
  VS.Fi.S2 <- VS.S2[11:15,11:15] 
  VS.Fi.S3 <- VS.S3[11:15,11:15]
  # Real Estate Sector
  VS.RE.S1 <- VS.S1[16:20,16:20]
  VS.RE.S2 <- VS.S2[16:20,16:20] 
  VS.RE.S3 <- VS.S3[16:20,16:20]
}

# Vector of average monthly returns of the assets divided by the three periods and the 4 sectors
{
  # Energy Sector
  mS.En.S1 <- m.S1[1:5,1]
  mS.En.S2 <- m.S2[1:5,1]
  mS.En.S3 <- m.S3[1:5,1]
  # Technology Sector
  mS.Te.S1 <- m.S1[6:10,1]
  mS.Te.S2 <- m.S2[6:10,1]
  mS.Te.S3 <- m.S3[6:10,1]
  # Financials Sector
  mS.Fi.S1 <- m.S1[11:15,1]
  mS.Fi.S2 <- m.S2[11:15,1]
  mS.Fi.S3 <- m.S3[11:15,1]
  # Real Estate Sector
  mS.RE.S1 <- m.S1[16:20,1]
  mS.RE.S2 <- m.S2[16:20,1]
  mS.RE.S3 <- m.S3[16:20,1]
}

# Calculation of weights, returns, and volatilities of the maximum trade-off portfolios in the three periods for the 4 sectors
{
  # Energy Sector
  wMT.En.S1 <- inv(VS.En.S1) %*% mS.En.S1
  wMT.En.S2 <- inv(VS.En.S2) %*% mS.En.S2
  wMT.En.S3 <- inv(VS.En.S3) %*% mS.En.S3
  wMT.En.S1 <- wMT.En.S1 / sum(wMT.En.S1) # Weights of individual assets in the Maximum Trade-Off portfolio
  wMT.En.S2 <- wMT.En.S2 / sum(wMT.En.S2)
  wMT.En.S3 <- wMT.En.S3 / sum(wMT.En.S3)
  rMT.En.S1 <- t(wMT.En.S1) %*% mS.En.S1 # Returns of the Maximum Trade-Off portfolios
  rMT.En.S2 <- t(wMT.En.S2) %*% mS.En.S2
  rMT.En.S3 <- t(wMT.En.S3) %*% mS.En.S3
  sMT.En.S1 <- t(wMT.En.S1) %*% VS.En.S1 %*% wMT.En.S1 # Risk of the Maximum Trade-Off portfolios
  sMT.En.S2 <- t(wMT.En.S2) %*% VS.En.S2 %*% wMT.En.S2
  sMT.En.S3 <- t(wMT.En.S3) %*% VS.En.S3 %*% wMT.En.S3
}
{
  # Technology Sector
  wMT.Te.S1 <- inv(VS.Te.S1) %*% mS.Te.S1
  wMT.Te.S2 <- inv(VS.Te.S2) %*% mS.Te.S2
  wMT.Te.S3 <- inv(VS.Te.S3) %*% mS.Te.S3
  wMT.Te.S1 <- wMT.Te.S1 / sum(wMT.Te.S1) # Weights of individual assets in the Maximum Trade-Off portfolio
  wMT.Te.S2 <- wMT.Te.S2 / sum(wMT.Te.S2)
  wMT.Te.S3 <- wMT.Te.S3 / sum(wMT.Te.S3)
  rMT.Te.S1 <- t(wMT.Te.S1) %*% mS.Te.S1 # Returns of the Maximum Trade-Off portfolios
  rMT.Te.S2 <- t(wMT.Te.S2) %*% mS.Te.S2
  rMT.Te.S3 <- t(wMT.Te.S3) %*% mS.Te.S3
  sMT.Te.S1 <- t(wMT.Te.S1) %*% VS.Te.S1 %*% wMT.Te.S1 # Risk of the Maximum Trade-Off portfolios
  sMT.Te.S2 <- t(wMT.Te.S2) %*% VS.Te.S2 %*% wMT.Te.S2
  sMT.Te.S3 <- t(wMT.Te.S3) %*% VS.Te.S3 %*% wMT.Te.S3
}
{
  # Financials Sector
  wMT.Fi.S1 <- inv(VS.Fi.S1) %*% mS.Fi.S1
  wMT.Fi.S2 <- inv(VS.Fi.S2) %*% mS.Fi.S2
  wMT.Fi.S3 <- inv(VS.Fi.S3) %*% mS.Fi.S3
  wMT.Fi.S1 <- wMT.Fi.S1 / sum(wMT.Fi.S1) # Weights of individual assets in the Maximum Trade-Off portfolio
  wMT.Fi.S2 <- wMT.Fi.S2 / sum(wMT.Fi.S2)
  wMT.Fi.S3 <- wMT.Fi.S3 / sum(wMT.Fi.S3)
  rMT.Fi.S1 <- t(wMT.Fi.S1) %*% mS.Fi.S1 # Returns of the Maximum Trade-Off portfolios
  rMT.Fi.S2 <- t(wMT.Fi.S2) %*% mS.Fi.S2
  rMT.Fi.S3 <- t(wMT.Fi.S3) %*% mS.Fi.S3
  sMT.Fi.S1 <- t(wMT.Fi.S1) %*% VS.Fi.S1 %*% wMT.Fi.S1 # Risk of the Maximum Trade-Off portfolios
  sMT.Fi.S2 <- t(wMT.Fi.S2) %*% VS.Fi.S2 %*% wMT.Fi.S2
  sMT.Fi.S3 <- t(wMT.Fi.S3) %*% VS.Fi.S3 %*% wMT.Fi.S3
}
{
  # Real Estate Sector
  wMT.RE.S1 <- inv(VS.RE.S1) %*% mS.RE.S1
  wMT.RE.S2 <- inv(VS.RE.S2) %*% mS.RE.S2
  wMT.RE.S3 <- inv(VS.RE.S3) %*% mS.RE.S3
  wMT.RE.S1 <- wMT.RE.S1 / sum(wMT.RE.S1) # Weights of individual assets in the Maximum Trade-Off portfolio
  wMT.RE.S2 <- wMT.RE.S2 / sum(wMT.RE.S2)
  wMT.RE.S3 <- wMT.RE.S3 / sum(wMT.RE.S3)
  rMT.RE.S1 <- t(wMT.RE.S1) %*% mS.RE.S1 # Returns of the Maximum Trade-Off portfolios
  rMT.RE.S2 <- t(wMT.RE.S2) %*% mS.RE.S2
  rMT.RE.S3 <- t(wMT.RE.S3) %*% mS.RE.S3
  sMT.RE.S1 <- t(wMT.RE.S1) %*% VS.RE.S1 %*% wMT.RE.S1 # Risk of the Maximum Trade-Off portfolios
  sMT.RE.S2 <- t(wMT.RE.S2) %*% VS.RE.S2 %*% wMT.RE.S2
  sMT.RE.S3 <- t(wMT.RE.S3) %*% VS.RE.S3 %*% wMT.RE.S3
}

# Calculation of Sharpe ratios for the maximum trade-off portfolios for the 3 periods and 4 sectors
{
  # Energy Sector
  sh.En.S1 <- (rMT.En.S1 - rf.S1) / sMT.En.S1
  sh.En.S2 <- (rMT.En.S2 - rf.S2) / sMT.En.S2
  sh.En.S3 <- (rMT.En.S3 - rf.S3) / sMT.En.S3
  # Technology Sector
  sh.Te.S1 <- (rMT.Te.S1 - rf.S1) / sMT.Te.S1
  sh.Te.S2 <- (rMT.Te.S2 - rf.S2) / sMT.Te.S2
  sh.Te.S3 <- (rMT.Te.S3 - rf.S3) / sMT.Te.S3
  # Financials Sector
  sh.Fi.S1 <- (rMT.Fi.S1 - rf.S1) / sMT.Fi.S1
  sh.Fi.S2 <- (rMT.Fi.S2 - rf.S2) / sMT.Fi.S2
  sh.Fi.S3 <- (rMT.Fi.S3 - rf.S3) / sMT.Fi.S3
  # Real Estate Sector
  sh.RE.S1 <- (rMT.RE.S1 - rf.S1) / sMT.RE.S1
  sh.RE.S2 <- (rMT.RE.S2 - rf.S2) / sMT.RE.S2
  sh.RE.S3 <- (rMT.RE.S3 - rf.S3) / sMT.RE.S3
}

# Assessing which sector has the worst performance based on the Sharpe ratio of the maximum trade-off portfolio
{
  sharpe.mat <- matrix(data=c(sh.En.S1, sh.Te.S1, sh.Fi.S1, sh.RE.S1,
                              sh.En.S2, sh.Te.S2, sh.Fi.S2, sh.RE.S2,
                              sh.En.S3, sh.Te.S3, sh.Fi.S3, sh.RE.S3),
                       nrow=3, byrow=TRUE)
  rownames(sharpe.mat) <- c("S1", "S2", "S3")
  colnames(sharpe.mat) <- c("Energy", "Technology", "Financials", "Real Estate")  ; sharpe.mat
}
# For period S1, the sector with the worst performance is Technology
# For period S2, it is Technology
# For period S3, it is Real Estate


# Exclusion Test for the sector with the worst performance in the three periods

#S1
{
  # Sharpe ratio of the portfolio excluding the Technology assets for period S1
  A1 <- t(mS.S1[-(6:10)]) %*% inv(VS.S1[-(6:10), -(6:10)]) %*% mS.S1[-(6:10)]
  B1 <- t(iv[-(6:10)]) %*% inv(VS.S1[-(6:10), -(6:10)]) %*% mS.S1[-(6:10)]
  C1 <- t(iv[-(6:10)]) %*% inv(VS.S1[-(6:10), -(6:10)]) %*% iv[-(6:10)]
  De1 <- A1 * C1 - B1 * B1
  sh.excl.S1 <- sqrt(A1 - 2 * B1 * rf.S1 + C1 * rf.S1 * rf.S1) 
  # Test statistic
  # H0: Sh = sh.excl.S1
  Excl.test.TE.S1 <- T * (sh.S1 * sh.S1 - sh.excl.S1 * sh.excl.S1) / (1 + sh.excl.S1 * sh.excl.S1)
  # P-value
  pchisq(Excl.test.TE.S1, 5, lower.tail=FALSE)
}
# Excluding the Technology sector from the portfolio in period S1 does not result in a statistically significant shift
# in the efficient frontier slope

#S2
{
  # Sharpe ratio of the portfolio excluding the Technology assets for period S2
  A2 <- t(mS.S2[-(6:10)]) %*% inv(VS.S2[-(6:10), -(6:10)]) %*% mS.S2[-(6:10)]
  B2 <- t(iv[-(6:10)]) %*% inv(VS.S2[-(6:10), -(6:10)]) %*% mS.S2[-(6:10)]
  C2 <- t(iv[-(6:10)]) %*% inv(VS.S2[-(6:10), -(6:10)]) %*% iv[-(6:10)]
  De2 <- A2 * C2 - B2 * B2
  sh.excl.S2 <- sqrt(A2 - 2 * B2 * rf.S2 + C2 * rf.S2 * rf.S2) 
  Excl.test.TE.S2 <- T * (sh.S2 * sh.S2 - sh.excl.S2 * sh.excl.S2) / (1 + sh.excl.S2 * sh.excl.S2)
  pchisq(Excl.test.TE.S2, 5, lower.tail=FALSE)
}
# Excluding the Technology sector from the portfolio in period S2 does not result in a statistically significant shift
# in the efficient frontier slope

#S3
{
  # Sharpe ratio of the portfolio excluding the Real Estate sector for period S3
  A3 <- t(mS.S3[-(16:20)]) %*% inv(VS.S3[-(16:20), -(16:20)]) %*% mS.S3[-(16:20)]
  B3 <- t(iv[-(16:20)]) %*% inv(VS.S3[-(16:20), -(16:20)]) %*% mS.S3[-(16:20)]
  C3 <- t(iv[-(16:20)]) %*% inv(VS.S3[-(16:20), -(16:20)]) %*% iv[-(16:20)]
  De3 <- A3 * C3 - B3 * B3
  sh.excl.S3 <- sqrt(A3 - 2 * B3 * rf.S3 + C3 * rf.S3 * rf.S3) 
  Excl.test.TE.S3 <- T * (sh.S3 * sh.S3 - sh.excl.S3 * sh.excl.S3) / (1 + sh.excl.S3 * sh.excl.S3)
  pchisq(Excl.test.TE.S3, 5, lower.tail=FALSE)
}
# Excluding the Real Estate sector from the portfolio in period S3 does not result in a statistically significant shift
# in the efficient frontier slope

#------------------------------------------------------------------------------------
#### Point 7 ###
#------------------------------------------------------------------------------------

# Save the variances of the 20 assets considered, obtained from the diagonal
# of the covariance matrix of the assets for the three periods considered
{
  s.var.S1 <- diag(VS.S1)
  s.var.S2 <- diag(VS.S2)
  s.var.S3 <- diag(VS.S3)
}

# Obtain the portfolio weights as the inverse of the variances for the three periods considered,
# adjusted so that their sum equals 1
{
  wEP.S1 <- (1/s.var.S1)/sum(1/s.var.S1)
  wEP.S2 <- (1/s.var.S2)/sum(1/s.var.S2)
  wEP.S3 <- (1/s.var.S3)/sum(1/s.var.S3)
}

# Calculate the Sharpe ratios for the three portfolios constructed
{
  shEP.S1 <- sum(mS.S1 * wEP.S1) / sqrt(t(wEP.S1) %*% VS.S1 %*% wEP.S1)
  shEP.S2 <- sum(mS.S2 * wEP.S2) / sqrt(t(wEP.S2) %*% VS.S2 %*% wEP.S2)
  shEP.S3 <- sum(mS.S3 * wEP.S3) / sqrt(t(wEP.S3) %*% VS.S3 %*% wEP.S3)
}

# Calculate the returns and risk of the three portfolios constructed
{
  rEP.S1 <- t(wEP.S1) %*% mS.S1
  rEP.S2 <- t(wEP.S2) %*% mS.S2
  rEP.S3 <- t(wEP.S3) %*% mS.S3
  sEP.S1 <- t(wEP.S1) %*% VS.S1 %*% wEP.S1
  sEP.S2 <- t(wEP.S2) %*% VS.S2 %*% wEP.S2
  sEP.S3 <- t(wEP.S3) %*% VS.S3 %*% wEP.S3
}

# Perform a test to verify whether the three portfolios lie on the
# efficient frontier for each period
{
  # Test statistic
  # H0: Sh = ShEP  # equality of the Sharpe ratio of the portfolio built
  # with inverse weights based on the asset variances
  # with the Sharpe ratio of the tangency portfolio
  Effic.test.S1 <- T * (sh.S1^2 - shEP.S1^2) / (1 + shEP.S1^2)  # Slide 161
  Effic.test.S2 <- T * (sh.S2^2 - shEP.S2^2) / (1 + shEP.S2^2)
  Effic.test.S3 <- T * (sh.S3^2 - shEP.S3^2) / (1 + shEP.S3^2)
  
  # P-value
  pval.Effic.test.S1 <- pchisq(Effic.test.S1, N-1, lower.tail = FALSE)
  pval.Effic.test.S2 <- pchisq(Effic.test.S2, N-1, lower.tail = FALSE)
  pval.Effic.test.S3 <- pchisq(Effic.test.S3, N-1, lower.tail = FALSE)
  
  test.mat <- matrix(c(Effic.test.S1, Effic.test.S2, Effic.test.S3,
                       pval.Effic.test.S1, pval.Effic.test.S2, pval.Effic.test.S3),
                     nrow = 2, byrow = TRUE)
  colnames(test.mat) <- c("S1", "S2", "S3")
  rownames(test.mat) <- c("test", "p-value")
  test.mat
}
# All p-values are close to 1, therefore, we conclude that the portfolios containing assets with weights
# proportional to the inverse of their variances (and thus the inverse of their riskiness) are all efficient portfolios.

#------------------------------------------------------------------------------------
#### Point 8 ###
#------------------------------------------------------------------------------------
# We need to apply linear constraints on the portfolio
prezzi.S1 = xts(prezzi.S1[,-1], order.by = prezzi.S1$Date, frequency = "monthly")
prezzi.S2 = xts(prezzi.S2[,-1], order.by = prezzi.S2$Date, frequency = "monthly")
prezzi.S3 = xts(prezzi.S3[,-1], order.by = prezzi.S3$Date, frequency = "monthly")

# Excess returns
Z.S1 <- prezzi.S1 - rf.S1
Z.S2 <- prezzi.S2 - rf.S2
Z.S3 <- prezzi.S3 - rf.S3
# Linear model 
Y.S1 <- matrix(1, T.S1 + 1, 1)
Y.S2 <- matrix(1, T.S2 + 1, 1)
Y.S3 <- matrix(1, T.S3 + 1, 1)
outLM.S1 <- lm(Y.S1 ~ Z.S1 - 1)
outLM.S2 <- lm(Y.S2 ~ Z.S2 - 1)
outLM.S3 <- lm(Y.S3 ~ Z.S3 - 1)
# Extract coefficients and residuals
betac.S1 <- coefficients(outLM.S1)
betac.S2 <- coefficients(outLM.S2)
betac.S3 <- coefficients(outLM.S3)

# Estimate the variance of residuals
err.S1 <- resid(outLM.S1)
err.S2 <- resid(outLM.S2)
err.S3 <- resid(outLM.S3)
sigma2err.S1 <- sum(err.S1^2) / (T.S1 - 20)
sigma2err.S2 <- sum(err.S2^2) / (T.S2 - 20)
sigma2err.S3 <- sum(err.S3^2) / (T.S3 - 20)
# Since we want the tangent portfolio to be evenly distributed among the 4 sectors we've selected, the weight of 
# the first 5 assets should be 25%, as should the weight of the next 5, and so on.
Rmat <- rbind(c(matrix(1, 1, 5), matrix(0, 1, 15)),
              c(matrix(0, 1, 5), matrix(1, 1, 5), matrix(0, 1, 10)),
              c(matrix(0, 1, 10), matrix(1, 1, 5), matrix(0, 1, 5)),
              c(matrix(0, 1, 15), matrix(1, 1, 5))) 
rvec <- rbind(0.25 * sum(betac.S1), 0.25 * sum(betac.S1), 0.25 * sum(betac.S1), 0.25 * sum(betac.S1))
p <- 4  # number of restrictions
# Test statistic
Fstat.S1 <- (t(Rmat %*% betac.S1 - rvec) %*% inv(Rmat %*% inv(t(Z.S1) %*% Z.S1) %*% t(Rmat)) %*% 
               (Rmat %*% betac.S1 - rvec) / p) / sigma2err.S1 
pval.Fstat.S1 <- pf(Fstat.S1, p, T.S1 - 20, lower.tail = FALSE)
pval.Fstat.S1
Fstat.S2 <- (t(Rmat %*% betac.S2 - rvec) %*% inv(Rmat %*% inv(t(Z.S2) %*% Z.S2) %*% t(Rmat)) %*% 
               (Rmat %*% betac.S2 - rvec) / p) / sigma2err.S2 
pval.Fstat.S2 <- pf(Fstat.S2, p, T.S1 - 20, lower.tail = FALSE)
pval.Fstat.S2
Fstat.S3 <- (t(Rmat %*% betac.S3 - rvec) %*% inv(Rmat %*% inv(t(Z.S3) %*% Z.S3) %*% t(Rmat)) %*% 
               (Rmat %*% betac.S3 - rvec) / p) / sigma2err.S3 
pval.Fstat.S3 <- pf(Fstat.S3, p, T.S3 - 20, lower.tail = FALSE)
pval.Fstat.S3

# The test statistic always rejects the null hypothesis, which leads us to conclude that the tangent portfolio is 
# not evenly distributed across the 4 sectors considered (in each of the 3 samples)

