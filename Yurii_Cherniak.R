library("quantmod")
library("xts")

library(quantmod)
load.data <- function(ticker){
  data.raw <- read.csv(paste("./data/", ticker,"_Yahoo.csv", sep = ""), header = TRUE)
  Date <- as.Date(data.raw$Date, format = "%Y-%m-%d")
  data.raw <- cbind(Date, data.raw[, -1])
  data.raw <- data.raw[order(data.raw$Date),]
  data.raw <- xts(data.raw[, 2:7],order.by = data.raw[, 1])
  
  A <- "Open"
  B <- "High"
  C <- "Low"
  D <- "Close"
  E <- "Adjusted"
  G <- "Volume"
  names(data.raw) <- paste(c(A, B, C, D, E, G))
  data.raw <- cbind(data.raw[, 1:4], data.raw[, 6], data.raw[, 5])
  
  names(data.raw) <- paste(c(A, B, C, D, G, E))
  data.raw <- to.monthly(data.raw)
  return(data.raw)
}
load.daily.data <- function(ticker){
  data.raw <- read.csv(paste("./data/", ticker,"_Yahoo.csv", sep = ""), header = TRUE)
  Date <- as.Date(data.raw$Date, format = "%Y-%m-%d")
  data.raw <- cbind(Date, data.raw[, -1])
  data.raw <- data.raw[order(data.raw$Date),]
  data.raw <- xts(data.raw[, 2:7],order.by = data.raw[, 1])
  
  A <- "Open"
  B <- "High"
  C <- "Low"
  D <- "Close"
  E <- "Adjusted"
  G <- "Volume"
  names(data.raw) <- paste(c(A, B, C, D, E, G))
  data.raw <- cbind(data.raw[, 1:4], data.raw[, 6], data.raw[, 5])
  return(data.raw)
}
spolocnosti <- c("AAL", "AEE", "AEP", "ALL", "AMCR","AMZN","AXP","GOOG","GOOGL","MO")

data <- lapply(spolocnosti, load.data)
data <- lapply(data, na.omit)
head(data[[5]])

# 3

display.price.total.ret <- function(x, symbol) {
 first.hrb <- as.numeric(x[1, 6])
 hrb.ret <- x[,6] / first.hrb

 dt <- index(hrb.ret)
 y.range <- range(hrb.ret - 1,hrb.ret)
 plot(x=dt,y=hrb.ret,xlab="Date",ylim=y.range,ylab="Vynos",type="l",col="blue",main=paste("Comparing",symbol,"and Total Return"))
 lines(x=dt,y=hrb.ret - 1,col="darkgreen")
 abline(h=1, col="black")
 legend("topleft",c("Hruby vynos","Cisty vynos"),col=c("blue","darkgreen"),lwd=c(1,1) )
}

display.price.total.ret(data[[1]], spolocnosti[[1]])
display.price.total.ret(data[[2]], spolocnosti[[2]])
display.price.total.ret(data[[3]], spolocnosti[[3]])
display.price.total.ret(data[[4]], spolocnosti[[4]])
display.price.total.ret(data[[5]], spolocnosti[[5]])
display.price.total.ret(data[[6]], spolocnosti[[6]])
display.price.total.ret(data[[7]], spolocnosti[[7]])
display.price.total.ret(data[[8]], spolocnosti[[8]])
display.price.total.ret(data[[9]], spolocnosti[[9]])
display.price.total.ret(data[[10]], spolocnosti[[10]])

# 4
options(scipen = 999)

w <- 1/length(spolocnosti)
weight <- rep(w, length(spolocnosti))

rets <- lapply(data, function(x) Delt(x$data.raw.Adjusted))

returns <- do.call(cbind, rets);
returns <- returns[-1,]
returns <- na.omit(returns)
names(returns) <- spolocnosti

port.ret <- 1 + returns
cum.ret <- cumprod(port.ret)

portfolio <- rep(0, nrow(cum.ret))

for (i in 1:length(weight)) {
  portfolio <- portfolio + cum.ret[, i] * weight[i]
}
names(portfolio) <- c("cum.ret.portfolio")

cum.ret <- na.omit(cum.ret)
cum.ret <- cum.ret[nrow(cum.ret)] - 1
cum.ret #kumulativny cisty vynos

mat.weight <- matrix(weight,1);mat.weight
mat.returns <- matrix(cum.ret, length(spolocnosti));mat.returns
port.ret <- as.numeric(mat.weight %*% mat.returns);port.ret

mat.rets <- as.matrix(returns)
vcov <- cov(mat.rets) * 252
#vypocet rizika portfolia

mat.port.var <- mat.weight %*% vcov %*% t(mat.weight)
mat.port.sd <- sqrt(mat.port.var)

heatmap(vcov, Rowv = NA, Colv = NA, main = "Matica kovariancii")

# 5

data.spy <- load.data("SPY")

ret.spy <- Delt(data.spy$data.raw.Adjusted)

ret.spy <- na.omit(ret.spy)
gret.spy <- 1 + ret.spy;
cret.spy <- cumprod(gret.spy)

plot(x = index(cret.spy),y = cret.spy,xlab = "Date",ylab = "Value of Investment",type = "l",col = "darkgreen",main = "Value of $1 Invested in the S&P 500 index and portfolio")
lines(x = index(portfolio), y = portfolio,col = "blue")
abline(h = 1)
legend("topleft",c("SPY", "My Portfolio"),lty = 1,col = c("darkgreen", "blue"))

# 6
data <- lapply(data, function (x) {
  t <- Delt(x$data.raw.Adjusted)
  
  t <- na.omit(t)
  t <- 1 + t;
  t <- cumprod(t)
  t
})

alphas <- lapply(data, function (x) {
  combo <- cbind(x, cret.spy)
  names(combo) <- c("Market","Port.Ret")
  capm <- lm(Port.Ret ~ Market, data = combo)
  alpha <- summary(capm)$coefficients[1]
  alpha
})

betas <- lapply(data, function (x) {
  combo <- cbind(x, cret.spy)
  names(combo) <- c("Market","Port.Ret")
  
  capm <- lm(Port.Ret ~ Market, data = combo)
  
  beta <- summary(capm)$coefficients[2]
  beta
})

daily.data <- lapply(spolocnosti, load.daily.data)
daily.data <- lapply(daily.data, function (x) {
  t <- Delt(x$Adjusted)
  
  t <- na.omit(t)
  t <- 1 + t;
  t <- cumprod(t)
  t
})

daily.data.spy <- load.daily.data("SPY")

ret.daily.spy <- Delt(daily.data.spy$Adjusted)

ret.daily.spy <- na.omit(ret.spy)
gret.daily.spy <- 1 + ret.spy;
cret.daily.spy <- cumprod(gret.spy)


require(zoo)
coeffs <- lapply(daily.data, function (company) {
 company$SPY <- cret.daily.spy
 r <-  rollapply(company,
            width = 250,
            FUN = function(X)
            {
              names(X) <- c("Symbol", "SPY")
              roll.reg = lm(Symbol ~ SPY,
                            data = as.data.frame(X))
              return(roll.reg$coef)
            },
            by.column = FALSE)
 r <- na.omit(r)
 names(r) <- c("Alpha", "Beta")
 r
}) 

display.roll.alpha.beta <- function (index){
  r_alpha <- range(coeffs[[index]]$Alpha)
  r_beta <- range(coeffs[[index]]$Beta)
  par(mfrow = c(2, 1))
  plot(y = coeffs[[index]]$Alpha, x = index(coeffs[[index]]), xlab = "", ylab = "Alpha", ylim = r_alpha, type = "l", col = "blue", main = paste(spolocnosti[[index]], "Alpha, 250-Day Rolling Window "))
  plot(y = coeffs[[index]]$Beta,
     x = index(coeffs[[index]]),
     xlab = "",
     ylab = "Beta",
     type = "l",
     col = "red",
     main = paste(spolocnosti[[index]], "Beta, 250-Day Rolling Window "))
  par(mfrow = c(1, 1))
}

display.roll.alpha.beta(1)
display.roll.alpha.beta(2)
display.roll.alpha.beta(3)
display.roll.alpha.beta(4)
display.roll.alpha.beta(5)
display.roll.alpha.beta(6)
display.roll.alpha.beta(7)
display.roll.alpha.beta(8)
display.roll.alpha.beta(9)
display.roll.alpha.beta(10)

# 7

mat.ret <- matrix(returns, nrow(returns))
colnames(mat.ret) <- spolocnosti
head(mat.ret)

vcov <- cov(mat.ret)

avg.ret <- matrix(apply(mat.ret, 2, mean))
colnames(avg.ret) <- paste("Avg.Ret")
rownames(avg.ret) <- paste(spolocnosti)
avg.ret

min.ret <- min(avg.ret)
max.ret <- max(avg.ret)

increments <- 100

tgt.ret <- seq(min.ret, max.ret, length = increments)
head(tgt.ret)

tgt.sd <- rep(0, length = increments)

wgt <- matrix(0, nrow = increments, ncol = length(avg.ret))
head(wgt)

library(quadprog)
for (i in 1:increments){
  Dmat <- 2 * vcov
  dvec <- c(rep(0, length(avg.ret)))
  Amat <- cbind(rep(1, length(avg.ret)), avg.ret,
                diag(1, nrow = ncol(mat.ret)))
  bvec <- c(1, tgt.ret[i], rep(0, ncol(mat.ret)))
  soln <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
  tgt.sd[i] <- sqrt(soln$value)
  wgt[i, ] <- soln$solution
}

head(tgt.sd)

head(wgt)

tgt.port <- data.frame(cbind(tgt.ret, tgt.sd, wgt))
names(tgt.port)[c(3:12)] <- paste("w.", spolocnosti, sep="")
head(tgt.port)

minvar.port <- subset(tgt.port, tgt.sd == min(tgt.sd))
minvar.port

rf <- 0.0155 / 12
tgt.port$Sharpe <- (tgt.port$tgt.ret - rf) / tgt.port$tgt.sd
head(tgt.port)

tangency.port <- subset(tgt.port, Sharpe == max(Sharpe))
tangency.port

eff.frontier <- subset(tgt.port, tgt.ret >= max(minvar.port$tgt.ret))
dim(eff.frontier)
head(eff.frontier)

plot(x = tgt.sd,
     xlab = "Portfolio Risk",
     y = tgt.ret,
     ylab = "Portfolio Return",
     col = "blue",
     main = "Mean--Variance Efficient Frontier of Two Assets
(Based on the Quadratic Programming Approach)")

points(x = eff.frontier$tgt.sd,
       col = "blue",
       y = eff.frontier$tgt.ret, pch = 16)

points(x = minvar.port$tgt.sd,
       y = minvar.port$tgt.ret,
       col = "red",
       pch = 15, cex = 2.5)

points(x = tangency.port$tgt.sd,
       y = tangency.port$tgt.ret,
       col = "darkgreen",
       pch = 17, cex = 2.5)

min.risk.w <- subset(minvar.port, tgt.ret >= max(minvar.port$tgt.ret))[3:length(colnames(minvar.port))]

# calculate min.risk.rets

rets <- lapply(data, function(x) Delt(x$data.raw.Adjusted))

returns <- do.call(cbind, rets);
returns <- returns[-1,]
returns <- na.omit(returns)
names(returns) <- spolocnosti

port.ret <- 1 + returns
cum.ret <- cumprod(port.ret)

portfolio <- rep(0, nrow(cum.ret))

for (i in 1:length(min.risk.w)) {
  portfolio <- portfolio + cum.ret[, i] * min.risk.w[1, i]
}
names(portfolio) <- c("min.risk.rets")

data.spy <- load.data("SPY")

ret.spy <- Delt(data.spy$data.raw.Adjusted)

ret.spy <- na.omit(ret.spy)
gret.spy <- 1 + ret.spy;
cret.spy <- cumprod(gret.spy)

plot(x = index(cret.spy),y = cret.spy,xlab = "Date",ylab = "Value of Investment",type = "l",col = "darkgreen",main = "Value of $1 Invested in the S&P 500 index and portfolio")
lines(x = index(portfolio), y = portfolio,col = "blue")
abline(h = 1)
legend("topleft",c("SPY", "My Portfolio"),lty = 1,col = c("darkgreen", "blue"))

