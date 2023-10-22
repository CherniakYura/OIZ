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
  return(data.raw)
}
spolocnosti <- c("AAL", "AEE", "AEP", "ALL", "AMCR","AMZN","AXP","GOOG","GOOGL","MO")

data <- lapply(spolocnosti, load.data)
data <- lapply(data, na.omit)
head(data[[1]])

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

rets <- lapply(data, function(x) Delt(x$Adjusted))

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

ret.spy <- Delt(data.spy$Adjusted)

ret.spy <- na.omit(ret.spy)
gret.spy <- 1 + ret.spy;
cret.spy <- cumprod(gret.spy)

plot(x = index(cret.spy),y = cret.spy,xlab = "Date",ylab = "Value of Investment",type = "l",col = "darkgreen",main = "Value of $1 Invested in the S&P 500 index and portfolio")
lines(x = index(portfolio), y = portfolio,col = "blue")
abline(h = 1)
legend("topleft",c("SPY", "My Portfolio"),lty = 1,col = c("darkgreen", "blue"))

# 6

# ///


