setwd("/Users/Kevin/Desktop/R/FedChicagoNFCI")
library(quantmod)
library(forecast)
library(vars)
library(tseries)

###
### Pull Data
###

### SP500
SP500 <- getSymbols('^GSPC',from='1971-01-08',auto.assign=FALSE,periodicity = 'monthly')
SP500.ts <- ts(SP500[,6], start=c(1971,2), frequency=12)
SP500.per = (diff(SP500[,6])/SP500[-nrow(SP500),6]) + 1
SP500.l = log(SP500.per)
SP500.l = na.omit(SP500.l)

### NFCI 
NFCI <- getSymbols('NFCI',src='FRED',auto.assign=FALSE,periodicity = "monthly")
NFCI.m <- period.apply(NFCI, endpoints(NFCI, on = "months"), last)
NFCI.d <- diff(NFCI.x)
NFCI.d = NFCI.d[1:580]
NFCI.d = ts(NFCI.d, start=c(1971,3),frequency=12)
### Merge
data <- cbind(SP500.l,NFCI.d)
data = ts(data,start=c(1971,3),frequency=12)

###
### Test for stationary
###

pp.test(data[,1])
adf.test(data[,1])

pp.test(data[,2])
adf.test(data[,2])


###
### Vector Autoregression
###

VAR.NFCI <- VAR(data,lag.max=10,type="const",ic="SC")
summary(VAR.NFCI)

causality(VAR.NFCI,cause="NFCI.d")$Granger


### Matrix setup 
n.end <- 478
t <- length(data[,1]) 
n <- t-n.end-1
beg.test = data[1:n.end,]
beg.test = ts(beg.test,start=c(1971,3),frequency=12)

### Forecast Matrix
fcast.wn <- matrix(rep(0,1*n),n,1)
fcast.wn.actual = matrix(rep(0,2*n),n,2)
fcast.var.nfci <- matrix(rep(0,1*n),n,1)
fcast.var.actual = matrix(rep(0,2*n),n,2)


for(i in 1:n){
  
  ### White Noise
  SP500.wn = data[,1]
  MRKT_INDEX.wn.test <- SP500.wn[1:n.end+i-1]
  wn.model <- arima(MRKT_INDEX.wn.test,order=c(0,0,0))
  fcast.wn[i] <- predict(wn.model,n.ahead=1,se.fit=FALSE)
  fcast.wn.actual[i,1] = exp(fcast.wn[i]) * SP500[n.end+i-1,6]
  fcast.wn.actual[i,2] = SP500[n.end+i,6]
  

  nfci.test <- data[1:n.end+i-1,] 
  var.nfci <- VAR(nfci.test,lag.max=10,type="const",ic="SC")
  pred.var.nfci <- predict(var.nfci,n.ahead=1,se.fit=FALSE)
  fcast.var.nfci[i] <- pred.var.nfci$fcst$GSPC.Adjusted[1]
  fcast.var.actual[i,1] = exp(pred.var.nfci$fcst$GSPC.Adjusted[1]) * SP500[n.end+i-1,6]
  fcast.var.actual[i,2] = SP500[n.end+i,6]
  
}

fcast.var.actual = ts(fcast.var.actual,start=c(2011,1),frequency=12)
fcast.wn.actual = ts(fcast.wn.actual,start=c(2011,1),frequency=12)

forecasts = cbind(fcast.wn.actual[,2],fcast.wn.actual[,1],fcast.var.actual[,1])

wn_error = fcast.wn.actual[,2] - fcast.wn.actual[,1]
wn_rmse = sqrt(mean(wn_error^2))

var_error = fcast.var.actual[,2] - fcast.var.actual[,1]
var_rmse = sqrt(mean(var_error^2))

improvement = ((var_rmse - wn_rmse) / wn_rmse) * 100
improvement
