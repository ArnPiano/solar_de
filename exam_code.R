#####################
# DATA MANIPULATION #
#####################

#the following file holds hourly data on solar power national generation and solar power capacity in MWh
#from date 01/01/2015 till 09/01/2019
solar.de <- read.csv("german_solar_capacity_actual_01012015-09012019.csv")

generation.de <-ts(solar.de$DE_solar_generation_actual)

# To ease the analysis the data is transformed from hourly to daily by taking the daily mean.

hourly.to.daily <- function(dataset, column){
  output <- c()
  for (i in 1:(length(dataset))/24 ){
    n <- 0
    for (j in 1:24){
      n <- n + dataset[ (i-1) * 24 + j ]
    }
    output[i] = n/24.0
  }
  return(output)
}

daily.gen.de <- hourly.to.daily(generation.de)

daily.to.monthly <-function(dataset, set.dates){
  
  monthly.temp <-c()
  monthly.dates <- format(set.dates, format="%Y-%m")
  i <- 1
  j <- 2
  
  monthly.temp[i] = dataset[i]
  for (k in 2:length(dataset)){
    if(monthly.dates[k]==monthly.dates[k-1]){
      monthly.temp[i] <- monthly.temp[i] + dataset[k]
      j = j + 1
    }
    else{
      monthly.temp[i] = monthly.temp[i]/j
      j = 1
      i = i+1
      monthly.temp[i] <- dataset[k]
      
    }
    
    
  }
  monthly.temp[i] <- monthly.temp[i]/9
  return (monthly.temp)
}

start.date.de <- as.Date(solar.de$cet_cest_timestamp[1])
dates.de <- start.date.de + 0:(length(daily.gen.de)-1)
monthly.gen.de <- daily.to.monthly(daily.gen.de, dates.de)
monthly.dates.de <- format(dates.de, format="%Y-%m")
monthly.gen.de <- ts(monthly.gen.de, frequency = 12, start = c(2015,1))
train <- subset(monthly.gen.de, end=length(monthly.gen.de)-5)
test <- subset(monthly.gen.de, start=length(monthly.gen.de)-4)

########################
# EXPLORATORY ANALYSIS #
########################
library(dplyr)
library(fpp)
m <- rbind(c(1, 1), c(2, 3))
layout(m)
train %>% plot(main= "Mean Monthly Solar power generation in Germany",ylab="MWh")
boxplot(train ~ cycle(train), xlab = "Month", ylab = "MWh", main = "Monthly MWh  in Germany - Boxplot")
hist(train, xlab="MWh")
lines(density(train))
layout(c(1,1))

#####################
# STATIONARITY TEST #
#####################

library(urca)

tested.ts <- train
Box.test(tested.ts, lag=24)
adf.test(tested.ts, k=12)
tested.ts%>% ur.kpss(lags = "long") %>% summary()

tested.ts <- diff(train)
Box.test(tested.ts, lag=24)
adf.test(tested.ts, k=12)
tested.ts%>% ur.kpss(lags = "long") %>% summary()

gen.d12<-diff((train), lag=12)
tested.ts <- gen.d12
Box.test(tested.ts, lag=24)
adf.test(tested.ts, k=12)
tested.ts%>% ur.kpss(lags = "long") %>% summary()

tested.ts <- diff(gen.d12)
Box.test(tested.ts, lag=24)
adf.test(tested.ts, k=12)
tested.ts%>% ur.kpss(lags = "long") %>% summary()

#################
# DECOMPOSITION #
#################

library(ggplot2)
library(gridExtra)
monthly.dec <- stl(train, s.window="periodic")
p1 <-autoplot(monthly.dec, main = "Loess Time Series Decomposition")
Trend <- monthly.dec$time.series[,"trend"]
MWh <- train
p2 <- autoplot(train, ylab="MWh")+ autolayer(MWh)+autolayer(Trend)
grid.arrange(p1, p2, ncol = 1)

###############

library(fpp)
ggtsdisplay(train, lag.max = 25, main= "Time series, Autocorrelation Function, and Partial Autocorrelation Function")

#################
# MODEL FITTING #
#################

model0 <- Arima(train, order=c(0,0,0), seasonal= c(0,1,0))
# #boxtest ok;
# #nonstandard residuals
# #ACF ok
# #p-value 0.3686
# #AIC=506.83   AICc=506.97   BIC=508.3
model1 <- Arima(train, order=c(0,0,0), seasonal= c(1,1,0))
# # worse results
# #Box-test fail
# #nonstandard residuals
# #ACF ok
# #p-value 0.2225
# #AIC=508.33   AICc=508.75   BIC=511.27
model2 <- Arima(train, order=c(0,0,0), seasonal= c(0,1,1))
# #   worse results
# #Box-test fail
# #nonstandard residuals
# #p-value 0.2192
# #AIC=508.31   AICc=508.73   BIC=511.24
model3 <- Arima(train, order=c(0,0,1), seasonal= c(0,1,0))
# #   worse results
# #Box-test ok
# #nonstandard residuals
# #p-value 0.4693
# #AIC=507.76   AICc=508.17   BIC=510.69
#model4 <- Arima(train, order=c(1,0,1), seasonal= c(0,1,0))
# #   not working
# model5 <- Arima(train, order=c(1,0,0), seasonal= c(0,1,0))
# #   worse result
# #boxtest succeed;
# #nonstandard residuals
# #ACF ok
# #p-value 0.4691
# #AIC=507.45   AICc=507.87   BIC=510.38
#As this proceeding will lead to the same results of the automated model, the option to use d=1 is adopted
model6 <- Arima(train, order=c(1,1,0), seasonal= c(0,1,0))
# #   ambiguous result
# #boxtest fail;
# #nonstandard residuals
# #ACF ok
# #p-value 0.1072
# #AIC=497.09   AICc=497.52   BIC=499.96
model7 <- Arima(train, order=c(1,1,1), seasonal= c(0,1,0))
# #   slight improvement
# #boxtest succeed;
# #sufficiently standard residuals
# #ACF ok
# #p-value 0.4994
# #AIC=494.39   AICc=495.28   BIC=498.69
model8 <- Arima(train, order=c(2,0,0), seasonal= c(0,1,0))
# #   worse result
model9 <- Arima(train, order=c(3,0,0), seasonal= c(0,1,0))
# #   worse result
# #boxtest succeed;
# #standard residuals
# #ACF ok
# #p-value 0.7164
# #AIC=507.91   AICc=509.39   BIC=513.77
model10 <- Arima(train, order=c(1,1,2), seasonal= c(0,1,0))
# # slight improvement result
# #boxtest succeed;
# #standard residuals
# #ACF ok
# #p-value 0.4294
# #AIC=496.3   AICc=497.84   BIC=502.04
model11 <- Arima(train, order=c(0,1,1), seasonal= c(1,1,0))
#   improvement
#boxtest succeed;
#standard residuals
#ACF ok
#p-value 0.5913
#AIC=491.66   AICc=492.55   BIC=495.96
model12 <- Arima(train, order=c(0,1,4), seasonal= c(1,1,0))
# NOW ELIMINATE ALL PARAMETERS p SUCH THAT | p/se(p) | > 2
# we return to model11

fit1 <- model6
fit2 <- model11
fit3 <- auto.arima(train, stationary = FALSE, seasonal=TRUE, ic="aicc")
extra <- model3
test.model <-trial
ggtsdisplay(test.model$residuals, lag.max = 25)
test.model
tsdiag(test.model)
checkresiduals(test.model)

#################

accuracy(fit1)
accuracy(fit2)
accuracy(fit3)

#################

tsdiag(fit1, lag = 25, gof.lag = 25)
tsdiag(fit2, lag = 25, gof.lag = 25)
tsdiag(fit3, lag = 25, gof.lag = 25)

###############
# FORECASTING #
###############

library(gridExtra)
xlab = "Time"
ylab = "MWh"
fc1 <- forecast(fit1 , h=length(test))
p1<- autoplot(fc1, color = "blue",xlab = xlab, ylab=ylab) +  autolayer(fc1, color="blue", lwd=2) + autolayer(test, color="orange", lwd=2 ) + autolayer(train, color="orange", lwd=2)
fc2 <- forecast(fit2 , h=length(test))
p2<-autoplot(fc2, color = "blue", xlab = xlab,ylab=ylab) +  autolayer(fc2, color="blue", lwd=2) + autolayer(test, color="orange", lwd=2) +autolayer(train, color="orange", lwd=2)
fc3 <- forecast(fit3 , h=length(test))
p3<-autoplot(fc3, color = "blue",xlab = xlab, ylab=ylab) +  autolayer(fc3, color="blue", lwd=2) + autolayer(test, color="orange", lwd=2) +autolayer(train, color="orange", lwd=2)
grid.arrange(p1,p2,p3, ncol=1)

accuracy(fc1,test)
accuracy(fc2,test)
accuracy(fc3,test)

####################
# CROSS VALIDATION #
####################

my.CV <- function( data, model ){
  train.min <- model$arma[5]*2
  i = 0
  errors <- c()
  while (train.min+i+1 < length(monthly.gen.de) ){
    train <- subset(monthly.gen.de, end = train.min+i)
    
    model.train <- Arima(train, model = model)
    #solar.train %>% forecast(h=12) %>% autoplot() + autolayer(test)
    #accuracy(forecast(solar.train,h=12))
    
    fc <- forecast(model.train, h=1)
    errors[i+1] <- (data[train.min+i+1]-fc$mean[1])^2#RSE
    i<-i+1
  }
  CV.RMSE <- sqrt(sum(errors)/length(errors))
  return(CV.RMSE)
}
my.CV(monthly.gen.de, fit1)
my.CV(monthly.gen.de, fit2)
my.CV(monthly.gen.de, fit3)