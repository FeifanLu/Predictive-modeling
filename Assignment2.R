library(forecast)
library(fpp)
library(ggplot2)
library(astsa)





###################################################Load data
uk.path <-  file.choose()
nasa.path <- file.choose()
uk.path
nasa.path
uk <- read.csv("C:\\Users\\lufei\\Desktop\\ARIMA\\UK_DATA.csv",header=FALSE)
nasa <- read.csv("C:\\Users\\lufei\\Desktop\\ARIMA\\NASA.csv")

colnames(uk) <-c('Year','Temp')
colnames(nasa) <-c('Year', 'Temp')

class(nasa)
class(uk)
summary(nasa)
summary(uk)
head(nasa)
head(uk)
#add 14
class(uk)
class(nasa)
#ts
uk$Temp <- uk$Temp+14
nasa$Temp <- nasa$Temp+14
nasa.ts <- ts(nasa[,2], start = c(1880, 1),end=c(2020,7),frequency = 12)
head(nasa.ts)
anyNA(nasa.ts)
uk.ts <- ts(uk[,2], start = c(1850, 1), end=c(2020,7),frequency = 12)
head(uk.ts)
anyNA(uk.ts)

library(xts)
#this library is for arregate.ts()
#uk model
UK <- Arima(uk.ts,order=c(2,1,2), seasonal=c(2,0,0),include.drift=TRUE)
#NASA model
NASA <- Arima(nasa.ts,order=c(4,1,3),seasonal=c(0,1,2))

#################################################Q6
#data preparation
head(uk)

(uk.ts)
#UK
##original frequency for ts is 12, by using nfrequency = 1, it aggregate 12 observations into 1
#and take the mean
#nfrequency = new number of observations per unit of time; must be a divisor of the frequency of x.
yearly.uk <- aggregate.ts(uk.ts,nfrequency = 1,mean)
head(yearly.uk)
yearly.uk
plot(yearly.uk)
plot(uk.ts)
mean.1850 <- mean(window(uk.ts, end=c(1850,12)))
mean.1850
mean.1851 <- mean(window(uk.ts,start=c(1851,1), end=c(1851,12)))
mean.1851


train.uk <- window(uk.ts, end=c(2006,12))
tail(train.uk)
train.uk.naive <- aggregate.ts(train.uk, nfrequency =1, mean)
test.uk <- window(uk.ts, start=c(2007,1), end=c(2017,12))
test.uk.naive <- aggregate.ts(test.uk, nfrequency =1, mean)
head(test.uk)
uk.q6 <- Arima(train.uk,order=c(2,1,2), seasonal=c(2,0,0),include.drift=TRUE)
prediction.uk <- forecast(uk.q6,h=132)
plot(prediction.uk)
uk.model <- accuracy(prediction.uk, test.uk)
uk.model
#########################################Q7
#1999 10 years interval 
train.uk.1999 <- window(uk.ts, end=c(1998,12))
tail(train.uk.1999)
train.uk.naive.1999 <- aggregate.ts(train.uk.1999, nfrequency =1, mean)
test.uk.1999 <- window(uk.ts, start=c(1999,1), end=c(2008,12))
test.uk.naive.1999 <- aggregate.ts(test.uk.1999, nfrequency =1, mean)

uk.q6.1999 <- Arima(train.uk.1999,order=c(2,1,2), seasonal=c(2,0,0),include.drift=TRUE)
prediction.uk.1999 <- forecast(uk.q6.1999,h=120)
plot(prediction.uk.1999)
uk.model.1999 <- accuracy(prediction.uk.1999, test.uk.1999)
uk.model.1999


naive.uk.1999 <- naive(train.uk.naive.1999,h=10)
autoplot(naive.uk.1999)
uk.naive.1999 <- accuracy(naive.uk.1999, test.uk.naive.1999)
uk.naive.1999


#UK naive

uk.2007 <- window(uk.ts,start=c(2007,1),end=c(2007,12))
mean(uk.2007)
naive.uk <- naive(train.uk.naive,h=11)
autoplot(naive.uk)
uk.naive <- accuracy(naive.uk, test.uk.naive)
uk.naive


#NASA
train.nasa <- window(nasa.ts, end=c(2006,12))
tail(train.nasa)
train.nasa.naive <- aggregate.ts(train.nasa, nfrequency =1, mean)
test.nasa <- window(nasa.ts, start=c(2007,1), end=c(2017,12))
test.nasa.naive <- aggregate.ts(test.nasa, nfrequency =1, mean)
head(test.nasa)
nasa.q6 <- Arima(train.nasa,order=c(4,1,3),seasonal=c(0,1,2))
prediction.nasa <- forecast(nasa.q6, h=132)
plot(prediction.nasa)
nasa.model <- accuracy(prediction.nasa, test.nasa)
nasa.model

#1999 10 years
train.nasa.1999 <- window(nasa.ts, end=c(1998,12))
tail(train.nasa.1999)
train.nasa.naive.1999 <- aggregate.ts(train.nasa.1999, nfrequency =1, mean)
test.nasa.1999 <- window(nasa.ts, start=c(1999,1), end=c(2008,12))
test.nasa.naive.1999 <- aggregate.ts(test.nasa.1999, nfrequency =1, mean)

nasa.q6.1999 <- Arima(train.nasa.1999,order=c(4,1,3), seasonal=c(0,1,2))
prediction.nasa.1999 <- forecast(nasa.q6.1999,h=120)
plot(prediction.nasa.1999)
nasa.model.1999 <- accuracy(prediction.nasa.1999, test.nasa.1999)
nasa.model.1999


naive.nasa.1999 <- naive(train.nasa.naive.1999,h=10)
autoplot(naive.nasa.1999)
nasa.naive.1999 <- accuracy(naive.nasa.1999, test.nasa.naive.1999)
nasa.naive.1999

#1999 20 years interval
train.nasa.1999.2 <- window(nasa.ts, end=c(1998,12))
tail(train.nasa.1999.2)
train.nasa.naive.1999.2 <- aggregate.ts(train.nasa.1999.2, nfrequency =1, mean)
test.nasa.1999.2 <- window(nasa.ts, start=c(1999,1), end=c(2018,12))
test.nasa.naive.1999.2 <- aggregate.ts(test.nasa.1999.2, nfrequency =1, mean)

nasa.q6.1999.2 <- Arima(train.nasa.1999.2,order=c(4,1,3), seasonal=c(0,1,2))
prediction.nasa.1999.2 <- forecast(nasa.q6.1999.2,h=240)
plot(prediction.nasa.1999.2)
nasa.model.1999.2 <- accuracy(prediction.nasa.1999.2, test.nasa.1999.2)
nasa.model.1999.2


naive.nasa.1999.2 <- naive(train.nasa.naive.1999.2,h=20)
autoplot(naive.nasa.1999.2)
nasa.naive.1999.2 <- accuracy(naive.nasa.1999.2, test.nasa.naive.1999.2)
nasa.naive.1999.2



#NAS naive

naive.nasa <- naive(train.nasa.naive,h=11)
autoplot(naive.nasa)
nasa.naive <- accuracy(naive.nasa, test.nasa.naive)
nasa.naive

accuracy.table <- rbind(uk.model,uk.naive,nasa.model,nasa.naive)



knitr::kable(accuracy.table, row.names = TRUE)




























#nasa.msts
nasa.msts <- msts(nasa[,2], start = c(1880, 1),seasonal.periods = c(12,48),ts.frequency=12)
head(nasa.msts)
nasa.msts



#Q1
#Forecast the global average temperatures through year 2100. 
#There are concerns about global temperatures raising by 2 degrees Celsius, 
#and possibly even by 4 degrees by then.
#Do your analyses provide support for or against these concerns?






                                                    #NASA
#stl
fit.nasa <- stl(nasa.ts, t.window=13, s.window="periodic", na.action=na.omit,robust=TRUE) #decompose using STL (Season and trend using Loess)
plot(fit.nasa)
plot(nasa.ts)
class(fit.nasa)
season.nasa <- window(nasa.ts, start=2000)
ggseasonplot(season.nasa, polar=TRUE)
ggseasonplot(season.nasa)

fit.nasa.msts <- mstl(nasa.msts, t.window=13, s.window=49,na.action=na.omit,robust=TRUE)
plot(fit.nasa.msts)
plot(nasa.msts)

season.nasa.msts <- window(nasa.msts, start=2000)
ggseasonplot(season.nasa.msts, polar=TRUE)
ggseasonplot(season.nasa)

#autoplot
#looks like threre is an upward trend but 
#it maybe a long-term cycle if the temperature eventually goes down
autoplot(window(nasa.ts,start=2000))

# Create a lag plot of the  data
# strong autocorrelation of every lag
gglagplot(nasa.ts)

# Create an ACF plot of the  data
ggAcf(nasa.ts)



nasa_AAN <- ets(nasa.ts, model="AAN")
nasa_AAZ <- ets(nasa.ts, model="AAZ", damped=FALSE)
nasa_MMN <- ets(nasa.ts, model="MMN", damped=FALSE)
nasa_MMZ <- ets(nasa.ts, model="MMZ", damped=FALSE)
nasa_ZZZ <- ets(nasa.ts, model="ZZZ", damped=TRUE )
nasa_ZZZ
checkresiduals(nasa_ZZZ)
autoplot(forecast(nasa_ZZZ, h=80*12))

#Since the data have strong autocorrelation, maybe ARIMA is a good choide
# auto-correlation function
autoplot(nasa.ts)
autoplot(diff(nasa.ts,1))
Acf(nasa.ts,main="") # data "as is"

Acf(diff(nasa.ts,48),main="", lag.max = 48*6) 
Pacf(diff(nasa.ts,48),main="", lag.max = 48*6) 

#p-value too small
nasa.fit <- auto.arima(nasa.ts, lambda = 0, stepwise = FALSE)
nasa.fit
checkresiduals(nasa.fit)

msts.fit <- auto.arima(nasa.ts, lambda = 0)
msts.fit
checkresiduals(msts.fit)

#Mandy's model, seems ok
nasa.fit2 <- auto.arima(nasa.ts)
nasa.fit2

checkresiduals(nasa.fit2)

#this one is the best
nasa.fit3 <- auto.arima(nasa.ts, lambda = 0, stepwise =FALSE)
nasa.fit3

checkresiduals(nasa.fit3)

#big p-value (3,1,3)(0,0,2)
nasa.fit4 <- auto.arima(nasa.ts, lambda = 0)
nasa.fit4
checkresiduals(nasa.fit4)


Acf(residuals(nasa.fit), lag.max = 48*6)
Pacf(residuals(nasa.fit), lag.max = 48*6)

#Manually Arima

#small p value for ljung box test
nasa.fit5 <- Arima(nasa.ts,order=c(3,1,3),seasonal=c(0,1,2),lambda=0,biasadj=TRUE)
nasa.fit5
checkresiduals(nasa.fit5)

nasa.fit5.sharon <- Arima(nasa.ts,order=c(3,1,3),seasonal=c(0,1,2))
nasa.fit5.sharon
checkresiduals(nasa.fit5.sharon)
autoplot(forecast(nasa.fit5.sharon, h=80*12))

nasa.fit5.sharon2 <- Arima(nasa.ts,order=c(4,1,3),seasonal=c(0,1,2))
nasa.fit5.sharon2

#the second charateristic roots look better
autoplot(nasa.fit5.sharon2)
autoplot(nasa.fit5.sharon)

msts.fit5 <- Arima(nasa.msts,order=c(3,1,3),seasonal=c(0,1,2),lambda=0,biasadj=TRUE)
msts.fit5
checkresiduals(msts.fit5)


#big p value for ljung box test
nasa.fit6 <- Arima(nasa.ts,order=c(1,1,2),seasonal=c(2,1,1),lambda=0,biasadj=TRUE)
nasa.fit6
checkresiduals(nasa.fit6)


msts.fit6 <- Arima(nasa.msts,order=c(1,1,2),seasonal=c(2,1,1),lambda=0,biasadj=TRUE)
msts.fit6
checkresiduals(msts.fit6)


#tabats

nasa.tbats <- tbats(nasa.ts)
#have to set use.damped.trend = FALSE
#otherwise, wouldn't consider use other p,q
nasa.tbast2 <- tbats(nasa.ts,use.trend=TRUE,use.arma.errors=TRUE,use.damped.trend=FALSE,
                     start.p = 5,
                     start.q = 5,
                     trace = TRUE)
nasa.tbast2
checkresiduals(nasa.tbast2)
autoplot(forecast(nasa.tbast2, h=80*12))


nasa.tbast.venkat <- tbats(atan(nasa.ts),use.trend=TRUE, seasonal.periods=48, damped.trend=FALSE,
                     start.p = 5,
                     start.q = 5,
                     trace = TRUE)
nasa.tbast.venkat
checkresiduals(nasa.tbast.venkat)
autoplot(forecast(nasa.tbast.venkat, h=80*12))



#predict

nasa.fit3 <- auto.arima(nasa.ts, lambda = 0, stepwise =FALSE)
nasa.fit3

autoplot(forecast(nasa.fit3, h=80*12))
autoplot(forecast(nasa.fit, h=80*12))
autoplot(forecast(nasa.fit2, h=80*12))
autoplot(forecast(nasa.fit5, h=80*12))
autoplot(forecast(nasa.fit6, h=80*12))

autoplot(nasa.fit5)
autoplot(nasa.fit6)


#use arima


fit.f5 <- auto.arima(nasa.ts, xreg = fourier(nasa.ts, K = 5), 
                     seasonal = FALSE, lambda = 0,biasadj=TRUE)

fit.f5
checkresiduals(fit.f5)



fit.f6 <- auto.arima(nasa.ts, xreg = fourier(nasa.ts, K = 6), 
                     seasonal = TRUE, lambda = 0,biasadj=TRUE, stepwise=FALSE)

fit.f6
checkresiduals(fit.f6)

autoplot(forecast(fit.f5,  xreg = fourier(nasa.ts, K = 5),h=80*12))

autoplot(forecast(fit.f6, xreg = fourier(nasa.ts, K = 6),h=80*12))


#Check fit 2,3,4,6 

Mandy <-forecast(nasa.fit2,h=80*12)
Mandy.lambda <- forecast(nasa.fit4, h=80*12)  
Mandy.lambda.stepfalse <- forecast(nasa.fit3, h=80*12)  
Mandy.lambda.stepfalse.seasonal.differencing <- forecast(nasa.fit6, h=80*12)  

accuracy(Mandy, nasa.ts)
accuracy(Mandy.lambda, nasa.ts)
accuracy(Mandy.lambda.stepfalse, nasa.ts)
accuracy(Mandy.lambda.stepfalse.seasonal.differencing, nasa.ts)



#
class(nasa.ts)
tail(nasa.ts,10)
train <- window(nasa.ts, start = 1880, end=c(1999,12))
test <- window(nasa.ts, start = 2000, end=c(2020,5))
train
tail(train)
test


sharon.fit5 <- Arima(train,order=c(3,1,3),seasonal=c(0,1,2))
sharon.fit5
checkresiduals(sharon.fit5)

train.fit5 <- Arima(train,order=c(3,1,3),seasonal=c(0,1,2),lambda=0,biasadj=TRUE)
train.fit5

train.fit6 <- Arima(train,order=c(1,1,2),seasonal=c(2,1,1),lambda=0,biasadj=TRUE)
train.fit6

fit.noboxcox <- forecast(sharon.fit5, h=241)
autoplot(fit.noboxcox)
accuracy(fit.noboxcox,test)

fit.train5 <- forecast(train.fit5, h=241)
autoplot(fit.train5)
accuracy(fit.train5,test)

fit.train6 <- forecast(train.fit6, h=241)
autoplot(fit.train6)
accuracy(fit.train6,test)






#Sarima

#48 slope is flatter 
library('MLmetrics')
tail(train)
head(test)
sarima_forecast.48 = sarima.for(train, n.ahead = length(test), p=3, d=1, q=3, P=0,D=1,Q=2,S=48)
sarima_forecast.12 = sarima.for(train, n.ahead = length(test), p=3, d=1, q=3, P=0,D=1,Q=2,S=12)
sarima_forecast2.48 = sarima.for(train, n.ahead = length(test), p=4, d=1, q=3, P=0,D=1,Q=2,S=48)
sarima_forecast2.12 = sarima.for(train, n.ahead = length(test), p=4, d=1, q=3, P=0,D=1,Q=2,S=12)


sarima_forecast.48
sarima_forecast.12
sarima_forecast2.48
sarima_forecast2.12

MAPE(sarima_forecast.48$pred,test) * 100
MAPE(sarima_forecast.12$pred,test) * 100
MAPE(sarima_forecast2.48$pred,test) * 100
MAPE(sarima_forecast2.12$pred,test) * 100

RMSE(sarima_forecast.48$pred,test)
RMSE(sarima_forecast.12$pred,test)
RMSE(sarima_forecast2.48$pred,test)
RMSE(sarima_forecast2.12$pred,test) 



sarima.nasa.48 <- sarima(nasa.ts,12*80, p=3, d=1, q=3, P=0,D=1,Q=2,S=48)
sarima.nasa.12 <- sarima(nasa.ts,12*80, p=3, d=1, q=3, P=0,D=1,Q=2,S=12)

sarima.nasa2.12 <-sarima(nasa.ts,12*80, p=4, d=1, q=3, P=0,D=1,Q=2,S=48)
sarima.nasa2.48 <-sarima(nasa.ts,12*80, p=4, d=1, q=3, P=0,D=1,Q=2,S=12)











                                                        #UK
#stl
fit.uk <- stl(uk.ts, t.window=13, s.window="periodic", na.action=na.fail,robust=TRUE) #decompose using STL (Season and trend using Loess)
plot(fit.uk)
plot(uk.ts)
Acf(uk.ts, lag.max = 120)
Pacf(uk.ts, lag.max = 120)

diffuk<- diff((uk.ts), lag = 1)
autoplot(diffuk)

uk.auto <- auto.arima(uk.ts, stepwise = FALSE, approximation = FALSE)
checkresiduals((uk.auto))
autoplot(forecast(uk.auto, h=80*12))

uk.ts %>% stl(s.window='periodic') %>% seasadj() -> ukdecompose
autoplot(ukdecompose)
ukdecompose %>% diff() %>% ggtsdisplay(main="")


uk_AAN <- ets(uk.ts, model="AAN")
uk_AAZ <- ets(uk.ts, model="AAZ", damped=FALSE)
#have negative values so multiple is inappropriate 
uk_MMN <- ets(uk.ts, model="MMN", damped=FALSE)
uk_MMZ <- ets(uk.ts, model="MMZ", damped=FALSE)
uk_ZZZ <- ets(uk.ts, model='ZZZ')
uk_ZZZ
autoplot(forecast(uk_ZZZ, h=965))
uk_tbats <- tbats(uk.ts)
uk_tbats
autoplot(forecast(uk_tbats, h=965))
uk.tbats2 <- tbats((uk.ts),use.trend=TRUE,
                     start.p = 3,
                     start.q = 3,
                     trace = TRUE)

checkresiduals(uk.tbats2)

uk.tbats3 <- tbats((uk.ts),use.trend=TRUE, use.damped.trend = TRUE,
                   start.p = 3,
                   start.q = 3,
                   trace = TRUE)


autoplot(forecast(uk.tbats2, h=965))

autoplot(forecast(uk.tbats3, h=965))


#choose diff lag = 48, (49-1), (50-2), (51-3)
Acf(diff(uk.ts,48),main="", lag.max = 48*6) 
Pacf(diff(uk.ts,48),main="", lag.max = 48*6) 


fit.noseason <- auto.arima(uk.ts,seasonal=FALSE)
fit.noseason
plot(residuals(fit.noseason))
plot(acf2AR())
Pacf((residuals(fit.noseason)),lag.max=48*6)
Pacf(diff(residuals(fit.noseason)),lag.max=48*6)



#
fit.season <- auto.arima(uk.ts,seasonal=TRUE)
fit.season
fit.noseason <- auto.arima(uk.ts, seasonal=FALSE)
checkresiduals(fit.noseason)
checkresiduals(fit.season)
plot(residuals(fit.season))
Pacf((residuals(fit.season)),lag.max=48*6)
Pacf(diff(residuals(fit.season)),lag.max=48*6)



#

uk.arima1 <- auto.arima(uk.ts) #(2,1,2)(2,0,0)
uk.arima2 <- auto.arima(uk.ts, stepwise = FALSE)#(1,1,2)(2,0,0)
uk.arima3 <- Arima(uk.ts,order=c(2,1,2),seasonal=c(2,1,0))
uk.arima4 <- Arima(uk.ts,order=c(1,1,2),seasonal=c(2,1,0))
uk.arima5 <- Arima(uk.ts,order=c(2,1,2),seasonal=c(2,0,1))
uk.arima6 <- Arima(uk.ts,order=c(1,1,2),seasonal=c(2,0,1))
uk.arima7 <- Arima(uk.ts,order=c(3,1,3),seasonal=c(0,1,2))
uk.arima8 <- Arima(uk.ts,order=c(2,1,2), seasonal=c(2,0,0),include.drift=TRUE)
  
accuracy(uk.arima1)
accuracy(uk.arima2)
accuracy(uk.arima4)
accuracy(uk.arima7)

autoplot(forecast(uk.arima1, h=965))
autoplot(forecast(uk.arima2, h=965))

autoplot(forecast(uk.arima3, h=965))
autoplot(forecast(uk.arima4, h=965))
autoplot(forecast(uk.arima7, h=965))
autoplot(forecast(uk.arima8, h=965))
checkresiduals(uk.arima1)
checkresiduals(uk.arima2)

checkresiduals(uk.arima3)
checkresiduals(uk.arima4)
checkresiduals(uk.arima6)
checkresiduals(uk.arima7)


















#use arima


fit.f5 <- auto.arima(nasa.ts, xreg = fourier(nasa.ts, K = 5), 
                  seasonal = FALSE, lambda = 0,biasadj=TRUE)

fit.f5
checkresiduals(fit.f5)



fit.f6 <- auto.arima(nasa.ts, xreg = fourier(nasa.ts, K = 6), 
                     seasonal = TRUE, lambda = 0,biasadj=TRUE, stepwise=FALSE)

fit.f6
checkresiduals(fit.f6)

autoplot(forecast(fit.f5,  xreg = fourier(nasa.ts, K = 5),h=80*12))

autoplot(forecast(fit.f6, xreg = fourier(nasa.ts, K = 6),h=80*12))






























#Check fit 2,3,4,6 

Mandy <-forecast(nasa.fit2,h=80*12)
Mandy.lambda <- forecast(nasa.fit4, h=80*12)  
Mandy.lambda.stepfalse <- forecast(nasa.fit3, h=80*12)  
Mandy.lambda.stepfalse.seasonal.differencing <- forecast(nasa.fit6, h=80*12)  

accuracy(Mandy, nasa.ts)
accuracy(Mandy.lambda, nasa.ts)
accuracy(Mandy.lambda.stepfalse, nasa.ts)
accuracy(Mandy.lambda.stepfalse.seasonal.differencing, nasa.ts)



#
class(nasa.ts)
tail(nasa.ts,10)
train <- window(nasa.ts, start = 1880, end=c(1999,12))
test <- window(nasa.ts, start = 2000, end=c(2020,5))
train
tail(train)
test


sharon.fit5 <- Arima(train,order=c(3,1,3),seasonal=c(0,1,2))
sharon.fit5
checkresiduals(sharon.fit5)

train.fit5 <- Arima(train,order=c(3,1,3),seasonal=c(0,1,2),lambda=0,biasadj=TRUE)
train.fit5

train.fit6 <- Arima(train,order=c(1,1,2),seasonal=c(2,1,1),lambda=0,biasadj=TRUE)
train.fit6

fit.noboxcox <- forecast(sharon.fit5, h=241)
autoplot(fit.noboxcox)
accuracy(fit.noboxcox,test)

fit.train5 <- forecast(train.fit5, h=241)
autoplot(fit.train5)
accuracy(fit.train5,test)

fit.train6 <- forecast(train.fit6, h=241)
autoplot(fit.train6)
accuracy(fit.train6,test)






#Sarima

#48 slope is flatter 
library('MLmetrics')
tail(train)
head(test)
sarima_forecast.48 = sarima.for(train, n.ahead = length(test), p=3, d=1, q=3, P=0,D=1,Q=2,S=48)
sarima_forecast.12 = sarima.for(train, n.ahead = length(test), p=3, d=1, q=3, P=0,D=1,Q=2,S=12)
sarima_forecast2.48 = sarima.for(train, n.ahead = length(test), p=4, d=1, q=3, P=0,D=1,Q=2,S=48)
sarima_forecast2.12 = sarima.for(train, n.ahead = length(test), p=4, d=1, q=3, P=0,D=1,Q=2,S=12)


sarima_forecast.48
sarima_forecast.12
sarima_forecast2.48
sarima_forecast2.12

MAPE(sarima_forecast.48$pred,test) * 100
MAPE(sarima_forecast.12$pred,test) * 100
MAPE(sarima_forecast2.48$pred,test) * 100
MAPE(sarima_forecast2.12$pred,test) * 100

RMSE(sarima_forecast.48$pred,test)
RMSE(sarima_forecast.12$pred,test)
RMSE(sarima_forecast2.48$pred,test)
RMSE(sarima_forecast2.12$pred,test) 



sarima.nasa.48 <- sarima(nasa.ts,12*80, p=3, d=1, q=3, P=0,D=1,Q=2,S=48)
sarima.nasa.12 <- sarima(nasa.ts,12*80, p=3, d=1, q=3, P=0,D=1,Q=2,S=12)

sarima.nasa2.12 <-sarima(nasa.ts,12*80, p=4, d=1, q=3, P=0,D=1,Q=2,S=48)
sarima.nasa2.48 <-sarima(nasa.ts,12*80, p=4, d=1, q=3, P=0,D=1,Q=2,S=12)










#why 
#forecast(method=naive)
#capture the seasonality gives seasonal naive???????????????????
plot(uk.ts)
fit <- stl(elecequip, t.window=13, s.window="periodic",
           robust=TRUE)

class(fit)
autoplot(elecequip)
autoplot(fit)
fit %>% seasadj() %>% autoplot
#stl returns seaonal, trend and remainder
#seasadj remove the sesonality, return univariate ts which is the sum of (trend and remainder)
#naive and snaive can only deal with univariate ts
#forecast can direcly deal with the stl outcome or ts
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")

#The prediction intervals shown in this graph are constructed in the same way as the point forecasts. 
#That is, the upper and lower limits of the prediction intervals
#on the seasonally adjusted data are "reseasonalised" by adding in the forecasts of 
#the seasonal component. In this calculation, the uncertainty in the forecasts 
#of the seasonal component has been ignored. 
#The rationale for this choice is that the uncertainty in the seasonal component 
#is much smaller than that for the seasonally adjusted data, 
#and so it is a reasonable approximation to ignore it.
#run the naive for the descomposed seasadj() data, then add the seasonality back.

#chapter 6.8
fit %>% forecast(method="naive") %>%
  autoplot() + ylab("New orders index")
class(elecequip)
autoplot(snaive(elecequip))
#couln't run. seems forecast() only allow you to specify method on stl
model.a <- tbats(elecequip)
model.b <- stl(elecequip,s.window = 'periodic')
elecequip%>% forecast(model=model.a)%>%
  autoplot() + ylab("New orders index")
#"ets", "arima", "naive", "rwdrift" are args for method
fit %>% forecast(method = 'arima') %>%
  autoplot() + ylab("New orders index")

fit.uk %>% forecast(method = 'arima', h=965) %>%
  autoplot() + ylab("New orders index")








