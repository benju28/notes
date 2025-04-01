Practical 2
Q1
data("BJsales")
print(BJsales)
BJsales_ts<-ts(BJsales,frequency = 12)
#b
plot(BJsales_ts,main="BJsales Time Series",ar="blue",xlab="Time",ylab="sales")
decomposed<-decompose(BJsales_ts,type = "additive")
plot(decomposed)
#c
time<-1:length(BJsales_ts)
trend_model<-lm(BJsales_ts~poly(time,2,raw=TRUE))
trend_model
trend_estimate<-predict(trend_model)
plot(BJsales_ts,main="Actual vs trend cmponent(Polynomial fitting)",col="grey",ylab="sales",xlab="time")
lines(trend_estimate,col="pink",lwd=2)
legend("topleft",legend=c("Actual","Trend"),col=c("grey","pink"),lty=1,lwd=2)
residuals<-BJsales_ts-trend_estimate
plot(residuals,main="Residuals after removing trend",col="purple",ylab="Residuals",xlab="Time")
#d
differenced_series<-diff(BJsales_ts,difference = 1)
plot(differenced_series, main="First differenced series",col="pink4", xlab="Time", ylab="Differenced Sales" )
#e
acf(differenced_series,main="Differenced series")
ljung_box_test<-Box.test(differenced_series,lag = 20,type = "Ljung-Box")
print(ljung_box_test)
#f
par(mfrow=c(2,1))
acf(BJsales_ts,main="ACF of Original Series")
acf(differenced_series,main="ACF of Stationary Series")
par(mfrow=c(1,1))


Q2
data("AirPassengers")
print(head(AirPassengers))
str(AirPassengers)
summary((AirPassengers))
plot(AirPassengers,main="International AirPassangers",col="navy",xlab="year",ylab=" No of Passengers")
##b
decomposed_data <- decompose(AirPassengers,type="multiplicative")
plot(decomposed_data)
print(decomposed_data)
##c
data("AirPassengers")
acf(AirPassengers,main="Autocorrelation function of AirPassenegers")
##d.kruskal-wallis test
library(zoo)
ts_data<- data.frame(Month = cycle(AirPassengers),Passengers = as.numeric(AirPassengers))
kruskal_test<- kruskal.test( Passengers ~ Month,data= ts_data)
print(kruskal_test)
##seasonal plot
library(forecast)
ggseasonplot(AirPassengers,main="seasonal plot for AirPassengers",year.labels=TRUE,col=rainbow(12))
##e
plot(AirPassengers,main="original Airpassengers series",col="darkblue",ylab="passengers",xlab="year")
## plot ACF of the original series
acf(AirPassengers,main="ACF of original Airpassengers series")
## log transformation and differenecing to make the series stationary
log_dif_series <- diff(log(AirPassengers))
plot(log_dif_series,main="stationary series(log-diff)",col="orange",ylab="diff log passsengers",xlab="year")
##plot ACF of stationary series
acf(log_dif_series,main="Acf of stationary series")
##summary of Ljung-box test for stationary series
ljung_box <- Box.test(log_dif_series,lag=20,type="Ljung-Box")
print(ljung_box)
##apply 1st diff to remove trend 
diff_airpassengers<- diff(AirPassengers) 
plot(diff_airpassengers,main="differenced airpassengers data",xlab="year",ylab="differenced passengers",col="blue",lwd=2)
##appply seasonal differencing(diff by 12 months)
seasonal_diff<- diff(AirPassengers,lag=12)
plot(seasonal_diff,main="seasonally differneced airpassengers data",xlab="year",ylab="seasonally differenced passengers",col="purple",lwd=2)
