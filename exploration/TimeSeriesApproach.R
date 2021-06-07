#TS approach
source("AuxFunctions.R")

library(tidyverse)


tsla_price <- read.csv("../data/data_h_corrected.csv")
plot(tsla_price$Open)

library(lubridate)
cut_date = ymd_h("2021-01-04 10")
cut_date_numeric <- as.numeric(cut_date)
cut_idx <- which(tsla_price$Date==cut_date)
#cut_idx <- 3000

library(tseries)
adf.test(tsla_price$Open)
#p-value < 0.05 indicates stationarity. Here it's obvious that it's not, but wanted to try the test. p-value: 0.97
kpss.test(tsla_price$Open)

returns <- convert_to_logret(tsla_price$Open)
end_idx <- length(returns)
returns_train <- returns[1:cut_idx]
returns_test <- returns[cut_idx:end_idx]
plot(returns_train,type='l')
points(cut_idx:end_idx,returns_test,col="blue",type='l')
legend("topleft",legend=c("train","test"),lwd=2,col=c("black","blue"))
adf.test(returns) #p-value < 0.01 => stationarity
kpss.test(returns)


#Lets plot the ACF and PACF
acf(returns_train)
pacf(returns_train)
#there are some lags that are above the line, we can try to investigate this.
#But first let's look to see if the returns are normally distributed

qqplot(returns_train,distribution="norm")


p <- ggplot(data.frame(returns_train),aes(sample = returns_train))
p + stat_qq(data=data.frame(returns_train)) + stat_qq_line() #definitely not normaly distributed. Heavy tails. let's try student
p + stat_qq(data=data.frame(returns_train),distribution = qt,dparams = 2.6) + stat_qq_line(data=data.frame(returns_train),distribution = qt,dparams = 2.6)
#looks a lot better with a student t2.6 distribution. For some reason the line is not right compared to python

#Cheking the squared returns to see if there is any structure in the volatility
sreturns <- returns_train**2
acf(sreturns)
pacf(sreturns)
#There's definitly a lag 7 periodicity now

#Let's try differentiating wrt lag 7, to try to remove the periodicity
sreturns7 <- diff(sreturns,7)
returns7 <- diff(returns,7)
acf(returns7)
pacf(returns7)
acf(sreturns7)
pacf(sreturns7)
#Just like in the Python version, this just made things worse

#let's do some GARCH
library(fGarch)
fit1 <- garchFit(~garch(1,0),returns_train,cond.dist='norm')
std.res1 <- fit1@residuals/fit1@sigma.t
#summary(fit1)
#plot(fit1)

plot(std.res1,type='l',main='',ylab='')
acf(std.res1,lag.max=100,main='')
pacf(std.res1,lag.max=100,main='')
qqnorm(std.res1,panel.first=abline(0,1,col="grey"))
acf(std.res1**2,lag.max=100)

fit2 <- garchFit(~garch(1,1),returns_train,cond.dist = 'norm')
std.res2 <- fit2@residuals/fit2@sigma.t

plot(std.res2,type='l',main='',ylab='')
acf(std.res2,lag.max=100,main='')
pacf(std.res2,lag.max=100,main='')
qqnorm(std.res2,panel.first=abline(0,1,col="grey"))
acf(std.res2**2,lag.max=100)

fit3 <- garchFit(~garch(1,1),returns_train,cond.dist = 'std',include.mean = FALSE)
std.res3 <- fit3@residuals/fit3@sigma.t

plot(std.res3,type='l',main='',ylab='')
plot(fit3@sigma.t,type='l',main='',ylab='')
acf(std.res3,lag.max=100,main='')
pacf(std.res3,lag.max=100,main='')
acf(std.res3**2,lag.max=100)

n <- length(std.res3)
qqplot(x=qt(p=(1:n)/(n+1),df=2.25)*sqrt((2.25-2)/2.25), y=std.res3)
qqline(y=std.res3,distribution= function(p) qt(p,df=2.25)) #this doesn't give the right line for some reason...
abline(a=0,b=1) #manually adding the line


cpgram(std.res3)

#prediction
predict(fit3,35,plot=TRUE,mse="cond")
#lines(round(0.25*cut_idx):(round(0.25*cut_idx)+length(returns_test)-1),returns_test)

#simulation
temp <- garchSpec(model=fit3@fit$par,cond.dist = 'std')
garchSim(temp)
plot(garchSim(temp))
