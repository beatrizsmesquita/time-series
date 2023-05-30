###Ficha 3###

# Exercício 1

# window()
par(mfrow=c(3,2))
set.seed(5)

##### exercise 1

### Ex1 i Stationary
a=0.8
sigma=1

arl=arima.sim(list(order=c(1,0,0), ar=a), n=144, sd= sigma) #simulating
arl

plot.ts(arl, main=" Process AR(1)m a=0.8, sigma=1")

#another plot with other title and vertical lines
#plot.ts(arl,type="b", axes=F); box(); axis(2)
#axis(1,seq(0,144,24))
# title (main=bquote(paste("Example of AR(1)", a==.(a), " ", sigma^2==.(sigma)))
#abline(v=seq(0,144,12), lty="dotted)

## acf-theoretical
acf_t=ARMAacf(ar=a, ma=0, 35) #pacf=FALSE
acf_t

plot(acf_t, type="h", xlab="lag") #
abline(h=0)

### acf estimated sample (sample acf)
acf_e=acf(arl,lag.max=35, plot=FALSE)
plot(acf_e, type="h", col="red", xlab="lag")
abline(h=0)

### pacf theoretical 
pacf_t=ARMAacf(ar=a,ma=0,35,pacf=TRUE)
plot(pacf_t,type="h", col="red", xlab="lag")
abline(h=0)
pacf_t

par(mfrow=c(1,1))

### pacf estimated

acf_p=pacf(arl, lag.max=35, plot=FALSE)
plot(acf_p, type="h", col="red", xlab="lag")
abline(h=0)

par(mfrow=c(1,1))


###########
### Ex1 iii Stationary
a=1
sigma=0.5

arl=arima.sim(list(order=c(1,0,0), ar=a), n=144, sd= sigma) #simulating
arl

plot.ts(arl, main=" Process AR(1)m a=0.8, sigma=1")

#another plot with other title and vertical lines
#plot.ts(arl,type="b", axes=F); box(); axis(2)
#axis(1,seq(0,144,24))
# title (main=bquote(paste("Example of AR(1)", a==.(a), " ", sigma^2==.(sigma)))
#abline(v=seq(0,144,12), lty="dotted)

## acf-theoretical
acf_t=ARMAacf(ar=a, ma=0, 35) #pacf=FALSE
acf_t

plot(acf_t, type="h", xlab="lag") #
abline(h=0)

### acf estimated sample (sample acf)
acf_e=acf(arl,lag.max=35, plot=FALSE)
plot(acf_e, type="h", col="red", xlab="lag")
abline(h=0)

### pacf theoretical 
pacf_t=ARMAacf(ar=a,ma=0,35,pacf=TRUE)
plot(pacf_t,type="h", col="red", xlab="lag")
abline(h=0)
pacf_t

par(mfrow=c(1,1))

### pacf estimated

acf_p=pacf(arl, lag.max=35, plot=FALSE)
plot(acf_p, type="h", col="red", xlab="lag")
abline(h=0)

par(mfrow=c(1,1))

### Ex1 ii Stationary
a=-0.8
sigma=1

arl=arima.sim(list(order=c(1,0,0), ar=a), n=144, sd= sigma) #simulating
arl

plot.ts(arl, main=" Process AR(1)m a=0.8, sigma=1")

#another plot with other title and vertical lines
#plot.ts(arl,type="b", axes=F); box(); axis(2)
#axis(1,seq(0,144,24))
# title (main=bquote(paste("Example of AR(1)", a==.(a), " ", sigma^2==.(sigma)))
#abline(v=seq(0,144,12), lty="dotted)

## acf-theoretical
acf_t=ARMAacf(ar=a, ma=0, 35) #pacf=FALSE
acf_t

plot(acf_t, type="h", xlab="lag") #
abline(h=0)

### acf estimated sample (sample acf)
acf_e=acf(arl,lag.max=35, plot=FALSE)
plot(acf_e, type="h", col="red", xlab="lag")
abline(h=0)

### pacf theoretical 
pacf_t=ARMAacf(ar=a,ma=0,35,pacf=TRUE)
plot(pacf_t,type="h", col="red", xlab="lag")
abline(h=0)
pacf_t

par(mfrow=c(1,1))

### pacf estimated

acf_p=pacf(arl, lag.max=35, plot=FALSE)
plot(acf_p, type="h", col="red", xlab="lag")
abline(h=0)

par(mfrow=c(1,1))


