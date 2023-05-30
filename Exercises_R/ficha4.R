#### Worksheet 4: Modeling SARIMA ####

# Exercise 1
require(astsa)


cmort 
?? cmort

mean(cmort)
var(cmort)
length(cmort)

plot.ts(cmort, main="MOrtalidade")

stl(cmort, s.window = "per")
decomp = stl(cmort, s.window = "per")
plot(decomp) #seasonality

install.packages("tseries")
library(tseries)
# check the stationarity,  using unit root testg (adf test) and KPSS test
adf.test(cmort) #H_o non-stationary
kpss.test(cmort, null=c("Level", "Trend"), lshort = TRUE)  #not stationary

# no caderno a explicacao de como funcionam os testes


### 1a

#first differences
dcmort = diff(cmort)
plot.ts(cbind(cmort,dcmort), main="")

adf.test(dcmort) #stationarity
kpss.test(dcmort, null = c("Level", "Trend"), lshort = TRUE)


#1b: tempt to choose a model
acf2(dcmort)
# fit AR(1) do first differences data?? look at pacf

#lc: fit the model and look at the signagtures of teh estimates
fit = sarima(cmort,1,1,0,0,0,0,0) # by default calculates the constant

fit #look at the significance of estimates
#when i ask to make a fit it imediatly fits the constant

#contant is not significant ?
#AICs 6.37107

fit=sarima(cmort,1,1,0,0,0,0,0, no.constant = TRUE)

#AICs 6.36715

# look ar AR parameter: |-0.5061|<1

#id: residual analysis
# look at the plots

sarima(cmort,1,1,0,0,0,0,0, no.constant = TRUE)$fit
  #summary(sarima(cmort,1,1,0,0,0,0,0), no.constant = TRUE)

res = residuals(sarima(cmort,1,1,0,0,0,0,0), no.constant = TRUE)


res
mean(res)
var(res)

Box.test(res,lag=10, type = "Box-Pierce") #global test until lag=10, for residuals correlation
#p value = 0.3722 not reject no correlation (till lag 10)

shapiro.test(res) #normality test p-value =0.4494>0.05, residual can be considered as normal
ks.test(res, pnorm, mean(res), sqrt(var(res))) # p-value = 0.9233, not reject normality

#whute noise

#modelo selecionado:

#####################################################################
#### Exercicio 2 ####

library(tseries)

sales
??sales #monthly sales

plot.ts(sales)
mean(sales)
var(sales)


lsales=log(sales)
mean(lsales)
var(lsales)
plot.ts(lsales)

dlsales = diff(lsales)
mean(dlsales)
var(dlsales)
plot.ts(dlseries)

plot.ts(cbind(sales,lsales,dlsales), main="")

adf.test(dlsales) #s_0 non-stationary
kpss.test(dlsales, null=c("Level","Trend"), lshort = TRUE)

dsales=diff(sales)
mean(dsales)
var(dsales)
plot.ts(dsales)


plot.tsd(cbind(sales,dsales), main="")

adf.test(dsales) #H_o non stationary
kpss.test(dsales, null = c("Level", "Trend"), lshort = TRUE) #stationary

# better using   dif log sales?Choice

#looks having periodicity? No
# stl(sales, s.window="per") will give error


##Remark: sometimes it is usefull to check the adequacy of lambda values of transformation
intall.packages("forecast")
library(forecast)
BoxCox.lambda(sales) #1.955924

# use dlsales

acf2(dlsales)
sarima(lsales,0,1,4,0,0,0,0) #ACF dif zero until lag 4 ??? 2 params=0?
sarima(lsales,0,1,2,0,0,0,0) #problem in Ljung Box test residuals correlated?
# pvalues are all very small close to 0 (smaller than 0.05) so we cant use this model
sqrima(lsales,2,1,0,0,0,0,0) #PACF dif zero until lag 2


# arima(lsales,2,1,0,0,0,0,0) #better
# AICc -7.35035
#model: y_t-const=0.24(a_(t-1)-const)+0.15(y_(t-2)-const)+epsilon(t)
#y_t = log x_t

#stationary? #when i apply the sarima function, it will choose always a model
# such that the parameters belong to the stationary condition. So, this question
# doesnt make much sense to be answered now, but before.

#analise the AR polynomial roots:

s=c(1,-0.24,-0.15)
raizes = polyroot(s)
# stationary? both roots outside the unit circus. (we already expending this
# when applying the sarima)

#or make KPSS test or adf test

s = c(1,-0.2765, -0.2342)
raizes=polyroot(s) # still stationary?

adf.test(dlsales) #stationary
kpss.test(dlsales, null="")