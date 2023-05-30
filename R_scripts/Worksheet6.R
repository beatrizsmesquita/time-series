############################ Series Temporais  #######################
####################### Exponential Smoothing Methods   ##############
############################   2022/23 #######################

install.packages("fpp", dependencies=TRUE)
library(fpp)
library(forecast)




#exer 1

####1a  example 1:with no trend (simple exponential smoothing)
??oil

oil
par(mfrow=c(2,1))
plot(oil)

fit <- ses(oil)#forecasts 10 obs, using simple exponential smoothing
plot(fit)
summary(fit)#show the model and forecasts

#or alternatively
fit_ets1=ets(oil, model="ANN")
accuracy(fit_ets1)#to get just the accuracy measures
res_ets1 <- residuals(fit_ets1)

par(mfrow=c(1,1))

plot(fit$model)#decomposition





####1b  examplo2: with linear trend
par(mfrow=c(3,1))
plot(ausair)
fit2 <- holt(ausair, h=5)  
plot(fit2)
summary(fit2)

#or alternatively
fit_ets2=ets(ausair, model="AAN")
#Smoothing parameters: alpha = 0.9999 ,beta  = 1e-04,very small
 #so, would it be better not to consider a trend  

fit2l <- ses(ausair, h=5) #no trend
plot(fit2l)
summary(fit2l)

#What is better?compare AIC and RMSE 

#plot(stl(fit_ets2$model, s.window = "per"))#gives the decomposition
plot(fit_ets2$model)#????

par(mfrow=c(1,1))



####1c  example:  strikes 


strikes
plot(strikes)

##exp.smoot with linear trend 
fit3 <- holt(strikes)

plot(fit3$model)#decomposition plot

fit3$model #fit
summary(fit3) #fit with forecasts

par(mfrow=c(2,1))

plot(fit3)
#or
plot(fit3)#with forecasts
lines(fitted(fit3), col="red")


#Smoothing parameters:alpha = 0.3603 ,beta  = 0.2299 



###### fitting just with level
fit31 <- ses(strikes)
summary(fit31)

plot(fit31$model) #decomposition 


plot(fit31)
lines(fitted(fit31), col="red")


accuracy(fit31)
accuracy(fit3)



#try at home considering exponential trend



####3d  example 4:Holt_winters (trend+seasonal)
austourists
par(mfrow=c(4,1))
plot(austourists)

aus1 <- hw(austourists) #additive seas
aus2 <- hw(austourists, seasonal="mult")
aus3 <- hw(austourists, seasonal="mult",damped=TRUE)
summary(aus1)
summary(aus2)
summary(aus3)

plot(aus1)
plot(aus2)
plot(aus3)


par(mfrow=c(1,1))
summary(aus1)
#     AIC     AICc      BIC 
#283.3940 288.1308 300.2348
#                  ME     RMSE      MAE        MPE     MAPE      MASE
#Training set 0.03929539 2.290844 1.645216 -0.5970376 5.030542 0.5676067
 
summary(aus2)
#     AIC     AICc      BIC 
#280.6575 285.3944 297.4983 
#                  ME     RMSE      MAE       MPE     MAPE      MASE    
Training set -0.2630849 2.088434 1.518531 -1.312793 4.749496 0.5238997

summary(aus3)
#     AIC     AICc      BIC 
#278.4072 284.3532 297.1193  , melhor a modelar
#                   ME     RMSE      MAE       MPE     MAPE      MASE
#Training set -0.02627106 2.098531 1.565807 -0.513562 4.694419 0.5402101

# aus3- fit
# aus2- forecast

par(mfrow=c(1,1))
plot(aus2$model)

########## exercise 2
####example:   ausbeer data, using ets function
ausbeer
par(mfrow=c(3,1))
plot(ausbeer)



##2a
fit1_ausbeer <- ets(ausbeer,model="AAA",damped=FALSE)##Holt-Winters addtive errors, trend and seas. 
fit1_ausbeer
#    AIC     AICc      BIC 
#2315.264 2316.159 2345.430


#OBS when it is used ets function, plot(model) does the decomposition
#plot(fit1_ausbeer)

fcast1 <- forecast(fit1_ausbeer, h=20)
fcast1

plot(fcast1)
lines(fitted(fcast1), col="red")

accuracy(fcast1)#RMSE  15.92394 

ou
plot(fit1_ausbeer)
  
##2b
fit2_ausbeer <- ets(ausbeer,model="MAM",damped=FALSE)##Holt-Winters mult: errors and seas; addit trend
#     AIC     AICc      BIC 
#2273.619 2274.515 2303.786  #best fit

fcast2 <- forecast(fit2_ausbeer, h=20)
plot(fcast2)
lines(fitted(fcast1), col="red")
accuracy(fcast2)# RMSE 15.65815  , better accuracy

##2c
fit_aut<- ets(ausbeer)#automatic fit
summary(fit_aut)


plot(fit_aut)#does the decomposition


fcast_aut<- forecast(fit_aut, h=20)

plot(fcast_aut)
lines(fitted(fcast_aut), col="red")

#ETS(M,A,M) 

res_ets_aut <- residuals(fit_aut)
plot(res_ets_aut)

