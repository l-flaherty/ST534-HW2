#################Written By Liam Flaherty For ST534###############################
#####1. Load Required Packages#####
library(stats)


#####2. Specify Parameters#####
length1=50
length2=1000
burnin=101

#m1=a_t+0.5a_{t-1}+0.24a_{t-2}#
m1coefma=c(0.5, 0.24)      #ARIMA(0,0,2) process#

#m2=0.8Z_{t-1}+a_t-0.3a_{t-1}#
m2coefma=-0.3
m2coefar=0.8               #ARIMA(1,0,1) process#


#####3. Specify Models#####
set.seed(534)               #To make reproducible#
m1_short=arima.sim(model=list(ma=m1coefma),
             n=length1, 
             n.start=burn)

m1_long=arima.sim(model=list(ma=m1coef),
                  n=length2, 
                  n.start=burn)

m2_short=arima.sim(model=list(ar=m2coefar, ma=m2coefma),
                   n=length1, 
                   n.start=burn)

m2_long=arima.sim(model=list(ar=m2coefar, ma=m2coefma),
                   n=length2, 
                  n.start=burn)





#####4. Plot Results Along With ACF and PACF##### 
###4a. ARIMA(0,0,2) short###
layout(matrix(c(1,1,2,3), nrow=2, ncol=2, byrow=TRUE))
plot(m1_short, 
     main=bquote(atop(
       paste("Simulated ARIMA(0,0,2) Of Length ", .(length1)), 
       paste(psi[1], "=", .(m1coefma[1]), ", ", psi[2], "=", .(m1coefma[2])))),
     xlab="Time", 
     ylab="Value",
     lwd=2, 
     lty=1, 
     col="blue")

m1_shortacf=acf(m1_short, lag.max=40, 
    main=paste0("ARIMA(0,0,2) Of Length ", length1, "\n", "Estimated ACF"), 
    ci.col="blue", 
    col="blue", 
    lwd=2)
m1_shortacf

m1_shortpacf=pacf(m1_short, lag.max=40, 
     main=paste0("ARIMA(0,0,2) Of Length ", length1, "\n", "Estimated PACF"), 
     ci.col="blue", 
     col="blue", 
     lwd=2)
m1_shortpacf



###4b. ARIMA(0,0,2) long###
layout(matrix(c(1,1,2,3), nrow=2, ncol=2, byrow=TRUE))
plot(m1_long, 
     main=bquote(atop(
       paste("Simulated ARIMA(0,0,2) Of Length ", .(length2)), 
       paste(psi[1], "=", .(m1coefma[1]), ", ", psi[2], "=", .(m1coefma[2])))),
     xlab="Time", 
     ylab="Value",
     lwd=2, 
     lty=1, 
     col="red")

m1_longacf=acf(m1_long, lag.max=40, 
    main=paste0("ARIMA(0,0,2) Of Length ", length2, "\n", "Estimated ACF"), 
    ci.col="blue", 
    col="red", 
    lwd=2)
m1_longacf


m1_longpacf=pacf(m1_long, lag.max=40, 
     main=paste0("ARIMA(0,0,2) Of Length ", length2, "\n", "Estimated PACF"), 
     ci.col="blue", 
     col="red", 
     lwd=2)
m1_longpacf



###4c. ARIMA(1,0,1) Short###
layout(matrix(c(1,1,2,3), nrow=2, ncol=2, byrow=TRUE))
plot(m2_short, 
     main=bquote(atop(
       paste("Simulated ARIMA(1,0,1) Of Length ", .(length1)), 
       paste(pi[1], "=", .(m2coefar[1]), ", ", psi[1], "=", .(m2coefma[1])))),
     xlab="Time", 
     ylab="Value",
     lwd=2, 
     lty=1, 
     col="seagreen4")

m2_shortacf=acf(m2_short, lag.max=40, 
    main=paste0("ARIMA(1,0,1) Of Length ", length1, "\n", "Estimated ACF"), 
    ci.col="blue", 
    col="seagreen4", 
    lwd=2)
m2_shortacf

m2_shortpacf=pacf(m2_short, lag.max=40, 
     main=paste0("ARIMA(1,0,1) Of Length ", length1, "\n", "Estimated PACF"), 
     ci.col="blue", 
     col="seagreen4", 
     lwd=2)
m2_shortpacf



###4d. ARIMA(1,0,1) long###
plot(m2_long, 
     main=bquote(atop(
       paste("Simulated ARIMA(1,0,1) Of Length ", .(length2)), 
       paste(pi[1], "=", .(m2coefar[1]), ", ", psi[1], "=", .(m2coefma[1])))),
     xlab="Time", 
     ylab="Value",
     lwd=2, 
     lty=1, 
     col="purple")

m2_longacf=acf(m2_long, lag.max=40, 
    main=paste0("ARIMA(1,0,1) Of Length ", length2, "\n", "Estimated ACF"), 
    ci.col="blue", 
    col="purple", 
    lwd=2)
m2_longacf

m2_longpacf=pacf(m2_long, lag.max=40, 
     main=paste0("ARIMA(1,0,1) Of Length ", length2, "\n", "Estimated PACF"), 
     ci.col="blue", 
     col="purple", 
     lwd=2)
m2_longpacf





######5. Theoretical P/ACF######
macoef=c(.5,.24)
ma2acf=round(ARMAacf(ma=macoef, lag.max=10),2)
ma2pacf=round(ARMAacf(ma=macoef, lag.max=10, pacf=TRUE),2)
ma2acf
ma2pacf

ARmacoef=0.8
arMAcoef=-0.3
arma11acf=round(ARMAacf(ar=ARmacoef, ma=arMAcoef, lag.max=10),2)
arma11pacf=round(ARMAacf(ar=ARmacoef, ma=arMAcoef, lag.max=10, pacf=TRUE),2)
arma11acf
arma11pacf





######6. Comparison#####
acf_comparison=data.frame(
  mylag=1:10,
  Estimated_MA_50=as.numeric(m1_shortacf[1:10]$acf),
  Estimaged_MA_1000=as.numeric(m1_longacf[1:10]$acf),
  Theoretical_MA=as.numeric(ma2acf[2:11]),
  Estimated_ARMA_50=as.numeric(m2_shortacf[1:10]$acf),
  Estimaged_ARMA_1000=as.numeric(m2_longacf[1:10]$acf),
  Theoretical_ARMA=as.numeric(arma11acf[2:11])
)

round(acf_comparison,4)






######################
mycoef=c(1, -.4, -.21)
polyroot(mycoef)
Mod(polyroot(mycoef))


mycoef=c(1, .7, .12)
polyroot(mycoef)
Mod(polyroot(mycoef))


###############scrap############
#####5. Plot Estimated ACF and PACF#####
par(mfrow=c(2,2),
    mar=c(5,4,4.5,1),  #b(5.1), l(4.1), t(4.1), r(2.1) inner margins#
    mgp=c(2,1,0),    #Move axis labels closer to the axis
    oma=c(0,0,0,0))  #outer margins#


macoef=c(-.3,-.5)
ma2acf=round(ARMAacf(ma=macoef, lag.max=10),2)
ma2acf
