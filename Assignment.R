data <- read.table("exchange rate.txt", sep=", ", header = TRUE)
data
df<- data[seq(dim(data)[1],1),]
df
Price <- df$Price
Date <- df$Date
Open <- df$Open
Close <- df$Close
High <- df$High
Low <- df$Low
Change <- df$Change
Price <- as.ts(Price)
lPrice <- log(Price)
# Q1. First graphically analyse the Price series and based on these plots 
#qualitatively describe the evolution of the weekly USD-INR rates during this 
# period.
# Graphical Visualization
par(mfrow=c(1,2))
plot(Price)
plot(lPrice)
# Q2. Next select an ARIMA model of an appropriate order, for the weekly 
# log-Price series. You must clearly state your reasons (showing only the 
# necessary details, but in a systematic step-by-step fashion), for selecting 
# the model, by comparing it with a few of its neighboring competitors, if 
# necessary.

#. Checking for Stationarity
par(mfrow=c(1,2))
acf(Price)
acf(Price, plot = F)
acf(lPrice, ylim=c(-0.1,0.2),pch=19)
par(mfrow=c(1,2))
pacf(lPrice, ylim=c(-0.1,0.2),pch=19)
pacf(lPrice, plot = F)
plot(diff(lPrice), pch=19)

# Performing Test for Stationarity
install.packages("tseries")
library(tseries)
adf.test(lPrice)
# Dickey Fuller test shows that data is non stationary
pp.test(lPrice)
# Phillips-Perron Unit Root Test also suggests non stationarity of data

kpss.test(lPrice)
# KPSS Test for Level Stationarity has very low p-value, which means that 
# there is no evidence for stationarity of data.

# Now checking stationarity of data for difference between consecutive prices so
# that if there is any baseline affect in data it could be ignored(detrending)
adf.test(diff(lPrice))
# Augmented Dickey-Fuller Test suggests that there is trone evidence against 
# non-stationarity of Price difference
pp.test(diff(lPrice))
# Phillips-Perron Unit Root Test for Price difference also suggests for stationarity.
kpss.test(diff(lPrice))
# KPSS Test for Level Stationarity has p-values is 0.1 which suggests that there
# no evidence against null hypothesis of stationarity.

# Order of Integrand
# Visualization for Stationarity of Price difference
par(mfrow=c(1,2))
acf(diff(lPrice))
pacf(diff(lPrice))
# this visualization suggests that order of Integrand is 1, as applying 
# just one difference operator we achieve stationarity (At lag of 1, stationarity
# becomes prominent)

# Checking for White Noise after applying Lag Operator on lPrice
Box.test(diff(lPrice), lag=30)
Box.test(diff(lPrice), type = "L")
Box.test(diff(lPrice),lag=30, type = "L")
# p-value for both Box test has been very low and it suggests
# that lPrice has no white noise and there is still some 
# necessary informations we can extract from our data.

# Now Checking for Most Appropriate ARIMA Model

arma_aics<-function(x,P,d,Q)
{
  aics<-matrix(nrow=P+1,ncol=Q+1)
  for(p in 0:P)
    for(q in 0:Q)
    {
      mdl<-arima(x,order=c(p,d,q),method = "ML")
      if( mdl$code==0 ) aics[p+1,q+1]<-mdl$aic
    }
  return(aics)
}

# Akaike Information Criterion to find best suited model 
aic10<-arma_aics(lPrice,10,1,10)
aic10
# Sorting AIC in order to understand their relative significance
sort(aic10)[1:12]
# Sorting best 12 ARIMA Models based on Models with lowest AIC values
model5_2<-arima(lPrice,order=c(5,1,2),method = "ML")
model2_5<-arima(lPrice,order=c(2,1,5),method = "ML")
model3_3<-arima(lPrice,order=c(3,1,3),method = "ML")
model0_2 <- arima(lPrice,order=c(0,1,2),method="ML")
model3_5<-arima(lPrice,order=c(3,1,5),method = "ML")
model5_3<-arima(lPrice,order=c(5,1,3),method = "ML")
model2_0<-arima(lPrice,order=c(2,1,0),method = "ML")
model0_3<-arima(lPrice,order=c(0,1,3),method = "ML")
model1_2<-arima(lPrice,order=c(1,1,2),method = "ML")
model6_6<-arima(lPrice,order=c(6,1,6),method = "ML")
model2_1<-arima(lPrice,order=c(2,1,1),method = "ML")
model3_0<-arima(lPrice,order=c(3,1,0),method = "ML")

# Checking each Models given above to understand their desirability
# 1. model5_2
model5_2
# ar1,ar5 terms are insignificant
# So we have to check the model with removing these insignificant terms
arima(lPrice,order=c(5,1,2),method = "ML",fixed=c(0,NA,NA,NA, 0,NA,NA))
# Above data suggests that there is very little improvement in model.

#2. model2_5
# MA5 term is insignificant so checking with removing it from model.
arima(lPrice,order=c(2,1,5),method = "ML",fixed=c(NA,NA,NA,NA, NA,NA,0))
# it suggests that ignored term hardly has any influence on this ARIMA model

#3. model3_3
model3_3
# all parameters are significant and can be considered as appropriate model.

#4. model0_2
model0_2
# all parameters are significant and it has only 2 parameters so most simplest model.
# However it is IMA model and so our model may not be best sutited because of lack of stationarity.

#5. model3_5
model3_5
# in this model ar1,ar3,ma1,ma3,ma4,ma5 terms are insignificant.
arima(lPrice,order=c(3,1,5),method = "ML",fixed=c(0,NA,0,0,NA,0,0,0))
# lot of information is being losteven with 8 parameters

#6. model5_3
model5_3
# as some of Parameters have NaN as their SE so will not be considering it.

#7. model2_0
model2_0
# Both terms are significant so it will be part of our consideration of model.

#8. model0_3
model0_3
# ma3 terms are insignificant
  wsss
# it becomes significant and it also explains more information for data.

#9. model1_2
model1_2
# ar1 and ma1 terms are insignificant.
arima(lPrice,order=c(0,1,3),method = "ML",fixed=c(0,0,NA))
# this model is not useful even after removing insignificant terms.

#10. model6_6
model6_6
# ar2,ar4,ma2,ma4 terms are insignificant so we can remove them form model.
arima(lPrice,order=c(6,1,6),method = "ML",fixed=c(NA,0,NA,0,NA,NA,NA,0,NA,0,NA,NA))

#11. model2_1
model2_1
# ar1 and ma1 terms are insignificant.
arima(lPrice,order=c(2,1,1),method = "ML",fixed=c(0,NA,0))

#12. model3_0
model3_0
# ar3 terms are insignofcant.
arima(lPrice,order=c(3,1,0),method = "ML",fixed=c(NA,NA,0))

# Analyzing Model with least AIC and least complexity 
#
