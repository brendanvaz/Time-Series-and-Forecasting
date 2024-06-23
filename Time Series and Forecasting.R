#------------ Question 1 --------------
#The Task is to analyse this dataset by fitting a suitable time 
#series model to describe the data.

#Reading the cet_temp.csv dataset
cet_df<-read.csv("cet_temp.csv",header=TRUE)

#Printing the data
print(cet_df)

#Setting the annual mean temperature to time series data
cet_temp<- ts(cet_df$avg_annual_temp_C,start=1900,frequency=1)

#Produce a time plot of the simulated data 'cet_temp'.
ts.plot(cet_temp, main = "Time Plot of annual temperature")

#Set the R graphics device to contain two plots (1 row, 2 columns)
x11()
par(mfrow=c(1,2))

#Plot the sample ACF
acf(cet_temp, main = "Sample ACF of CET Temperature Data")


#Plot the sample PACF
pacf(cet_temp,main = "Sample PACF of CET Temperature Data")

#On visualizing the plots to check stationarity, it is seen 
#that the process does not appear to be stationary. 
#The time plot depicts an increasing trend and has variation over time 
#whereas the sample ACF values do not decline sharply as the lag increases. 
#The pacf does not cut off after lag 1

#Hence, to achieve stationarity the data is differenced
cet_temp_diff<- diff(cet_temp)
# Plot the differenced data 
ts.plot(cet_temp_diff, main = "Time Plot of differenced Temperature Data")


x11()
par(mfrow=c(1,2))
#Plot the ACF for 'cet_temp_diff'
acf(cet_temp_diff, main = "Sample ACF of Differenced Temperature Data")

#Plot the PACF for 'cet_temp_diff'
pacf(cet_temp_diff,main = "Sample PACF of Differenced Temperature Data")

#In comparison, the above plots show that the first difference
#of the data is (weakly) stationary. 
#The time plot illustrates constant mean and variability over time.
#The ACF declines rapidly to zero after lag 0.
#Similarly, the PACF also tends to zero as the lag increases.
#This is indicative of an MA process

#Begin with an ARIMA(0,0,1) process

#Fit an ARIMA(0,0,1) model to the data
model0.MA1<- arima(cet_temp,order=c(0,0,1),method="ML")

#Print the ARIMA model summary
model0.MA1

#Calculate the residuals of the ARIMA(0,0,1) model
resid0<-residuals(model0.MA1)

#Plot the time series of the residuals
ts.plot(resid0,main='Time plot of MA(1) Model Residuals')

#Plot ACF of the residuals
acf(resid0,main='Plot of ACF of MA(1) Model Residuals')

#Function to produce P-values for the Ljung-Box test for different lags
#where an ARMA(p,q) model has been fitted.
#Note that k must be > p+q
LB_test<-function(resid,max.k,p,q){
  lb_result<-list()
  df<-list()
  p_value<-list()
  for(i in (p+q+1):max.k){
    lb_result[[i]]<-Box.test(resid,lag=i,type=c("Ljung-Box"),fitdf=(p+q))
    df[[i]]<-lb_result[[i]]$parameter
    p_value[[i]]<-lb_result[[i]]$p.value
  }
  df<-as.vector(unlist(df))
  p_value<-as.vector(unlist(p_value))
  test_output<-data.frame(df,p_value)
  names(test_output)<-c("deg_freedom","LB_p_value")
  return(test_output)
}

# Compute Ljung-Box test p-values for an ARMA(0,1) model with max lag 11
MODEL0.LB<-LB_test(resid0,max.k=11,p=0,q=1)
#Print the p-values for each lag
MODEL0.LB

#To produce a plot of the P-values against the degrees of freedom
plot(MODEL0.LB$deg_freedom,MODEL0.LB$LB_p_value,xlab="Degrees of freedom",ylab="Pvalue",main="Ljung-Box test P-values",ylim=c(0,1))
#Adding a blue dashed line at y = 0.05
abline(h=0.05,col="blue",lty=2)

#The time plot does is not stationary for above model.
#The sample ACF of residuals does not cut off to zero as lag increases.
#Additionally, the p-values are below the threshold off 0.05.
#This suggests that this model might not be a good fit.  

#Begin with an ARIMA(0,1,1) process

#Fit an ARIMA(0,1,1) model to the data
model1.MA1<- arima(cet_temp,order=c(0,1,1),method="ML")

#Print the summary of the ARIMA model
model1.MA1

#Calculate the residuals of the ARIMA(0,1,1) model
resid.MA1<-residuals(model1.MA1)
#Time plot of the residuals
ts.plot(resid.MA1,main='Time plot of ARIMA(0,1,1) Model Residuals')
#Plot ACF of the residuals
acf(resid.MA1,main='Plot of ACF of ARIMA(0,1,1) Model Residuals')

#The time plot of the residuals looks similar to white noise. 
#The sample ACF seems to be close to zero at all lags > 0. 
#Hence,the residuals are independent.

#Applying the LB_test function to the residuals of the ARIMA(0,1,1) model
MA1.LB<-LB_test(resid.MA1,max.k=11,p=0,q=1)
#Print the p-values for each lag value.
MA1.LB

#Produce a plot of the P-values against the degrees of freedom
plot(MA1.LB$deg_freedom,MA1.LB$LB_p_value,xlab="Degrees of freedom",ylab="Pvalue",main="Ljung-Box test P-values",ylim=c(0,1))
#Add a blue dashed line at y=0.05
abline(h=0.05,col="blue",lty=2)

#It is noted that all p-values are > 0.05 (and hence non-significant at the 5% level) some
#The p-values are fairly small and we may
#consider fitting an alternative model to the data.

#Fitting an ARIMA(1,1,1) model to the data
model2.ARIMA<- arima(cet_temp,order=c(1,1,1),method="ML")
#Print the summary of the ARIMA model 
model2.ARIMA

#Calculate the residuals of the ARIMA(1,1,1) model
resid2.ARIMA<-residuals(model2.ARIMA)
#Time plot of the residuals
ts.plot(resid2.ARIMA,main='Time plot of ARIMA(1,1,1) Model Residuals')
#Plot ACF of the residuals
acf(resid2.ARIMA)

#Applying the LB_test to the residuals of the ARIMA(1,1,1) model
ARIMA.LB<-LB_test(resid2.ARIMA,max.k=11,p=1,q=1)
ARIMA.LB

#Produce a plot of the P-values against the degrees of freedom 
plot(ARIMA.LB$deg_freedom,ARIMA.LB$LB_p_value,xlab="Degrees of freedom",ylab="Pvalue",main="Ljung-Box test P-values",ylim=c(0,1))
#Add a blue dashed line at 0.05
abline(h=0.05,col="blue",lty=2)
par(mfrow=c(1,1))

#aic = 227.63 FOR ARIMA(1,1,1) MODEL
#aic = 226.86 FOR ARIMA(0,1,1) MODEL

#The test statistic for a test of the hypotheses: H0 : φ = 0 versus
#H1 : φ != 0 is 0.1137/0.1026 = 1.1081 which is NOT greater than 2. 
#Hence we do not reject H0 and conclude that the
#AR term should NOT be included in the model

#Fitting an ARIMA(0,1,2) Model
model3.ARIMA<- arima(cet_temp,order=c(0,1,2),method="ML")
model3.ARIMA


#Calculate the residuals of the ARIMA(0,1,2) model
resid3.ARIMA<-residuals(model3.ARIMA)
#Time plot of the residuals
ts.plot(resid3.ARIMA,main='Time plot of ARIMA(0,1,2) Model Residuals')
#Plot ACF of the residuals
acf(resid3.ARIMA)

#Applying the LB test to residuals of the ARIMA(0,1,2) model
ARIMA3.LB<-LB_test(resid3.ARIMA,max.k=11,p=0,q=2)
ARIMA3.LB

#To produce a plot of the P-values against the degrees of freedom
plot(ARIMA3.LB$deg_freedom,ARIMA3.LB$LB_p_value,xlab="Degrees of freedom",ylab="Pvalue",main="Ljung-Box test P-values",ylim=c(0,1))
#Add a blue dashed line at 0.05
abline(h=0.05,col="blue",lty=2)

#aic = 227.77 FOR ARIMA(0,1,2) MODEL
#ma2 |-0.0847/ 0.0807|=1.049 (less than 2.....should not be included)

#As a further check we consider fitting an ARIMA(1,1,1) and an ARIMA(0,1,2)
#model to see if the addition of further AR or MA terms might improve the model
#fit. Each of these models has an AIC value higher than the ARIMA(0,1,1) model. 
#Moreover, hypothesis tests to check whether or not additional terms should be included result in large P-values, 
#thereby suggesting that the ARIMA(0,1,1) model is preferable.

#At this point, choose the ARIMA(0,1,1) model as the most appropriate
#for the above dataset.


#------------ Question 2 --------------
#The Task is to produce a time series model to forecast the monthly house prices
#for the first six months of 2020.

#Read the 'em_house_df' dataset
em_house_df<-read.csv("em_house_prices.csv",header=TRUE)

#Print the data 
print(em_house_df)

#Set the monthly average price to time series data
em_house_prices<- ts(em_house_df$average_price_gbp,start=2010,frequency=12)

#Produce a time plot of the simulated data
ts.plot(em_house_prices, main = "Time plot of monthly average")

#The data is not stationary as there is an upward 
#trend(increasing mean) and seasonality present.

#Plot the sample ACF
acf(em_house_prices,main = "Sample ACF")

#The sample ACF does not cut off to zero

#Plot the sample PACF
pacf(em_house_prices, main = "Sample PACF")

#The sample PACF drops to zero after the first lag. 
 
#In order to achieve stationarity, the data is differenced
#A first order difference is used.

#As the data have been collected monthly
#The first difference is done with lag 12.
em_house_prices_diff<- diff(em_house_prices, lag = 12)

#Plot the first-differenced data
ts.plot(em_house_prices_diff)

#Even though there isn't any seasonality, the mean is still
#not constant which suggests its not stationary.

#Plot the ACF of the first-differenced data
acf(em_house_prices_diff,ylim=c(-1,1), main = "ACF of first difference data")

#Plot the PACF of the first-differenced data
pacf(em_house_prices_diff, main = "PACF of first difference data")

#These plots illustrate that the first differenced data is not stationary.
#A first difference of the seasonally differenced data is done to 
#achieve stationarity.
diff_1<- diff(em_house_prices_diff)

#Plot the differenced data
ts.plot(diff_1, main= "Plot of the differenced data")

#Plot the ACF of the differenced data
acf(diff_1, main = "ACF")
#Plot the PACF of the differenced data
pacf(diff_1, main = "PACF")

#On observing the above plots the time series appears to be stationary 
#as it has a constant mean, no seasonality and constant variance over time.

#The sample ACF may cut off to zero after lag 1, which suggests that an
#MA(1) model may be used.

#Begin by fitting an ARIMA(0, 1, 1) × (0, 1, 0)12 model.

model1 <- arima(em_house_prices,order=c(0,1,1),
             seasonal=list(order=c(0,1,0), period=12),
             method="ML")
model1

#Note that the model1 aic = 1832.14 

#Calculate the residuals of the model
resid1<-residuals(model1)
#Time plot of the residuals
ts.plot(resid1, main = "Time plot of residuals")

#Plot ACF of model1 residuals
acf(resid1, main = "ACF of model1")


#Function to produce p-values for the Ljung_Box test for different lags
#where an ARIMA(p,d,q)x(P,D,Q)_h model has been fitted.
#Note that k must be > p+q+P+Q 
#Number of degrees of freedom for the test = k-p-q-P-Q

#Arguments for the function "LB_test"
#resid = residuals from a fitted ARIMA(p,d,q)x(P,D,Q)_h model

#max.k = the maximum value of k at which we perform the test
#Note that the minimum k is set at p+q+P+Q+1 (corresponding to a test with one degree
#of freedom)

#p = Order of the non-seasonal AR part of the model
#q = Order of the non-seasonal MA part of the model
#P = Order of the seasonal AR part of the model
#Q = Order of the seasonal MA part of the model 

#The function returns a table with one column showing the number of degrees 
#of freedom for the test and the other the associated P-value.
LB_test_SARIMA<-function(resid,max.k,p,q,P,Q){
  lb_result<-list()
  df<-list()
  p_value<-list()
  for(i in (p+q+P+Q+1):max.k){
    lb_result[[i]]<-Box.test(resid,lag=i,type=c("Ljung-Box"),fitdf=(p+q+P+Q))
    df[[i]]<-lb_result[[i]]$parameter
    p_value[[i]]<-lb_result[[i]]$p.value
  }
  df<-as.vector(unlist(df))
  p_value<-as.vector(unlist(p_value))
  test_output<-data.frame(df,p_value)
  names(test_output)<-c("deg_freedom","LB_p_value")
  return(test_output)
}


#Perform Ljung-Box tests for the model1 residuals
model1.LB<-LB_test_SARIMA(resid1, max.k=11, p=0, q=1, P=0, Q=0)
model1.LB

#Produce a plot of the p-values against the degrees of freedom 
plot(model1.LB$deg_freedom,model1.LB$LB_p_value,xlab="Degrees of freedom",ylab="Pvalue",main="Ljung-Box test P-values",ylim=c(0,1))
#Add a blue dashed line at 0.05
abline(h=0.05,col="blue",lty=2)

#The time plot of the residuals appears to look similar to that for white noise. 
#The sample ACF seems to be close to zero at all lags > 0. 
#Therefore, the residuals are independent.
#The p-values are also quite less. 
#An alternative model should be considered to fit the data.

#Fitting an ARIMA(0, 1, 2) × (0, 1, 0)12 model.
model2 <- arima(em_house_prices,order=c(0,1,2),
                seasonal=list(order=c(0,1,0), period=12),
                method="ML")
model2

#Note that the model2 aic = 1830.26

#Calculate the residuals of model2
resid2<-residuals(model2)
#Time plot of model2 residuals
ts.plot(resid2,main = "Time plot of residuals")

#Plot ACF of model2 residuals
acf(resid2, main = "ACF of model2 residuals")

#Ljung-Box tests for model2 residuals
model2.LB<-LB_test_SARIMA(resid2, max.k=11, p=0, q=2, P=0, Q=0)
model2.LB

#Produce a plot of the p-values against the degrees of freedom 
plot(model2.LB$deg_freedom,model2.LB$LB_p_value,xlab="Degrees of freedom",ylab="Pvalue",main="Ljung-Box test P-values",ylim=c(0,1))
#Add a blue dashed line at 0.05
abline(h=0.05,col="blue",lty=2)

#Even though the model1 aic = 1832.14 is greater than model2 aic = 1830.26,
#hypothesis test for ma2 |0.1893/0.0960|= 1.9718 (which is less than 2)
#Do not reject H0: theta2=0
#Hence ARIMA(0, 1, 2) × (0, 1, 0)12 model is not the best fit for the data.


#Fitting an ARIMA(1, 1, 1) × (0, 1, 0)12 model.
model3 <- arima(em_house_prices,order=c(1,1,1),
                seasonal=list(order=c(0,1,0), period=12),
                method="ML")
model3

#Note that model3 aic = 1831.9

#Calculate the residuals of model3
resid3<-residuals(model3)
#Time plot of model3 residuals
ts.plot(resid3,main = "Time plot of residuals")

#Plot ACF of model3 residuals
acf(resid3, main = "ACF of model3 residuals")

#Ljung-Box test for model3 residuals
model3.LB<-LB_test_SARIMA(resid3, max.k=11, p=1, q=1, P=0, Q=0)
model3.LB

#To produce a plot of the p-values against the degrees of freedom 
plot(model3.LB$deg_freedom,model3.LB$LB_p_value,xlab="Degrees of freedom",ylab="Pvalue",main="Ljung-Box test P-values",ylim=c(0,1))
#Add a blue dashed line at 0.05
abline(h=0.05,col="blue",lty=2)

#The model3 aic = 1831.9, however its p-values are fairly small,
#hence, ARIMA(1, 1, 1) × (0, 1, 0)12 model is not a good fit.

#Fitting an ARIMA(1, 1, 2) × (0, 1, 0)12 model.
model4 <- arima(em_house_prices,order=c(1,1,2),
                seasonal=list(order=c(0,1,0), period=12),
                method="ML")
model4

#Note that model4 aic = 1825.04

#Calculate the residuals of model4
resid4<-residuals(model4)
#Time plot of model4 residuals
ts.plot(resid4, main= "Time plot of model4 residuals")

#Plot ACF of model4 residuals
acf(resid4, main = "ACF of model4 residuals")

#Ljung-Box test for model4 residuals
model4.LB<-LB_test_SARIMA(resid4, max.k=11, p=1, q=2, P=0, Q=0)
model4.LB

#To produce a plot of the P-values against the degrees of freedom 
plot(model4.LB$deg_freedom,model4.LB$LB_p_value,xlab="Degrees of freedom",ylab="Pvalue",main="Ljung-Box test P-values",ylim=c(0,1))
#Add a blue dashed line at 0.05
abline(h=0.05,col="blue",lty=2)

#The model4 aic = 1825.04 is lesser than model1 aic = 1832.14
#Hypothesis test for ma2 |0.5549/0.1003|= 5.532 (greater than 2)
#All p values > 0.05
#Therefore, ARIMA(1, 1, 2) × (0, 1, 0)12 model is the best fit until now.
#Howver more models can be checked for best result.

#Fitting an ARIMA(2, 1, 1) × (0, 1, 0)12 model.
model5 <- arima(em_house_prices,order=c(2,1,1),
                seasonal=list(order=c(0,1,0), period=12),
                method="ML")
model5

#Note that model5 aic = 1833.84

#The current best model(model4 - aic = 1825.04) is lesser than that of model 5 - aic = 1833.84
#Therefore, ARIMA(2, 1, 1) × (0, 1, 0)12 model is not a good fit. 
#Additionally trying seasonality components for model 4 with P=1

model6 <- arima(em_house_prices,order=c(1,1,2),
                seasonal=list(order=c(1,1,0), period=12),
                method="ML")
model6

#Note that model6 aic = 1810.82

#Calculate the residuals of model6
resid6<-residuals(model6)
#Time plot of model6 residuals
ts.plot(resid6, main = "Time plot of model6 residuals")

#Plot ACF of model6 residuals
acf(resid6, main = "ACF of model6 residuals")

#Ljung-Box test for model6 residuals
model6.LB<-LB_test_SARIMA(resid6, max.k=11, p=1, q=2, P=1, Q=0)
model6.LB

#Produce a plot of p-values against the degrees of freedom
plot(model6.LB$deg_freedom,model6.LB$LB_p_value,xlab="Degrees of freedom",ylab="Pvalue",main="Ljung-Box test P-values",ylim=c(0,1))
#Add a blue dashed line at 0.05
abline(h=0.05,col="blue",lty=2)

#The model6 aic = 1810.82 is lesser than model4 aic = 1825.04,
#Hypothesis test for SAR1 |-0.4729/0.0944|= 5.0095(greater than 2)
#All thep values > 0.05

#Therefore, the best model until now is model6.

#Similarly, try seasonality components for model 4 with Q=1
model7 <- arima(em_house_prices,order=c(1,1,2),
                seasonal=list(order=c(0,1,1), period=12),
                method="ML")
model7

#Note that model7 aic = 1790.89

#Calculate the residuals of model7
resid7<-residuals(model7)
#Time plot of Residuals of model7
ts.plot(resid7, main = "Time plot of residuals of model7")

#Plot sample ACF of model7 residuals
acf(resid7, main = "ACF of model7 residuals")

#Ljung-Box test for model7 residuals
model7.LB<-LB_test_SARIMA(resid7, max.k=11, p=1, q=2, P=0, Q=1)
model7.LB

#Produce a plot of the p-values against the degrees of freedom
plot(model7.LB$deg_freedom,model7.LB$LB_p_value,xlab="Degrees of freedom",ylab="Pvalue",main="Ljung-Box test P-values",ylim=c(0,1))
#Add a blue dashed line at 0.05
abline(h=0.05,col="blue",lty=2)

#The model7 aic=1790.89 is lesser than model6 aic=1810.82
#Hypothesis test for SMA1 |-0.8109/0.1337|= 6.06(greater than 2)
#All p values > 0.05
#Model7 is the best fit model uptil now

#Seasonality components for model 7 with P=1
model8 <- arima(em_house_prices,order=c(1,1,2),
                seasonal=list(order=c(1,1,1), period=12),
                method="ML")
model8

#Note that model8 aic = 1792.74

#Calculate the model8 residuals
resid8<-residuals(model8)
#Time plot of residuals of model8
ts.plot(resid8, main = "Time plot of residuals of model8")

#Plot ACF of model8 residuals
acf(resid8, main(" ACF of model8 residuals"))

#Ljung-Box test for model8 residuals
model8.LB<-LB_test_SARIMA(resid8, max.k=11, p=1, q=2, P=1, Q=1)
model8.LB

#Produce a plot of the p-values against the degrees of freedom
#Add a blue dashed line at 0.05
plot(model8.LB$deg_freedom,model8.LB$LB_p_value,xlab="Degrees of freedom",ylab="Pvalue",main="Ljung-Box test P-values",ylim=c(0,1))
abline(h=0.05,col="blue",lty=2)

#The model8 aic = 1792.74 is greater than model7 aic = 1790.89

#Hypothesis test for SAR1 |0.0575/0.1502|= 0.3838(< 2)
#Hence, sar1 should not be included
#All p values > 0.05
#Model8 is not a good fit

#On analysis of multiple models, model 7 seems to be the best fit for the data with 
#least aic and all p-values > 0.05.
#ARIMA(1, 1, 2) × (0, 1, 1)12 model is best fit. 

#Forecasting using the selected ARIMA model
install.packages("forecast")

#Access the library forecast
library(forecast)

#Forecasting values for the next 6 months
forecast_values <- forecast(model7, h = 6)

#Display the forecasted values
print(forecast_values)

#Plot the forecasted values along with the the data
plot(forecast_values, main = "Forecasted Monthly House Prices (2020)",
     xlab = "Date", ylab = "House Price (GBP)")
