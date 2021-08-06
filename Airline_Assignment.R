install.packages("fpp")
install.packages("readr")
install.packages("smooth")
install.packages("tseries")
install.packages("forecast")
install.packages("fastDummies")
library("fastDummies")
library("forecast")
library("tseries")
library("readxl")
library("smooth")
library("readr")
library("fpp")

#Reading file
doc <- read_xlsx(file.choose())
View(doc)

#Converting the target column into time-series entity
tspass <- ts(doc$Passengers,frequency = 12)
plot(tspass)

#Dividing the data into training and testing data
train <- tspass[1:84]
train <- ts(train,frequency = 12)
test <- tspass[85:96]
test <- ts(test,frequency = 12)
View(test)
###################################################################
######################### MODEL BUILDING ##########################
###################################################################
                              
######################### Data Driven Approaches

#Using Holt-Winter Function with alpha value
holta <- HoltWinters(train,alpha = 0.2,beta=F,gamma=F)
holta
holta_pred <- data.frame(predict(holta,n.ahead=12))
holta_pred
holta_mape <- MAPE(holta_pred$fit,test)*100
holta_mape
plot(forecast(holta,h=12))

#With alpha and beta value
holtab <- HoltWinters(train,alpha = 0.2,beta=0.15,gamma=F)
holtab
holtab_pred <- data.frame(predict(holtab,n.ahead = 12))
holtab_pred
holtab_mape <- MAPE(holtab_pred$fit,test)*100
holtab_mape
plot(forecast(holtab,h=12))

#With alpha, beta and gamma value
holtabg <- HoltWinters(train,alpha=0.2,beta=0.15,gamma=0.05)
holtabg
holtabg_pred <- data.frame(predict(holtabg,n.ahead = 12))
holtabg_pred
holtabg_mape <- MAPE(holtabg_pred$fit,test)*100
holtabg_mape
plot(forecast(holtabg,h=12))

#With Optimum Value of alpha value
holtna <- HoltWinters(train,beta = F,gamma = F)
holtna
holtna_pred <- data.frame(predict(holtna,n.ahead = 12))
holtna_pred
holtna_mape <- MAPE(holtna_pred$fit,test)*100
holtna_mape
plot(forecast(holtna,h=12))

#With Optimum Values of alpha and beta value
holtnab <- HoltWinters(train,gamma=F)
holtnab
holtnab_pred <- data.frame(predict(holtnab,n.ahead=12))
holtnab_pred
holtnab_mape <- MAPE(holtnab_pred$fit,test)*100
holtnab_mape
plot(forecast(holtnab,h=12))

#With Optimum Values of alpha, beta and gamma value
holtnabg <- HoltWinters(train)
holtnabg
holtnabg_pred <- data.frame(predict(holtnabg,n.ahead = 12))
holtnabg_pred
holtnabg_mape <- MAPE(holtnabg_pred$fit,test)*100
holtnabg_mape
plot(forecast(holtnabg,h=12))

#Creating a table of Models and its MAPE Values
table1 <- data.frame(c("HoltWinter with Alpha","HoltWinter with Alpha and Beta",
                       "HoltWinter with Alpha, Beta and Gamma","HoltWinter with optimum value of Alpha",
                       "HoltWinter with Optimum value of Alpha and Beta","HoltWinter with Optimum value of Alpha, Beta and Gamma Value"),
                     c(holta_mape,holtab_mape,holtabg_mape,holtna_mape,holtnab_mape,holtnabg_mape))
colnames(table1) <- c("Model Name","MAPE Values")
View(table1)

# MAPE value is the least for HoltWinter with Optimum value of Alpha, Beta and Gamma
holtnabg_final <- HoltWinters(tspass)
holtnabg_finalpred <- data.frame(predict(holtnabg_final,n.ahead=12))
Final_mape <- MAPE(holtnabg_finalpred$fit,test)*100
Final_mape
plot(forecast(holtnabg_final,h=12))

#                -----------------------------------------------              
#Using SES, Holt and HW function

#SES Function with alpha value
sesa <- ses(tspass,alpha = 0.2,h=12)
plot(forecast(sesa,n.ahead=12))
sesa_pred <- data.frame(predict(sesa,h=12))
sesa_mape <- MAPE(sesa_pred$Point.Forecast,test)*100
sesa_mape

#SES Function with optimum alpha value
sesn <- ses(train,alpha = NULL,h=12)
plot(forecast(sesn,n.ahead=12))
sesn_pred <- data.frame(predict(sesn,h=12))
sesn_mape <- MAPE(sesn_pred$Point.Forecast,test)*100
sesn_mape

#Holt Function with alpha and beta value
hoab <- holt(train,alpha = 0.2,beta=0.15,h=12)
plot(forecast(hoab,n.ahead=12))
hoab_pred <- data.frame(predict(hoab,h=12))
hoab_mape <- MAPE(hoab_pred$Point.Forecast,test)*100
hoab_mape

#Holt Function with optimum alpha and beta value
honab <- holt(train,alpha=NULL,beta=NULL,h=12)
plot(forecast(honab,n.ahead=12))
honab_pred <- data.frame(predict(honab,h=12))
honab_mape <- MAPE(honab_pred$Point.Forecast,test)*100
honab_mape

#HW Function with alpha beta and gamma value
hwabg <- hw(train,alpha = 0.2,beta=0.15,gamma = 0.05)
plot(forecast(hwabg,n.ahead=12))
hwabg_pred <- data.frame(predict(hwabg,h=12))
hwabg_mape <- MAPE(hwabg_pred$Point.Forecast,test)*100
hwabg_mape

#HW Function with optimum alpha, beta and gamma value
hwn <- hw(train, alpha = NULL, beta = NULL, gamma = NULL,h=12)
plot(forecast(hwn,n.ahead=12))
hwn_pred <- data.frame(predict(hwn,h=12))
hwn_mape <- MAPE(hwn_pred$Point.Forecast,test)*100
hwn_mape

#Creating a table with MAPE Values
table2 <- data.frame(c("SES_Alpha","SES_No_Alpha","Holt_Alpha_Beta","Holt_No_Alpha_Beta",
                       "HW_ABG","HW_No_ABG"),c(sesa_mape,sesn_mape,hoab_mape,honab_mape,
                                               hwabg_mape,hwn_mape))
colnames(table2) <- c("Models","MAPE Values")
View(table2)

#Based on the table HW function with NULL values has the least MAPE Values
#Thus, the final model :
hwn_final <- hw(tspass,alpha=NULL,beta=NULL,gamma = NULL)
plot(forecast(hwn_final,n.ahead=12))
hwn_final_pred <- data.frame(predict(hwn_final,h=12))
hwn_final_mape <- MAPE(hwn_final_pred$Point.Forecast,test)*100
hwn_final_mape

#___________________________________________________________________________________________#

############################## MODEL Driven Approaches

#Pre-Processing data
View(doc)
X <- data.frame(months(doc$Month),doc$Passengers)
colnames(X) <- c("Month","Passengers")
file <- fastDummies::dummy_cols(X,select_columns = "Month")
View(file)
file["Log_Pass"]=log(file$Passengers)
file["t"]=c(1:96)
file["t_sq"]=file$t*file$t

#Training and Testing Data
trn=file[1:84,]
tst=file[85:96,]

# Linear Model
lin_mod <- lm(Passengers~t,data=trn)
summary(lin_mod)
lin_pred <- data.frame(predict(lin_mod,interval = "predict",newdata = tst))
rmse_lin <- sqrt(mean((tst$Passengers-lin_pred$fit)^2,na.rm = T))
rmse_lin

#Exponential Model
exp_mod <- lm(Log_Pass~t,data = trn)
summary(exp_mod)
exp_pred <- data.frame(predict(exp_mod,interval = "predict",newdata = tst))
rmse_exp <- sqrt(mean((tst$Passengers-exp(exp_pred$fit))^2,na.rm = T))
rmse_exp

#Quadratic Model
qua_mod <- lm(Passengers~t+t_sq,data=trn)
summary(qua_mod)
qua_pred <- data.frame(predict(qua_mod,interval = "predict",newdata = tst))
rmse_qua <- sqrt(mean((tst$Passengers-qua_pred$fit)^2,na.rm = T))
rmse_qua

#Additive Seasonality Model
adv_mod <- lm(Passengers~Month_January+Month_February+Month_March+Month_April+
                Month_May+Month_June+Month_July+Month_August+Month_September+Month_October+
                Month_November+Month_December ,data = trn)
summary(adv_mod)
adv_pred <- data.frame(predict(adv_mod,interval = 'predict',newdata = tst))
rmse_adv <- sqrt(mean((tst$Passengers-adv_pred$fit)^2,na.rm=T))
rmse_adv

#Additive Seasonality with Quadratic Model
advqua_mod <- lm(Passengers~t+t_sq+Month_January+Month_February+Month_March+Month_April+
                Month_May+Month_June+Month_July+Month_August+Month_September+Month_October+
                Month_November+Month_December ,data = trn)
summary(advqua_mod)
advqua_pred <- data.frame(predict(advqua_mod,interval = 'predict',newdata = tst))
rmse_advqua <- sqrt(mean((tst$Passengers-advqua_pred$fit)^2,na.rm=T))
rmse_advqua

#Multiplicative Seasonality Model
mul_mod <- lm(Log_Pass~Month_January+Month_February+Month_March+Month_April+
                Month_May+Month_June+Month_July+Month_August+Month_September+Month_October+
                Month_November+Month_December ,data = trn)
summary(mul_mod)
mul_pred <- data.frame(predict(mul_mod,interval = 'predict',newdata = tst))
rmse_mul <- sqrt(mean((tst$Passengers-mul_pred$fit)^2,na.rm = T))
rmse_mul

#Creating a table with all the RMSE Values
table3 <- data.frame(c("Linear Model","Exponential Model","Quadratic Model","Additive Seasonality",
                       "Additive Seasonality with Quadratic Model","Multiplicative Model"),
                     c(rmse_lin,rmse_exp,rmse_qua,rmse_adv,rmse_advqua,rmse_mul))
colnames(table3) <- c("Model","RMSE Values")
table3

#Additive Seasonality with Quadratic Model has the least RMSE Value
#So, Final Model and the prediction
final_mod <- lm(Passengers~t+t_sq+Month_January+Month_February+Month_March+Month_April+
                  Month_May+Month_June+Month_July+Month_August+Month_September+Month_October+
                  Month_November+Month_December ,data = file)
summary(final_mod)
final_pred <- data.frame(predict(final_mod,interval="predict",newdata=tst))
plot(final_mod)

#ACF Calculation
acf(final_mod$residuals,lag.max=10)
A <- arima(final_mod$residuals,order=c(1,0,0))
error <- data.frame(forecast(A,h=12))
plot(A)
acf(A$residuals,lag.max=10)
future_errors <- error$Point.Forecast

#Final Prediction
Predicted_Values=final_pred+future_errors
rmse_final <- sqrt(mean((tst$Passengers-Predicted_Values$fit)^2,na.rm = T))
rmse_final
Predicted_Values$fit
acf(Predicted_Values$fit,lag.max = 10)
 