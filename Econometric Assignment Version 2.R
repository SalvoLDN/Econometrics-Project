#####SET UP WORKING ENVIRONMENT#####
###Set working directory###
setwd("C:/Users/USERTEST/Desktop/EconometricsProject")

###Load libraries###
library(AER)
library(MASS)
library(dynlm)
library(forecast)
library(readxl)
library(stargazer)
library(scales)
library(quantmod)
library(urca)
library(tseries)
library(vars)
library(ggplot2)

###Read database file###
databaseproject <- read.csv("C:/Users/USERTEST/Desktop/EconometricsProject/AssignmentEconometrics.csv")

###View the data###
ls(databaseproject)
head(databaseproject)
View(databaseproject)

###Clean data of N/As###
databaseproject<-na.omit(databaseproject)

###Check how R has stored the data###
str(databaseproject)

###Format date column###
databaseproject$ï..Year <- as.Date(paste(databaseproject$ï..Year,12,31,sep="-"))

###Adjust column names###
colnames(databaseproject)[1]<-"Year"

####DATA VISUALISATION####
###Create variables needed and add column to the data set###
attach(databaseproject)
##Average hourly earnings##
databaseproject$AHW_F <- WBR_F.after.tax/E_F
databaseproject$AHW_M <- WBR_M..after.tax/E_M
##Gender wage gap##
attach(databaseproject)
databaseproject$G_W_Gap <- (AHW_M - AHW_F)/AHW_M
##GDP##
attach(databaseproject)
databaseproject$GDP <- ((AHW_M * E_M) + (AHW_F * E_F)) + PPR.after.tax
##Logs (to linearise exponential trends)##
databaseproject$ln_GDP<-log(databaseproject$GDP)
databaseproject$ln_AHW_M<-log(databaseproject$AHW_M)
databaseproject$ln_E_M<-log(databaseproject$E_M)
databaseproject$ln_AHW_F<-log(databaseproject$AHW_F)
databaseproject$ln_E_F<-log(databaseproject$E_F)
databaseproject$ln_G_W_Gap<-log(databaseproject$G_W_Gap)
databaseproject$ln_PPR.after.tax<-log(databaseproject$PPR.after.tax)
databaseproject$ln_WBR_M<-log(databaseproject$WBR_M)
##Create time-series vector##
ts.databaseproject<-ts(databaseproject[,2:15],start=c(1970),frequency = 1)
#GDP plot#
attach(databaseproject)
year=ts(1970:2012)
GDP_plot=ts(GDP,start=c(1970),frequency=1)
plot(GDP_plot,
     col = "black",
     lwd = 2,
     ylab = "GDP",
     xlab = "Year",
     main = "U.K. Yearly Real GDP")
#GDP Growth log in p.p.##
attach(databaseproject)
year=ts(1970:2012)
ln_GDP_plot=ts(ln_GDP,start=c(1970),frequency=1)
plot(ln_GDP_plot,
     col = "purple",
     lwd = 2,
     ylab = "GDP",
     xlab = "Year",
     main = "U.K. Yearly Real GDP Growth in p.p.")
#Average Hourly Wage Female p.p.#
attach(databaseproject)
ln_AHW_F_plot=ts(ln_AHW_F ,start=c(1970),frequency=1)
plot(ln_AHW_F_plot,
     col = "red",
     lwd = 2,
     ylab = "ln_AHW_F",
     xlab = "Year",
     main = "U.K.Average Hourly Wage of Females in p.p.")
#Average Hourly Wage Male p.p.#
attach(databaseproject)
ln_AHW_M_plot=ts(ln_AHW_M ,start=c(1970),frequency=1)
plot(ln_AHW_M_plot,
     col = "blue",
     lwd = 2,
     ylab = "ln_AHW_M",
     xlab = "Year",
     main = "U.K.Average Hourly Wage of Males in p.p.")
#Average Hourly Wages comparison#
attach(databaseproject)
plot(Year,
     ln_AHW_M,
     type="l",
     col = 4,
     ylim=c(1.7,3),
     ylab = "Average Hourly Wage in p.p.",
     xlab = "Year",
     main = "U.K.Average Hourly Wage for Males and Females in p.p.")
lines(Year,
      ln_AHW_F,
       type="l",
       col = 2)
legend("bottomright", legend=c("ln_AHW_M", "ln_AHW_F"),
       col=c("blue", "red"), lty=1, cex=0.7)
#Gender Wage Gap plot#
attach(databaseproject)
GenderwageGap_plot=ts(G_W_Gap,start=c(1970),frequency=1)
plot(GenderwageGap_plot,
     col = "green",
     lwd = 2,
     ylab = "Relative Wage Ratio",
     xlab = "Year",
     main = "U.K.Relative Gender Wage Ratio")
#Gender Wage Gap plot in p.p.#
attach(databaseproject)
ln_GenderwageGap_plot=ts(ln_G_W_Gap,start=c(1970),frequency=1)
plot(ln_GenderwageGap_plot,
     col = "green",
     lwd = 2,
     ylab = "Growth Rate in p.p.",
     xlab = "Year",
     main = "Relative Gender Wage Gap Growth Rate in p.p.")
#Average employment rate in hours Females#
attach(databaseproject)
E_F_plot=ts(E_F ,start=c(1970),frequency=1)
plot(E_F_plot,
     col = "red",
     lwd = 2,
     ylab = "E_F",
     xlab = "Year",
     main = "U.K.Average Employment rate of Females in hours")
#Average employment rate in hours Males#
attach(databaseproject)
E_M_plot=ts(E_M ,start=c(1970),frequency=1)
plot(E_M_plot,
     col = "blue",
     lwd = 2,
     ylab = "E_M",
     xlab = "Year",
     main = "U.K.Average Employment rate of Males in hours")
#Average employment rate in hours by gender#
attach(databaseproject)
plot(Year,
     E_M,
     type="l",
     col = "blue",
     ylim=c(19, 32),
     ylab = "Average Hours",
     xlab = "Year",
     main = "U.K.Average employment rate in hours by gender")
lines(Year,
      E_F,
      type="l",
      col = "red")
legend("bottomright", legend=c("E_M", "E_F"),
       col=c("blue", "red"), lty=1, cex=0.7)

####REGRESSION ANALYSIS####

###Regression on levels###
attach(databaseproject)
Model_1 <-lm(ln_GDP~ln_AHW_M+ln_G_W_Gap+ln_E_F+ln_WBR_M+ln_PPR.after.tax, data=databaseproject)
summary(Model_1)
head(fortify(Model_1))
residPlot<-ggplot(aes(x=.fitted, y=.resid), data=Model_1)+geom_line()+geom_hline(yintercept=0)+labs(x="Fitted Values", y="Residual")
residPlot

###Testing procedure###

##Log of GDP variable##
LogGDP<-log(GDP)

##Autocorrelation##

#Durbin-Watson Test#
durbinWatsonTest(Model_1)
plot(Model_1$residuals)
plot.ts(Model_1$residuals)

##Stationarity - Unit Root##
#Augmented Dickey-Fuller Test#
adf.test(LogGDP,alternative="stationary",k=1)
adf.test(ln_AHW_M,alternative="stationary",k=1)
adf.test(ln_G_W_Gap,alternative="stationary",k=1)
adf.test(ln_E_F,alternative="stationary",k=1)
adf.test(ln_WBR_M,alternative="stationary",k=1)
adf.test(ln_PPR.after.tax,alternative="stationary",k=1)
#ADF without a "drift" or a "linear-trend"#
ADF_test_none<-ur.df(LogGDP, 
                     type = "none", 
                     lags = 20, 
                     selectlags = "Fixed")
summary(ADF_test_none)
#ADF including a drift parameter#
ADF_test_drift<-ur.df(LogGDP, 
                      type = "drift", 
                      lags = 15, 
                      selectlags = "Fixed")
summary(ADF_test_drift)
#ADF with a trend#
ADF_test_trend<-ur.df(LogGDP, 
                      type = "trend", 
                      lags = 8, 
                      selectlags = "Fixed")
summary(ADF_test_trend)

#ADF using AIC#
#LogGDP
ADF_test_trend_LogGDP_AIC<-ur.df(LogGDP, 
                          type = "trend", 
                          lags = 15, 
                          selectlags = "AIC")
summary(ADF_test_trend_LogGDP_AIC)
#ln_AHW_M
ADF_test_trend_ln_AHW_M_AIC<-ur.df(ln_AHW_M, 
                                type = "trend", 
                                lags = 15, 
                                selectlags = "AIC")
summary(ADF_test_trend_ln_AHW_M_AIC)
#ln_W_Gap
ADF_test_trend_GWGAP_AIC<-ur.df(ln_G_W_Gap, 
                          type = "trend", 
                          lags = 15, 
                          selectlags = "AIC")
summary(ADF_test_trend_GWGAP_AIC)
#ln_E_F
ADF_test_trend_ln_E_F_AIC<-ur.df(ln_E_F, 
                                type = "trend", 
                                lags = 15, 
                                selectlags = "AIC")
summary(ADF_test_trend_ln_E_F_AIC)
#ln_WBR_M
ADF_test_trend_ln_WBR_M_AIC<-ur.df(ln_WBR_M, 
                                type = "trend", 
                                lags = 15, 
                                selectlags = "AIC")
summary(ADF_test_trend_ln_WBR_M_AIC)
#ln_PPR.after.tax
ADF_test_trend_ln_PPR.after.tax_AIC<-ur.df(ln_PPR.after.tax, 
                                type = "trend", 
                                lags = 15, 
                                selectlags = "AIC")
summary(ADF_test_trend_ln_PPR.after.tax_AIC)

#Phillips-Perron test#
pp.test(LogGDP,alternative="stationary",type="Z(t_alpha)")
pp.test(ln_AHW_M,alternative="stationary",type="Z(t_alpha)")
pp.test(ln_G_W_Gap,alternative="stationary",type="Z(t_alpha)")
pp.test(ln_E_F,alternative="stationary",type="Z(t_alpha)")
pp.test(ln_WBR_M,alternative="stationary",type="Z(t_alpha)")
pp.test(ln_PPR.after.tax,alternative="stationary",type="Z(t_alpha)")

##Cointegration##

#Johansen Test#
#Bind variables to test
dset <- cbind(ln_GDP, ln_AHW_M, ln_G_W_Gap, ln_E_F, ln_WBR_M, ln_PPR.after.tax)
#Lag Selection Criteria
lagselect <- VARselect(dset, lag.max = 15, type = "const")
lagselect$selection
#Johansen Testing (Trace)
Jo_test_tr <- ca.jo(dset, type = "trace", ecdet = "const", K = 4)
summary(Jo_test_tr)
#Johansen Testing (MaxEigen)
Jo_test_ei <- ca.jo(dset, type = "eigen", ecdet = "const", K = 4)
summary(Jo_test_ei)

#Engle-Granger#
#Step 1: Run a regression between the variables of interest and store the residuals
Step_1_reg<-lm (ln_GDP ~ ln_G_W_Gap)
summary(Step_1_reg)
resid_G_W_Gap_reg<-ts(residuals(Step_1_reg), start = 1970, frequency = 1)
plot(Step_1_reg$residuals)
plot.ts(Step_1_reg$residuals)
#Step 2: Test Stationarity of the residuals
ADF_test_resid_AIC<-ur.df(resid_G_W_Gap_reg, 
                          type ="none", 
                          lags = 12, 
                          selectlags = "AIC")
summary(ADF_test_resid_AIC)

##Regression in first differences model##

GDP_Diff<-diff(ln_GDP,lag=1)
AHW_M_Diff<-diff(ln_AHW_M,lag=1)
G_W_Gap_Diff<-diff(ln_G_W_Gap,lag=1)
E_F_Diff<-diff(ln_E_F,lag=1)
WBR_M_Diff<-diff(ln_WBR_M,lag=1)
PPR_Diff<-diff(ln_PPR.after.tax,lag=1)

regression_fo_diff<-lm(GDP_Diff ~ AHW_M_Diff + G_W_Gap_Diff + E_F_Diff + WBR_M_Diff + PPR_Diff)
summary(regression_fo_diff)
#Tests for new regression model#
#Autocorrelation
acf(residuals(regression_fo_diff), plot=T)
#Heteroskedasticity
bptest(regression_fo_diff)
#Adjustments to be made to coefficients#
#Heteroskedasticity adjustment
coeftest(regression_fo_diff, vcov. = vcovHC, type = "HC1")
#Autocorrelation adjustment
coeftest(regression_fo_diff, vcov = NeweyWest, prewhite = F, adjust = T)

##ARDL##
df2<-cbind(GDP_Diff, AHW_M_Diff, G_W_Gap_Diff, E_F_Diff, WBR_M_Diff, PPR_Diff)
Regr_ARDL<-dynlm(L(GDP_Diff)~L(GDP_Diff)+L(AHW_M_Diff)+L(G_W_Gap_Diff)+L(E_F_Diff)+L(WBR_M_Diff)+L(PPR_Diff))
summary(Regr_ARDL)
head(fortify(Regr_ARDL))
residPlot<-ggplot(aes(x=.fitted, y=.resid), data=(Regr_ARDL))+geom_line()+geom_hline(yintercept=0)+labs(x="Fitted Values", y="Residual")
residPlot
####EXTRACTION OF RELEVANT TABLES####

###Regression tables###
SIMPLEMRM<- Model_1
ARDL<- Regr_ARDL
REGFIRSTDIF<-regression_fo_diff


stargazer(SIMPLEMRM, ARDL, 
          type = "html",
          out="Regression tables.htm")
stargazer(REGFIRSTDIF, 
          type = "html",
          out="Regression first differences.htm")

###Tests###

##Autocorrelation##
#Durbin-Watson#

##Stationarity - Unit Root##
#Augmented Dickey-Fuller Test#
#ADF using AIC#
#logGDP
#ln_G_W_Gap
#Phillips-Perron test#

##Cointegration##
#Engle-Granger#
#Johansen Test#
#Johansen Testing (Trace)
#Johansen Testing (MaxEigen)

##autocorrelationfodiff##
#adf
##heteroskedasticityfo##
#Breush-Pagan
#adjustments#
#Newey-West
