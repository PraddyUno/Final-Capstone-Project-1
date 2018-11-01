install <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) 
    install.packages(new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
}

# usage
required.packages <- c("fpp","gbm","randomForest","GGally","ggplot2", "reshape2", "strucchange", "forecast", "astsa", "lmtest","fUnitRoots","FitARMA","Rmisc","fBasics","urca")
install(required.packages)

suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(astsa))
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(fUnitRoots))
suppressPackageStartupMessages(library(FitARMA))
suppressPackageStartupMessages(library(strucchange))
suppressPackageStartupMessages(library(reshape))
suppressPackageStartupMessages(library(Rmisc))
suppressPackageStartupMessages(library(fBasics))
suppressPackageStartupMessages(library(urca))
suppressPackageStartupMessages(library(GGally))
suppressPackageStartupMessages(library(corrplot))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(gbm))
suppressPackageStartupMessages(library(fpp))
# Define theme
theme_ts <- theme(panel.border = element_rect(fill = NA, 
                                              colour = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.minor = element_line(colour = "grey85"),
                  panel.grid.major = element_line(colour = "grey85"),
                  panel.grid.major.x = element_line(colour = "grey85"),
                  axis.text = element_text(size = 13, face = "bold"),
                  axis.title = element_text(size = 15, face = "bold"),
                  plot.title = element_text(size = 16, face = "bold"),
                  strip.text = element_text(size = 16, face = "bold"),
                  strip.background = element_rect(colour = "black"),
                  legend.text = element_text(size = 15),
                  legend.title = element_text(size = 16, face = "bold"),
                  legend.background = element_rect(fill = "white"),
                  legend.key = element_rect(fill = "white"))

# load and transform data; drop unneeded columns
raw_dat <- read.csv("C:/Users/Pardeep/Documents/R/PardeepR/Capstone/County_Zhvi_AllHomes.csv" )
raw_dat <- raw_dat[raw_dat$State=="CA",
                    !(names(raw_dat) %in% c("RegionID","State","Metro", "StateCodeFIPS", "MunicipalCodeFIPS", "SizeRank"))]

# Define counties of interest
count_of_interest <- c("Alameda", "Contra Costa", "Marin", "San Francisco", "San Mateo", "Santa Clara", "Solano")
subdat <- raw_dat[raw_dat$RegionName %in% count_of_interest,]

# Transpose the data frame to organize the data with each row representing date of mesurement of time series
county.names <- subdat$RegionName # Extract the County names
county_dat <- as.data.frame(t(subdat[,-1])) # Remove the first column as it does not contain the data
colnames(county_dat) <- county.names # Assign the county names as coulmn labels to identify the data
# Convert row names as date (first of every month)
ym <- as.yearmon(rownames(county_dat), "X%Y.%m")
Hdate <- as.Date(ym)
county_datD <- cbind(Hdate, county_dat)
colnames(county_datD)[1] <- "Date"


# Now we hve clean data to work with. Lets convert the housing values into time series
ts_county_dat <- ts(county_dat,frequency = 12, start = c(1996,4))
# Plot all 5 time series
autoplot(ts_county_dat/100000)+
  ggtitle("Median-Monthly Housing Prices")+
  ylab("Cost ($100K)")+ 
  xlab("Year")+labs (col = "County")+
  theme_ts

#########################################
#########################################
############ Chapter 2 ##################
#########################################
#########################################

#########################################
## Correlation Plot
#########################################

cordata <- cor(ts_county_dat/100000)
corplot <- corrplot(cordata,cl.lim = c(0.5, 1))

AlamedaTS <- ts(county_dat$Alameda/100000, frequency = 12, start = c(1996,4)) # Define Alameda Time Series Variable.

autoplot(AlamedaTS)+
  ggtitle("Median-Monthly Housing Prices in Alameda County")+
  ylab("Cost ($100K)")+ 
  xlab("Year")+
  theme_ts

#########################################
## STL Decomposition
#########################################

AlamedaTS %>% mstl(lambda =1) -> fit.stl # lambda = 0 for additive and =1 for seasonal series
  fit.stl %>% autoplot()+
    ylab("Housing Price ($100K)")+
    theme_ts
#########################################
## Computing Trend Strength Equation 4.4
#########################################
fac <- var(remainder(fit.stl))/var(remainder(fit.stl)+trendcycle(fit.stl))
print("Trend strength factor:")
(Ft <- max(c(0,1-fac)))

##########################################
# Define test and train set indices
#########################################
# time is expressed as Year+(month-1)/12

tt <- time(AlamedaTS)
boundDate <- which(tt == 2018)
TrainIndex <- c(1:boundDate-1)

#################################################
####### Holt Exponential Smoothing #############
#################################################

# Training set time = Apr 1996 (index = 1) to Dec 2017 (index = 261)
Holt.AlamedaTS <- holt(AlamedaTS[TrainIndex], h=7)
Holt.AlamedaTS$model # Get the model Parameters (alpha, beta)

# Test data time = January 2018(index = 262) to July 2018 (index = 268)

# Computing Errors
# Cross Validation test error
Holt.Error <- tsCV(AlamedaTS[TrainIndex],holt, h=7) 
print("CV Error (%):")
(Holt.meanCV <- (mean(Holt.Error^2,na.rm = T))*100) # Mean Square of Errors

# Test MSE
holt.residual <- Holt.AlamedaTS$mean-AlamedaTS[-TrainIndex]
print("Test MSE (%):")
(Holt.MSE <- mean((holt.testError^2)*100))# Pecentage test MSE

# Output Matrix
Holt.op <- data.frame(Hdate[-TrainIndex],AlamedaTS[-TrainIndex],Holt.AlamedaTS$mean,holt.residual , holt.residual ^2)
colnames(Holt.op) <- c("Time","Test Data","Forecast", "Residuals", "Residual^2")
print(Holt.op, row.names=F)





ErrorMat <- cbind(Holt.meanCV,Holt.meanTest)
# Plot the fitted and Original Data
Holt.AlamedaTS %>% autoplot()+
  autolayer(fitted(Holt.AlamedaTS), series = "Fitted")+
  theme_ts
# Zoomed Plot for Test Data
matplot(Holt.AlamedaTS.Mat, pch = 19, type = "b", col = c(1:6), ylab = "Housing Price ($100K)", xlab = "Month",
        main = "Alameda County (2018): Test vs Prediction Data")
legend(1,9.3,legend=c("Forecast","Lower 80%","Lower 95%","Upper 80%","Upper 95%","Test Value"),pch=19, col = c(1:6))
grid()

#################################################
#################################################
####### TEST FOR STATIONARITY ########
#################################################
#################################################

# Augumented Dick Fuller Unit Root Test (Table 4.2)
AlamedaTS %>% ur.df("trend") %>% summary() # Test on original data
AlamedaTS %>% diff() %>% ur.df("trend") %>% summary() # Test on time series after first difference
AlamedaTS %>% diff() %>% diff() %>% ur.df("trend") %>% summary() # Test on time series after second difference


#  KPSS Unit Root Test (Table 4.3)
AlamedaTS %>% ur.kpss("tau") %>% summary() # Test on original data
AlamedaTS %>% diff()  %>% ur.kpss("tau") %>% summary() # Test on time series after first difference
AlamedaTS %>% diff() %>% diff() %>% ur.kpss("tau") %>% summary()  # Test on time series after second difference


##############################################
############## ARIMA MODELS ################
#################################################

# Model 1 - auto.arima()
#################################################
arima.fit1 <- auto.arima(AlamedaTS[TrainIndex],seasonal = F, stepwise = F, approximation = F)
summary(arima.fit1)
arima.fit1 %>% residuals() %>% ggtsdisplay()

# Model Forecast
# auto.arima forecast for next 7 months (Jan 2018 to July 2018)
arima.fc1 <- forecast(arima.fit1, h=7)
arima1.error <- AlamedaTS[-TrainIndex] - arima.fc1$mean # Residual

print("Test MSE (%):")
(arima1.MSE <- mean((arima.fc1$mean-AlamedaTS[-TrainIndex])^2*100))# Pecentage test MSE

arima1.op <- data.frame(Hdate[-TrainIndex],AlamedaTS[-TrainIndex],arima.fc1$mean,arima1.error, arima1.error^2)
colnames(arima1.op) <- c("Time","Test Data","Forecast", "Residuals", "Residual^2")
print(arima1.op, row.names=F)

# Zoomed Plot 
arima1.mat<- cbind(arima.fc1$mean,arima.fc1$lower,arima.fc1$upper,AlamedaTS[-TrainIndex])

matplot(arima1.mat, pch = 19, type = "b", col = c(1:6), ylab = "Housing Price ($100K)", xlab = "Month",
        main = "Alameda County (2018): Test vs Prediction Data")
legend(1,9.3,legend=c("Forecast","Lower 80%","Lower 95%","Upper 80%","Upper 95%","Test Value"),pch=19, col = c(1:6))
grid()


#################################################
# Model 2 - Arima(5,2,5)
#################################################
arima.fit2 <- Arima(AlamedaTS[TrainIndex],order = c(5,2,5))
summary(arima.fit2)
arima.fit2 %>% checkresiduals() 

# Model 2 Forecast
# auto.arima forecast for next 7 months (Jan 2018 to July 2018)
arima.fc2 <- forecast(arima.fit2, h=7)
arima2.error <- AlamedaTS[-TrainIndex] - arima.fc2$mean # Residual

print("Test MSE (%):")
(arima2.MSE <- mean((arima.fc2$mean-AlamedaTS[-TrainIndex])^2*100))# Pecentage test MSE

arima2.op <- data.frame(Hdate[-TrainIndex],AlamedaTS[-TrainIndex],arima.fc2$mean,arima2.error, arima2.error^2)
colnames(arima2.op) <- c("Time","Test Data","Forecast", "Residuals", "Residual^2")
print(arima2.op, row.names=F)

# Zoomed Plot 
arima2.mat<- cbind(arima.fc2$mean,arima.fc2$lower,arima.fc2$upper,AlamedaTS[-TrainIndex])

matplot(arima2.mat, pch = 19, type = "b", col = c(1:6), ylab = "Housing Price ($100K)", xlab = "Month",
        main = "Alameda County (2018): Test vs Prediction Data")
legend(1,9.3,legend=c("Forecast","Lower 80%","Lower 95%","Upper 80%","Upper 95%","Test Value"),pch=19, col = c(1:6))
grid()



#################################################
#################################################
####### BreakPoint Analysis#######
#################################################
#################################################

#################################################
# Intercept Only
#################################################

summary(lm1 <- lm(AlamedaTS~1))

plot(AlamedaTS, ylab = "Housing Prices (100K)", main = "Alameda County: Intercept Only Model", lwd = 3)
lines(ts(lm1$fitted.values, start = c(1996,4), frequency = 12), col = "red", lwd = 3)
grid()

# Breakpoint analysis - Intercept only
(bp1 <- breakpoints(AlamedaTS~1))
plot(bp1) # Plot for errors vs breakpoints
grid()
summary(bp1)
# Intercept only Breakpoint analysis fit
plot(AlamedaTS, ylab = "Housing Prices (100K)", main = "Alameda County: 5 Breakpoints", lwd = 3)
lines(fitted(bp1, breaks = 5), col = 4, lwd = 3)
lines(confint(bp1, breaks = 5))

#################################################
# Linear Fit
#################################################
 # Time variable
summary(lm2 <- lm(AlamedaTS~tt))

plot(AlamedaTS, ylab = "Housing Prices (100K)", main = "Alameda County: Linear Fit", lwd = 3)
lines(ts(lm2$fitted.values, start = c(1996,4), frequency = 12), col = "red", lwd = 3)
grid()


(bp2 <- breakpoints(AlamedaTS~tt))
plot(bp2) # Plot for errors vs breakpoints
grid()
summary(bp2)

par(mfcol = c(1,2))
# Linear-fit 5 Breakpoints
plot(AlamedaTS, ylab = "Housing Prices (100K)", main = "Alameda County: 5 Breakpoints Linear Fit", lwd = 3)
lines(fitted(bp2, breaks = 5), col = 4, lwd = 3)
lines(confint(bp2, breaks = 5))

# Linear-fit 2 Breakpoints
plot(AlamedaTS, ylab =, main = "Alameda County: 2 Breakpoints Linear Fit", lwd = 3)
lines(fitted(bp2, breaks = 2), col = 4, lwd = 3)
lines(confint(bp2, breaks = 2))

par(mfcol = c(1,1))
#################################################
#################################################
####### ARIMA with Structural Breaks #######
#################################################
#################################################

#################################################
# ARIMA Model with 2 breakpoints
#################################################


nTrainIndex <-  c(which(tt == (2011+8/12)):which(tt == (2017+11/12)))
testIndex <- c(which(tt == (2018+0/12)):which(tt == (2018+6/12)))
arima.fit3 <- auto.arima(AlamedaTS[nTrainIndex],seasonal = F, stepwise = F, approximation = F)
summary(arima.fit3)
arima.fit3 %>% residuals() %>% ggtsdisplay()


# Model Forecast
# auto.arima forecast for next 7 months (Jan 2018 to July 2018)
arima.fc3 <- forecast(arima.fit3, h=7)
arima3.error <- AlamedaTS[-TrainIndex] - arima.fc3$mean # Residual

print("Test MSE (%):")
(arima3.MSE <- mean((arima.fc3$mean-AlamedaTS[-TrainIndex])^2*100))# Pecentage test MSE

arima3.op <- data.frame(Hdate[-TrainIndex],AlamedaTS[-TrainIndex],arima.fc3$mean,arima3.error, arima3.error^2)
colnames(arima3.op) <- c("Time","Test Data","Forecast", "Residuals", "Residual^2")
print(arima3.op, row.names=F)

# Zoomed Plot 
arima3.mat<- cbind(arima.fc3$mean,arima.fc3$lower,arima.fc3$upper,AlamedaTS[-TrainIndex])

matplot(arima3.mat, pch = 19, type = "b", col = c(1:6), ylab = "Housing Price ($100K)", xlab = "Month",
        main = "Alameda County (2018): Test vs Prediction Data")
legend(1,9.3,legend=c("Forecast","Lower 80%","Lower 95%","Upper 80%","Upper 95%","Test Value"),pch=19, col = c(1:6))
grid()



#################################################
# ARIMA Model with 5 breakpoints
#################################################


nTrainIndex <-  c(which(tt == (2013+0/12)):which(tt == (2017+11/12)))
arima.fit4 <- auto.arima(AlamedaTS[nTrainIndex],seasonal = F, stepwise = F, approximation = F)
summary(arima.fit4)
arima.fit4 %>% residuals() %>% ggtsdisplay()


# Model Forecast
# auto.arima forecast for next 7 months (Jan 2018 to July 2018)
arima.fc4 <- forecast(arima.fit4, h=7)
arima4.error <- AlamedaTS[-TrainIndex] - arima.fc4$mean # Residual

print("Test MSE (%):")
(arima4.MSE <- mean((arima.fc4$mean-AlamedaTS[-TrainIndex])^2*100))# Pecentage test MSE

arima4.op <- data.frame(Hdate[-TrainIndex],AlamedaTS[-TrainIndex],arima.fc4$mean,arima4.error, arima4.error^2)
colnames(arima4.op) <- c("Time","Test Data","Forecast", "Residuals", "Residual^2")
print(arima4.op, row.names=F)

# Zoomed Plot 
arima4.mat<- cbind(arima.fc4$mean,arima.fc4$lower,arima.fc4$upper,AlamedaTS[-TrainIndex])

matplot(arima4.mat, pch = 19, type = "b", col = c(1:6), ylab = "Housing Price ($100K)", xlab = "Month",
        main = "Alameda County (2018): Test vs Prediction Data")
legend(1,9.3,legend=c("Forecast","Lower 80%","Lower 95%","Upper 80%","Upper 95%","Test Value"),pch=19, col = c(1:6))
grid()



########################################################
########################################################
############## ARIMA+STRUCTURAL CHANGE+REGRESION########
########################################################
########################################################

# San Francisco Time series
SanFranciscoTS <- ts(county_datD$`San Francisco`/100000, start = c(1996,4), frequency = 12)
# Based on decision tree
newreg <- cbind(tt,SanFranciscoTS)

arima.fit5 <- auto.arima(AlamedaTS[TrainIndex],xreg = newreg[TrainIndex,])
summary(arima.fit5)
arima.fit5 %>% residuals() %>% ggtsdisplay()
# Check regression and white noise residuals
cbind("Regression Errors" = residuals(arima.fit5, type="regression"),
      "ARIMA errors" = residuals(arima.fit5, type="innovation")) %>%
  autoplot(facets=TRUE)



# Model Forecast
# auto.arima forecast for next 7 months (Jan 2018 to July 2018)
arima.fc5 <- forecast(arima.fit5, h=7,xreg = newreg[-TrainIndex,])
arima5.error <- AlamedaTS[-TrainIndex] - arima.fc5$mean # Residual

print("Test MSE (%):")
(arima5.MSE <- mean((arima.fc5$mean-AlamedaTS[-TrainIndex])^2*100))# Pecentage test MSE

arima5.op <- data.frame(Hdate[-TrainIndex],AlamedaTS[-TrainIndex],arima.fc5$mean,arima5.error, arima5.error^2)
colnames(arima5.op) <- c("Time","Test Data","Forecast", "Residuals", "Residual^2")
print(arima5.op, row.names=F)

# Zoomed Plot 
arima5.mat<- cbind(arima.fc5$mean,arima.fc5$lower,arima.fc5$upper,AlamedaTS[-TrainIndex])

matplot(arima5.mat, pch = 19, type = "b", col = c(1:6), ylab = "Housing Price ($100K)", xlab = "Month",
        main = "Alameda County (2018): Test vs Prediction Data")
legend(1,9.3,legend=c("Forecast","Lower 80%","Lower 95%","Upper 80%","Upper 95%","Test Value"),pch=19, col = c(1:6))
grid()
