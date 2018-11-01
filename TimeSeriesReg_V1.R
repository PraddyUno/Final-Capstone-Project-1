install <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) 
    install.packages(new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
}

# usage
required.packages <- c("gbm","randomForest","GGally","ggplot2", "reshape2", "strucchange", "forecast", "astsa", "lmtest","fUnitRoots","FitARMA","Rmisc","fBasics","urca","seasonal")
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
suppressPackageStartupMessages(library(seasonal))
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

# Install population data

popdata <- read.csv("C:/Users/Pardeep/Documents/R/PardeepR/Capstone/PopdataDecade.csv" )
autoplot(ts(popdata, frequency = 12, start = 2010))+
  ggtitle("Alameda County: Estimated Data")+
  ylab("Population")+ 
  xlab("Year")+
  theme_ts

# Install the CPI index

cpiInd <- read.csv("C:/Users/Pardeep/Documents/R/PardeepR/Capstone/CPIndex.csv" )
cpiInd$time <- as.Date(cpiInd$time, format = "%m/%d/%Y")

cpiMain <- ts(cpiInd$ts_cpi.x-cpiInd$ts_cpi.seasonal, frequency = 12, start = 2010)

ts(cpiInd$ts_cpi.x, frequency = 12, start=c(2010,1)) %>% 
  seas(x11="") -> decompCPI
autoplot(decompCPI) +
  ggtitle("X11 decomposition of electrical equipment index")

cpiMain <- trendcycle(decompCPI)-seasonal(decompCPI)
autoplot(cpiMain)+
  ggtitle("Geographical Region: San Francisco-Oakland-Hayward")+
  ylab("Consumer Price Index")+ 
  xlab("Year")+
  theme_ts



# Get the inventory list

raw_Inventory <- read.csv("C:/Users/Pardeep/Documents/R/PardeepR/Capstone/InventoryMeasure_SSA_County_Public.csv" )
raw_Inventory <- raw_Inventory[raw_Inventory$StateFullName=="California",
                   !(names(raw_Inventory) %in% c("RegionType","CountyName","Metro", "StateFullName", "DataTypeDescription"))]

# Define counties of interest
count_of_interest <- c("Alameda", "Contra Costa", "Marin", "San Francisco", "San Mateo", "Santa Clara", "Solano")
sub_Inventory <- raw_Inventory[raw_Inventory$RegionName %in% count_of_interest,]

# Transpose the data frame to organize the data with each row representing date of mesurement of time series
county.names <- sub_Inventory$RegionName # Extract the County names
county_Inventory <- as.data.frame(t(sub_Inventory[,-1])) # Remove the first column as it does not contain the data
colnames(county_Inventory) <- county.names # Assign the county names as coulmn labels to identify the data

autoplot(ts(county_Inventory, frequency = 12, start = 2010))+
  ggtitle("Housing Inventory")+
  ylab("No. of Houses")+ 
  xlab("Year")+labs (col = "County")+
  theme_ts
county_Inventory <- county_Inventory[-103,]


# Get Monthly turnover list
raw_Turnover <- read.csv("C:/Users/Pardeep/Documents/R/PardeepR/Capstone/MonthlyTurnover_AllHomes.csv" )
raw_Turnover  <- raw_Turnover[raw_Turnover$StateName=="California",
                               !(names(raw_Turnover) %in% c("RegionID", "StateName", "SizeRank"))]

sub_Turnover <- raw_Turnover[raw_Turnover$RegionName %in% count_of_interest,]

# Transpose the data frame to organize the data with each row representing date of mesurement of time series
county.names <- sub_Turnover$RegionName # Extract the County names
county_Turnover <- as.data.frame(t(sub_Turnover[,-1])) # Remove the first column as it does not contain the data
colnames(county_Turnover) <- county.names # Assign the county names as coulmn labels to identify the data

ym <- as.yearmon(rownames(county_Turnover), "X%Y.%m")
Hdate <- as.Date(ym)
county_Turnover <- cbind(Hdate, county_Turnover)
colnames(county_Turnover)[1] <- "Date"
cr <- c(county_Turnover$Date >= "2010-01-01")

cr[length(cr)] <- FALSE
county_Turnover <- county_Turnover[cr,]

autoplot(ts(county_Turnover[,-1], frequency = 12, start = 2010))+
  ggtitle("Monthly Turnover Rate")+
  ylab("Percentage of houses sold")+ 
  xlab("Year")+labs (col = "County")+
  theme_ts

# Get Median Price Data
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
county_datD <- (cbind(Hdate, county_dat))
colnames(county_datD)[1] <- "Date"

R <- c(county_datD$Date >= "2010-01-01")
R[length(R)] <- FALSE
county_price <- county_dat[R,]

# Define time as a predictor
tt <- time(ts(county_price$Alameda, start = c(2010,1), frequency = 12))

## Final Data Frame
AlamedaData <- data.frame(county_price/100000,tt,popdata$Alameda,cpiMain,county_Inventory$Alameda,county_Turnover$Alameda)
colnames(AlamedaData) <- c("SantaClara","Alameda","ContraCosta","SanFrancisco","SanMateo","Solano","Marin","time","Pop","CPI","Inven","TurnOver")
# Define training set from 2010/1 to 2017/12 -> total 96 training set values
train <- c(1:96)
TestData <- AlamedaData[-train,]
###########################################
################ Bagging ##################
###########################################
set.seed(1)
Alameda.Bagging <- randomForest(Alameda~.,data = AlamedaData,subset = train,mtry = 11, importance = T)
Alameda.Bagging #Model summary

importance(Alameda.Bagging) # List each predictors and their relative importance measures
varImpPlot(Alameda.Bagging) # Plot the relative importace measures

# OOB vs No of Trees
plot(Alameda.Bagging$mse*100, lwd = 3, xlab ="No. of Trees", main = "Training Set Error: Alameda County", ylab = "OOB Error (%)")
grid()

# Model Predictions
pred.Bag<-predict(Alameda.Bagging ,TestData) #Predictions on Test Set for each Tree
error.Bag <- TestData$Alameda - pred.Bag # Residual
print("Test MSE (%):")
(TestError.Bag <- mean(error.Bag^2)*100)# Pecentage test MSE
Bag.output <- cbind(TestData$Alameda,pred.Bag, error.Bag, error.Bag^2)
colnames(Bag.output) <- c("Test Data","Model Predictions", "Residuals", "Residual^2")
print(Bag.output)


###########################################
############# Random Forest ###############
###########################################
set.seed(3)
Alameda.RandomForest <- randomForest(Alameda~.,data = AlamedaData,subset = train, importance = T)
Alameda.RandomForest #Model summary

importance(Alameda.RandomForest ) # List each predictors and their relative importance measures
varImpPlot(Alameda.RandomForest ) # Plot the relative importace measures

# OOB Error Plot of Random Forest and Bagging
plot(Alameda.RandomForest$mse*100, pch = 20, xlab ="No. of Trees", ylab = "OOB Error (%)")
points(Alameda.Bagging$mse*100, pch = 20, col = "red")
legend("topright",c("Random Forest","Bagging"), col=c("black","red"),pch = c(20,20))
grid()

# Model Predictions
pred.RF<-predict(Alameda.RandomForest ,TestData) #Predictions on Test Set for each Tree
error.RF <- TestData$Alameda - pred.RF # Residual
print("Test MSE (%):")
(TestError.RF <- mean(error.RF^2)*100)# Pecentage test MSE
RF.output <- cbind(TestData$Alameda,pred.RF,error.RF, error.RF^2)
colnames(RF.output) <- c("Test Data","Model Predictions", "Residuals", "Residual^2")
print(RF.output)


# COmparison of Bagging and Random Forest

OOB.ErrorRF <- double(11) # 9 variables
test.ErrorRF <- double(11) # 9 variables

for (mtry in 1:11) {
  set.seed(3)
  rf=randomForest(Alameda~.,data = AlamedaData, subset = train,mtry=mtry,ntree=500) 
  OOB.ErrorRF[mtry] = rf$mse[500] #Error of all Trees fitted
  
  pred<-predict(rf,AlamedaData[-train,]) #Predictions on Test Set for each Tree
  test.ErrorRF[mtry]<- with(AlamedaData[-train,], mean( (Alameda - pred)^2)) #Mean Squared Test Error
  
}
Error.Matrix <- cbind(OOB.ErrorRF*100,test.ErrorRF)

matplot(1:mtry, Error.Matrix, pch=19 , col=c("red","blue"),type="b",ylab="Error",
        xlab="Number of Predictors at each Split", main = "OOB (Scaled 100 Times) and Test MSE", ylim = c(0,0.38))
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
grid()
min(test.ErrorRF)

Resd.Matrix <- cbind(error.Bag^2,error.RF^2)

matplot(c(1:6), Resd.Matrix,pch=19 ,col=c("red","blue"),type="b",ylab="Squared Residuals",
        xlab="Months", main = "Housing Price Predictions Errors (2018)", ylim = c(0,0.7), lwd = 3)
legend("topleft",legend=c("Bagging","Random Forest"),pch=19, col=c("red","blue"))
grid()