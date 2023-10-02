#GARCh Madels - Gneralized AutoRegressive Conditonal Heteroscedasticity
#Hetero.... means that variants do not remail same, as example of volatile stock price
#source : https://www.youtube.com/watch?v=9OVleScSjKs

#1. Modeling & Analysis of Apple Stock Prices in R | GARCH Models
# Libraries
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(rugarch)
library(shiny)
library(plotly)
library(bslib)
library(forecast) #for Arima


# Apple daily prices
getSymbols("WEED.TO", src = 'yahoo', from = "2017-01-01",to = "2023-12-31")
chartSeries(WEED.TO)
chartSeries(WEED.TO["2023"], theme = 'white', TA = "addVo(); addBBands(); addCCI();
                addEMA(50, col= 'blue'); addEMA(20, col= 'green');
                addEMA(5, col= 'orange')")
tail(WEED.TO)
# Daily returns
return <- CalculateReturns(WEED.TO$WEED.TO.Close)
return <- return[-1] #i.e to remove first row data as NA value
hist(return)
chart.Histogram(return,
                methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'green', 'red'))
chartSeries(return)
chartSeries(return["2023"])

# Annualized volatility
sd(return)
sqrt(252)*sd(return) #assumping 252 trading days in a year
sqrt(252)*sd(return["2017"])
sqrt(252)*sd(return["2023"])

chart.RollingPerformance(R = return["2017::2023"],
                         width = 22,
                         FUN = "sd.annualized",
                         scale = 252,
                         main = "Canopy Growth Corporation monthly rolling volatility")

chart.RollingPerformance(R = return["2017::2023"],
                         width = 252,
                         FUN = "sd.annualized",
                         scale = 252,
                         main = "Canopy Growth Corporation yearly rolling volatility")


#2. Standard Model with Interpretation in R
#topics: Model, Model equations, Model output, 12 Plots, & Forecast
#source: https://www.youtube.com/watch?v=vrdb3RK1G9o
# 1. sGARCH model with contant mean
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)), #ar - auto regressive, ma - moving average 
                variance.model = list(model = "sGARCH"),
                distribution.model = 'norm')
m <- ugarchfit(data = return, spec = s)
m #note this is Standard Garch model sGARCH(1,1), check 3:59 for illustration
#?plot #to indicated twelve different ways of plots
#plot(m)
plot(m, which = 'all')
f <- ugarchforecast(fitORspec = m, n.ahead = 20) #to use the model for forecasting next 20 days
plot(fitted(f)) #if see the line as horizontally, it indicates prediction is constant
plot(sigma(f)) #if it is +ve slot, indicates the risk is getting higher during the prediction period

#Video about - Variants of GARCH Model
#https://www.youtube.com/watch?v=8KQ2LLFIGwo&t=11s
#topics - GARCH with sstd, GJR-GARCH, AR(1) GJR-GARCH, and GJR-GARCH in mean
# Application example - portfolio allocation
v <- sqrt(252) * sigma(m) #to analyze validity for next 252 trading dates.
w <- 0.10/v # w - weight assigned to risky assets say 10% as 0.1
plot(merge(v, w), multi.panel = T)
tail(w) #it tells the percentage of the portfolio should be assigned to this stock given the value of w

# 2. GARCH with sstd 
#as if you seen the normal distribution is not the good model base on QQ-plot
#sstd - sstd: Skew Student-t Distribution 
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'sstd') #rather norm
m <- ugarchfit(data = return, spec = s)
m 
#say look at the Goodness-of-Fit, if p value >0.05, then reject Ho, and conclude sstd is better choice
#note that the Information Criteria has been improved (if lower than previous one)
#note that the skew value is 1.000 (as +ve skewness distribution, no skewness == 0)
plot(m, which='all')

#Model 3... girGARCH - developed by Glosten-Jagannathan-Runkle
# 3. GJR-GARCH
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "gjrGARCH"), #rather than sGARCH
                distribution.model = 'sstd')
m <- ugarchfit(data = return, spec = s)
m #you can see more estimates in Optimal parameters - look at 6:53 for illustration
plot(m, which = 'all')
#plot(m) #select 12 for New-Impact Cure ... indicated that -ve news have bigger impact than +ve news


#4. AR(1) GJR-GARCH
s <- ugarchspec(mean.model = list(armaOrder = c(1,0)), # is ar is 1
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m <- ugarchfit(data = return, spec = s)
m #ar1 is not significant if Pr >0.05 i.e. adding this term is not helping our GARCH model
#so we do not use this model - ar(1)
plot(m, which='all')


#5. GJR-GARCH in mean
s <- ugarchspec(mean.model = list(armaOrder = c(0,0),
                                  archm =T,
                                  archpow = 2), #the new variable archm is add
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m <- ugarchfit(data = return, spec = s)
m #again check if it is significant, if not - don't use this model
plot(m, which='all')

#This is to demonstrate simulating the stock price given the (best) model II being selected
#https://www.youtube.com/watch?v=qXoq6Lqb684
# Simulation
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'sstd') #rather norm
m <- ugarchfit(data = return, spec = s)
sfinal <- s
plot(m, which='all')
setfixed(sfinal) <- as.list(coef(m))

f2022 <- ugarchforecast(data = return["/2022-12"],
                        fitORspec = sfinal,
                        n.ahead = 252) # run the prediction for the following year = 252 days
f2023 <- ugarchforecast(data = return["/2023-12"],
                        fitORspec = sfinal,
                        n.ahead = 252)
par(mfrow = c(1,1))
plot(sigma(f2022))
plot(sigma(f2023))

sim <- ugarchpath(spec = sfinal,
                  m.sim = 3,
                  n.sim = 1*60,
                  rseed = 123) #simulate 3 series, foe 252 days, with random seed

plot.zoo(fitted(sim))
plot.zoo(sigma(sim)) #sigma is as actually simulated for next year (2020 as example)
tail(WEED.TO)

p <- 1.13*apply(fitted(sim), 2, 'cumsum') + 1.13
matplot(p, type = "l", lwd = 3)


#Topic : ARIMA - Autoregressive Integrated Moving Average
#https://www.youtube.com/watch?v=5PqWx2drQO4
chartSeries(WEED.TO)
data <- WEED.TO$WEED.TO.Close
#data <- log(data)
plot(data)

model <- auto.arima(data)
acf(model$residuals)
pacf(model$residuals)

# Box-Ljung test
Box.test(model$residuals, lag=5, type="Ljung-Box")

# Residual plot
hist(model$residuals,
     col = "lightblue",
     xlab = "Error",
     main = "Histogram of Residuals",
     freq = FALSE)
lines(density(model$residuals))
#Forecast
f <- forecast(model, 60)
autoplot(f) 

chartSeries(WEED.TO["2023-09"])





#Forecasting Markets using eXtreme Gradient Boosting (XGBoost)

df = data.frame(date = index(WEED.TO), WEED.TO, row.names=NULL)
colnames(df) <- c("Date", "Open","High","Low", "Close", "Volume","Adjusted")


# Install and load necessary packages  - Predciton on SPY - S&P500
library(xgboost)

# Step 1: Fetch historical stock price data (e.g., Apple Inc. - SPY)
getSymbols("SPY", src = 'yahoo', from = "2017-01-01",to = Sys.Date())
chartSeries(SPY$SPY.Adjusted)

# Step 2: Feature Engineering
# Create some common technical indicators
SPY$SMA10 <- SMA(Cl(SPY), n = 10)  # 10-day Simple Moving Average
SPY$SMA50 <- SMA(Cl(SPY), n = 50)  # 50-day Simple Moving Average
SPY$RSI <- RSI(Cl(SPY))            # Relative Strength Index


SPY = data.frame(date = index(SPY), SPY, row.names=NULL)
# Step 3: Data Split
train_size <- 0.8  # 80% of data for training, 20% for testing
split_index <- floor(train_size * nrow(SPY))
train_data <- SPY[1:split_index, ]
test_data <- SPY[(split_index + 1):nrow(SPY), ]

# Step 4: Model Training
# Convert data to a matrix format for XGBoost
#train_matrix <- as.matrix(train_data[, c("SMA10", "SMA50", "RSI")])
#test_matrix <- as.matrix(test_data[, c("SMA10", "SMA50", "RSI")])

train_matrix <- as.matrix(train_data[, c("SPY.Open", "SPY.Volume", "SMA10", "SMA50", "RSI")])
test_matrix <- as.matrix(test_data[, c("SPY.Open", "SPY.Volume", "SMA10", "SMA50", "RSI")])

# Define target variable (e.g., Adjusted Closing Price)
train_labels <- train_data$SPY.Adjusted
test_labels <- test_data$SPY.Adjusted

# Train the XGBoost model
xgb_model <- xgboost(data = train_matrix, label = train_labels, nrounds = 100, objective = "reg:squarederror")

# Step 5: Prediction
predictions <- predict(xgb_model, test_matrix)

# Step 6: Evaluation
# Calculate RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((test_labels - predictions)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Plot the actual vs. predicted prices
plot(test_data$SPY.Adjusted, type = "l", col = "blue", main = "SPY Stock Price Prediction")
lines(predictions, col = "red")
legend("topleft", legend = c("Actual", "Predicted"), col = c("blue", "red"))


print(predictions)
print(test_data$SPY.Adjusted)
aaccuracy = accuracy(test_data$SPY.Adjusted, predictions)
print(aaccuracy)

#Python - https://www.youtube.com/watch?v=fGLY0dIHJ2w&t=347s
