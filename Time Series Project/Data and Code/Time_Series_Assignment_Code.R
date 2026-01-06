setwd('/Users/manvendramishra/Documents/Semester 1/Time Series/Codes/Practical_Dataset')

# Load necessary libraries
library(ggplot2)
library(forecast)

# Load the data
load("pond.RData")
head(X)
str(X)
time_series <- ts(data$level, frequency = 12)  # Adjust frequency if needed

# Plot the data
plot(X, main="Pond Level Data", ylab="Level", xlab="Time")

time <- 1:length(X)

linear_model <- lm(X ~ time)
res <- residuals(linear_model)
linear_model
detrended

plot(res, main="Detrended Pond Level Data", ylab="Detrended Level", xlab="Time")

# Plot ACF and PACF of residuals
acf(res, main="ACF of Residuals (Y)")
pacf(res, main="PACF of Residuals (Y)")

ar1 <- ar(res, order.max=1, method="yule-walker")
ar2 <- ar(res, order.max=2, method="yule-walker")
ar3 <- ar(res, order.max=3, method="yule-walker")

summary(ar1)
summary(ar2)
summary(ar3)

par(mfrow=c(1,3))
acf(na.omit(residuals(ar1)), main="AR(1) Residuals")
acf(na.omit(residuals(ar2)), main="AR(2) Residuals")
acf(na.omit(residuals(ar3)), main="AR(3) Residuals")
par(mfrow=c(1,1))


par(mfrow=c(1,3))
spectrum(X, main="Periodogram of X")
spectrum(res, main="Periodogram of Y")
spectrum(na.omit(residuals(ar3)), main="Periodogram of Z") 
par(mfrow=c(1,1))

final_model <- arima(X, order=c(3,0,0)) # adjust the order as per the chosen model
print(final_model)

summary(final_model)
