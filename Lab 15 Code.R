
############################ Lab 15 Code ######################################

#### Step 1:Install and Load Required Packages #####

# Install necessary packages (only run if not already installed)
packages <- c("tseries", "forecast", "TTR", "ggplot2", "dplyr", "tidyr", "lubridate")
installed <- packages %in% installed.packages()

if (any(!installed)) {
  install.packages(packages[!installed])
}

# Load the libraries
library(tseries)
library(forecast)
library(TTR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)


#### Step 2: Download and Import Data ####

# Set working directory (update the path as needed)
setwd("C:/Users/sever/OneDrive/Documents/Purdue/Spring 2025/AGR 333/Lab 15/AGR_333_Lab_15")
getwd()


# Load the datasets
soybeans <- read.csv("soybean-prices-historical-chart-data.csv")
print(head(soybeans,3))


CPI <- read.csv("historical-cpi-u-202503.csv")

print(head(CPI, 3))

# Pivot CPI data

# Transform CPI data into longer table 
#CPI_raw_long <- 

CPI <- CPI %>% pivot_longer(cols = -Year, names_to = "Month", values_to = "CPI")

head(CPI)

# Convert month names 
CPI <- CPI %>% mutate(Month = match(gsub("\\.", "", Month), month.abb))  # Convert month names to numbers

# Create date column
CPI <- CPI %>% mutate(date = as.Date(paste(Year, Month, 1, sep = "-")))

# Format date as m/1/yyyy
CPI <- CPI %>% mutate(date = format(date, "%m/1/%Y"))

# Drop the Year and Month columns and ensure chronological order
CPI <- CPI %>% select(date, CPI) %>% arrange(as.Date(date, format = "%m/%d/%Y"))  

head(CPI)

#### Step 3 ####

soybeans$date <- as.Date(soybeans$date, format = "%m/%d/%Y")
str(soybeans)

soybeans <- soybeans %>% 
  filter(date >= as.Date("01/01/1969", format = "%m/%d/%Y") & date <= as.Date("03/31/2025", format = "%m/%d/%Y"))

head(soybeans)



soybeans <- soybeans %>%
  mutate(date = floor_date(date, "month")) %>%        # Set each date to the first of the month
  group_by(date) %>%
  summarize(price = mean(value, na.rm = TRUE)) %>%    # Calculate monthly average
  ungroup()

# Filter for the from data from `January 1969` to `March 2025`
# Filter for the from data from `1969-01-01` to `2025-03-31`
soybeans <- soybeans %>% filter(date >= as.Date("01/01/1969", format = "%m/%d/%Y") & date <= as.Date("03/31/2025", format = "%m/%d/%Y"))

head(soybeans)

tail(soybeans)

# Filter CPI data

CPI$date <- as.Date(CPI$date, format = "%m/%d/%Y") 

CPI <- CPI %>%
  filter(date >= as.Date("1969-01-01") & date <= as.Date("2025-03-01"))

str(CPI)   # Should show 675 rows and 2 variables
head(CPI)  # Verify a sample of the filtered data

# Transform the CPI date column in Date format using the as.Date() function
#CPI$date <-

# Filter for the from data from `1969-01-01` to `2025-03-31`
#CPI <- CPI %>% filter()

head(CPI)

# Transform the CPI date column to Date format using the as.Date() function
CPI$date <- as.Date(CPI$date, format = "%m/%d/%Y") # Adjust format based on your date values

# Filter the data from `1969-01-01` to `2025-03-31`
CPI <- CPI %>%
  filter(date >= as.Date("1969-01-01") & date <= as.Date("2025-03-31"))

head(CPI)

tail(CPI)

# Merge the soybeans and CPI data frames by date

library(dplyr)

# Merge the two datasets by date
soybeans <- soybeans %>%
  left_join(CPI, by = "date")

# View the first few rows of the merged dataset
head(soybeans)


#### Step 4 ####
soybeans$price_real<-soybeans$price/(soybeans$CPI/100)

head(soybeans)


soybeans$date <- as.Date(soybeans$date)
str(soybeans)

#### Step 5: Plot ####

ggplot() +
  geom_line(data = soybeans,aes(x = date, y = price, color = 'Nominal')) +
  geom_line(data = soybeans,aes(x = date, y = price_real, color = 'Real' )) +
  scale_color_manual(name = "Type", values = c('Nominal' = "blue", 'Real' = "purple"))

#### Step 6: Time-Series Decomposition and Smoothing ####

price.ts <- ts(soybeans$price, start = c(1969, 1), end=c(2025, 3), frequency= 12)

str(price.ts)

head(price.ts)


#### Step 7 ####

price_components <- decompose(price.ts, type="additive")

plot(price_components)
price_components$figure

#### Step 8 ####
price_adj <- price.ts - price_components$seasonal

par(mfrow = c(1, 1))  # Ensure single plot layout
par(mar = c(5, 4, 4, 2) + 0.1)  # Adjust margins (bottom, left, top, right)
plot.ts(price_adj, main = "Seasonally Adjusted Soybean Prices", ylab = "Price", xlab = "Time", col = "blue", lwd = 2)

library(TTR)
library(ggplot2)

dev.off()

par(mar = c(4, 4, 2, 1), mfrow = c(3, 1))

# Compute moving averages
price_sma3 <- SMA(price.ts, n = 3)   # 3-month SMA
price_sma6 <- SMA(price.ts, n = 6)   # 6-month SMA
price_sma12 <- SMA(price.ts, n = 12) # 12-month SMA

# Plot moving averages
plot.ts(price_sma3, main = "Soybeans Price - 3-Month SMA", xlab = "", ylab = "SMA Price ($)", col = "blue", lwd = 2)
plot.ts(price_sma6, main = "Soybeans Price - 12-Month SMA", xlab = "", ylab = "SMA Price ($)", col = "purple", lwd = 2)
plot.ts(price_sma12, main = "Soybeans Price - 48-Month SMA", xlab = "", ylab = "SMA Price ($)", col = "red", lwd = 2)

# Reset layout to default
par(mfrow = c(1, 1))

price_adj <- price.ts - price_components$seasonal

par(mar = c(5, 4, 4, 2) + 0.1)
plot.ts(price_adj, main = "Seasonally Adjusted Soybean Prices", ylab = "Price ($)", xlab = "Time", col = "blue", lwd = 2)

#### Step 9: Ensuring Stationarity ####

price_diff.ts <- diff(price.ts, differences = 1)

# Plot the differenced time-series
plot.ts(price_diff.ts, main = "First Difference of Soybean Prices", ylab = "Price Change ($)", xlab = "Time", col = "blue", lwd = 2)

log_price.ts <- log(price.ts)

log_price_diff.ts <- diff(log_price.ts, differences = 1)

# Plot the log differenced time-series (month-over-month % change)
plot.ts(log_price_diff.ts, 
        main = "Month-over-Month % Change in Soybeans Prices Over Time", 
        xlab = "", 
        ylab = "% Price Change", 
        col = "blue", 
        lwd = 2)


#### Step 10:Forecasting and Model Evaluation ####

library(forecast)

# Fit an AR(3) model
ar_model <- arima(log_price_diff.ts, order = c(3, 0, 0))

print(ar_model)

checkresiduals(ar_model, lag.max = 60)

# Forecast 6 months ahead
forecast_ar3 <- forecast(ar_model, h = 6)

# Print forecast values
print(forecast_ar3)

# Visualize forecast
autoplot(forecast_ar3, include = 36) +
  labs(title = "Soybeans Price - Monthly % Returns Forecast",
       x = "",
       y = "Soybeans price - monthly % returns")

#### Step 11: Forecasting ####

library(forecast)

# Fit AR(3) model 
ar_model <- arima(log_price_diff.ts, order = c(3, 0, 0))

# Generate 6-month forecast
forecast_ar3 <- forecast(ar_model, h = 6)

# Display forecast output
print(forecast_ar3)

# Plot forecast with previous 36 months for context
autoplot(forecast_ar3, include = 36) +
  labs(title = "Soybeans Price - Monthly % Returns Forecast",
       x = "",
       y = "Soybeans price - monthly % returns")




