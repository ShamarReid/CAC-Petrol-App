library(tidyverse)
library(forecast)
library(tseries)
library(httr)

petrol_data <- GET("http://cac.gov.jm/dev/SurveyEnquiry/PetrolPrices.php?Key=e8189538-d0ca-4899-adae-18f454eca9f9") %>%
  content()

forecastPetrolData <- petrol_data%>%
  filter(Price !=0) %>%
  group_by(StartDate, ItemName) %>%
  summarise(NatlMean = mean(Price, na.rm = T)) %>%
  ungroup() %>%
  filter(ItemName == "E-10 GASOLENE - 87 OCTANE NONE 1 L")

ggplot(forecastPetrolData,
       aes(StartDate,
           NatlMean)) +
  geom_line() +
  scale_x_date("Month") +
  labs(y = "Monthl Mean Price (J$/l)")

price_ts = ts(forecastPetrolData[,c("NatlMean")])

forecastPetrolData$clean_price = tsclean(price_ts)

ggplot() +
  geom_line(data = forecastPetrolData,
            aes(x = StartDate,
                y = NatlMean))

forecastPetrolData$clean_price_ma = ma(forecastPetrolData$clean_price,
                                       order = 4)

ggplot() +
  geom_line(data = forecastPetrolData,
            aes(x = StartDate,
                y = clean_price,
                color = "Price")) +
  geom_line(data = forecastPetrolData,
            aes(x = StartDate,
                y = clean_price_ma,
                color = "Monthly Average Price")) +
  ylab("Price")

price_ma = ts(na.omit(forecastPetrolData$clean_price_ma),
              frequency = 4)
decomp = stl(price_ma, s.window = "periodic")
deseasonal_price <- seasadj(decomp)
autoplot(decomp)

adf.test(price_ma, alternative = "stationary")

price_d2 = diff(deseasonal_price, differences = 2)
autoplot(price_d2)

adf.test(price_d2, alternative = "stationary")

ggtsdisplay(price_d2)

fit <- auto.arima(deseasonal_price, 
                  seasonal = T,
                  approximation = F)

checkresiduals(fit)

seas_fcast <- forecast(fit, h=24)
autoplot(seas_fcast)

hold <- window(ts(deseasonal_price), start = 80)

fit_no_hold = arima(ts(deseasonal_price[-c(80:108)]),
                    order = c(2,2,5), 
                    seasonal = c(0,0,4))

checkresiduals(fit_no_hold)

tsdisplay(residuals(fit_no_hold))

fcast_no_holdout <- forecast(fit_no_hold, h=28)
plot(fcast_no_holdout)
lines(ts(deseasonal_price))