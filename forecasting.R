# Step 1 Load Packages and Data

## Packages
library(tidyverse)
library(forecast)
library(tseries)
library(httr)
library(readxl)


## Get Petrol data
petrol_data <- GET("http://cac.gov.jm/dev/SurveyEnquiry/PetrolPrices.php?Key=e8189538-d0ca-4899-adae-18f454eca9f9") %>%
  content()

## Get CPI data for transformation
CPI <- read_excel("C:/Users/egalbraith/Desktop/CAC Petrol App/forecasting/DZ-2018-10-18-15-41-23.xls") %>%
  mutate(DATE = as.Date(DATE, format = "%Y-%d-%m")) %>%
  mutate(YearMon = strftime(strftime(DATE, "%Y-%d-%m"),"%Y-%m"))

## Summarise to national arithmetic mean
forecastPetrolData <- petrol_data%>%
  filter(Price !=0) %>%
  group_by(StartDate, ItemName) %>%
  summarise(NatlMean = mean(Price, na.rm = T)) %>%
  ungroup() %>%
  mutate(YearMon = strftime(StartDate,"%Y-%m"))

## Untransformed Data

forecastPetrolData_Untransformed <- forecastPetrolData %>%
  spread(ItemName, NatlMean)

## Join Petrol Data and CPI for transformation

forecastPetrolData_Transformed <- forecastPetrolData %>%
  left_join(CPI, by=c("YearMon")) %>%
  ### transform prices to 2010 dollars
  mutate(tNatMean2010 = (NatlMean/VALUE) * 152.6) %>%
  select(-NatlMean) %>%
  ### spread data
  group_by(ItemName, StartDate) %>%
  mutate(id = row_number()) %>%
  ungroup() %>%
  spread(ItemName, tNatMean2010) %>%
  select(-id, -YearMon, -DATE) %>%
  rename("CPI" = "VALUE")

# Step 2 Examine your data

## Visualize with time series plot

ggplot(forecastPetrolData_Transformed, 
                  aes(StartDate, 
                      `E-10 GASOLENE - 87 OCTANE NONE 1 L`,
                      group = 1)) +
  geom_line() +
  scale_x_date("month") +
  labs(title = "National Mean Price of E-10 87 (2000 JA$)", 
       y = "Mean Price (J$/l)")

ggplot(forecastPetrolData_Untransformed, 
       aes(StartDate, 
           `E-10 GASOLENE - 87 OCTANE NONE 1 L`,
           group = 1)) +
  geom_line() +
  scale_x_date("month") +
  labs(title = "National Mean Price of E-10 87", 
       y = "Mean Price (J$/l)")

## clean data
untransformedPrice_ts = ts(forecastPetrolData_Untransformed[,("E-10 GASOLENE - 87 OCTANE NONE 1 L")])
forecastPetrolData_Untransformed$clean87 = tsclean(untransformedPrice_ts)

transformedPrice_ts = ts(forecastPetrolData_Transformed[,c("E-10 GASOLENE - 87 OCTANE NONE 1 L")])
forecastPetrolData_Transformed$clean87 = tsclean(transformedPrice_ts)

### create MAs
#forecastPetrolData_Transformed$clean87_ma4 <- ma(forecastPetrolData_Transformed$clean87, 
  #                                   order = 4)

forecastPetrolData_Transformed$clean87_ma <- ma(forecastPetrolData_Transformed$clean87,
                                      order = 1)

## plot clean data
ggplot(data = forecastPetrolData_Transformed,
            aes(x = StartDate,
                y = clean87))+
  geom_line() +
  scale_x_date('month') +
  labs(y = "Cleaned Mean Price (2000 J$/l)")

ggplot(data = forecastPetrolData_Untransformed,
       aes(x = StartDate,
           y = clean87))+
  geom_line() +
  scale_x_date('month') +
  labs(y = "Cleaned Mean Price (J$/l)")

  # Step 3 Decompose your data
 seasadjE1087 <- forecastPetrolData_Transformed$clean87_ma %>%
   na.omit() %>%
   ts(frequency = 30) %>%
   stl(seasadjE1087, s.window = 'periodic') %>%
   seasadj()

transformedDecomp = stl(forecastPetrolData_Transformed$clean87_ma, 
                        s.window = 'periodic')


  plot(transformedDecomp)
  
  # Step 4 Stationarity
  
  ## check for stationarity
  
  adf.test(na.omit(forecastPetrolData_Transformed$clean87_ma4), 
           alternative = "stationary")
  
  # Step 5: Autocorrelations and Choosing Model Order
  clean87_ma4 <- na.omit(forecastPetrolData_Transformed$clean87_ma4)
  
  Acf(clean87_ma4, main = "")
  
  Pacf(clean87_ma4, main = "")
  
  ## correcting for non-stationarity 
  ## only if assumption rejected in step 4
  
  price_d1 = diff(deseasonal_price, differences = 1)
  plot(price_d1)
  adf.test(price_d1, alternative = "stationary")
  
  Acf(price_d1)
  Pacf(price_d1)

  # Step 6 Fitting an ARIMA model
  
  auto.arima(deseasonal_price, seasonal = F)
  
  # Step 7 Evaluation and iterate
  
  ## evaluate by looking for pattersn in residuals
  
  fit <- auto.arima(deseasonal_price, seasonal = F)
  
  tsdisplay(residuals(fit),
            lag.max = 45,
            main = "(2,1,2) Model Residuals")
  
  ## adjust based on evaluation results
  
  fit2 = arima(deseasonal_price, order = c(2,1,23))
  
  fit2
  
  tsdisplay(residuals(fit2),
            lag.max = 30,
            main = "(2,1,23) Model Residuals")
  
  ## create forecasting plot
  fcast <- forecast(fit2, h=30)
  
  plot(fcast)
  
  ## evaluate plot
  
  hold <- window(ts(deseasonal_price), 
                 start=100)
  
  fit_no_holdout = arima(ts(deseasonal_price[-c(100:107)]),
                         order = c(2,1,23))
  
  fcast_no_holdout <- forecast(fit_no_holdout,
                               h=25)
  
  plot(fcast_no_holdout, main = "")
  
  lines(ts(deseasonal_price))
  
  ## adjust to include seasonal data
  
  fit_w_seasonality <- auto.arima(deseasonal_price, seasonal = T)
  
  fit_w_seasonality
  
  tsdisplay(residuals(fit_w_seasonality),
            lag.max = 45,
            main = "(2,1,0)(1,0,0)[12] model residuals")
  
  fit_w_seasonality2 <- Arima(deseasonal_price,
                              order = c(2,1,9),
                              seasonal = c(1,0,0))
  
  tsdisplay(residuals(fit_w_seasonality2),
            lag.max = 45,
            main = "(2,1,9)(1,0,0)[12] model residuals")
  
  seas_fcast <- forecast(fit_w_seasonality,
                         h = 30)
  
  plot(seas_fcast)
  
  seas_fcast2 <- forecast(fit_w_seasonality2,
                          h = 30)
  
  plot(seas_fcast2)
  
  ## evaluate seasonal arima model
  
  hold <- window(ts(deseasonal_price), 
                 start=90)
  
  fit_no_holdout2 = arima(ts(deseasonal_price[-c(90:107)]),
                         order = c(2,1,9),
                         seasonal = c(1,0,0))
  
  fcast_no_holdout2 <- forecast(fit_no_holdout2,
                               h=25)
  
  plot(fcast_no_holdout2, main = "")
  
  lines(ts(deseasonal_price))
