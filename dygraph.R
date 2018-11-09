library(tidyverse)
library(xts)
library(dygraphs)


plot_data<- petrol_data %>%
  filter(!is.na(Price),
         Price != 0,
         Price < 400) %>%
  select(StartDate,ItemName, Price) %>%
  group_by(StartDate, ItemName) %>%
  summarise(Price = mean(Price, na.rm = T)) %>%
  ungroup() %>%
  spread(ItemName, Price)

rownames(plot_data) <- plot_data$StartDate

petrol_xts <- as.xts(plot_data)

dygraph(petrol_xts,
        main = "National Average Observed Petrol Prices (J$/L)") %>%
  dyRangeSelector() %>%
  dyHighlight(highlightSeriesBackgroundAlpha = 0.2) %>%
  dyLegend(show = "follow",
           width = 500)