library(dplyr)
library(ggplot2)

clean <- raw %>%
  filter(tariff_manuf <= 50)   # keep only realistic tariffs

ggplot(clean, aes(x = manuf_gdp, y = tariff_manuf, color = income)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    title = "Tariff vs Manufacturing Share (outliers removed)",
    x = "Manufacturing, value added (% of GDP)",
    y = "Applied tariff, weighted mean, manufactured products (%)"
  ) +
  theme_minimal()

