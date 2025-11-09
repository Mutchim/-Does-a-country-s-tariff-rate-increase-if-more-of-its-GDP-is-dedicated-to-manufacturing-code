library(ggplot2)
library(ggpubr)

ggplot(last_year, aes(x = manuf_gdp_lin, y = tariff_lin)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(method = "pearson", label.x = 20, label.y = 15) +
  labs(
    title = "Correlation between Manufacturing Share and Tariffs",
    x = "Manufacturing, value added (% of GDP)",
    y = "Applied tariff, weighted mean (%)"
  ) +
  theme_minimal()
