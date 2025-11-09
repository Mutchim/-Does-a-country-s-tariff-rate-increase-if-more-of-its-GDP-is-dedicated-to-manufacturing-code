trend <- panel_final %>%
  group_by(year) %>%
  summarize(
    avg_tariff = mean(tariff_lin, na.rm = TRUE),
    avg_manuf = mean(manuf_gdp_lin, na.rm = TRUE)
  )

ggplot(trend, aes(x = year)) +
  geom_line(aes(y = avg_tariff, color = "Tariff")) +
  geom_line(aes(y = avg_manuf, color = "Manufacturing")) +
  labs(
    title = "Global Trends: Manufacturing vs Tariffs (2000–2020)",
    y = "Value (%)",
    color = "Variable"
  ) +
  theme_minimal()

