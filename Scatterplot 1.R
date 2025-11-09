install.packages(c("WDI", "dplyr", "tidyr", "stringr", "countrycode", "zoo", "readr", "magrittr"))
library(WDI)
library(dplyr)
library(tidyr)
library(stringr)
library(countrycode)
library(zoo)
library(readr)
indicators <- c(
  tariff_manuf = "TM.TAX.MANF.WM.AR.ZS", 
  manuf_gdp   = "NV.IND.MANF.ZS"         
)
start_year <- 2000
end_year   <- 2020
raw <- WDI(
  country = "all",
  indicator = indicators,
  start = start_year,
  end   = end_year,
  extra = TRUE,   
  cache = NULL
)
data <- raw %>%
  filter(!is.na(region), region != "Aggregates")
panel <- data %>%
  transmute(
    iso3c,
    country  = country,
    year     = year,
    tariff   = tariff_manuf,   # %
    manuf_gdp = manuf_gdp,     # %
    income   = income,
    region   = region
  ) %>%
  arrange(iso3c, year)
summary(panel[, c("tariff", "manuf_gdp")])
table(panel$year) %>% head()
panel %>% summarize(
  n_countries = n_distinct(iso3c),
  n_obs       = n()
)
panel_interp <- panel %>%
  group_by(iso3c) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    tariff_lin    = na.approx(tariff,    x = year, na.rm = FALSE, maxgap = 2),
    manuf_gdp_lin = na.approx(manuf_gdp, x = year, na.rm = FALSE, maxgap = 2)
  ) %>%
  ungroup()
min_years <- 15
panel_final <- panel_interp %>%
  mutate(valid_year = !is.na(tariff_lin) & !is.na(manuf_gdp_lin)) %>%
  group_by(iso3c) %>%
  mutate(n_valid = sum(valid_year, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(n_valid >= min_years, year >= start_year, year <= end_year)
write_csv(panel_final, "wdi_tariff_manufacturing_2000_2020.csv")
last_year <- panel_final %>%
  group_by(iso3c) %>%
  filter(year == max(year)) %>%
  ungroup()
plot(last_year$manuf_gdp_lin, last_year$tariff_lin,
     xlab = "Manufacturing, value added (% of GDP)",
     ylab = "Applied tariff, weighted mean, manufactured products (%)",
     main = "Cross-section (latest year per country)")

