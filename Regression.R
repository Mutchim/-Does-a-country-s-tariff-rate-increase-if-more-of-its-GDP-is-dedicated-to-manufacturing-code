model <- lm(tariff_lin ~ manuf_gdp_lin, data = last_year)
summary(model)
library(ggplot2)

ggplot(last_year, aes(x = manuf_gdp_lin, y = tariff_lin)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
  labs(
    title = "Regression of Tariffs on Manufacturing Share",
    x = "Manufacturing, value added (% of GDP)",
    y = "Applied tariff, weighted mean (%)"
  ) +
  theme_minimal()


install.packages(c("openxlsx"), repos = "https://cloud.r-project.org")
library(openxlsx)


stopifnot(all(c("tariff_lin","manuf_gdp_lin") %in% names(last_year)))
model  <- lm(tariff_lin ~ manuf_gdp_lin, data = last_year)
sm     <- summary(model)


n      <- sm$df[1] + sm$df[2] + 1L
p      <- 1L                      
df_reg <- p
df_res <- n - (p + 1L)
df_tot <- n - 1L

sst <- sum( (last_year$tariff_lin - mean(last_year$tariff_lin, na.rm = TRUE))^2, na.rm = TRUE )
sse <- sum( sm$residuals^2 )
ssr <- sst - sse

ms_reg <- ssr / df_reg
ms_res <- sse / df_res
Fstat  <- ms_reg / ms_res
sigF   <- pf(Fstat, df1 = df_reg, df2 = df_res, lower.tail = FALSE)

R2     <- sm$r.squared
adjR2  <- sm$adj.r.squared
multipleR <- sqrt(R2)                
SE_reg <- sm$sigma                   


coefs      <- sm$coefficients
conf       <- confint(model, level = 0.95)
coef_df <- data.frame(
  Term       = rownames(coefs),
  Coefficients = unname(coefs[, "Estimate"]),
  SE           = unname(coefs[, "Std. Error"]),
  t_stat       = unname(coefs[, "t value"]),
  P_val        = unname(coefs[, "Pr(>|t|)"]),
  Lower_95     = conf[, 1],
  Upper_95     = conf[, 2],
  row.names = NULL,
  check.names = FALSE
)


wb <- createWorkbook()
addWorksheet(wb, "Regression Analysis")


writeData(wb, 1, "Regression Analysis", startCol = 1, startRow = 1)
addStyle(wb, 1, createStyle(textDecoration = "bold"), rows = 1, cols = 1, gridExpand = TRUE)


writeData(wb, 1, "Regression Statistics", startCol = 1, startRow = 3)
writeData(wb, 1, data.frame(
  Metric = c("Multiple R","R Square","Adjusted R Square","SE","Observations"),
  Value  = c(multipleR, R2, adjR2, SE_reg, n)
), startCol = 1, startRow = 5, colNames = TRUE)


writeData(wb, 1, "ANOVA", startCol = 1, startRow = 12)
anova_tbl <- data.frame(
  row.names = NULL,
  df = c(df_reg, df_res, df_tot),
  SS = c(ssr, sse, sst),
  MS = c(ms_reg, ms_res, NA_real_),
  F  = c(Fstat, NA_real_, NA_real_),
  `Significance F` = c(sigF, NA_real_, NA_real_)
)
anova_tbl <- cbind(
  Source = c("Regression","Residual","Total"),
  anova_tbl
)
writeData(wb, 1, anova_tbl, startCol = 1, startRow = 14, colNames = TRUE)


writeData(wb, 1, "", startCol = 1, startRow = 20) 
coef_tbl <- coef_df
names(coef_tbl)[1] <- "" 
writeData(wb, 1, coef_tbl, startCol = 1, startRow = 21, colNames = TRUE)


num_fmt <- createStyle(numFmt = "0.0000")
addStyle(wb, 1, num_fmt, rows = 6:9, cols = 2, gridExpand = TRUE)                   
addStyle(wb, 1, num_fmt, rows = 15:(14+nrow(anova_tbl)), cols = 3:6, gridExpand = TRUE)
addStyle(wb, 1, num_fmt, rows = 22:(21+nrow(coef_tbl)), cols = 2:7, gridExpand = TRUE) 

