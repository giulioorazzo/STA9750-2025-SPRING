library(ggplot2)
ggplot(aggregate_payroll, aes(x = fiscal_year, y = tot_payroll)) +
  geom_line(color = "darkblue", size = 1) +  
  geom_point(color = "orange", size = 3) +  
  labs(
    title = "City's Aggregate Payroll Growth Over the Past 10 Years",
    x = "Fiscal Year",
    y = "Total Payroll $ (in Billions)"
  ) +
  scale_y_continuous(labels = label_dollar(scale = 1e-9, suffix = "B")) +  
  scale_x_continuous(breaks = seq(min(aggregate_payroll$fiscal_year), max(aggregate_payroll$fiscal_year), by = 1)) +  
  theme_minimal()  