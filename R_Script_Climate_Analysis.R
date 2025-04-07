# Anna-Katharina Köpke
# Climate Analysis - based on Data from FigShare
# for more information see here: https://climatechange.chicago.gov/climate-change-science/future-climate-change

# Load necessary libraries and install if not available
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(ggplot2)
library(readxl)
library(dplyr)

# Load data
data <- read_excel("Climate_Analysis_Excel.xlsx", sheet = "Tabelle1")

# Convert Year to numeric
data$Year <- as.numeric(data$Year)
data$`Carbon Dioxide [ppm]` <- as.numeric(data$`Carbon Dioxide [ppm]`)
data$`Temperature [K]` <- as.numeric(data$`Temperature [K]`)


# Summary statistics
summary_stats <- data %>% 
  summarise(
    Mean_CO2 = mean(`Carbon Dioxide [ppm]`, na.rm = TRUE),
    Mean_Temperature = mean(`Temperature [K]`, na.rm = TRUE),
    Correlation_CO2_Temp = cor(`Carbon Dioxide [ppm]`, `Temperature [K]`, use = "complete.obs")
  )
print(summary_stats)

# Plot CO2 concentration over time
ggplot(data, aes(x = Year, y = `Carbon Dioxide [ppm]`)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue") +
  ggtitle("CO2 Concentration Over Time") +
  xlab("Year") +
  ylab("CO2 Concentration (ppm)") +
  theme_minimal()
ggsave("CO2_Time.png")

# Plot Temperature over time
ggplot(data, aes(x = Year, y = `Temperature [K]`)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red") +
  ggtitle("Temperature Change Over Time") +
  xlab("Year") +
  ylab("Temperature (K)") +
  theme_minimal()
ggsave("Temperature_Change_Over_Time.png")

# Correlation analysis
cor_co2_temp <- cor(data$`Carbon Dioxide [ppm]`, data$`Temperature [K]`, use = "complete.obs")
print(paste("Correlation between CO2 and Temperature:", round(cor_co2_temp, 2)))

# Scatter plot of CO2 vs Temperature
ggplot(data, aes(x = `Carbon Dioxide [ppm]`, y = `Temperature [K]`)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", color = "black") +
  ggtitle("CO2 vs Temperature") +
  xlab("CO2 Concentration (ppm)") +
  ylab("Temperature (K)") +
  theme_minimal()
ggsave("CO2_vs_Temp.png")

# Trend Analysis
model <- lm(`Temperature [K]` ~ `Carbon Dioxide [ppm]`, data = data)
summary(model)

# Print regression equation
cat("Regression Equation: Temperature =", round(coef(model)[1], 2), "+", round(coef(model)[2], 2), "* CO2 Concentration\n")

