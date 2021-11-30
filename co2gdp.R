library(data.table)
library(ggplot2)
library(maps)
library(dplyr)

## https://ourworldindata.org/grapher/co2-emissions-vs-gdp?tab=table&time=2018
## tonnes per capita

interestingCountries = c("Germany", "China", "Poland", "United States", "Russia")
data = read.csv("data/co2-emissions-vs-gdp.csv")
data <- mutate(data, GDP.per.capita = GDP.per.capita/1000) %>% 
  rename(CO2.per.capita = Annual.CO2.emissions..per.capita.,
         Country = Entity) %>% 
  select(Country, Year, GDP.per.capita, CO2.per.capita) %>% 
  filter(!is.na(GDP.per.capita),
         !is.na(CO2.per.capita),
         Year >= 1980,
         Year <= 2015,
         Country %in% interestingCountries)


data %>%
  ggplot() +
  geom_line(aes(x = Year, y = CO2.per.capita, color = Country),
            size = 1.2,
            lineend = "round") +
  labs(title = "Annual CO2 emissions per capita",
       subtitle = "1980 - 2015",
       y = "CO2 per capita in tonnes")

data %>%
  ggplot() +
  geom_line(aes(x = Year, y = GDP.per.capita, color = Country),
            size = 1.2,
            lineend = "round") +
  labs(title = "GDP per capita",
       subtitle = "1980 - 2015, Gross domestic product (GDP) per capita is measured in international-$ in 2011 prices to adjust for
price differences between countries and adjust for inflation",
       y = "GDP per capita in $")

data %>%
  mutate(CO2.per.GDP.per.capita = CO2.per.capita * 1000 / GDP.per.capita) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = CO2.per.GDP.per.capita, color = Country),
            size = 1.2,
            lineend = "round") +
  labs(title = "Annual CO2 emissions per GDP per capita",
       subtitle = "1980 - 2015",
       y = "CO2 per GPD per capita in kg/$")

# Jak widać w chinach emisje per capita są niskie jedynie z powodu niskiego PKB

