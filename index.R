library(dplyr)
library(ggplot2)
library(data.table)

load_data = function (name){
  print(paste("data/",name,".csv"))
  data = read.csv(paste("data/",name,".csv", sep=""), sep = "|",  col.names = c("DT", "Country", "Partner", "Industry", "Year", "Value"))
  data = data.table(data)
  data[Partner== "WLD" & Industry=="DTOTAL"][,c("DT","Partner", "Industry"):=NULL]
}


## million tonnes
name = "FD_CO2"
everything = load_data("FD_CO2")
import = load_data("IMGR_TCO2")
export = load_data("EXGR_TCO2")
territorial = load_data("PROD_CO2")
gdp = read.csv("data/co2-emissions-vs-gdp.csv")


# import <- rename(import, Imported = Value)
# export <- rename(export, Exported = Value)
# im_ex <- inner_join(import, export, by=c("Country", "Year"))


interestingEntites = c("United States", "Poland", "China", "Finland", "Germany")

inner_join(everything, gdp, by=c("Country"="Code", "Year")) %>% 
  select(-X145446.annotations, -Continent, -Country) %>%
  filter(Entity %in% interestingEntites) %>% 
  rename(Balance = Value,
         Population = Total.population..Gapminder..HYDE...UN.,
         Country = Entity) %>% 
  mutate(Index = Balance*1e9 / Population / GDP.per.capita) %>%
  mutate(Index_norm = (Index - 0.2) / (max(Index) - 0.2)) %>%
  ggplot() +
  geom_line(aes(x = Year, y = Index_norm, colour = Country), size = 1.3) +
  scale_x_continuous(breaks = 2005:2015) +
  scale_y_continuous(breaks = seq(0, 1, by=0.1), limits=c(0, 1)) +
  labs(title = "Value of index in the years 2005-2015",
       subtitle = "Index was normalized using (x - 0.2) / (index_max - 0.2)",
       x = "Year",
       y = "Index") +
  theme_bw()
