gdp_plot <- function(countries){
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
  
  
  interestingEntites = countries
  
  # kg/$
  
  inner_join(everything, gdp, by=c("Country"="Code", "Year")) %>% 
    select(-X145446.annotations, -Continent, -Country) %>%
    filter(Entity %in% interestingEntites) %>% 
    rename(Balance = Value,
           Population = Total.population..Gapminder..HYDE...UN.,
           Country = Entity) %>% 
    mutate(Index = Balance*1e9 / Population / GDP.per.capita) %>%
    mutate(Index_norm = (Index - min(Index)) / (max(Index) - min(Index))) %>%
    ggplot() +
    geom_line(aes(x = Year, y = Index_norm, colour = Country), size = 1.3) +
    scale_x_continuous(breaks = 2005:2015, expand = c(0.01, 0)) +
    scale_y_continuous(breaks = seq(0, 1, by=0.1), limits=c(0, 1), expand = c(0 ,0, 0.05, 0)) +
    labs(title = "Value of index in the years 2005-2015",
         subtitle = "Index was normalized using the following formula:
         (index - index_min) / (index_max - index_min),
         where index_min = TODO, index_max = TODO",
         x = "Year",
         y = "Normalized index") +
    theme_bw()
  
  # imo lepsze
  # $/kg
  
  inner_join(everything, gdp, by=c("Country"="Code", "Year")) %>% 
    select(-X145446.annotations, -Continent, -Country) %>%
    filter(Entity %in% interestingEntites) %>% 
    rename(Balance = Value,
           Population = Total.population..Gapminder..HYDE...UN.,
           Country = Entity) %>% 
    mutate(Index = GDP.per.capita / (Balance*1e9 / Population)) %>%
    mutate(Index_norm = (Index - min(Index)) / (max(Index) - min(Index))) %>%
  ggplot() +
    geom_line(aes(x = Year, y = Index, colour = Country), size = 1.5) +
    scale_x_continuous(breaks = 2005:2015, expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), breaks=seq(1.75, 4.25, by=0.25), limits = c(1.75, 4.25)) +
    labs(title = "Value of index in the years 2005-2015",
         x = "Year",
         y = "Index in $/kg") +
    theme_bw()
  
  
  gdp %>%
    filter(Entity %in% interestingEntites, Year >= 2005, Year <= 2015) %>% 
    ggplot() +
    geom_line(aes(x = Year, y = GDP.per.capita, colour = Entity), size = 1.5) +
    theme_bw()

}
