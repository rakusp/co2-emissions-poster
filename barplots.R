library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)

load_data = function (name){
  print(paste("data/",name,".csv"))
  data = read.csv(paste("data/",name,".csv", sep=""), sep = "|",  col.names = c("DT", "Country", "Partner", "Industry", "Year", "Value"))
  data = data.table(data)
  data[Partner== "WLD" & Industry=="DTOTAL"][,c("DT","Partner", "Industry"):=NULL]
}

everything = load_data("FD_CO2")
import = load_data("IMGR_TCO2")
export = load_data("EXGR_TCO2")
teritorial = load_data("PROD_CO2")


df <- merge(import, export, by=c("Country","Year"), all = TRUE)
colnames(df)[colnames(df) == 'Value.x'] <- 'Import'
colnames(df)[colnames(df) == 'Value.y'] <- 'Export'
df <- merge(df, teritorial, by=c("Country","Year"), all = TRUE)
colnames(df)[colnames(df) == 'Value'] <- 'Teritorial'

df <- df %>% mutate(Balance = Teritorial + Import - Export) 
df <- df %>% filter(!(Country %in% c("G20", "APEC", "NONOECD", "OECD", 
                                             "ZASI", "EASIA", "ZNAM", "ZOTH", 
                                             "ZEUR", "EU28", "EU15", "ROW", "EA19", 
                                             "EA12", "ASEAN", "ZSCA", "EU13", "WLD")))

#wczytanie populacji
x <- read.csv("testowa.csv")
x <- x %>% select(c(1:2, 50:60))
x <- x %>%  pivot_longer(
  cols = starts_with("X"),
  names_to = "Year",
  names_prefix = "X",
  values_to = "Population",
  values_drop_na = TRUE)
x$Year = as.integer(x$Year)
#połaczona ramka z populacją
df_all <- inner_join(df, x, by = c("Country"= "Country.Code", "Year" = "Year"))
df_all <- df_all %>% mutate(Import_Pop = Import/ (Population/1000000), 
                           Export_Pop = Export/ (Population/1000000), 
                           Teritorial_Pop = Teritorial/ (Population/1000000), 
                           Balance_Pop = Balance/ (Population/1000000))

#wybrane państwa 
df_selected <- df_all %>% filter(Country %in% c("AUS", "CAN", "DEU", "ESP", "GBR", "IND", 
                                                "ITA", "JPN", "NOR", "POL", "RUS", "SAU", 
                                                "SWE", "USA", "CHN")) 
  
# poglądowo
# 2005-2015 jak się zmieniał import, export itd per capita albo nie
df_selected %>% ggplot() +
  geom_line(aes(x = Year, y = Balance_Pop, colour = Country), size = 1.3) +
  scale_x_continuous(breaks = 2005:2015) +
  #scale_y_continuous(breaks = seq(0, 10, by=0.5), limits=c(0, 10)) +
  labs(title = "Balance per capita",
       subtitle = "",
       x = "Year",
       y = "balance") +
  theme_bw()


# słupki
fun_col_country <- function(country){
  df_selected %>% 
    mutate(Export_Pop = -Export_Pop) %>% 
    filter(Year == "2015", Country == country) %>% 
    select(!c(Country, ď.żCountry.Name )) %>% 
    pivot_longer(everything()) %>% 
    slice(7:10) %>%  
    ggplot()+
    geom_col(aes(x = name, 
                 y = value))+
    scale_x_discrete(limits = c("Balance_Pop", "Teritorial_Pop", "Import_Pop", "Export_Pop")) +
    labs(x = country)
}


USA <- fun_col_country("USA")
CHN <- fun_col_country("CHN")
DEU <- fun_col_country("DEU")
GBR <- fun_col_country("GBR")
NOR <- fun_col_country("NOR")
POL <- fun_col_country("POL")

library(patchwork)
(USA + CHN + DEU)/(GBR + NOR + POL) & scale_y_continuous(limits = c(-7, 20), 
                                                         breaks = seq(-7, 20, by=2)) &
  labs(y= NULL)


