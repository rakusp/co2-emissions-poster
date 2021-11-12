library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)

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

# #wybrane państwa 
# df_selected <- df_all %>% filter(Country %in% c("AUS", "CAN", "DEU", "ESP", "GBR", "IND", 
#                                                 "ITA", "JPN", "NOR", "POL", "RUS", "SAU", 
#                                                 "SWE", "USA", "CHN")) 


# słupki
fun_col_country <- function(country){
  df_all %>% 
    mutate(Export_Pop = -Export_Pop) %>% 
    filter(Year == "2015", Country == country) %>% 
    select(!c(Country, ď.żCountry.Name )) %>% 
    pivot_longer(everything()) %>% 
    slice(7:10) %>%  
    ggplot()+
    geom_col(aes(x = name, 
                 y = value))+
    scale_x_discrete(limits = c("Balance_Pop", "Teritorial_Pop", "Import_Pop", "Export_Pop"), 
                     labels= c("Balance_Pop" = "balance", 
                                "Teritorial_Pop" = "teritorial",
                                "Import_Pop" = "import",
                                "Export_Pop" = "export")) +
    labs(x = country)
}



fun_countries <- function(c1, c2, c3, c4, c5, c6){
  c1 <- fun_col_country(c1)
  c2 <- fun_col_country(c2)
  c3 <- fun_col_country(c3)
  c4 <- fun_col_country(c4)
  c5 <- fun_col_country(c5)
  c6 <- fun_col_country(c6)
  (c1 + c2 + c3 + c4 + c5 + c6) & scale_y_continuous(limits = c(-7, 20), 
                                                    breaks = seq(-7, 20, by=2)) & labs(y= NULL)
}

fun_countries("USA", "CHN", "DEU", "CAN", "NOR", "POL")

