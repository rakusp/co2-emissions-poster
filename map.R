
map_total <- function (countries){
  library(tidyverse)
  library(viridis)
  library(DT)
  options(knitr.table.format = "html")
  library(jpeg)
  library(maps)
  library(geosphere)
  library(grid)
  library(data.table)
  library(countrycode)
  
  
  population_gdp = data.table(read.csv("data/co2-emissions-vs-gdp.csv"))
  population_gdp$GDP = population_gdp$GDP.per.capita*population_gdp$Total.population..Gapminder..HYDE...UN.
  population_gdp = population_gdp[,Population:=Total.population..Gapminder..HYDE...UN.][!is.na(Code) & !is.na(Population) & !is.na(GDP),.(Code,Population,GDP, Year)]
  
  load_data = function (name){
    data = read.csv(paste("data/",name,".csv", sep=""), sep = "|",  col.names = c("DT", "Country", "Partner", "Industry", "Year", "Emissions"))
    data = data.table(data)
    #data = data[Partner== "WLD" & Industry=="DTOTAL"][,c("DT","Partner", "Industry"):=NULL]
    data = data[Industry=="DTOTAL"][,c("DT","Industry"):=NULL]
    data = data[population_gdp, on = .(Country = Code, Year = Year), nomatch=0]
    data[,EmissionsPerCapita:=Emissions/Population]
    data[,EmissionsPerGDP:=Emissions/GDP]
    data
  }
  export_d = load_data("EXGR_TCO2")
  export = export_d[,.(Country,Partner,Emissions, Year)]
  
  prepareDataEdges <- function(data,year,topN=100, countries=NULL){
    mapData <- data[Year == year][,c("Year"):=NULL]
    mapData$CountryName <- countrycode(mapData$Country, origin = 'iso3c', destination = 'country.name')
    mapData$PartnerName <- countrycode(mapData$Partner, origin = 'iso3c', destination = 'country.name')
    mapData = mapData[!is.na(CountryName) & !is.na(PartnerName)]
    mapData[CountryName == "United States"]$CountryName = "USA"
    mapData[PartnerName == "United States"]$PartnerName = "USA"
    mapData <- mapData[CountryName != PartnerName]
    if (!is.null(countries)){
      mapData <- mapData[CountryName %in% countries & PartnerName %in% countries] 
    }
    mapData <- mapData[order(mapData$Emissions, decreasing=TRUE)[1:topN],]
    
    mapData
    
  }
  
  prepareDataForPlot <- function(mapData){
    data_for_connection=function( dep_lon, dep_lat, arr_lon, arr_lat, group, emissions, countryname){
      inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), n=40, addStartEnd=TRUE, breakAtDateLine=F)             
      inter=data.table(inter)
      inter$group=0
      inter$size = emissions
      inter$countryname = countryname
      diff_of_lon=abs(dep_lon) + abs(arr_lon)
      if( dep_lon <= arr_lat){
        # inter$ends="last"
        inter <- inter[order(lon, decreasing = TRUE)]
      }else{
        inter <- inter[order(lon)]
        # inter$ends="first"
      }
      
      if(diff_of_lon > 180){
        inter$group[ which(inter$lon>=0)]=paste(group, "A",sep="")
        inter$group[ which(inter$lon<0)]=paste(group, "B",sep="")
      }else{
        inter$group=group
      }
      return(inter)
    }
    worldMap<- data.table(map_data("world"))
    worldMap = worldMap[worldMap$region != "Antarctica"]
    
    worldMap = worldMap[,.(lat=mean(lat),long=mean(long)), by = region]
    print(worldMap)
    mapData <- mapData[worldMap, on=.(CountryName=region), nomatch=0]
    setnames(mapData, c("lat", "long"), c("CountryLat", "CountryLong"))
    mapData <- mapData[worldMap, on=.(PartnerName=region), nomatch=0]
    setnames(mapData, c("lat", "long"), c("PartnerLat", "PartnerLong"))
    mapData$PartnerLat <- mapData$PartnerLat + + runif(length(mapData$PartnerLong), min = -1, max = -0.2)*3
    mapData$PartnerLong <- mapData$PartnerLong+ runif(length(mapData$PartnerLat), min = 0.2, max = 1)*3
    print(unique(mapData$CountryName))
    data_ready_plot=data.table()
    for(i in c(1:nrow(mapData))){
      tmp=data_for_connection(mapData$CountryLon[i], mapData$CountryLat[i], mapData$PartnerLon[i], mapData$PartnerLat[i] , i, mapData$Emissions[i], mapData$CountryName[i])
      #tmp$homecontinent=summary$homecontinent[i]
      data_ready_plot=rbind(tmp, data_ready_plot)
    }
    #data_ready_plot$homecontinent=factor(data_ready_plot$homecontinent, levels=c("Asia","Europe","Australia","Africa","North America","South America","Antarctica"))
    data_ready_plot
  }
  
  interestingCountries <- c("USA", "Poland", "China", "Australia", "United Arab Emirates", "Brazil", "South Africa", "Norway")
  dataForMap <-prepareDataForPlot(prepareDataEdges(export,2014,1000, interestingCountries))
  
  dataForMap$size=dataForMap$size/max(dataForMap$size)
  #dataForMap <- dataForMap[order(lon)]
  worldMap<- data.table(map_data("world"))
  #worldMap = worldMap[worldMap$region != "Antarctica"]
  ggplot() +
    #annotation_custom(earth, xmin = -180, xmax = 180, ymin = -90, ymax = 90) +
    geom_map(data=worldMap, map=worldMap, aes(map_id=region), fill="gray", color="#7f7f7f", size=0.5) +
    geom_map(data=dataForMap,aes(map_id=countryname, color=countryname), fill = "gray", map = worldMap, size=3,  alpha =0.1) +
    #  geom_line(data=dataForMap, aes(x=lon, y=lat, group=group, color=countryname, size=10*log(1 +size)/5), arrow = arrow(length=unit(0.30,"cm"), type = "open", ends=dataForMap[order(group)]$ends)) +
    #geom_line(data=dataForMap, aes(x=lon, y=lat, group=group, color=countryname, size=10*log(1 +size)/5)) +
    #geom_segment(data=dataForMap, aes(x=lon, y=lat, group=group, color=countryname, size=10*log(1 +size)/5)) +
    geom_path(data=dataForMap, aes(x=lon, y=lat, group=group, color=countryname, size=10*log(1 +size)/5), arrow = arrow(length=unit(0.30,"cm"), type = "open")) +
    theme_void() +
    theme(
      legend.position=NULL,
      panel.background = element_rect(fill = "black", colour = "black"),
      panel.spacing=unit(c(0,0,0,0), "null"),
      plot.margin=grid::unit(c(0,0,0,0), "cm"),
    ) +
    #ggplot2::annotate("text", x = -150, y = -45, hjust = 0, size = 11, label = paste("Where surfers travel."), color = "white") +
    #ggplot2::annotate("text", x = -150, y = -51, hjust = 0, size = 8, label = paste("data-to-viz.com | NASA.gov | 10,000 #surf tweets recovered"), color = "white", alpha = 0.5) +
    xlim(-180,180) +
    ylim(-60,80) +
    scale_x_continuous(expand = c(0.006, 0.006)) +
    coord_equal() +
    scale_size_continuous(range = c(1,7))
  
}