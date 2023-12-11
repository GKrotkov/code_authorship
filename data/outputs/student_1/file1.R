## ----setup, include=FALSE--------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------------------------------------
#loading necessary packages

library(countrycode)
library(tidyverse)
library(ggnewscale)
library(ggplot2)
library(ggseas)
library(geomtextpath)
library(ggsoccer)
library(scatterpie)
library(reshape2)


## --------------------------------------------------------------------------------------------------------
#reading in and cleaning the data

futbol = read_csv2("./2021-2022 Football Player Stats.csv")
extra_match = c("Africa", "Africa", "Europe", "Africa", "Africa", "Americas", "Americas", "Europe", "Africa", "Europe", "Europe", "Africa", "Africa", "Europe", "Europe", "Americas", "Africa", "Americas", "Europe", "Africa", "Europe", "Europe", "Americas", "Asia", "Europe", "Africa", "Europe", "Europe", "Africa", "Americas", "Europe", "Africa", "Africa")
names(extra_match) = c("ALG", "ANG", "BUL", "CGO", "CHA", "CHI", "CRC", "CRO", "CTA", "DEN", "ENG", "EQG", "GAM", "GER", "GRE", "GRN", "GUI", "HON", "KVX", "MAD", "NED", "NIR", "PAR", "PHI", "POR", "RSA", "SCO", "SUI", "TOG", "URU", "WAL", "ZAM", "ZIM")

extra_match2 = c("Algeria", "Angola", "Bulgaria", "Congo", "Chad", "Chile", "Costa Rica", "Croatia", "Central African Republic", "Denmark", "England", "Equatorial Guinea", "Gambia", "Germany", "Greece", "Grenada", "Guinea", "Honduras", "Kosovo", "Madagascar", "Netherlands", "Northern Ireland", "Paraguay", "Philippines", "Portugal", "South Africa", "Scotland", "Switzerland", "Togo", "Uruguay", "Wales", "Zambia", "Zimbabwe")
names(extra_match2) = c("ALG", "ANG", "BUL", "CGO", "CHA", "CHI", "CRC", "CRO", "CTA", "DEN", "ENG", "EQG", "GAM", "GER", "GRE", "GRN", "GUI", "HON", "KVX", "MAD", "NED", "NIR", "PAR", "PHI", "POR", "RSA", "SCO", "SUI", "TOG", "URU", "WAL", "ZAM", "ZIM")
futbol$Continent = countrycode(futbol$Nation, "iso3c", "continent", custom_match = extra_match)


futbol$Country = countrycode(futbol$Nation, "iso3c", "country.name", custom_match = extra_match2)
simpPos = function (x) {
  if (x == "MFFW") {
    return ("MF")
  } else if (x == "FWMF") {
    return ("FW")
  } else if (x == "DFMF") {
    return ("DF")
  } else if (x == "FWDF") {
    return ("FW")
  } else if (x == "MFDF") {
    return ("MF")
  } else if (x == "DFFW") {
    return ("DF")
  } else if (x == "GKMF") {
    return ("GK")
  } else {
    return (x)
  }
}
futbol$ShoDist = futbol$ShoDist/10
futbol$Pos_simplified = sapply(futbol$Pos, FUN = simpPos)

world = map_data("world")

conditions <- c("USA")
replacement_values <- c("United States")
world$region <- replace(world$region, world$region %in% conditions, replacement_values)


futbol$Goals = as.numeric(futbol$Goals)
futbol$`G/SoT` = as.numeric(futbol$`G/SoT`)
futbol$Shots = as.numeric(futbol$Shots)


noGoalie = filter(futbol, Pos_simplified=="DF" | Pos_simplified=="FW" | Pos_simplified == "MF")
noGoalieorZero = filter(noGoalie, ShoDist!=0)


meanDF = median(filter(futbol, Pos_simplified== "DF")$ShoDist)
meanFW = median(filter(futbol, Pos_simplified== "FW")$ShoDist)
meanMF = median(filter(futbol, Pos_simplified== "MF")$ShoDist)
meanGK = median(filter(futbol, Pos_simplified== "GK")$ShoDist)





## --------------------------------------------------------------------------------------------------------
noGoalieorZero %>% ggplot(aes(x = ShoDist)) +
geom_density(aes(fill = Pos_simplified), alpha = 0.5, adjust = 1) +
  scale_fill_discrete(name = "Position", labels = c("Defender", "Forward", "Midfielder")) + 
  labs(title = "Distribution of Shot Distance across Player Position", x = "Shot Distance (Yards)", y = "Density")


## --------------------------------------------------------------------------------------------------------
world = filter(world, is.na(subregion))
average_long = world %>% 
  group_by(region) %>% 
  summarise("average long" = mean(long))

average_lat = world %>% 
  group_by(region) %>% 
  summarise("average lat" = mean(lat))
colnames(average_lat) = c("Country", "lat")
colnames(average_long) = c("Country", "long")

futbol = merge(futbol,average_lat, "Country")
futbol = merge(futbol, average_long, "Country")

aggregate = futbol %>% 
  group_by(Comp, Country, long, lat) %>% 
  summarize(NumPlayers=sum(Country==Country))



## --------------------------------------------------------------------------------------------------------
ggplot() +
  geom_map(data=world, map=world, aes(x=long, y=lat, map_id=region), color="grey", fill="white") +
  geom_point(data=aggregate, mapping=aes(x=long, y=lat, color=Comp, size=NumPlayers)) +
  labs(title="Players' Nationalities by League") + xlab("Longitude") + ylab("Latitude") + scale_size(range=c(1,3),
             breaks=c(5,50,100,250),
             labels=c("<10","<50", "<100", ">=100"),
             name = "Number of Players",
             guide="legend") +  scale_color_manual(values = c("Bundesliga" = "red", "La Liga" = "chocolate", "Ligue 1" = "forestgreen", "Premier League" = "deepskyblue", "Serie A" = "purple"))



## --------------------------------------------------------------------------------------------------------
ggplot() +
  geom_map(data=world, map=world, aes(x=long, y=lat, map_id=region), color="grey", fill="white") +
  geom_point(data=aggregate, mapping=aes(x=long, y=lat, color=Comp, size=NumPlayers)) +
  labs(title="Players' Nationalities by League (Europe)") + xlab("Longitude") + ylab("Latitude") + scale_size(range=c(1,7),
             breaks=c(5,10,100,250,400),
             labels=c("0-5","6-10", "10-100", "100>", "251-400"),
             name = "Number of Players",
             guide="legend") +  coord_cartesian(ylim=c(30,72), xlim=c(-10,35)) + scale_color_manual(values = c("Bundesliga" = "red", "La Liga" = "chocolate", "Ligue 1" = "forestgreen", "Premier League" = "deepskyblue", "Serie A" = "purple"))





## --------------------------------------------------------------------------------------------------------
futbol$TotalTkl = as.numeric(futbol$TklAtt3rd) + as.numeric(futbol$TklMid3rd) + as.numeric(futbol$TklDef3rd)
futbol$TotalTou = as.numeric(futbol$TouAtt3rd) + as.numeric(futbol$TouMid3rd) + as.numeric(futbol$TouDef3rd)

futbol_nonzero = filter(futbol, TotalTkl>0 & TotalTou>0)
tackle_stats = futbol_nonzero %>% 
  group_by(Pos_simplified) %>% 
  summarize(TklAtt= sum(as.numeric(TklAtt3rd)), TklMid = sum(as.numeric(TklMid3rd)),TklDef= sum(as.numeric(TklDef3rd)), TouAtt= sum(as.numeric(TouAtt3rd)), TouMid= sum(as.numeric(TouMid3rd)), TouDef= sum(as.numeric(TouDef3rd)) )


tackle.long<-melt(tackle_stats)
tackle.long$Zone = c("Attacking 1/3", "Attacking 1/3", "Attacking 1/3", "Attacking 1/3", "Middle 1/3", "Middle 1/3", "Middle 1/3", "Middle 1/3", "Defensive 1/3", "Defensive 1/3", "Defensive 1/3", "Defensive 1/3")
tackle.long$Type = c("Tackles", "Tackles","Tackles","Tackles","Tackles","Tackles","Tackles","Tackles","Tackles","Tackles","Tackles","Tackles", "Touches","Touches","Touches","Touches","Touches","Touches","Touches","Touches","Touches","Touches","Touches","Touches")
ggplot(tackle.long,aes(Pos_simplified,value,fill=Pos_simplified))+
     geom_bar(stat="identity",position="dodge") + facet_wrap(~Type+Zone, scales="free") + scale_fill_discrete(name = "Position", labels = c("Defender", "Forward", "Goalkeeper", "Midfielder")) + labs(title="Total Player Touches and Tackles by Zone and Position") + xlab("") 

