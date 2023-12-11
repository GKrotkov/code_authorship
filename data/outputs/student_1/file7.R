## --------------------------------------------------------------------------------------------------------
# PUT YOUR CODE AND PLOT HERE


## --------------------------------------------------------------------------------------------------------
library(countrycode)
library(tidyverse)
library(ggnewscale)
library(ggplot2)
library(ggseas)
library(geomtextpath)
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


## --------------------------------------------------------------------------------------------------------
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

world <- map_data("world")
ggplot(world, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = factor(1), color = factor(1))) + 
  scale_fill_manual(values = c("white"), guide = "none") +
  scale_color_manual(values = c("darkgrey"), guide = "none") +
  coord_sf(xlim = NULL, 
           ylim = NULL, expand = FALSE) +
  new_scale_color() + 
  geom_point(data = aggregate, 
             aes(x = long, y = lat, color = Comp, size=NumPlayers), alpha = 1) +
  labs(title = "Where Soccer Players are From Around the World by League", 
       color = "League") +  xlab("Longitude") + ylab("Latitude") 



ggplot() +
  geom_map(data=world, map=world, aes(x=long, y=lat, map_id=region), color="grey", fill="white") +
  geom_point(data=aggregate, mapping=aes(x=long, y=lat, color=Comp, size=NumPlayers)) +
  labs(title="Where Players are From Around the World by League") + xlab("Longitude") + ylab("Latitude") + scale_size(range=c(1,3),
             breaks=c(10,50,100),
             labels=c("<10","<50",">100"),
             name = "Number of Players",
             guide="legend")


  #geom_point(data=aggregate, aes(x=long, y=lat, color=Comp, size=NumPlayers), alpha=1)



