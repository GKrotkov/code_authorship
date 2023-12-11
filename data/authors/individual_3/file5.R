## --------------------------------------------------------------------------------------------------------
# PUT YOUR CODE AND PLOT HERE


## --------------------------------------------------------------------------------------------------------
library(tidyverse)
library(GGally)
student_data <- read_csv("https://raw.githubusercontent.com/FSKoerner/Fall23-36315-data/main/students.csv")


## --------------------------------------------------------------------------------------------------------
ggplot(aes(x=RaisedHands, y=VisitedResources), data = student_data) + 
  geom_point(alpha = 0.5)+ geom_density_2d() + 
  labs(title = "Scatterplot of Visited Resources and Raised Hands with Contour Lines", 
       x = "Raised Hands", y = "Visited Resources")


## --------------------------------------------------------------------------------------------------------
ggplot(aes(x=RaisedHands, y=VisitedResources), data = student_data) + 
  geom_point(alpha = 0.5)+ geom_density_2d(h=c(10,10)) + 
  labs(title = "Scatterplot of Visited Resources and Raised Hands with Contour Lines", 
       x = "Raised Hands", y = "Visited Resources")


## --------------------------------------------------------------------------------------------------------
ggplot(aes(x=RaisedHands, y=VisitedResources), data = student_data) + 
  geom_point(aes(color = Grade, shape = Gender), alpha = 0.8) + geom_density_2d(h=c(80,80)) + 
  labs(title = "Scatterplot of Visited Resources and Raised Hands with Contour Lines", 
       x = "Raised Hands", y = "Visited Resources")


## --------------------------------------------------------------------------------------------------------
student_data %>%
ggplot(aes(x = RaisedHands,y = VisitedResources)) + 
  stat_density2d(aes(fill = after_stat(density)), geom = "tile",contour = FALSE) +
  geom_point(alpha = 0.2) + coord_fixed() + 
  scale_fill_gradient(low = "white",high = "red") + 
  labs(title = "Heat Plot of Raised Hands and Visited Resources", 
       x = "Raised Hands", y = "Visited Resources")


## --------------------------------------------------------------------------------------------------------
library(hexbin)
student_data %>%
ggplot(aes(x = RaisedHands,y = VisitedResources)) + 
  geom_hex() + 
  coord_fixed() + 
  scale_fill_gradient2(low = "darkblue",high = "darkorange", midpoint = 3) + 
  labs(title = "Hexagonal Heatmap of Raised Hands and Visited Resources", 
       x = "Raised Hands", y = "Visited Resources")


## --------------------------------------------------------------------------------------------------------
data.subset = student_data %>% dplyr::select(RaisedHands, VisitedResources, AnnouncementsView, Discussion, Gender, Grade)

ggpairs(data.subset, columns = 1:4)


## --------------------------------------------------------------------------------------------------------
ggpairs(data.subset, column = c(1,2,6), mapping = ggplot2::aes(color = Gender, alpha = .5))


## --------------------------------------------------------------------------------------------------------
olive <- read_csv("https://raw.githubusercontent.com/FSKoerner/Fall23-36315-data/main/olive.csv")


## --------------------------------------------------------------------------------------------------------
olive_quant = olive %>% dplyr::select("palmitic", "palmitoleic", "stearic", "linoleic", "linolenic", "arachidic", "eicosenoic")
olive_std = scale(olive_quant, center=TRUE, scale=TRUE)
olive_dist = dist(olive_std)
olive_mds = cmdscale(d=olive_dist, k=2)
olive <- olive %>%
   mutate(mds1 = olive_mds[,1], mds2 = olive_mds[,2])
olive %>% ggplot(aes(x=mds1, y=mds2)) + geom_point(alpha=.5) +
  labs(x = "MDS Coordinate 1", y = "MDS Coordinate 2")


## --------------------------------------------------------------------------------------------------------
olive_quant = olive %>% dplyr::select("palmitic", "palmitoleic", "stearic", "linoleic", "linolenic", "arachidic", "eicosenoic")
olive_std = scale(olive_quant, center=TRUE, scale=TRUE)
olive_dist = dist(olive_std)
olive_mds = cmdscale(d=olive_dist, k=2)
olive <- olive %>%
   mutate(mds1 = olive_mds[,1], mds2 = olive_mds[,2])
olive %>% ggplot(aes(x=mds1, y=mds2)) + geom_point(aes(color = area), alpha=.5) +
  geom_density2d()+
  labs(x = "MDS Coordinate 1", y = "MDS Coordinate 2")


## --------------------------------------------------------------------------------------------------------
MDS_1 <- lm(mds1~palmitic + palmitoleic + stearic + oleic + linoleic + arachidic + eicosenoic, data = olive)
summary(MDS_1)


## --------------------------------------------------------------------------------------------------------
ggpairs(olive, columns = c(4,5,6,9), mapping = ggplot2::aes(color = area, alpha = .5) )

