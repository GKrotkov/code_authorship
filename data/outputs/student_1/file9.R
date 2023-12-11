## ---- echo = FALSE---------------------------------------------------------------------------------------
library(tidyverse)
can_olympics <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv') %>%
  filter(noc == "CAN") %>%
  dplyr::select(-team, -noc) %>%
  mutate(won_medal = as.numeric(!is.na(medal)),
         medal = ifelse(is.na(medal), "None", medal))


## --------------------------------------------------------------------------------------------------------
can_olympics <- can_olympics %>% mutate(won_medal = factor(won_medal))
can_olympics <- can_olympics %>%
  arrange(factor(medal, levels = c('None', 'Bronze', 'Silver', 'Gold')))



## --------------------------------------------------------------------------------------------------------
year_won <- can_olympics %>%
  group_by(year, won_medal) %>%
  summarize(proportion = n()/length(can_olympics$year))
year_won %>% 
  ggplot(aes(x = year, y = proportion, 
             fill = factor(won_medal, labels = c("No", "Yes")))) + 
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Year and Medals Won", x = "Year", 
       y = "Proportion of athletes", fill = "Medal Won?")



## --------------------------------------------------------------------------------------------------------
won_year = filter(can_olympics, won_medal == "1")$year
lost_year = filter(can_olympics, won_medal == "0")$year
t.test(won_year, lost_year)



## --------------------------------------------------------------------------------------------------------
can_olympics_medals <- filter(can_olympics, won_medal == "1")


## --------------------------------------------------------------------------------------------------------
medal_season <- can_olympics_medals %>%
    group_by(medal, season) %>%
  summarize(count = n())

 medal_season %>%
  ggplot(aes(x = medal, y = count, fill = season)) +
  geom_bar(stat = "identity") + 
   labs(title = "Number of Each Type of Medal, colored by Season Won", 
        x = "Medal Type", y = "Number of Medals Won")


## --------------------------------------------------------------------------------------------------------
can_olympics_medals <- can_olympics_medals %>%
   mutate(medal = fct_drop(medal, "None"))


## --------------------------------------------------------------------------------------------------------
tab2b <- table(can_olympics_medals$medal, can_olympics_medals$season)
chisq.test(tab2b)


## --------------------------------------------------------------------------------------------------------
mosaicplot(tab2b, 
           main = "Mosaic Plot grouping Medal Type by Season Won", shade = TRUE)


## --------------------------------------------------------------------------------------------------------
ggplot(data = can_olympics, aes(x=height, y = won_medal)) + 
  geom_boxplot(aes(fill = won_medal), alpha =1) +
labs(title = "Boxplot of Athlete Heights Hactored by Whether a Medal Was Won", 
     x = "Height in cm", y = "") + scale_fill_discrete(labels=c('No', 'Yes'))
  



## --------------------------------------------------------------------------------------------------------
oneway.test(height ~ won_medal, data = can_olympics)


## --------------------------------------------------------------------------------------------------------
can_olympics_medals %>%
  ggplot(aes(x = height)) +
  stat_ecdf(aes(color = medal)) +
  labs(title = "ECDF of heights for different types of medals",
       x = "Height", y = "Fn(x)") + 
  scale_color_manual(breaks=c("Bronze","Silver","Gold"),
                     values=c("#E69F00","gray","gold")) + 
  theme_bw()


## --------------------------------------------------------------------------------------------------------
bronze_height <- (filter(can_olympics_medals, medal == "Bronze"))$height
silver_height <- (filter(can_olympics_medals, medal == "Silver"))$height
gold_height <- (filter(can_olympics_medals, medal == "Gold"))$height
ks.test(bronze_height, y = silver_height)
ks.test(bronze_height, y = gold_height)
ks.test(silver_height, y = gold_height)
alpha = 0.05
alpha_corrected = alpha/3
alpha_corrected


## --------------------------------------------------------------------------------------------------------
can_olympics %>% ggplot(aes(x=year, y=height)) + geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", level = 0.99) + 
  labs(title = "Scatterplot of height vs year with 99% Confidence Interval")



## --------------------------------------------------------------------------------------------------------
can_olympics %>% ggplot(aes(x=year, y=height, color = sex)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", level = 0.99) + 
  labs(title = "Scatterplot of height vs year by sex with 99% Confidence Interval")


## --------------------------------------------------------------------------------------------------------
linear_model <- lm(height ~ year * sex, data = can_olympics)
out <- summary(linear_model)
out
print("beta0")
out$coefficients[1,1]
print("betaYear")
out$coefficients[2,1]
print("betaM")
out$coefficients[3,1]
print("interaction coefficient")
out$coefficients[4,1]
print("p-values")
out$coefficients[,4]


