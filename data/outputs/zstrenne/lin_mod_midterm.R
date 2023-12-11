load("/Users/zachstrennen/Downloads/Chicago.RData")

corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)

chicago <- dplyr::filter(chicago, poptotal > 2)

chicago <- na.omit(chicago)

chicago <-
  chicago %>%
  mutate(non_crime = rowSums(chicago[14:49]) / poptotal)

chicago <-
  chicago %>%
  mutate(non_crime = rowMeans(chicago[14:49]) / poptotal)

chicago <-
  chicago %>%
  mutate(can_crime = rowMeans(chicago[50:85]) / poptotal)

chicago <-
  chicago %>%
  mutate(total_crime = rowMeans(chicago[14:85]) / poptotal)
##
chicago <-
  chicago %>%
  mutate(prop_black = popblack/poptotal)

chicago <-
  chicago %>%
  mutate(prop_white = popwhite/poptotal)

chicago <-
  chicago %>%
  mutate(prop_asian = popasian/poptotal)

chicago <-
  chicago %>%
  mutate(avg_income = rowMeans(chicago[6:7]))
##
chicago <-
  mutate(chicago, non_high_low = ifelse(non_crime > quantile(non_crime, prob = .9), "High", "Low"))

chicago <-
  mutate(chicago, can_high_low = ifelse(can_crime > quantile(can_crime, prob = .9), "High", "Low"))

chicago <-
  mutate(chicago, total_high_low = ifelse(total_crime > quantile(total_crime, prob = .9), "High", "Low"))

chicago_limited_non <-
  dplyr::filter(chicago, non_crime > quantile(non_crime, prob = .8) | non_crime < quantile(non_crime, prob = .01))
##

chicago <- dplyr::filter(chicago, total_crime < .01)

ggplot(chicago, aes(x = non_crime)) +
  geom_histogram(color = "black", fill = "white") +
  geom_vline(aes(xintercept = quantile(non_crime, prob = .95)))

chicago %>%
  ggplot(aes(x = longitude, y = latitude, color = non_high_low)) +
  geom_point(alpha = .5) +
  theme_bw() +
  labs(title="Spread of top 10% of Non-Cannabis Related Crime\nRates Across Chicago",
       x = "Longitude",
       y = "Latitude",
       color = "Crime Rate")

chicago %>%
  ggplot(aes(x = longitude, y = latitude, color = total_high_low)) +
  geom_point(alpha = .5)

chicago %>%
  ggplot(aes(x = longitude, y = latitude, color = can_high_low)) +
  geom_point(alpha = .5) +
  theme_bw() +
  labs(title="Spread of top 10% of Cannabis Related Crime Rates\nAccross Chicago",
       x = "Longitude",
       y = "Latitude",
       color = "Crime Rate")

chicago %>%
  ggplot(aes(x = longitude, y = latitude, color = non_crime)) +
  scale_fill_continuous(type = "viridis") +
  geom_hex(bins = 33)


chicago %>%
  ggplot(aes(x =non_crime, y = can_crime,color=income.male)) +
  geom_point(alpha = .3)

aggregate(chicago$total_crime, list(chicago$Ward), mean) %>%
  ggplot(aes(x = Group.1, y = x)) +
  geom_bar(stat="identity") +
  theme_bw() +
  labs(x= "Ward",
       y= "Average Crime Rate")

aggregate(chicago$total_crime, list(chicago$Community.Area), mean) %>%
  ggplot(aes(x = Group.1, y = x)) +
  geom_bar(stat="identity") +
  theme_bw() +
  labs(x= "Community Area",
        y= "Average Crime Rate")

aggregate(chicago$non_crime, mean)

colSums(chicago[14:49])
colSums(chicago[50:85])
rowSums

t.test()

prop.test(c(mean(chicago$non_crime)*sum(chicago$poptotal),mean(chicago$can_crime)*sum(chicago$poptotal)),
          c(sum(chicago$poptotal),sum(chicago$poptotal)))

# response = number of crimes in given area
# predictor = population info (race and crime)
# build model off of jan 2011 to september 2012

# predict crime rate regardless of time
# lattitude
# longitude
# pop black white asian
# income male and female

chicago %>%
  ggplot(aes(x = log(non_crime), y = log(can_crime),color=prop_asian)) +
  geom_point(alpha = .3) +
  scale_colour_gradient2(mid = 2, high="blue", low="red") +
  theme_bw() + 
  labs(x = "log(Crime Rate for Non-Cannabis Related Crime)")

chicago %>%
  ggplot(aes(x = sqrt(non_crime), y = sqrt(can_crime),color=prop_black)) +
  geom_point(alpha = .3) +
  scale_colour_gradient2(mid = 2, high="blue", low="red") + 
  theme_bw() + 
  labs(x = "sqrt(Crime Rate for Non-Cannabis Related Crime)",
       y = "sqrt(Crime Rate for Cannabis Related Crime)",
       color = "Proportion of\nPopulation that\nis Black")

chicago %>%
  ggplot(aes(x = log(non_crime), y = log(can_crime),color=prop_white)) +
  geom_point(alpha = .3) +
  scale_colour_gradient2(mid = 2, high="blue", low="red") +
  theme_bw()

#ALL CRIME CAN BE LUMPED TOGETHER
lm(data=chicago, log(can_crime) ~ log(non_crime)) %>%
  summary()
model <- lm(data=chicago, sqrt(can_crime) ~ sqrt(non_crime))
model %>%
  summary()

par(mfrow = c(2, 2))
plot(model)



chicago %>%
  ggplot(aes(x = sqrt(prop_white), y = sqrt(total_crime))) +
  geom_point(alpha = .3)

chicago %>%
  ggplot(aes(x = sqrt(prop_black), y = sqrt(total_crime))) +
  geom_point(alpha = .3)

chicago %>%
  ggplot(aes(x = log(total_crime), y = longitude)) +
  geom_point(alpha = .3)

chicago %>%
  ggplot(aes(x = avg_income, y = sqrt(total_crime))) +
  geom_point(alpha = .3)+
  theme_bw() +
  labs(x="Average Income",
       y="sqrt(Crime Rate)")

lm(data=chicago, sqrt(avg_income) ~ sqrt(prop_black)) %>%
  summary()

chicago %>%
  ggplot(aes(x = log(prop_black), y = log(avg_income),color=prop_black)) +
  geom_point(alpha = .3)

lm(data=chicago, log(total_crime) ~ prop_black + income.male + longitude*latitude) %>%
  summary()

chicago %>%
  ggplot(aes(x = sqrt(prop_black), y = total_crime,color=prop_black)) +
  geom_point(alpha = .3)
chicago %>%
  ggplot(aes(x = sqrt(prop_black), y = avg_income,color=prop_black)) +
  geom_point(alpha = .3)

lm(data=chicago, sqrt(total_crime) ~ prop_black + avg_income + poptotal) %>%
  summary()

i <- c(2:5,10:13,86:92)
corr_matrix <- cor(scale(chicago[i]))
ggcorrplot(corr_matrix)

i <- c(14:49)
j <- c(50:85)
# arcsin sqrt

months <- c("1/1/2010","2/1/2010","3/1/2010","4/1/2010","5/1/2010","6/1/2010",
            "7/1/2010","8/1/2010","9/1/2010","10/1/2010","11/1/2010","12/1/2010",
            "1/1/2011","2/1/2011","3/1/2011","4/1/2011","5/1/2011","6/1/2011",
            "7/1/2011","8/1/2011","9/1/2011","10/1/2011","11/1/2011","12/1/2011",
            "1/1/2012","2/1/2012","3/1/2012","4/1/2012","5/1/2012","6/1/2012",
            "7/1/2012","8/1/2012","9/1/2012","10/1/2012","11/1/2012","12/1/2012")

d1 <- colSums(chicago[i])
d2 <- colSums(chicago[j])
time <- data.frame(d1+d2)
time$months = months
colnames(time) <- c("crime_rate","months")
time$crime_rate <- time$crime_rate/sum(chicago$poptotal)
glimpse(time)

time$months <- as.Date(time$months,"%m/%d/%Y")

ggplot(time,aes(x=months, y=crime_rate)) + 
  geom_point(alpha=.7) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_smooth(method='lm') +
  labs(x="Time",
       y="Average Crime Rate")


d1 <- chicago[i]
d2 <- chicago[j]
colnames(d1) <- months
colnames(d2) <- months
time <- d1+d2
#time <- data.frame(colSums(time))
#time$months = months
time <-
  time %>% 
  gather(var, val)
colnames(time) <- c("months","crime_rate")
time$months <- as.Date(time$months,"%m/%d/%Y")


ggplot(time, aes(x = months, y = crime_rate))+
  geom_point(alpha=0.5)

regfit.full <- regsubsets(sqrt(total_crime) ~ sqrt(prop_black) + avg_income + longitude * latitude + Community.Area + log(poptotal), data = chicago, nvmax = 6, method = "forward")
reg.summary <- summary(regfit.full)
var_num_bic <- which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(var_num_bic, reg.summary$bic[var_num_bic], col = "red", cex = 2, pch = 20)
coef(regfit.full, var_num_bic)

lm(data=chicago, sqrt(total_crime) ~ sqrt(prop_black) + avg_income + log(poptotal)) %>%
  summary()

lm(data=chicago, sqrt(total_crime) ~ sqrt(prop_black) + log(poptotal)) %>%
  summary()

ggplot(chicago, aes(x =  log(poptotal), y = sqrt(total_crime)))+
  geom_point(alpha=0.5)+
  labs(x="log(Block Group Population)",
       y="sqrt(Crime Rate)")+
  theme_bw()

i <-c(14:25,50:61)
chicago0 <-
  chicago %>%
  mutate(total_crime = rowMeans(chicago[i]) / poptotal)
chicago0 <- dplyr::filter(chicago0, total_crime < .02)
chicago0 <-
  chicago0 %>%
  mutate(year = 0)


i <-c(26:37,62:73)
chicago1 <-
  chicago %>%
  mutate(total_crime = rowMeans(chicago[i]) / poptotal)
chicago1 <- dplyr::filter(chicago1, total_crime < .02)
chicago1 <-
  chicago1 %>%
  mutate(year = 1)

i <-c(38:46,74:82)
chicago2 <-
  chicago %>%
  mutate(total_crime = rowMeans(chicago[i]) / poptotal)
chicago2 <- dplyr::filter(chicago2, total_crime < .02)
chicago2 <-
  chicago2 %>%
  mutate(year = 2)

i <-c(47:49,83:85)
chicago_pred <-
  chicago %>%
  mutate(total_crime = rowMeans(chicago[i]) / poptotal)
chicago_pred <- dplyr::filter(chicago_pred, total_crime < .02)
chicago_pred <-
  chicago_pred %>%
  mutate(year = 2)

i <- c(1:13,86:91)
chicago0 <- chicago0[i]
chicago1 <- chicago1[i]
chicago2 <- chicago2[i]
chicago_pred <- chicago_pred[i]

chicago_years <- rbind(chicago0,chicago1,chicago2)

regfit.full <- regsubsets(sqrt(total_crime) ~ sqrt(prop_black) + avg_income + year + longitude + latitude + log(poptotal), data = chicago_years, nvmax = 6, method = "forward")
reg.summary <- summary(regfit.full)
var_num_bic <- which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(var_num_bic, reg.summary$bic[var_num_bic], col = "red", cex = 2, pch = 20)
coef(regfit.full, var_num_bic)

model <- lm(data=chicago_years, sqrt(total_crime) ~ sqrt(prop_black) + year + log(poptotal))
model %>%
  summary()
par(mfrow = c(2, 2))
plot(model)

#model <- lm(data=chicago_years, sqrt(total_crime) ~ prop_black + year + poptotal)


pred <- data.frame(chicago_pred$prop_black,chicago_pred$year,chicago_pred$poptotal)
colnames(pred) <- c("prop_black","year","poptotal")
pred_acc <- data.frame(chicago_pred$total_crime)
colnames(pred_acc) <- c("total_crime")
predictions <- predict(model, newdata = pred)^2

sum((predictions-pred_acc)^2)

predictions-pred_acc
pred_acc[1:2101]
2105
predictions
2101
plot(predictions-pred_acc, type="l")







1.0745 + 0.8065*log(100000) + 0.56828*log(5) + 1.36754


12.64183




   