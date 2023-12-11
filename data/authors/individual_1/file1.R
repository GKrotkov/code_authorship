## ----setup, include=FALSE--------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, 
                      cache = TRUE)


## ----data loading, message = FALSE, warning = FALSE------------------------------------------------------
library(tidyverse)
library(gridExtra)
library(car)
library(sf)
library(reshape2)
library(leaps)
library(glmnet)
load("Chicago.RData")
raw <- chicago

standardize <- function(v){
    return((v - mean(v, na.rm = TRUE)) / sd(v, na.rm = TRUE))
}

compute_mse <- function(model, validation, true_response){
    preds <- predict(model, newdata = validation)
    errors <- (preds - true_response)
    mse <- mean((errors ** 2))
    return(mse)
}


## --------------------------------------------------------------------------------------------------------
# remove rows with 0 population
chicago <- chicago[!chicago$poptotal < 5, ]
# remove rows with NA for important predictors
chicago <- chicago[-which(is.na(chicago$income.male) | 
                              is.na(chicago$income.female)), ]

chicago$Ward <- factor(chicago$Ward)
chicago$Community.Area <- factor(chicago$Community.Area)
chicago$pctwhite <- round(chicago$popwhite / chicago$poptotal, digits = 3)
chicago$pctblack <- round(chicago$popblack / chicago$poptotal, digits = 3)
chicago$pctasian <- round(chicago$popasian / chicago$poptotal, digits = 3)


## ----transformations for q1------------------------------------------------------------------------------
cannabis_cidx <- startsWith(colnames(chicago), "CANNABIS")
noncannabis_cidx <- startsWith(colnames(chicago), "NOTCANNABIS")
cann_data <- chicago[, cannabis_cidx]
noncann_data <- chicago[, noncannabis_cidx]

chicago$cannabis_sum <- rowSums(chicago[, cannabis_cidx])
chicago$noncannabis_sum <- rowSums(chicago[, noncannabis_cidx])
chicago$crime_sum <- chicago$cannabis_sum + chicago$noncannabis_sum

chicago$crime_percap <- round(chicago$crime_sum / chicago$poptotal, digits = 3)
chicago$cannabis_percap <- round(chicago$cannabis_sum / chicago$poptotal, 
                                 digits = 3)
chicago$noncannabis_percap <- round(chicago$noncannabis_sum / chicago$poptotal, 
                                    digits = 3)

chicago$income.comb = chicago$income.male + chicago$income.female


## ----seasonal transformations----------------------------------------------------------------------------
# meteorological convention is summer is June, July, August
summer_cidx <- endsWith(colnames(chicago), "6") | 
    endsWith(colnames(chicago), "7") | endsWith(colnames(chicago), "8") 
winter_cidx <- endsWith(colnames(chicago), "12") | 
    endsWith(colnames(chicago), "1") | endsWith(colnames(chicago), "2")
spring_cidx <- endsWith(colnames(chicago), "3") | 
    endsWith(colnames(chicago), "4") | endsWith(colnames(chicago), "5")
fall_cidx <- endsWith(colnames(chicago), "9") | 
    endsWith(colnames(chicago), "10") | endsWith(colnames(chicago), "11")

chicago$summer_sum <- rowSums(chicago[, summer_cidx])
chicago$winter_sum <- rowSums(chicago[, winter_cidx])
chicago$spring_sum <- rowSums(chicago[, spring_cidx])
chicago$fall_sum <- rowSums(chicago[, fall_cidx])

chicago$summer_cann_sum <- rowSums(chicago[, summer_cidx & cannabis_cidx])
chicago$winter_cann_sum <- rowSums(chicago[, winter_cidx & cannabis_cidx])
chicago$spring_cann_sum <- rowSums(chicago[, spring_cidx & cannabis_cidx])
chicago$fall_cann_sum <- rowSums(chicago[, fall_cidx & cannabis_cidx])

chicago$summer_noncann_sum <- rowSums(chicago[, summer_cidx & noncannabis_cidx])
chicago$winter_noncann_sum <- rowSums(chicago[, winter_cidx & noncannabis_cidx])
chicago$spring_noncann_sum <- rowSums(chicago[, spring_cidx & noncannabis_cidx])
chicago$fall_noncann_sum <- rowSums(chicago[, fall_cidx & noncannabis_cidx])

chicago$summer_percap <- round(chicago$summer_sum / chicago$poptotal, 
                               digits = 3)
chicago$winter_percap <- round(chicago$winter_sum / chicago$poptotal, 
                               digits = 3)
chicago$spring_percap <- round(chicago$spring_sum / chicago$poptotal, 
                               digits = 3)
chicago$fall_percap <- round(chicago$fall_sum / chicago$poptotal, 
                             digits = 3)

chicago$summer_cann_percap <- round(chicago$summer_cann_sum / chicago$poptotal, 
                                    digits = 3)
chicago$winter_cann_percap <- round(chicago$winter_cann_sum / chicago$poptotal, 
                                    digits = 3)
chicago$spring_cann_percap <- round(chicago$spring_cann_sum / chicago$poptotal, 
                                    digits = 3)
chicago$fall_cann_percap <- round(chicago$fall_cann_sum / chicago$poptotal, 
                                  digits = 3)


chicago$summer_noncann_percap <- round(chicago$summer_noncann_sum / 
                                           chicago$poptotal, 
                                       digits = 3)
chicago$winter_noncann_percap <- round(chicago$winter_noncann_sum / 
                                           chicago$poptotal, 
                                       digits = 3)
chicago$spring_noncann_percap <- round(chicago$spring_cann_sum / 
                                           chicago$poptotal, 
                                       digits = 3)
chicago$fall_noncann_percap <- round(chicago$fall_noncann_sum / 
                                         chicago$poptotal, 
                                     digits = 3)


seasonal <- data.frame(percap = c(chicago$winter_percap, chicago$spring_percap, 
                                  chicago$summer_percap, chicago$fall_percap), 
                       season = c(rep("winter", nrow(chicago)), 
                                  rep("spring", nrow(chicago)), 
                                  rep("summer", nrow(chicago)), 
                                  rep("fall", nrow(chicago))), 
                       cann_percap = c(chicago$winter_cann_percap, 
                                       chicago$spring_cann_percap, 
                                       chicago$summer_cann_percap, 
                                       chicago$fall_cann_percap), 
                       noncann_percap = c(chicago$winter_noncann_percap, 
                                          chicago$spring_noncann_percap, 
                                          chicago$summer_noncann_percap, 
                                          chicago$fall_noncann_percap))


## ----final transformations-------------------------------------------------------------------------------
modeldata <- chicago[, 1:13]
modeldata$pctwhite <- round(modeldata$popwhite / modeldata$poptotal, digits = 3)
modeldata$pctblack <- round(modeldata$popblack / modeldata$poptotal, digits = 3)
modeldata$pctasian <- round(modeldata$popasian / modeldata$poptotal, digits = 3)

melted <- melt(cann_data)
melted$code <- as.character(melted$variable)
tmp <- unlist(strsplit(melted$code, "[.]"))
melted$type = tmp[seq(1, length(tmp), 3)]
melted$year = tmp[seq(2, length(tmp), 3)]
melted$month = tmp[seq(3, length(tmp), 3)]
melted$variable <- NULL
melted$code <- NULL
melted$crime <- melted$value
melted$value <- NULL

cann_store <- melted

melted <- melt(noncann_data)
melted$code <- as.character(melted$variable)
tmp <- unlist(strsplit(melted$code, "[.]"))
melted$type = tmp[seq(1, length(tmp), 3)]
melted$year = tmp[seq(2, length(tmp), 3)]
melted$month = tmp[seq(3, length(tmp), 3)]
melted$variable <- NULL
melted$code <- NULL
melted$crime <- melted$value
melted$value <- NULL


# lengthen modeldata to match
modeldata <- modeldata %>% 
    slice(rep(1:n(), each = ncol(cann_data) + ncol(noncann_data)))

modeldata <- cbind(modeldata, rbind(cann_store, melted))

modeldata$season <- ifelse(modeldata$month %in% c(12, 1, 2), "winter", 
                           ifelse(modeldata$month %in% c(3, 4, 5), "spring", 
                                  ifelse(modeldata$month %in% c(6, 7, 8), 
                                         "summer", "fall")))
modeldata$season <- factor(modeldata$season)
modeldata$type <- factor(modeldata$type)

modeldata$crime_percap <- modeldata$crime / modeldata$poptotal


## ----model selection-------------------------------------------------------------------------------------
test_rows <- (modeldata$year == 2012 & modeldata$month %in% c(10, 11, 12))
validation <- modeldata[test_rows, ]

# fixing community area bug
validation[which(validation$Community.Area %in% c(31, 33, 34)), 
           "Community.Area"] <- 32

train <- modeldata[!test_rows, ]

selected <- regsubsets(crime_percap ~ income.male + income.female + 
                           factor(Community.Area) + factor(type) +
                           income.male + income.female +
                           pctblack:factor(type) + 
                           pctasian + pctwhite + factor(season), 
                         nvmax = 50,
                         method = "forward",
                         data = train)

summ <- summary(selected)
bic_min <- which(summ$bic == min(summ$bic))
selected <- colnames(summ$outmat)[which(summ$outmat[bic_min, ] == "*")]

model <- lm(crime_percap ~ income.male + factor(Community.Area) + 
                pctblack + pctwhite + type, 
            data = train)

mse <- compute_mse(model, validation, validation$crime_percap)
predictions <- predict(model, newdata = validation)

errors <- (predictions - validation$crime_percap) ** 2
# error needs to be transformed for the per captia adjustment
mse <- mean(errors * validation$poptotal)


## --------------------------------------------------------------------------------------------------------
cat("The mean squared error of my predictor on the validation set is:", 
    round(mse, 5))

errorplt <- ggplot(data.frame(errors), aes(x = errors)) + 
    geom_histogram() + 
    labs(title = "Error distribution")

logerrplt <- ggplot(data.frame(errors), aes(x = log(errors))) + 
    geom_histogram() + 
    labs(title = "Error distribution", 
         subtitle = "Log transformed for readability")

grid.arrange(errorplt, logerrplt, nrow = 1)


## ---- fig.height = 4-------------------------------------------------------------------------------------
chicago %>%
    ggplot(aes(x = longitude, y = latitude, 
               color = sqrt(crime_percap), 
               alpha = sqrt(crime_percap))) + 
    geom_point() + 
    scale_color_gradient(low = "lightblue", high = "darkorange") + 
    labs(title = "Geographical Distribution of Crime in Chicago", 
         x = "Longitude", y = "Latitude", 
         color = "Sqrt Crime Per Capita", 
         alpha = "Sqrt Crime Per Capita") + 
    theme_bw() + 
    theme(panel.grid = element_blank())


## ---- fig.height = 3-------------------------------------------------------------------------------------
chicago %>%
    ggplot(aes(x = factor(Ward), y = log(crime_percap))) + 
    geom_violin(fill = "lightblue") + 
    geom_boxplot(fill = "darkblue", alpha = 0.5) + 
    labs(title = "Crime Rate by Ward", 
         x = "Ward", y = "Log Crime Rate") + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank())

chicago %>%
    ggplot(aes(x = factor(Community.Area), y = log(crime_percap))) + 
    geom_violin(fill = "lightblue") + 
    geom_boxplot(fill = "darkblue", alpha = 0.5) + 
    labs(title = "Crime Rate by Community Area",
         x = "Community Area", y = "Log Crime Rate") + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank())


## ---- fig.height = 3-------------------------------------------------------------------------------------
chicago %>%
    group_by(Community.Area) %>%
    summarize(cannabis_avg = mean(cannabis_percap), 
              noncannabis_avg = mean(noncannabis_percap)) %>%
    ggplot(aes(x = cannabis_avg, y = noncannabis_avg)) + 
    geom_point() + 
    labs(title = "Community Area Avg Cannabis vs Noncannabis Crime Rate", 
         subtitle = "Average for each block in a community area",
         x = "Average Cannabis Crime Rate", 
         y = "Average Noncannabis Crime Rate") + 
    theme_bw() +
    theme(panel.grid = element_blank())


## ---- fig.height = 3-------------------------------------------------------------------------------------
ggplot(chicago, aes(x = log(cannabis_percap), y = log(noncannabis_percap))) + 
    geom_point(alpha = 0.2) + 
    labs(title = "Cannabis and Noncannabis crime are related", 
         x = "log Cannabis Crime Per Capita", 
         y = "log Noncannabis Crime Per Capita") + 
    theme_bw() +
    theme(panel.grid = element_blank())


## ---- fig.height = 4-------------------------------------------------------------------------------------
income_plt <- ggplot(chicago, aes(x = income.comb, y = log(crime_percap))) + 
    geom_point(alpha = 0.2) + 
    labs(title = "Crime Rate by Combined Income", 
         subtitle = "Combined male and female income",
         x = "Combined Income", 
         y = "Log Crimes Per Capita") + 
    theme_bw() +
    theme(panel.grid = element_blank())

race <- data.frame(pct = c(chicago$pctwhite, chicago$pctasian, 
                           chicago$pctblack), 
                   race = c(rep("white", nrow(chicago)), 
                            rep("asian", nrow(chicago)), 
                            rep("black", nrow(chicago))), 
                   crime_percap = rep(chicago$crime_percap, 3))

race_plt <- ggplot(race, aes(x = pct, y = log(crime_percap), color = race)) + 
    geom_point(alpha = 0.3) + 
    labs(title = "Crime distribution by race",
         x = "Percentage of Race", 
         y = "Log Crime Rate") + 
    theme_bw() +
    theme(panel.grid = element_blank())

grid.arrange(income_plt, race_plt, nrow = 1)


## ---- fig.height = 4-------------------------------------------------------------------------------------
seasonal_plt <- ggplot(seasonal, aes(x = log(percap), fill = season)) +
    geom_density(alpha = 0.4) + 
    labs(title = "Crime Rate by Season",  
         x = "Log Per Capita Crime Rate", y = "Density") + 
    theme_bw() + 
    theme(panel.grid = element_blank())

seasonal_cann_plt <- ggplot(seasonal, aes(x = log(cann_percap), 
                                          fill = season)) +
    geom_density(alpha = 0.4) + 
    labs(title = "Cannabis Crime Rate by Season", 
         x = "Log Per Capita Crime Rate", y = "Density") + 
    theme_bw() + 
    theme(panel.grid = element_blank())

seasonal_noncann_plt <- ggplot(seasonal, aes(x = log(noncann_percap), 
                                             fill = season)) +
    geom_density(alpha = 0.4) + 
    labs(title = "Noncannabis Crime Rate by Season", 
         x = "Log Per Capita Crime Rate", y = "Density") + 
    theme_bw() + 
    theme(panel.grid = element_blank())

grid.arrange(seasonal_plt, seasonal_cann_plt, seasonal_noncann_plt, 
             nrow = 2)


## --------------------------------------------------------------------------------------------------------
tmp <- head(chicago[order(chicago$crime_percap, decreasing = TRUE), 
                    c("fips", "crime_sum", "crime_percap")], 10)
knitr::kable(tmp, 
             col.names = c("Block ID", "Absolute", 
                           "Per Capita"),
             caption = "Top 10 Crime-Heavy blocks")

tmp <- head(chicago[order(chicago$cannabis_percap, decreasing = TRUE), 
                    c("fips", "cannabis_sum", "cannabis_percap")], 10)
knitr::kable(tmp, 
             col.names = c("Block ID", "Absolute", 
                           "Per Capita"), 
             caption = "Top 10 Cannabis Crime-heavy blocks")

tmp <- head(chicago[order(chicago$noncannabis_percap, decreasing = TRUE), 
                    c("fips", "noncannabis_sum", "noncannabis_percap")], 10)
knitr::kable(tmp, 
             col.names = c("Block ID", "Absolute", 
                           "Per Capita"),
             caption = "Top 10 Non-Cannabis Crime-heavy blocks")


## --------------------------------------------------------------------------------------------------------
majority_race <- ifelse(chicago$popwhite > chicago$popblack &
                            chicago$popwhite > chicago$popasian, "white", 
                        ifelse(chicago$popblack > chicago$popasian, "black", 
                        "asian"))

ggplot(cbind(chicago, majority_race), 
       aes(x = log(cannabis_percap), y = log(noncannabis_percap))) + 
    geom_point(alpha = 0.2) + 
    labs(title = "Cannabis and Noncannabis crime are related", 
         x = "log Cannabis Crime Per Capita", 
         y = "log Noncannabis Crime Per Capita") + 
    theme_bw() +
    theme(panel.grid = element_blank()) + 
    facet_wrap(~majority_race)


## --------------------------------------------------------------------------------------------------------
model3null <- lm(noncannabis_percap ~ cannabis_percap, data = chicago)

model3race <- lm(noncannabis_percap ~ cannabis_percap + pctwhite + pctasian + 
                     pctblack, data = chicago)

model3black <- lm(noncannabis_percap ~ cannabis_percap + pctblack, 
                  data = chicago)

model3income <- lm(noncannabis_percap ~ cannabis_percap + income.male + 
                       income.female, data = chicago)

model3main <- lm(noncannabis_percap ~ cannabis_percap + pctwhite + pctasian + 
                     pctblack + income.male + income.female, data = chicago)

model3interact <- lm(noncannabis_percap ~ cannabis_percap + 
                         cannabis_percap:income.male + 
                         cannabis_percap:income.female, 
                     data = chicago)

model3allint <- lm(noncannabis_percap ~ cannabis_percap + pctwhite:cannabis_percap 
                   + pctasian:cannabis_percap + pctblack:cannabis_percap + 
                       income.male:cannabis_percap + 
                       income.female:cannabis_percap, 
                   data = chicago)

anova(model3null, model3race)
anova(model3null, model3income)
anova(model3null, model3main)
anova(model3null, model3black)
anova(model3null, model3allint)

