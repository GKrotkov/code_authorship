params <-
list(team = 6672L, solo_score_max = 70L)

## ---- echo = FALSE, message = FALSE----------------------------------------------------------------------
count_in_bound <- function(n, lo, hi){
    return(sum(n >= lo & n <= hi))
}

nas_to_0 <- function(n){
    n[is.na(n)] <- 0
    return(n)
}

compute_points <- function(elem){
    ah <- as.numeric(elem["autoHigh"])
    am <- as.numeric(elem["autoMid"])
    al <- as.numeric(elem["autoLow"])
    th <- as.numeric(elem["teleHigh"])
    tm <- as.numeric(elem["teleMid"])
    tl <- as.numeric(elem["teleLow"])
    
    pts <- ifelse(elem["mobility"], 3, 0)
    pts = pts + (ah * 6) + (th * 5)
    pts = pts + (am * 4) + (tm * 3)
    pts = pts + (al *3) + (tl * 2)
    return(pts)
}


## ---- echo = FALSE, warning = FALSE, message = FALSE-----------------------------------------------------
# libraries and data
library(tidyverse)
library(grid)
library(gridExtra)
library(jpeg)
library(png)
library(pander)
#library(stringr)

# needs debugging, look at example of rattlers scoring only high pieces

pic_path <- paste("photos/", params$team, ".jpg", sep = "")
if (!file.exists(pic_path)) pic_path <- "photos/default.jpg"

data <- read.csv("data/apollo.csv")
data <- data[which(data$teamNum == params$team), ]
data[, 25:44] <- NULL

data$autoCones <- as.character(data$autoCones)
data$autoCubes <- as.character(data$autoCubes)
data$teleopCones <- as.character(data$teleopCones)
data$teleopCubes <- as.character(data$teleopCubes)

# combine cones and cubes
autoScores <- paste(data$autoCones, data$autoCubes, sep = ",")
teleScores <- paste(data$teleopCones, data$teleopCubes, sep = ",")
# strip out NAs and preceding commas
autoScores <- gsub(pattern = ",NA", replacement = "", x = autoScores)
teleScores <- gsub(pattern = ",NA", replacement = "", x = teleScores)
# convert scores to numeric
autoScores <- lapply(strsplit(autoScores, ","), as.numeric)
teleScores <- lapply(strsplit(teleScores, ","), as.numeric)
# calculate game piece volume by counting nonNAs; sorry it's not very readable
data$autoScores <- unlist(lapply(lapply(autoScores, is.na), 
                                 function(l){sum(!l)}))
data$teleScores <- unlist(lapply(lapply(teleScores, is.na), 
                                 function(l){sum(!l)}))
data$autoHigh <- unlist(lapply(autoScores, count_in_bound, lo = 1, hi = 9))
data$autoMid <- unlist(lapply(autoScores, count_in_bound, lo = 10, hi = 18))
data$autoLow <- unlist(lapply(autoScores, count_in_bound, lo = 19, hi = 27))
data$teleHigh <- unlist(lapply(teleScores, count_in_bound, lo = 1, hi = 9))
data$teleMid <- unlist(lapply(teleScores, count_in_bound, lo = 10, hi = 18))
data$teleLow <- unlist(lapply(teleScores, count_in_bound, lo = 19, hi = 27))
idx <- which(colnames(data) == "autoHigh")
idx <- idx:(idx+5)
data[, idx] <- apply(data[, idx], 2, nas_to_0)

data$points <- apply(data, 1, compute_points)

idx <- which(colnames(data) == "communityPickups")
colnames(data)[idx:(idx+3)] <- c("community", "neutral", "single", "double")


## ---- eval = FALSE, include = FALSE----------------------------------------------------------------------
## 
## # Icon stuff from Dr Oliver
## 
## 
## # set include to TRUE once the icon images are incorporated
## ## team image return text content base64 for image extract
## team_avatar<-function(team){
## if(is.numeric(team))
##   {url = paste0("https://frc-api.firstinspires.org/v3.0/2023/avatars?teamNumber=",team)
##   output<-GET(url,authenticate(FRC_Username, Auth_Token))
##   a <- content(output, as="text")
##   a<-fromJSON(a)
##   return(a$teams$encodedAvatar)}
## else
##   print("not a numeric team number")
## }
## #handle base 64 encodedwebimage and returns grob ready to plot
## text_2_grob<-function(a)
## {#raw <- (txt)
## b<-RCurl::base64Decode(txt =  a, mode="raw")
## if (all(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))==b[1:8]))
##   print('png')
##   img <- png::readPNG(b)
##   #handle transparent pixel
##   # transparent <- img[,,4] == 0
##   # img <- as.raster(img[,,1:3])
##   # img[transparent] <- NA
## if(all(as.raw(c(0xff, 0xd8, 0xff, 0xd9))==b[c(1:2, length(b)-(1:0))]))
##   {print('jpeg')
##   img <- jpeg::readJPEG(b)}
## g <- rasterGrob(img, interpolate=TRUE)
## return(g)
## }
## 
## plot_avatar <- function(team=6672) {
##   g <- text_2_grob(team_avatar(team))
##   p <- ggplot() +
##     annotation_custom(g)
##   return(p)
## }
## 
## #pic <- readJPEG(pic_path)
## 
## points_plot <- ggplot(data, aes(x = points)) +
##     geom_density(fill = "purple") +
##     xlim(0, params$solo_score_max) +
##     labs(title = "Distribution of Solo Points",
##          x = "Points")+
##   annotation_custom(
##     grob = text_2_grob(team_avatar(params$team)),
##     xmin = params$solo_score_max-5,
##     xmax = params$solo_score_max,
##     ymin = 0,
##     ymax = Inf
##   )
## #plot_avatar(params$team)
## grid.arrange(
##   #text_2_grob(team_avatar(params$team)),
##   points_plot, nrow = 1)
## 


## ---- echo = FALSE, message = FALSE, fig.height = 4------------------------------------------------------

pic <- readJPEG(pic_path)

points_plot <- ggplot(data, aes(x = points)) + 
    geom_density(fill = "purple") + 
    xlim(0, params$solo_score_max) + 
    labs(title = "Distribution of Solo Points", 
         x = "Points")

grid.arrange(rasterGrob(pic), points_plot, nrow = 1)


## ---- echo = FALSE, message = FALSE----------------------------------------------------------------------

trend_plot <- ggplot(data, aes(x = 1:nrow(data), y = points, group = 1)) + 
    geom_line() + geom_point() + 
    labs(title = "Solo Points Trend", 
         x = "Match #", y = "Game Piece Points")

# game pieces scored plot
volume_plot <- ggplot(data, aes(autoScores + teleScores)) + 
    geom_bar(fill = "darkblue") + 
    labs(title = "Game Piece Volume", 
         x = "# of Game Pieces", y = "Count")

# game piece height plot
heights_idx <- which(colnames(data) == "autoHigh")
heights_idx <- heights_idx:(heights_idx + 5)
heights <- colSums(data[, heights_idx])
heights <- c(heights["autoHigh"] + heights["teleHigh"], 
             heights["autoMid"] + heights["teleMid"], 
             heights["autoLow"] + heights["teleLow"])
names(heights) <- c("high", "mid", "low")

height_plot <- ggplot(data.frame(heights = heights, row = names(heights)), 
                      aes(x = row, y = heights, fill = row)) + 
    geom_bar(stat = "identity") + 
    labs(title = "Scoring Row", x = "Height", y = "Count")

# intake bar plot
intakes_idx <- 13:16
intakes <- colSums(data[, intakes_idx])

intake_plot <- ggplot(data.frame(intakes = intakes, zone = names(intakes)), 
       aes(x = zone, y = intakes, fill = zone)) + 
    geom_bar(stat = "identity") + 
    labs(title = "Intake Preference", 
         x = "Zone", y = "Count")


rm(intakes_idx, intakes)

grid.arrange(volume_plot, height_plot, 
             trend_plot, intake_plot,
             ncol = 2)


## ---- echo = FALSE, message = FALSE----------------------------------------------------------------------
# endgame bar plot
endgame_plot <- ggplot(data, aes(teleopBalance, fill = teleopBalance)) + 
    geom_bar() + 
    labs(title = "Endgame Result", 
         x = "Endgame status", y = "Count")

balance_plot <- ggplot(data, aes(autoBalance, fill = autoBalance)) + 
    geom_bar() + 
    labs(title = "Auto Balance", 
         x = "Result", y = "Count")

scoring_plot <- ggplot(data, aes(autoScores)) + 
    geom_bar(fill = "darkblue") + 
    labs(title = "Auto Scoring Distribution", 
         x = "# Scores", y = "Count")

mobility_plot <- ggplot(data, aes(mobility)) + 
    geom_bar(fill = "darkgreen") + 
    labs(title = "Mobility", 
         x = "Result", y = "Count")

grid.arrange(endgame_plot, balance_plot, 
             scoring_plot, mobility_plot,
             ncol = 2)


## ---- echo = FALSE, message = FALSE----------------------------------------------------------------------
combined <- data$autoScores + data$teleScores
sumStat <- c(mean(combined, na.rm = TRUE), 
             median(combined, na.rm = TRUE), 
             max(combined, na.rm = TRUE), 
             mean(combined[combined > 0], na.rm = TRUE), 
             sd(combined, na.rm = TRUE))
names(sumStat) <- c("Mean", "Median", "Max", "Mean>0", "StDev")
pander(sumStat)


