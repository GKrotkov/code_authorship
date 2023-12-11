library(shiny)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(scales)
library(formattable)
library(utils)
library(devtools)
library(shinyFiles)
library(zip)
library(bslib)
library(ggrepel)
library(plotly)
library(rmarkdown)
library(tinytex)

load_all("tbaR")



#link to pull from statbotics
statbotics <- "https://api.statbotics.io/v2/"



# Values you may need to change
# Make sure to update these each time you update Kraken

tbaKey <- "2023mil"


path <- "C:\\Users\\gkrotkov\\OneDrive - CBRE, Inc\\Documents\\krakendata\\"

year <- "2023"

ourTeamNum <- 6672




setwd("krakendata")



week <- 5

numCols <- 28

defaultEPA <- 30



# The object that stores all of the values for the app
vals <- reactiveValues(
  mainframe = data.frame(teamNum = c(),
                         matchNum = c(),
                         alliance = c(),
                         driveStation = c(),
                         startLocation = c(),
                         preload = c(),
                         mobility = c(),
                         autoPickups = c(),
                         autoFailedPickups = c(),
                         autoCones = c(),
                         autoCubes = c(),
                         autoBalance = c(),
                         communityPickups = c(),
                         neutralPickups = c(), 
                         singlePickups = c(),
                         doublePickups = c(),
                         teleopCones = c(),
                         teleopCubes = c(),
                         shuttle = c(),
                         teleopBalance = c(),
                         buddyClimb = c(),
                         driver = c(),
                         scoutName = c(),
                         comments = c(),
                         
                         scoredT = c(),
                         scoredA = c(),
                         scoredCones = c(),
                         scoredCubes = c(),
                         scoredTCones = c(),
                         scoredTCubes =  c(),
                         scoredACones = c(),
                         scoredACubes = c(),
                         totalPickups = c(),
                         pointsT = c(),
                         pointsA = c(),
                         pointsE = c(),
                         pointsTotal = c(),
                         
                         scoredLowT = c(),
                         scoredMidT = c(),
                         scoredHighT = c(),
                         scoredLowA = c(),
                         scoredMidA = c(),
                         scoredHighA = c(),
                         
                         ct = c()
                         ),
  
  ssframe = data.frame(matchNum = c(),
                       
                       redScore = c(),
                       redLinks = c(),
                       
                       blueScore = c(),
                       blueLinks = c()
                       ),
  
  searchframe = data.frame(teamNum = c(),
                           matchNum = c(),
                           alliance = c(),
                           driveStation = c(),
                           startLocation = c(),
                           preload = c(),
                           mobility = c(),
                           autoPickups = c(),
                           autoFailedPickups = c(),
                           autoCones = c(),
                           autoCubes = c(),
                           autoBalance = c(),
                           communityPickups = c(),
                           neutralPickups = c(), 
                           singlePickups = c(),
                           doublePickups = c(),
                           teleopCones = c(),
                           teleopCubes = c(),
                           shuttle = c(),
                           teleopBalance = c(),
                           buddyClimb = c(),
                           driver = c(),
                           scoutName = c(),
                           comments = c(),
                           
                           scoredT = c(),
                           scoredA = c(),
                           scoredCones = c(),
                           scoredCubes = c(),
                           scoredTCones = c(),
                           scoredTCubes =  c(),
                           scoredACones = c(),
                           scoredACubes = c(),
                           totalPickups = c(),
                           pointsT = c(),
                           pointsA = c(),
                           pointsE = c(),
                           pointsTotal = c(),
                           
                           scoredLowT = c(),
                           scoredMidT = c(),
                           scoredHighT = c(),
                           scoredLowA = c(),
                           scoredMidA = c(),
                           scoredHighA = c(),
                           
                           ct = c()
                           ),
  
  matchsearchframe = data.frame(teamNum = c(),
                                matchNum = c(),
                                alliance = c(),
                                driveStation = c(),
                                startLocation = c(),
                                preload = c(),
                                mobility = c(),
                                autoPickups = c(),
                                autoFailedPickups = c(),
                                autoCones = c(),
                                autoCubes = c(),
                                autoBalance = c(),
                                communityPickups = c(),
                                neutralPickups = c(), 
                                singlePickups = c(),
                                doublePickups = c(),
                                teleopCones = c(),
                                teleopCubes = c(),
                                shuttle = c(),
                                teleopBalance = c(),
                                buddyClimb = c(),
                                driver = c(),
                                scoutName = c(),
                                comments = c(),
                                
                                scoredT = c(),
                                scoredA = c(),
                                scoredCones = c(),
                                scoredCubes = c(),
                                scoredTCones = c(),
                                scoredTCubes =  c(),
                                scoredACones = c(),
                                scoredACubes = c(),
                                totalPickups = c(),
                                pointsT = c(),
                                pointsA = c(),
                                pointsE = c(),
                                pointsTotal = c(),
                                
                                scoredLowT = c(),
                                scoredMidT = c(),
                                scoredHighT = c(),
                                scoredLowA = c(),
                                scoredMidA = c(),
                                scoredHighA = c(),
                                
                                ct = c()
  ),
  
  searchcalcframe = data.frame(matchNum = c(),
                             autoBalance = c(),
                             scoredA = c(),
                             scoredLowA = c(),
                             scoredMidA = c(),
                             scoredHighA = c(),
                             mobility = c(),
                             scoredT = c(),
                             scoredLowT = c(),
                             scoredMidT = c(), 
                             scoredHighT = c(),
                             teleopBalance = c(),
                             comments = c()
                             ),
  
  previewframe = data.frame(),
  sspreviewframe = data.frame(),
  
  teammatchesFound = FALSE,
  
  teammatchesframe = data.frame(teamNum = c(),
                                matches = c(),
                                alliances = c()
                                ),
  
    aagframe = data.frame(teamNum = c(),
                        aBalS = c(),
                        aBalF = c(),
                        mRate = c(),
                        sHa = c(),
                        sMa = c(),
                        sLa = c(),
                        
                        aSt = c(),
                        tMed = c(),
                        tMax = c(),
                        sDev = c(),
                        tMin = c(),
                        tMinG0 = c(),
                        
                        pcNeut = c(),
                        pcCom = c(),
                        pcSing = c(),
                        pcDoub = c(),
                        
                        tHigh = c(),
                        tMid = c(),
                        tLow = c(),
                        tCones = c(),
                        tCubes = c(),
                        
                        balE = c(),
                        balD = c(),
                        balF = c(),
                        balP = c(),
                        balN = c()
                        ),

  #team number, 
  #internal # of matches played, 
  #internal EPA, 
  ## of matches played, 
  #EPA,
  #ect- estimated cycle time. basically about how long it takes them to pick up and score a piece
  #aPPG- average points per game,
  #SEf - scoring efficiency. basicallly how good they are at scoring links to maximize points. 
  #aSt- average scored teleop
  #aSa - average scored auton
  #aS - average scored
  #BC - balance consistency
  
  teamframe = data.frame(teamNum = c(),
                         matchesPlayedi = c(),
                         EPAi = c(),
                         matchesPlayed = c(),
                         EPA = c(),
                         ECT = c(),
                         aPPG = c(),
                         aPPGa = c(),
                         aPPGt = c(),
                         aPPGe = c(),
                         SEf = c(),
                         aSt = c(),
                         aSa = c(),
                         aS = c(),
                         BC = c()
                         ),
  
  scheduleframe = data.frame(round = c(),
                             match_number = c(),  
                             red1 = c(),
                             red2 = c(),
                             red3 = c(),
                             blue1 = c(),
                             blue2 = c(),
                             blue3 = c(),
                             winChances = c(),
                             predictedWinners = c()
  ),
  
  plannerframe = data.frame(),
  
  constantframe = data.frame(teamNum = c(),
                             k = c(),
                             m = c()
  ),
  
  matchesourTeam = data.frame(matches = c(),
                           allliance = c(),
                           station = c()
                           ),
  
  autonScoring = data.frame(r1 = c("O", "O", "X"),
                            r2 = c("X", "X", "X"),
                            r3 = c("O", "O", "X"),
                            r4 = c("O", "O", "X"),
                            r5 = c("X", "X", "X"),
                            r6 = c("O", "O", "X"),
                            r7 = c("O", "O", "X"),
                            r8 = c("X", "X", "X"),
                            r9 = c("O", "O", "X")
  ),
  
  teleopScoring = data.frame(r1 = c("O", "O", "X"),
                             r2 = c("X", "X", "X"),
                             r3 = c("O", "O", "X"),
                             r4 = c("O", "O", "X"),
                             r5 = c("X", "X", "X"),
                             r6 = c("O", "O", "X"),
                             r7 = c("O", "O", "X"),
                             r8 = c("X", "X", "X"),
                             r9 = c("O", "O", "X")
  ),
  
  matchautonScoring = data.frame(r1 = c("O", "O", "X"),
                            r2 = c("X", "X", "X"),
                            r3 = c("O", "O", "X"),
                            r4 = c("O", "O", "X"),
                            r5 = c("X", "X", "X"),
                            r6 = c("O", "O", "X"),
                            r7 = c("O", "O", "X"),
                            r8 = c("X", "X", "X"),
                            r9 = c("O", "O", "X")
  ),
  
  matchteleopScoring = data.frame(r1 = c("O", "O", "X"),
                             r2 = c("X", "X", "X"),
                             r3 = c("O", "O", "X"),
                             r4 = c("O", "O", "X"),
                             r5 = c("X", "X", "X"),
                             r6 = c("O", "O", "X"),
                             r7 = c("O", "O", "X"),
                             r8 = c("X", "X", "X"),
                             r9 = c("O", "O", "X")
  ),
  
  winnersCalculated = FALSE,
  
  startupDone = FALSE
)


# Functions to update values

calcValues <- function(df) {
  
  info <- data.frame(
    teamNum = c(),
    matchNum = c(),
    alliance = c(),
    driveStation = c(),
    startLocation = c(),
    preload = c(),
    mobility = c(),
    autoPickups = c(),
    autoFailedPickups = c(),
    autoCones = c(),
    autoCubes = c(),
    autoBalance = c(),
    communityPickups = c(),
    neutralPickups = c(), 
    singlePickups = c(),
    doublePickups = c(),
    teleopCones = c(),
    teleopCubes = c(),
    shuttle = c(),
    teleopBalance = c(),
    buddyClimb = c(),
    driver = c(),
    scoutName = c(),
    comments = c(),
    
    scoredT = c(),
    scoredA = c(),
    scoredCones = c(),
    scoredCubes = c(),
    scoredTCones = c(),
    scoredTCubes =  c(),
    scoredACones = c(),
    scoredACubes = c(),
    totalPickups = c(),
    pointsT = c(),
    pointsA = c(),
    pointsE = c(),
    pointsTotal = c(),
    
    scoredLowT = c(),
    scoredMidT = c(),
    scoredHighT = c(),
    scoredLowA = c(),
    scoredMidA = c(),
    scoredHighA = c(),
    
    ct = c()
  )
  
  info <- rbind(info, df)
  
  info$scoredT <- numeric(1)
  info$scoredA <- numeric(1)
  info$scoredCones <- numeric(1)
  info$scoredCubes <- numeric(1)
  info$totalPickups <-  numeric(1)
  
  info$scoredLowT <- numeric(1)
  info$scoredMidT <- numeric(1)
  info$scoredHighT <- numeric(1)
  info$scoredLowA <- numeric(1)
  info$scoredMidA <- numeric(1)
  info$scoredHighA <- numeric(1)
  
  info$scoredTCones <- numeric(1)
  info$scoredTCubes <- numeric(1)
  info$scoredACones <- numeric(1)
  info$scoredACubes <- numeric(1)
  
  
  
  
  
  parsevals <- function(string) {
    if(string == "NA") {
      values <- character(0)
    } else {
      values <- unlist(strsplit(string, ",")) 
    }
    return(values)
  }
  
  findLength <- function(string) {
    return(length(parsevals(string)))
  }
  
  findPointVal <- function(string, time) {
    pScores <- as.integer(unlist(strsplit(string, ",")))
    points <- c(0, 0, 0)
    
    if(time == "t") {
      points[1] <- length(which(pScores <= 9))
      points[2] <- length(which(pScores > 9 & pScores <= 18))
      points[3] <- length(which(pScores > 18))
    } else if(time == "a") {
      points[1] <- length(which(pScores <= 9))
      points[2] <- length(which(pScores > 9 & pScores <= 18))
      points[3] <- length(which(pScores > 18))
    }
    
    return(points)
  }
  
  
  # Interior vals
  
  info$scoredTCones[1] <- findLength(info$teleopCones[1])
  info$scoredTCubes[1] <- findLength(info$teleopCubes[1])
  info$scoredACones[1] <- findLength(info$autoCones[1])
  info$scoredACubes[1] <- findLength(info$autoCubes[1])
  
  info$scoredT[1] <- unlist(info$scoredTCones[1]) + unlist(info$scoredTCubes[1])
  info$scoredA[1] <- unlist(info$scoredACones[1]) + unlist(info$scoredACubes[1])
  
  info$scoredCones[1] <- unlist(info$scoredTCones[1]) + unlist(info$scoredACones[1])
  info$scoredCubes[1] <- unlist(info$scoredTCubes[1]) + unlist(info$scoredACubes[1])
  
  info$totalPickups[1] <- unlist(info$scoredCones[1]) + unlist(info$scoredCubes[1])
  
  
  if(info$teleopCones[1] == "NA") {
    tConePoints <- c(0, 0, 0)
  } else {
    tConePoints <- findPointVal(info$teleopCones[1], "t")
  }
  
  if(info$teleopCubes[1] == "NA") {
    tCubePoints <- c(0, 0, 0)
  } else {
    tCubePoints <- findPointVal(info$teleopCubes[1], "t")
  }
  
  if(info$autoCones[1] == "NA") {
    aConePoints <- c(0, 0, 0)
  } else {
    aConePoints <- findPointVal(info$autoCones[1], "a")
  }
  
  if(info$autoCubes[1] == "NA") {
    aCubePoints <- c(0, 0, 0)
  } else {
    aCubePoints <- findPointVal(info$autoCubes[1], "a")
  }
  
  info$scoredLowT[1] <- tConePoints[3] + tCubePoints[3]
  info$scoredMidT[1] <- tConePoints[2] + tCubePoints[2]
  info$scoredHighT[1] <- tConePoints[1] + tCubePoints[1]
  
  info$scoredLowA[1] <- aConePoints[3] + aCubePoints[3]
  info$scoredMidA[1] <- aConePoints[2] + aCubePoints[2]
  info$scoredHighA[1] <- aConePoints[1] + aCubePoints[1]
  
  info$pointsT[1] <- info$scoredLowT[1] * 2 + info$scoredMidT[1] * 3 + info$scoredHighT[1] * 5
  info$pointsA[1] <- info$scoredLowA[1] * 3 + info$scoredMidA[1] * 4 + info$scoredHighA[1] * 6
  
  
  if(info$autoBalance[1] == "engage") {
    info$pointsA[1] <- info$pointsA[1] + 12
  } else if(info$autoBalance[1] == "dock") {
    info$pointsA[1] <- info$pointsA[1] + 10
  }
  
  if(info$teleopBalance[1] == "engage") {
    info$pointsE[1] <- 10
  } else if(info$teleopBalance[1] == "dock") {
    info$pointsE[1] <- 8
  } else {
    info$pointsE[1] <- 0
  }
  
  if(info$scoredT[1] > 0) {
    info$ct[1] <- round(135 / unlist(info$scoredT[1]), digits = 3)
  } else {
    info$ct[1] <- 0
  }
  
  links <- 0
  
  ssindex <- which(vals$ssframe$mmatchNum == info$matchNum[1])
  if(length(ssindex) > 0) {
    if(info$alliance == "r") {
      links <- vals$ssframe$redLinks[ssindex]
    } else {
      links <- vals$ssframe$blueLinks[ssindex]
    }
  }
  
  info$pointsT[1] <- info$pointsT[1] + (links * 5/3)
  
  info$pointsTotal[1] <- info$pointsT[1] + info$pointsA[1] + info$pointsE[1]
  
  
  teamNum <- info$teamNum[1]
  teamIdx <- as.integer(which(vals$teamframe$teamNum == teamNum))
  
  matches <- data.frame()
  
  matches <- rbind(matches, info)
  
  matchIndexes <- which(vals$mainframe$teamNum == teamNum)
  for(match in matchIndexes) {
    matches <- rbind(matches, vals$mainframe[match, ])
  }
  
  # aSt
  vals$teamframe$aSt[teamIdx] <- round(mean(matches$scoredT), digits = 2)
  
  # aSa
  vals$teamframe$aSa[teamIdx] <- round(mean(matches$scoredA), digits = 2)
  
  # aS
  vals$teamframe$aS[teamIdx] <- round((vals$teamframe$aSa[teamIdx] + vals$teamframe$aSt[teamIdx]), digits = 2)
  
  # ECT
  goodmatches <- which(matches$ct != 0)
  
  vals$teamframe$ECT[teamIdx] <- round(mean(as.double(matches$ct[goodmatches])), digits = 1)
  
  # aPPG
  vals$teamframe$aPPG[teamIdx] <- round(mean(matches$pointsTotal), digits = 2)
  
  vals$teamframe$aPPGt[teamIdx] <- round(mean(matches$pointsT), digits = 2)
  
  vals$teamframe$aPPGa[teamIdx] <- round(mean(matches$pointsA), digits = 2)
  
  vals$teamframe$aPPGe[teamIdx] <- round(mean(matches$pointsE), digits = 2)
  
  
  
  # BC
  balances <- matches$teleopBalance
  
  engages <- 0
  attempts <- 0
  
  for(b in 1:length(balances)) {
    if(!(is.na(balances[b]))) {
      if(balances[b] == "engage") {
        engages <- engages + 1
        attempts <- attempts + 1
      } else if(balances[b] == "fail" || balances[b] == "dock") {
        attempts <- attempts + 1
      }
    }
  }
  
  if(engages > 0 & attempts > 0) {
    vals$teamframe$BC[teamIdx] <- round((engages / attempts) * 100, digits = 0)
  } else {
    vals$teamframe$BC[teamIdx] <- 0
  }
  

  
  return(info)
}

getSchedule <- function() {
  
  matches <- read_event_matches(tbaKey)
  
  schedule <- data.frame(round = c(),
                         match_number = c(),
                         red1 = c(),
                         red2 = c(),
                         red3 = c(),
                         blue1 = c(),
                         blue2 = c(),
                         blue3 = c()
                         )
  
  for(m in 1:length(matches)) {
    curmatch <- matches[[m]]
    
    df <- data.frame(round = c(),
                     match_number = c(),
                     red1 = c(),
                     red2 = c(),
                     red3 = c(),
                     blue1 = c(),
                     blue2 = c(),
                     blue3 = c()
    )
    
    r1 <- curmatch$alliances$red$team_keys[[1]]
    r2 <- curmatch$alliances$red$team_keys[[2]]
    r3 <- curmatch$alliances$red$team_keys[[3]]
    
    b1 <- curmatch$alliances$blue$team_keys[[1]]
    b2 <- curmatch$alliances$blue$team_keys[[2]]
    b3 <- curmatch$alliances$blue$team_keys[[3]]
    
    r1 <- as.integer(substr(r1, 4, 7))
    r2 <- as.integer(substr(r2, 4, 7))
    r3 <- as.integer(substr(r3, 4, 7))
    
    b1 <- as.integer(substr(b1, 4, 7))
    b2 <- as.integer(substr(b2, 4, 7))
    b3 <- as.integer(substr(b3, 4, 7))
    
    mNum <- as.integer(curmatch$match_number)
    
    df <- data.frame(round = c("qf"),
                     match_number = c(mNum),
                     red1 = c(r1),
                     red2 = c(r2),
                     red3 = c(r3),
                     blue1 = c(b1),
                     blue2 = c(b2),
                     blue3 = c(b3)
    )
    
    schedule <- rbind(schedule, df)
  }
  
  vals$scheduleframe <- schedule[order(schedule$match_number), ]
  
}

recalcMatchValues <- function() {
  
  for(m in 1:nrow(vals$mainframe)) {
    
    row <- vals$mainframe[m, ]
    
    
    tryCatch({
      row$teamNum[1] <- as.integer(row$teamNum[1])
      row$matchNum[1] <- as.integer(row$matchNum[1])
      row$startLocation[1] <- as.integer(row$startLocation[1])
      row$communityPickups[1] <- as.integer(row$communityPickups[1])
      row$neutralPickups[1] <- as.integer(row$neutralPickups[1])
      row$singlePickups[1] <- as.integer(row$singlePickups[1])
      row$doublePickups[1] <- as.integer(row$doublePickups[1])
      row$driver[1] <- as.integer(row$driver[1])
      
      row$alliance[1] <- tolower(row$alliance[1])
      row$autoBalance[1] <- tolower(row$autoBalance[1])
      row$teleopBalance[1] <- tolower(row$teleopBalance[1])
      
      
      row$mobility[1] <- as.logical(row$mobility[1])
      row$shuttle[1] <- as.logical(row$shuttle[1])
      row$buddyClimb[1] <- as.logical(row$buddyClimb[1])
      
      
      
      if(is.na(row$autoPickups[1])) {
        row$autoPickups[1] <- "NA"
      } else {
        row$autoPickups[1] <- as.character(row$autoPickups[1])
      }
      
      if(is.na(row$autoFailedPickups[1])) {
        row$autoFailedPickups[1] <- "NA"
      } else {
        row$autoFailedPickups[1] <- as.character(row$autoFailedPickups[1])
      }
      
      if(is.na(row$autoCones[1])) {
        row$autoCones[1] <- "NA"
      } else {
        row$autoCones[1] <- as.character(row$autoCones[1])
      }
      
      if(is.na(row$autoCubes[1])) {
        row$autoCubes[1] <- "NA"
      } else {
        row$autoCubes[1] <- as.character(row$autoCubes[1])
      }
      
      if(is.na(row$teleopCones[1])) {
        row$teleopCones[1] <- "NA"
      } else {
        row$teleopCones[1] <- as.character(row$teleopCones[1])
      }
      
      if(is.na(row$teleopCubes[1])) {
        row$teleopCubes[1] <- "NA"
      } else {
        row$teleopCubes[1] <- as.character(row$teleopCubes[1])
      }
      
      if(row$teleopCones[1] != "NA") {
        teleop <- unlist(strsplit(row$teleopCones[1], ","))
        auto <- unlist(strsplit(row$autoCones[1], ","))
        
        bads <- numeric(0)
        
        for(e in 1:length(auto)) {
          bads <- append(bads, which(teleop == auto[e]))
        }
        
        if(length(bads) > 0) {
          ntele <- teleop[-(bads)]
          
          row$teleopCones[1] <- paste(ntele, collapse = ",")
        }
      }
      
      
      
      if(row$teleopCubes[1] != "NA") {
        teleop <- unlist(strsplit(row$teleopCubes[1], ","))
        auto <- unlist(strsplit(row$autoCubes[1], ","))
        
        bads <- numeric(0)
        
        for(e in 1:length(auto)) {
          bads <- append(bads, which(teleop == auto[e]))
        }
        
        if(length(bads) > 0) {
          ntele <- teleop[-(bads)]
          
          row$teleopCubes[1] <- paste(ntele, collapse = ",")
        }
      }
      
      row <- calcValues(row)
    },
    
    error = function(e) {
      
      
      print("Error with data in:")
      print(paste0("Team: ", row$teamNum[1], " Match: ", row$matchNum[1]))
      print(e)
      
      showNotification("Error in data. Check console for more info.", type = "error")
    }
    )
    
    vals$mainframe[m, ] <- row
  }
  
}

calcAAGVals <- function() {
  vals$aagframe = data.frame(teamNum = c(),
                             aBalS = c(),
                             aBalF = c(),
                             mRate = c(),
                             sHa = c(),
                             sMa = c(),
                             sLa = c(),
                             
                             aSt = c(),
                             tMed = c(),
                             tMax = c(),
                             sDev = c(),
                             tMin = c(),
                             tMinG0 = c(),
                             
                             pcNeut = c(),
                             pcCom = c(),
                             pcSing = c(),
                             pcDoub = c(),
                             
                             tHigh = c(),
                             tMid = c(),
                             tLow = c(),
                             tCones = c(),
                             tCubes = c(),
                             
                             balE = c(),
                             balD = c(),
                             balF = c(),
                             balP = c(),
                             balN = c()
                             )
  
  for(team in 1:nrow(vals$teamframe)) {
    tMatch <- data.frame(teamNum = c(),
                         matchNum = c(),
                         alliance = c(),
                         driveStation = c(),
                         startLocation = c(),
                         preload = c(),
                         mobility = c(),
                         autoPickups = c(),
                         autoFailedPickups = c(),
                         autoCones = c(),
                         autoCubes = c(),
                         autoBalance = c(),
                         communityPickups = c(),
                         neutralPickups = c(), 
                         singlePickups = c(),
                         doublePickups = c(),
                         teleopCones = c(),
                         teleopCubes = c(),
                         shuttle = c(),
                         teleopBalance = c(),
                         buddyClimb = c(),
                         driver = c(),
                         scoutName = c(),
                         comments = c(),
                         
                         scoredT = c(),
                         scoredA = c(),
                         scoredCones = c(),
                         scoredCubes = c(),
                         scoredTCones = c(),
                         scoredTCubes =  c(),
                         scoredACones = c(),
                         scoredACubes = c(),
                         totalPickups = c(),
                         pointsT = c(),
                         pointsA = c(),
                         pointsE = c(),
                         pointsTotal = c(),
                         
                         scoredLowT = c(),
                         scoredMidT = c(),
                         scoredHighT = c(),
                         scoredLowA = c(),
                         scoredMidA = c(),
                         scoredHighA = c(),
                         
                         ct = c()
                         )
    
    tNum <- vals$teamframe$teamNum[team]
    
    mfI <- which(vals$mainframe$teamNum == tNum)
    
    if(length(mfI) > 0) {
      
      for(i in 1:length(mfI)) {
        tMatch <- rbind(tMatch, vals$mainframe[mfI[i], ])
      }
      
      total <- nrow(tMatch)
      absuccess <- 0
      abfail <- 0
      asH <- 0
      asM <- 0
      asL <- 0
      mob <- 0
      
      tPieces <- 0
      tHigh <- 0
      tLow <- 0
      tMid <- 0
      tCone <- 0
      tCube <- 0
      
      neut <- 0
      com <- 0
      single <- 0
      double <- 0
      
      parks <- 0
      docks <- 0
      engages <- 0
      fails <- 0
      nas <- 0
      
      if(nrow(tMatch) > 0) {
        
        for(r in 1:nrow(tMatch)) {
          if(tMatch$autoBalance[r] == "fail" || tMatch$autoBalance[r] == "dock") {
            abfail <- abfail + 1
          } else if(tMatch$autoBalance[r] == "engage") {
            absuccess <- absuccess + 1
          }
          asH <- asH + tMatch$scoredHighA[r]
          asM <- asM + tMatch$scoredMidA[r]
          asL <- asL + tMatch$scoredLowA[r]
          
          if(tMatch$mobility[r] == TRUE) {
            mob <- mob + 1
          }
          tPieces <- tPieces + tMatch$scoredT[r]
          tHigh <- tHigh + tMatch$scoredHighT[r]
          tMid <- tMid + tMatch$scoredMidT[r]
          tLow <- tLow + tMatch$scoredLowT[r]
          
          tCone <- tCone + tMatch$scoredTCones[r]
          tCube <- tCube + tMatch$scoredTCubes[r]
          
          neut <- neut + tMatch$neutralPickups[r]
          com <- com + tMatch$communityPickups[r]
          single <- single + tMatch$singlePickups[r]
          double <- double + tMatch$doublePickups[r]
          
          if(tMatch$teleopBalance[r] == "engage") {
            engages <- engages + 1
          } else if(tMatch$teleopBalance[r] == "dock") {
            docks <- docks + 1
          } else if(tMatch$teleopBalance[r] == "park") {
            parks <- parks + 1
          } else if(tMatch$teleopBalance[r] == "fail") {
            fails <- fails + 1
          } else {
            nas <- nas + 1
          }
        }
      }
      
      mobPC <- round(mob / total, digits = 2) * 100
      aP <- round(mean(tMatch$scoredT), digits = 2)
      mP <- round(median(tMatch$scoredT), digits = 2)
      maxP <- round(max(tMatch$scoredT), digits = 2)
      dP <- round(sd(tMatch$scoredT), digits = 2)
      
      minP <- round(min(tMatch$scoredT), digits = 2)
      if(length(which(tMatch$scoredT > 0)) > 0) {
        minG0P <- round(min(tMatch$scoredT[which(tMatch$scoredT > 0)]), digits = 2)
      } else {
        minG0P <- 0
      }
      
      pcHigh <- round(tHigh / tPieces, digits = 2) * 100
      pcMid <- round(tMid / tPieces, digits = 2) * 100
      pcLow <- round(tLow / tPieces, digits = 2) * 100
      
      pcCones <- round(tCone / tPieces, digits = 2) * 100
      pcCubes <- round(tCube / tPieces, digits = 2) * 100
      
      intakes <- neut + com + single + double
      
      pcNeut <- round(neut / intakes, digits = 2) * 100
      pcCom <- round(com / intakes, digits = 2) * 100
      pcSingle <- round(single / intakes, digits = 2) * 100
      pcDouble <- round(double / intakes, digits = 2) * 100
      
      
      
      r <- data.frame(
        teamNum = c(tNum),
        aBalS = c(absuccess),
        aBalF = c(abfail),
        mRate = c(mobPC),
        sHa = c(asH),
        sMa = c(asM),
        sLa = c(asL),
        
        aSt = c(aP),
        tMed = c(mP),
        tMax = c(maxP),
        sDev = c(dP),
        tMin = c(minP),
        tMinG0 = c(minG0P),
      
        pcNeut = c(pcNeut),
        pcCom = c(pcCom),
        pcSing = c(pcSingle),
        pcDoub = c(pcDouble),
        
        tHigh = c(pcHigh),
        tMid = c(pcMid),
        tLow = c(pcLow),
        tCones = c(pcCones),
        tCubes = c(pcCubes),
        
        balE = c(engages),
        balD = c(docks),
        balF = c(fails),
        balP = c(parks),
        balN = c(nas)
      )
      
      vals$aagframe <- rbind(vals$aagframe, r)
      
    }
    
  }
  
}

recalcValues <- function() {
  for(team in 1:nrow(vals$teamframe)) {
    
    
    teamNum <- vals$teamframe$teamNum[team]
    matchI <- which(vals$mainframe$teamNum == teamNum)
    
    matches <- data.frame()
    
    for(match in 1:length(matchI)) {
      matchIndex <- matchI[match]
      
      matches <- rbind(matches, vals$mainframe[matchIndex, ])
    }
    
    
    
    # aSt
    vals$teamframe$aSt[team] <- round(mean(matches$scoredT), digits = 2)
    
    # aSa
    vals$teamframe$aSa[team] <- round(mean(matches$scoredA), digits = 2)
    
    # aS
    vals$teamframe$aS[team] <- round(vals$teamframe$aSa[team] + vals$teamframe$aSt[team], digits = 2)
    
    # ECT
    vals$teamframe$ECT[team] <- round(mean(as.double(matches$ct)), digits = 1)
    
    # aPPG
    vals$teamframe$aPPG[team] <- round(mean(matches$pointsTotal), digits = 2)
    
    vals$teamframe$aPPGa[team] <- round(mean(matches$pointsA), digits = 2)
    
    vals$teamframe$aPPGt[team] <- round(mean(matches$pointsT), digits = 2)
    
    vals$teamframe$aPPGe[team] <- round(mean(matches$pointsE), digits = 2)
    
    
    balances <- matches$teleopBalance
    
    engages <- 0
    attempts <- 0
    
    for(b in 1:length(balances)) {
      if(!(is.na(balances[b]))) {
        if(balances[b] == "engage") {
          engages <- engages + 1
          attempts <- attempts + 1
        } else if(balances[b] == "fail" || balances[b] == "dock") {
          attempts <- attempts + 1
        }
      }
    }
    
    if(engages > 0 & attempts > 0) {
      vals$teamframe$BC[team] <- round((engages / attempts) * 100, digits = 0)
    } else {
      vals$teamframe$BC[team] <- 0
    }
    
    
    if(length(matchI) == 0) {
      vals$teamframe$aSt[team] <- integer(1)
      vals$teamframe$aSa[team] <- integer(1)
      vals$teamframe$aS[team] <- integer(1)
      vals$teamframe$ECT[team] <- integer(1)
      vals$teamframe$aPPG[team] <- integer(1)
      vals$teamframe$aPPGa[team] <- integer(1)
      vals$teamframe$aPPGt[team] <- integer(1)
      vals$teamframe$aPPGe[team] <- integer(1)
      vals$teamframe$BC[team] <- integer(1)
    }
    
  }
}

findTeamMatch <- function(tNum, mNum) {
  return(which(vals$mainframe$teamNum == tNum & vals$mainframe$matchNum == mNum))
}

getTeamMatches <- function() {
  vals$teammatchesframe <- data.frame(teamNum = c(),
                                      matches = c(),
                                      alliances = c())
  
  
  for(team in 1:nrow(vals$teamframe)) {
    
    teamNum <- vals$teamframe$teamNum[team]
    
    matches <- c()
    alliances <- c()
    
    for(match in 1:nrow(vals$scheduleframe)) {
      
      if(vals$scheduleframe$red1[match] == teamNum ||
         vals$scheduleframe$red2[match] == teamNum ||
         vals$scheduleframe$red3[match] == teamNum) {
        
        matches <- append(matches, as.character(match))
        alliances <- append(alliances, "r")
        
      } else if(vals$scheduleframe$blue1[match] == teamNum ||
                vals$scheduleframe$blue2[match] == teamNum ||
                vals$scheduleframe$blue3[match] == teamNum) {
        
        matches <- append(matches, as.character(match))
        alliances <- append(alliances, "b")
        
      }
      
    }
    
    matchString <- paste(matches, collapse = ",")
    allianceString <- paste(alliances, collapse = ",")
    
    vals$teammatchesframe <- rbind(vals$teammatchesframe, data.frame(teamNum = c(teamNum), 
                                                                     matches = c(matchString),
                                                                     alliances = c(allianceString)))
  }
  
  vals$teammatchesFound = TRUE
}

calcSSValues <- function() {
  for(team in 1:nrow(vals$teamframe)) {
    vals$teamframe$EPA[team] <- vals$teamframe$EPAi[team]
    vals$teamframe$matchesPlayed[team] <- vals$teamframe$matchesPlayedi[team]
    
    teamNum <- vals$teamframe$teamNum[team]
    teamMatches <- as.integer(unlist(strsplit(as.character(vals$teammatchesframe$matches[team]), split = ",")))
    teamAlliances <- unlist(strsplit(vals$teammatchesframe$alliances[team], split = ","))
    
    ssmatches <- c()
    ssMatchI <- c()
    
    k <- 0
    m <- 0
    
    for(i in 1:length(teamMatches)) {
      ssmatches <- append(ssmatches, vals$ssframe$matchNum[which(vals$ssframe$matchNum == teamMatches[i])])
    }
    
    tAlliances <- teamAlliances[ssmatches]
    
    tIndexes <- which(vals$mainframe$teamNum == teamNum)
    
    mplayed <- as.integer(vals$teamframe$matchesPlayedi[team]) + length(tIndexes)
    
    
    
    
    
    if(mplayed <= 6) {
      k <- 0.5
    } else if(mplayed <= 12 & mplayed > 6) {
      k <- 0.5 - ((1/30) * (mplayed - 6))
    } else {
      k <- 0.3
    }
    
    if(mplayed <= 12) {
      m <- 0
    } else if(mplayed <= 36 & mplayed > 12) {
      m <- 1/24 * (mplayed - 12)
    } else {
      m <- 1
    }
    
    for(match in 1:nrow(vals$ssframe)) {
      if(length(which(vals$ssframe$matchNum[match] == teamMatches)) > 0) {
        mNum <- vals$ssframe$matchNum[match]
        
        red <- unlist(as.list(vals$scheduleframe[mNum, 3:5]))
        blue <- unlist(as.list(vals$scheduleframe[mNum, 6:8]))
        
        alliance <- character(1)
        
        if(length(which(teamNum == red)) > 0) {
          alliance <- "r"
        } else if(length(which(teamNum == blue)) > 0) {
          alliance <- "b"
        }
        
        redEPAs <- c(vals$teamframe$EPA[findTeamIndex(red[1])],
                     vals$teamframe$EPA[findTeamIndex(red[2])],
                     vals$teamframe$EPA[findTeamIndex(red[3])])
        blueEPAs <- c(vals$teamframe$EPA[findTeamIndex(blue[1])],
                      vals$teamframe$EPA[findTeamIndex(blue[2])],
                      vals$teamframe$EPA[findTeamIndex(blue[3])])
        
        redEPA <- sum(redEPAs)
        blueEPA <- sum(blueEPAs)
        
        redScore <- vals$ssframe$redScore[match]
        blueScore <- vals$ssframe$blueScore[match]
        
        deltaEPA <- k * (1/(1 + m)) * ((redScore - redEPA) - m * (blueScore - blueEPA))
        
        if(alliance == "r") {
          vals$teamframe$EPA[team] <- round((vals$teamframe$EPA[team] + deltaEPA), digits = 2)
        } else if(alliance == "b") {
          vals$teamframe$EPA[team] <- round((vals$teamframe$EPA[team] - deltaEPA), digits = 2)
        }
        
      }
    }
    
    
    
    SEf <- 0

    if(length(ssmatches) > 0) {
      for(m in 1:length(ssmatches)) {
        match <- ssmatches[m]
        
        alliance <- teamAlliances[which(match == teamMatches)]
        
        matchI <- which(vals$ssframe$matchNum == match)
        
        rscored <- 0
        bscored <- 0
        
        teamsInMatch <- as.list(vals$scheduleframe[match, 3:8])
        
        for(t in 1:6) {
          
          tNum <- teamsInMatch[t]
          
          tMatchI <- which(vals$mainframe$teamNum == tNum & vals$mainframe$matchNum == match)
          tScored <- vals$mainframe$scoredT[tMatchI] + vals$mainframe$scoredA[tMatchI]
          
          if(length(tScored) > 0) {
            if(t < 4) {
              rscored <- rscored + tScored
            } else {
              bscored <- bscored + tScored
            }
          }
        }
        
        if(alliance == "r") {
          if(rscored > 0 & vals$ssframe$redLinks[matchI] > 0) {
            SEf <- append(SEf, (floor(rscored / 3) * 3) / vals$ssframe$redLinks[matchI])
          } else {
            SEf <- append(SEf, 0)
          }
        } else if(alliance == "b") {
          if(bscored > 0 & vals$ssframe$blueLinks[matchI] > 0) {
            SEf <- append(SEf, (floor(bscored / 3) * 3) / vals$ssframe$blueLinks[matchI])
          } else {
            SEf <- append(SEf, 0)
          }
        }
        
      }
    }
    
    if(length(SEf) > 0) {
      vals$teamframe$SEf[team] <- round(mean(SEf), digits = 2)
    } else {
      vals$teamframe$SEf[team] <- 0
    }
    
    
  }
}

findTeamAlliance <- function(team, match) {
  matchRow <- vals$scheduleframe[match, ]
  
  teams <- c(matchRow$red1, matchRow$red2, matchRow$red3, matchRow$blue1, matchRow$blue2, matchRow$blue3)
  
  if(teams[1] == team || teams[2] == team || teams[3]) {
    return("r")
  } else {
    return("b")
  }
  
  
}

findTeamIndex <- function(teamNum) {
  return(which(vals$teamframe$teamNum == teamNum))
}

updatePlannerTable <- function(idx) {
  vals$plannerframe <- data.frame()
  
  
  
  matchrow <- vals$scheduleframe[idx, ]
  
  red1 <- matchrow$red1[1]
  red2 <- matchrow$red2[1]
  red3 <- matchrow$red3[1]
  
  blue1 <- matchrow$blue1[1]
  blue2 <- matchrow$blue2[1]
  blue3 <- matchrow$blue3[1]
  
  vals$plannerframe <- rbind(vals$plannerframe, vals$teamframe[findTeamIndex(red1), ])
  vals$plannerframe <- rbind(vals$plannerframe, vals$teamframe[findTeamIndex(red2), ])
  vals$plannerframe <- rbind(vals$plannerframe, vals$teamframe[findTeamIndex(red3), ])
  
  vals$plannerframe <- rbind(vals$plannerframe, vals$teamframe[findTeamIndex(blue1), ])
  vals$plannerframe <- rbind(vals$plannerframe, vals$teamframe[findTeamIndex(blue2), ])
  vals$plannerframe <- rbind(vals$plannerframe, vals$teamframe[findTeamIndex(blue3), ])
  
  vals$plannerframe <- cbind(vals$plannerframe, alliance = c("r", "r", "r", "b", "b", "b"))
}

saveMainframe <- function() {
  write.csv(vals$mainframe, paste0(path, "mainframe.csv"), row.names = FALSE)
}

saveTeamframe <- function() {
  write.csv(vals$teamframe, paste0(path, "teamframe.csv"), row.names = FALSE)
}

saveSSframe <- function() {
  write.csv(vals$ssframe, paste0(path, "ssframe.csv"), row.names = FALSE)
}

saveTeamMatchFrame <- function() {
  write.csv(vals$teammatchesframe, paste0(path, "teammatches.csv"), row.names = FALSE)
}

findOurMatchIndex <- function(match) {
  for(row in 1:nrow(vals$matchesourTeam)) {
    if(vals$matchesourTeam$matches[row] == as.integer(match)) {
      return(row)
    }
  }
}

canPingSite <- function(test.site) {
  !as.logical(system(paste("ping", test.site)))
}

parseRData <- function(string) {
  info <- unlist(strsplit(unlist(strsplit(string, split = "|", fixed = TRUE)), "="))
  
  firstone <- info[1]
  
  if(firstone != "teamNum") {
    return(NULL)
  }

  
  names <- info[seq(1, length(info), 2)]
  values <- info[seq(2, length(info), 2)]
  
  names(values) <- names
  
  parsedData <- data.frame(as.list(values))
  
  parsedData$teamNum[1] <- as.integer(parsedData$teamNum[1])
  parsedData$matchNum[1] <- as.integer(parsedData$matchNum[1])
  parsedData$startLocation[1] <- as.integer(parsedData$startLocation[1])
  parsedData$communityPickups[1] <- as.integer(parsedData$communityPickups[1])
  parsedData$neutralPickups[1] <- as.integer(parsedData$neutralPickups[1])
  parsedData$singlePickups[1] <- as.integer(parsedData$singlePickups[1])
  parsedData$doublePickups[1] <- as.integer(parsedData$doublePickups[1])
  parsedData$driver[1] <- as.integer(parsedData$driver[1])
  
  parsedData$alliance[1] <- tolower(parsedData$alliance[1])
  parsedData$autoBalance[1] <- tolower(parsedData$autoBalance[1])
  parsedData$teleopBalance[1] <- tolower(parsedData$teleopBalance[1])
  
  
  parsedData$mobility[1] <- as.logical(parsedData$mobility[1])
  parsedData$shuttle[1] <- as.logical(parsedData$shuttle[1])
  parsedData$buddyClimb[1] <- as.logical(parsedData$buddyClimb[1])
  
  if(substr(parsedData$driveStation[1], 1, 1) == "R") {
    end <- nchar(parsedData$driveStation[1])
    parsedData$driveStation[1] <- paste0("r", substr(parsedData$driveStation[1], end, end))
  } else {
    end <- nchar(parsedData$driveStation[1])
    parsedData$driveStation[1] <- paste0("b", substr(parsedData$driveStation[1], end, end))
  }
  
  if(parsedData$autoPickups[1] == "[]") {
    parsedData$autoPickups[1] <- "NA"
  } else {
    parsedData$autoPickups[1] <- str_replace_all(parsedData$autoPickups[1], "\\[|\\]", "")
    parsedData$autoPickups[1] <- str_replace_all(parsedData$autoPickups[1], " ", "")
  }
  
  if(parsedData$autoFailedPickups[1] == "[]") {
    parsedData$autoFailedPickups[1] <- "NA"
  } else {
    parsedData$autoFailedPickups[1] <- str_replace_all(parsedData$autoFailedPickups[1], "\\[|\\]", "")
    parsedData$autoFailedPickups[1] <- str_replace_all(parsedData$autoFailedPickups[1], " ", "")
  }
  
  "2"
  
  if(parsedData$autoCones[1] == "[]") {
    parsedData$autoCones[1] <- "NA"
  } else {
    parsedData$autoCones[1] <- str_replace_all(parsedData$autoCones[1], "\\[|\\]", "")
    parsedData$autoCones[1] <- str_replace_all(parsedData$autoCones[1], " ", "")
  }
  
  if(parsedData$autoCubes[1] == "[]") {
    parsedData$autoCubes[1] <- "NA"
  } else {
    parsedData$autoCubes[1] <- str_replace_all(parsedData$autoCubes[1], "\\[|\\]", "")
    parsedData$autoCubes[1] <- str_replace_all(parsedData$autoCubes[1], " ", "")
  }
  
  "3"
  
  if(parsedData$teleopCones[1] == "[]") {
    parsedData$teleopCones[1] <- "NA"
  } else {
    parsedData$teleopCones[1] <- str_replace_all(parsedData$teleopCones[1], "\\[|\\]", "")
    parsedData$teleopCones[1] <- str_replace_all(parsedData$teleopCones[1], " ", "")
  }
  
  if(parsedData$teleopCubes[1] == "[]") {
    parsedData$teleopCubes[1] <- "NA"
  } else {
    parsedData$teleopCubes[1] <- str_replace_all(parsedData$teleopCubes[1], "\\[|\\]", "")
    parsedData$teleopCubes[1] <- str_replace_all(parsedData$teleopCubes[1], " ", "")
  }
  
  if(parsedData$teleopCones != "NA") {
    teleop <- unlist(strsplit(parsedData$teleopCones, ","))
    auto <- unlist(strsplit(parsedData$autoCones, ","))
    
    bads <- numeric(0)
    
    for(e in 1:length(auto)) {
      bads <- append(bads, which(teleop == auto[e]))
    }
    
    if(length(bads) > 0) {
      ntele <- teleop[-(bads)]
      
      if(length(ntele) > 0) {
        parsedData$teleopCones[1] <- paste(ntele, collapse = ",")
      } else {
        parsedData$teleopCones[1] <- "NA"
      }
    }
  }
  
  if(parsedData$teleopCubes != "NA") {
    teleop <- unlist(strsplit(parsedData$teleopCubes, ","))
    auto <- unlist(strsplit(parsedData$autoCubes, ","))
    
    bads <- numeric(0)
    
    for(e in 1:length(auto)) {
      bads <- append(bads, which(teleop == auto[e]))
    }
    
    if(length(bads) > 0) {
      ntele <- teleop[-(bads)]
      
      if(length(ntele) > 0) {
        parsedData$teleopCubes[1] <- paste(ntele, collapse = ",")
      } else {
        parsedData$teleopCubes[1] <- "NA"
      }
    }
  }
  
  
  
  
  return(parsedData)
}

parseSData <- function(string) {
  info <- unlist(strsplit(string, "|", fixed = TRUE))
  
  firstone <- info[1]
  
  if(firstone == "teamNum") {
    return(NULL)
  }
  
  
  data <- data.frame(matchNum = integer(1),
                     
                          redScore = integer(1),
                          redLinks = integer(1),
                     
                          blueScore = integer(1),
                          blueLinks = integer(1)
  )
  
  mNum <- info[1]
  rScore <- info[2]
  rLinks <- info[3]
  bScore <- info[4]
  bLinks <- info[5]
  
  
  data$matchNum[1] <- as.integer(mNum)
  
  data$redScore[1] <- as.integer(rScore)
  data$redLinks[1] <- as.integer(rLinks)
  
  data$blueScore[1] <- as.integer(bScore)
  data$blueLinks[1] <- as.integer(bLinks)

  
  return(data)
  
}

clearTFrame <- function(type) {
  if(type == "team") {
    vals$teleopScoring <- data.frame(r1 = c("O", "O", "X"),
                                     r2 = c("X", "X", "X"),
                                     r3 = c("O", "O", "X"),
                                     r4 = c("O", "O", "X"),
                                     r5 = c("X", "X", "X"),
                                     r6 = c("O", "O", "X"),
                                     r7 = c("O", "O", "X"),
                                     r8 = c("X", "X", "X"),
                                     r9 = c("O", "O", "X"))
  } else if(type == "match") {
    vals$matchteleopScoring <- data.frame(r1 = c("O", "O", "X"),
                                     r2 = c("X", "X", "X"),
                                     r3 = c("O", "O", "X"),
                                     r4 = c("O", "O", "X"),
                                     r5 = c("X", "X", "X"),
                                     r6 = c("O", "O", "X"),
                                     r7 = c("O", "O", "X"),
                                     r8 = c("X", "X", "X"),
                                     r9 = c("O", "O", "X"))
  }
}

clearAFrame <- function(type) {
  if(type == "team") {
    vals$autonScoring <- data.frame(r1 = c("O", "O", "X"),
                                    r2 = c("X", "X", "X"),
                                    r3 = c("O", "O", "X"),
                                    r4 = c("O", "O", "X"),
                                    r5 = c("X", "X", "X"),
                                    r6 = c("O", "O", "X"),
                                    r7 = c("O", "O", "X"),
                                    r8 = c("X", "X", "X"),
                                    r9 = c("O", "O", "X"))
  } else if(type == "match") {
    vals$matchautonScoring <- data.frame(r1 = c("O", "O", "X"),
                                    r2 = c("X", "X", "X"),
                                    r3 = c("O", "O", "X"),
                                    r4 = c("O", "O", "X"),
                                    r5 = c("X", "X", "X"),
                                    r6 = c("O", "O", "X"),
                                    r7 = c("O", "O", "X"),
                                    r8 = c("X", "X", "X"),
                                    r9 = c("O", "O", "X"))
  }
}

getWinChances <- function() {
  updateOurMatches()
  
  vals$winnersCalculated <- TRUE
  
  winChances <- c()
  predictedWinners <- c()
  
  for(matchNum in 1:nrow(vals$scheduleframe)) {
    winChance <- as.numeric(calculateWinChance(matchNum))
    predictedWinner <- NA
    
    # ERROR HERE
    if(winChance > 50) {
      predictedWinner <- "r"
    } else if(winChance < 50) {
      predictedWinner <- "b"
      winChance <- 100 - winChance
    } else {
      predictedWinner <- "even"
    }
    
    winChances <- append(winChances, winChance)
    predictedWinners <- append(predictedWinners, predictedWinner)
  }
  
  vals$scheduleframe["winChances"] <- winChances
  vals$scheduleframe["predictedWinners"] <- predictedWinners
}

getCoords <- function(index) {
  coords <- c()
  
  if(index <= 9) {
    coords <- append(coords, 1)
    coords <- append(coords, index)
  } else if(9 < index & index <= 18) {
    coords <- append(coords, 2)
    coords <- append(coords, (index - 9))
  } else if(18 < index & index <= 27) {
    coords <- append(coords, 3)
    coords <- append(coords, (index - 18))
  }
  
  return(coords)
}

calculatePredScore <- function(match) {
  r1EPA <- vals$teamframe$EPA[findTeamIndex(vals$scheduleframe$red1[as.integer(match)])]
  r2EPA <- vals$teamframe$EPA[findTeamIndex(vals$scheduleframe$red2[as.integer(match)])]
  r3EPA <- vals$teamframe$EPA[findTeamIndex(vals$scheduleframe$red3[as.integer(match)])]
  
  b1EPA <- vals$teamframe$EPA[findTeamIndex(vals$scheduleframe$blue1[as.integer(match)])]
  b2EPA <- vals$teamframe$EPA[findTeamIndex(vals$scheduleframe$blue2[as.integer(match)])]
  b3EPA <- vals$teamframe$EPA[findTeamIndex(vals$scheduleframe$blue3[as.integer(match)])]
  
  redScore <- r1EPA + r2EPA + r3EPA
  blueScore <- b1EPA + b2EPA + b3EPA
  
  return(c(redScore, blueScore))
}

addScores <- function(data, time, type, datatype) {
  
  for(num in 1:length(data)) {
    loc <- as.integer(data[num])
    row <- getCoords(loc)[1]
    col <- getCoords(loc)[2]
    
    if(datatype == "team") {
      if(time == "a") {
        if(type == "cone") {
          vals$autonScoring[row, col] <- "🔺"
        } else if(type == "cube") {
          vals$autonScoring[row, col] <- "🟦"
        }
      } else if(time == "t") {
        if(type == "cone") {
          vals$teleopScoring[row, col] <- "🔺"
        } else if(type == "cube") {
          vals$teleopScoring[row, col] <- "🟦"
        }
      }
    } else if(datatype == "match") {
      if(time == "a") {
        if(type == "cone") {
          vals$matchautonScoring[row, col] <- "🔺"
        } else if(type == "cube") {
          vals$matchautonScoring[row, col] <- "🟦"
        }
      } else if(time == "t") {
        if(type == "cone") {
          vals$matchteleopScoring[row, col] <- "🔺"
        } else if(type == "cube") {
          vals$matchteleopScoring[row, col] <- "🟦"
        }
      }
    }
    
  }
}

pullTBAData <- function() {
  vals$scheduleframe = data.frame(round = c(),
                                  match_number = c(),  
                                  red1 = c(),
                                  red2 = c(),
                                  red3 = c(),
                                  blue1 = c(),
                                  blue2 = c(),
                                  blue3 = c()
  )
  
  tbaMatchListTemp <- event_matches(tbaKey)
  
  View(tbaMatchListTemp[order(tbaMatchListTemp$match_number)])
  
  tbaMatchListTemp <- tbaMatchListTemp[order(tbaMatchListTemp$match_number), ]
  
  for(match in 1:nrow(tbaMatchListTemp)) {
    if(tbaMatchListTemp$comp_level[match] == "qm") {
      currentMatchTeams <- data.frame(round = tbaMatchListTemp$comp_level[match],
                                      match_number = tbaMatchListTemp$match_number[match],
                                      red1 = substr(tbaMatchListTemp$red$teamkeys[1][match], 4, 7),
                                      red2 = substr(tbaMatchListTemp$red2[match], 4, 7),
                                      red3 = substr(tbaMatchListTemp$red3[match], 4, 7),
                                      blue1 = substr(tbaMatchListTemp$blue1[match], 4, 7),
                                      blue2 = substr(tbaMatchListTemp$blue2[match], 4, 7),
                                      blue3 = substr(tbaMatchListTemp$blue3[match], 4, 7)
      )
      
      vals$scheduleframe <- rbind(vals$scheduleframe, currentMatchTeams)
    }
  }
}

pullStatboticsData <- function() {
  teamsInfo <- event_teams(tbaKey)
  
  teamNums <- c()
  
  for(team in 1:length(teamsInfo$key)) {
    teamNums <- append(teamNums, substr(teamsInfo$key[team], 4, 7))
  }
  
  for(team in  1:length(teamsInfo$key)) {
    t <- list(teamNum = NA,
              matchesPlayedi = NA,
              EPAi = NA)
    
    teamNumber <- teamNums[team]
    
    teamInfo <- getStatboticsTeam(teamNumber)
    
    t$teamNum <- teamNumber
    
    if(!(is.null(teamInfo$epa_end))) {
      t$EPAi <- teamInfo$epa_end
    } else {
      t$EPAi <- defaultEPA
    }
    
    if(!(is.null(teamInfo$count))) {
      t$matchesPlayedi <- teamInfo$count
    } else {
      if(week == 1) {
        t$matchesPlayedi <- 0
      } else {
        t$matchesPlayedi <- 5
      }
    }
    
    vals$teamframe <- rbind(vals$teamframe, t)
  }
  nrows <- nrow(vals$teamframe)
  
  vals$teamframe$EPA <- vals$teamframe$EPAi
  vals$teamframe$matchesPlayed <- vals$teamframe$matchesPlayedi
  
  vals$teamframe$ECT <- numeric(1)
  vals$teamframe$aPPG <- numeric(1)
  vals$teamframe$aPPGa <- numeric(1)
  vals$teamframe$aPPGt <- numeric(1)
  vals$teamframe$aPPGe <- numeric(1)
  vals$teamframe$SEf <- numeric(1)
  vals$teamframe$aSt <- numeric(1)
  vals$teamframe$aSa <- numeric(1)
  vals$teamframe$aS <- numeric(1)
  vals$teamframe$BC <- numeric(1)
}

updateStatboticsEPAs <- function() {
  for(team in 1:nrow(vals$teamframe)) {
    teamNum <- vals$teamframe$teamNum[team]
    
    tEPA <- getStatboticsTeam(teamNum)$epa_end
    
    vals$teamframe$EPA[team] <- tEPA
  }
}

calculateWinChance <- function(matchNum, fromAlliance = "default") {
  
  red1 <- vals$scheduleframe$red1[matchNum]
  red2 <- vals$scheduleframe$red2[matchNum]
  red3 <- vals$scheduleframe$red3[matchNum]
  
  blue1 <- vals$scheduleframe$blue1[matchNum]
  blue2 <- vals$scheduleframe$blue2[matchNum]
  blue3 <- vals$scheduleframe$blue3[matchNum]
  
  red1EPA <- vals$teamframe$EPA[findTeamIndex(red1)]
  red2EPA <- vals$teamframe$EPA[findTeamIndex(red2)]
  red3EPA <- vals$teamframe$EPA[findTeamIndex(red3)]
  
  blue1EPA <- vals$teamframe$EPA[findTeamIndex(blue1)]
  blue2EPA <- vals$teamframe$EPA[findTeamIndex(blue2)]
  blue3EPA <- vals$teamframe$EPA[findTeamIndex(blue3)]
  
  epaDiff <- (red1EPA + red2EPA + red3EPA) - (blue1EPA + blue2EPA + blue3EPA)
  
  winChance <- 1/(1 + 10 ^ (epaDiff/400))
  
  formattedWinChance <- format(winChance * 100, digits = 3)
  
  return(formattedWinChance)
}


updateOurMatches <- function() {
  for(match in 1:nrow(vals$scheduleframe)) {
    
    d <- NA
    if(vals$scheduleframe$red1[match] == ourTeamNum) {
      d <- list(match, "r", 1)
    } else if(vals$scheduleframe$red2[match] == ourTeamNum) {
      d <- list(match, "r", 2)
    } else if(vals$scheduleframe$red3[match] == ourTeamNum) {
      d <- list(match, "r", 3)
    } else if(vals$scheduleframe$blue1[match] == ourTeamNum) {
      d <- list(match, "b", 1)
    } else if(vals$scheduleframe$blue2[match] == ourTeamNum) {
      d <- list(match, "b", 2)
    } else if(vals$scheduleframe$blue3[match] == ourTeamNum) {
      d <- list(match, "b", 3)
    }
    
    if(!(is.na(d[1]))) {
      names(d) <- list("matches", "alliance", "station")
      vals$matchesourTeam <- rbind(vals$matchesourTeam, d)
    }
  }
}


getStatboticsTeam <- function(teamNum) {
  link <- paste(statbotics, "team_year/", teamNum, "/", year, sep = "")
  
  team <- content(GET(link))
}

# Dictates the layout of the UI



ui <- navbarPage(
  
  title = div(icon("gitkraken", lib = "font-awesome", style = "color: #bb520a;"), "   Kraken"),
  
  tabPanel("Data",
           fluidPage(
               column(fluidRow(
                 sidebarPanel(
                   radioButtons(
                     "inputType", "What type of data?",
                     c("Regular Scout" = "regScout",
                       "Superscout" = "supScout")
                   ),
                   textAreaInput("dataInput", "Input Scout Data", width = "300px", height = "150px", resize = "none"),
                   actionButton("enterData", "Enter"),
                   fluidRow(
                     h4("Apply Data?"),
                     actionButton("yesData", "Yes", width = '60px'),
                     actionButton("noData", "No", width = '60px')),
                   width = 12
                 )),
                 fluidRow(
                   sidebarPanel(
                     h5("WARNING: this button will delete all current data. Consider exporting the data first."),
                     actionButton("deleteFiles", "Delete Files"),
                     width = 12
                   )
                 ),
                 
                 width = 3
               ),
               column(
                 conditionalPanel(
                   condition = "input.inputType == 'regScout'",
                   DTOutput("preview")
                 ),
                 conditionalPanel(
                   condition = "input.inputType == 'supScout'",
                   DTOutput("ssPreview")
                 ),
                 width = 9
               )
             
             
             
             
             
             
           )
  ),
  
  tabPanel("Teams",
           column(
             sidebarPanel(
               fluidRow(
                 selectInput("pickTeamNum",
                             "Choose Team",
                             choices = ""),
                 actionButton("enterSearch",
                              "Enter")
               ),
               br(),
               fluidRow(
                 plotOutput("teamPhoto"),
               ),
               width = 12
             ),
             width = 3
           ),
           column(
             tabsetPanel(
               tabPanel("Overview",
                        fluidRow(
                          DTOutput("searchCalcData")
                        ),
                        fluidRow(
                          column(
                            sidebarPanel(
                              titlePanel("Auton"),
                              textOutput("autoBSuccesses"),
                              textOutput("autoBFails"),
                              textOutput("mobilitySuccessRate"),
                              textOutput("autoScoreHigh"),
                              textOutput("autoScoreMid"),
                              textOutput("autoScoreLow"),
                              width = 12
                            ),
                            width = 3
                          ),
                          column(
                            sidebarPanel(
                              fluidRow(
                                titlePanel("Teleop")
                              ),
                              fluidRow(
                                column(
                                  textOutput("teleopMean"),
                                  textOutput("teleopMedian"),
                                  textOutput("teleopMax"),
                                  textOutput("teleopDeviation"),
                                  textOutput("teleopMin"),
                                  textOutput("teleopMinG0"),
                                  br(),
                                  textOutput("type"),
                                  textOutput("cone"),
                                  textOutput("cube"),
                                  
                                  width = 6
                                ),
                                column(
                                  textOutput("scoringLocs"),
                                  textOutput("pcNeut"),
                                  textOutput("pcCom"),
                                  textOutput("pcSingle"),
                                  textOutput("pcDouble"),
                                  br(),
                                  textOutput("height"),
                                  textOutput("high"),
                                  textOutput("mid"),
                                  textOutput("low"),
                                  
                                  width = 6
                                )
                              ),
                              width = 12
                            ),
                            width = 6
                          ),
                          column(
                            sidebarPanel(
                              titlePanel("Endgame"),
                              textOutput("BC"),
                              br(),
                              textOutput("engages"),
                              textOutput("docks"),
                              textOutput("fails"),
                              textOutput("parks"),
                              textOutput("nas"),
                              width = 12
                            ),
                            width = 3
                          )
                        ),
               ),
               tabPanel("Raw Data",
                        fluidRow(
                          DTOutput("searchframeout")  
                        ),
                        fluidRow(
                          actionButton("clearteamscoringDTs", "Clear Score Viewers")
                        ),
                        fluidRow(
                          column(
                            sidebarPanel(
                              h5("Auton:"),
                              tableOutput("teamautonscoring"),
                              width = 12
                            ),
                            width = 6
                          ),
                          column(
                            sidebarPanel(
                              h5("Teleop:"),
                              tableOutput("teamteleopscoring"),
                              width = 12
                            ),
                            width = 6
                          )
                        )
               )
             ),
             width = 9
           )
  ), 
  
  tabPanel("Matches",
           fluidPage(
             column(
               fluidRow(
                 sidebarPanel(
                   textInput("matchsearch",
                             "Search:",
                             placeholder = "enter match number"),
                   actionButton("entermatchSearch", "Enter"),
                   width = 12
                 )
               ),
               fluidRow(
                 sidebarPanel(
                   width = 12
                 )
               ),
               width = 3
             ),
             column(
               fluidRow(
                 DTOutput("matchsearchDT")
               ),
               fluidRow(
                 actionButton("clearmatchscoringDTs", "Clear Score Viewers")
               ),
               fluidRow(
                 column(
                   sidebarPanel(
                     h5("Auton:"),
                     tableOutput("matchautonScoring"),
                     width = 12
                   ),
                   width = 6
                 ),
                 column(
                   sidebarPanel(
                     h5("Teleop:"),
                     tableOutput("matchteleopScoring"),
                     width = 12
                   ),
                   width = 6
                 )
               ),
               width = 9
             )
           )),
  
  tabPanel("Competition",
           DTOutput("mainframeOutput")),
  
  tabPanel("Superscout",
           DTOutput("ssframeOutput")),
  
  tabPanel("Graph",
           # add additional fluidRows with plotOut or plotlyOutput to this tab
           fluidRow(
             splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot1"), plotOutput("plot2"))
           ),
           fluidRow(
             splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot3"), plotOutput("plot4"))
           ),
           fluidRow(
             plotlyOutput(outputId = "plot5")
           )
           
  ),
  
  tabPanel("Match Planner",
           fluidPage(
             column(
               fluidRow(
                 sidebarPanel(
                   selectInput(
                     "selectedMatch",
                     "Select a Match",
                     choices = ""
                   ),
                   width = 12
                 )
               ),
               fluidRow(
                 sidebarPanel(
                  textOutput("winChanceourTeam"),
                  textOutput("predictedScore"),
                  textOutput("driverStation"),
                  textOutput("alliance"),
                  width = 12 
                 )
               ),
               width = 3
             ),
             column(
               fluidRow(
               DTOutput("plannertable"),
               width = 9
               ),
               fluidRow(
                 column(
                   sidebarPanel(
                     titlePanel("Auton"),
                     textOutput("PautoBSuccesses"),
                     textOutput("PautoBFails"),
                     textOutput("PmobilitySuccessRate"),
                     textOutput("PautoScoreHigh"),
                     textOutput("PautoScoreMid"),
                     textOutput("PautoScoreLow"),
                     width = 12
                   ),
                   width = 3
                 ),
                 column(
                   sidebarPanel(
                     fluidRow(
                       titlePanel("Teleop")
                     ),
                     fluidRow(
                       column(
                         textOutput("PteleopMean"),
                         textOutput("PteleopMedian"),
                         textOutput("PteleopMax"),
                         textOutput("PteleopDeviation"),
                         textOutput("PteleopMin"),
                         textOutput("PteleopMinG0"),
                         br(),
                         textOutput("Ptype"),
                         textOutput("Pcone"),
                         textOutput("Pcube"),
                         
                         width = 6
                       ),
                       column(
                         textOutput("PscoringLocs"),
                         textOutput("PpcNeut"),
                         textOutput("PpcCom"),
                         textOutput("PpcSingle"),
                         textOutput("PpcDouble"),
                         br(),
                         textOutput("Pheight"),
                         textOutput("Phigh"),
                         textOutput("Pmid"),
                         textOutput("Plow"),
                         
                         width = 6
                       )
                     ),
                     width = 12
                   ),
                   width = 6
                 ),
                 column(
                   sidebarPanel(
                     titlePanel("Endgame"),
                     textOutput("PBC"),
                     br(),
                     textOutput("Pengages"),
                     textOutput("Pdocks"),
                     textOutput("Pfails"),
                     textOutput("Pparks"),
                     textOutput("Pnas"),
                     width = 12
                   ),
                   width = 3
                 )
               ),
               width = 9
             )
           )),
  
  tabPanel("Stats",
           materialSwitch(inputId = "showstats", label = "Show All Stats"),
           DTOutput("statsData")),
  tabPanel("Schedule",
           DTOutput("matchScheduleDT")),
  tabPanel("Functions",
           fluidRow(
             textOutput("functionStatus")
           ),
           fluidRow(
             h4("Calculate Values"),
             actionButton("getWinChances", "Get Win Percents"),
             actionButton("calcSSData", "Calculate SS Values"),
             actionButton("recalcVals", "Recalculate Calculated Values"),
             actionButton("findTeamMatches", "Find Team Matches"),
             actionButton("resetEPAs", "Reset EPAs to defaults"),
             actionButton("calcAAG", "Calculate AAG Values"),
             actionButton("recalcMatchVals", "Recalculate Match Values")
           ),
           fluidRow(
             h4("File Editing"),
             actionButton("updateData", "Update Data from Files"),
             actionButton("saveData", "Save Data")
           ),
           fluidRow(
             h4("Online Functions"),
             actionButton("pullStatboticsEPAs", "Update EPAs from Statbotics"),
             actionButton("testConnection", "Test Internet Connection")
           ),
           fluidRow(
            h4("Report Generating"),
            actionButton("generateReports", "Generate Reports")
           )
           ),
  
  selected = "Data"
)


tags$script(src="https://kit.fontawesome.com/7f698a1940.js")


# Outlines the server function which dictates the logic of the app

server <- function(input, output, session) {
  
  observe({  
    if(!vals$startupDone) {
      
      if(!(file.exists(paste0(path, "schedule.csv")))) {
        getSchedule()
        
        write.csv(vals$scheduleframe, paste0(path, "schedule.csv"), row.names = FALSE)
      } else {
        vals$scheduleframe <- read.csv(paste0(path, "schedule.csv"))
      }
      
      if(file.exists(paste0(path, "mainframe.csv"))) {
        vals$mainframe <- read.csv(paste0(path, "mainframe.csv"))
      }
      
      if(file.exists(paste0(path, "teamframe.csv"))) {
        vals$teamframe <- read.csv(paste0(path, "teamframe.csv"))
      } else {
        pullStatboticsData()
        
        write.csv(vals$teamframe, paste0(path, "teamframe.csv"), row.names = FALSE)
      }
      
      if(file.exists(paste0(path, "ssframe.csv"))) {
        vals$ssframe <- read.csv(paste0(path, "ssframe.csv"))
      }
      
      if(file.exists(paste0(path, "teammatches.csv"))) {
        vals$teammatchesframe <- read.csv(paste0(path, "teammatches.csv"))
      }
      
      calcAAGVals()
      
      vals$startupDone <- TRUE
    }
  })
  
  
  # Data Page
  
  observeEvent(input$enterData, {
    if(input$inputType == "regScout") {
      f <- input$dataInput
      
      parsed <- parseRData(f)
      
      vals$previewframe <- parsed
    } else if(input$inputType == "supScout") {
      f <- input$dataInput
      
      parsed <- parseSData(f)
      
      vals$sspreviewframe <- parsed
    }
  })
  
  observeEvent(input$yesData, {
    if(input$inputType == "regScout") {
      if(nrow(vals$previewframe) == 1) {
        if(nrow(vals$mainframe) == 0) {
          vals$mainframe <- rbind(vals$mainframe, calcValues(vals$previewframe[1, ]))
          
          teamIndex <- findTeamIndex(vals$previewframe$teamNum[1])
          
          vals$teamframe$matchesPlayed[teamIndex] <- vals$teamframe$matchesPlayed[teamIndex] + 1
          
          saveMainframe()
          saveTeamframe()
          vals$previewframe <- data.frame()
          updateTextAreaInput(session, "dataInput", value = "")
          return(NULL)
        } else {
          
          repeatFound <- FALSE
          
          for(row in 1:length(vals$mainframe$teamNum)) {
            if(vals$mainframe[row, 1] == vals$previewframe[1, 1] & vals$mainframe[row, 2] == vals$previewframe[1, 2]) {
              repeatFound <- TRUE
              showModal(repeatModal())
            }
          }
          
          if(repeatFound == FALSE) {
            vals$mainframe <- rbind(vals$mainframe, calcValues(vals$previewframe[1, ]))
            
            teamIndex <- findTeamIndex(vals$previewframe$teamNum[1])
            
            vals$teamframe$matchesPlayed[teamIndex] <- vals$teamframe$matchesPlayed[teamIndex] + 1
            
            saveMainframe()
            saveTeamframe()
            vals$previewframe <- data.frame()
            updateTextAreaInput(session, "dataInput", value = "")
          }
          
        }
      } else if(nrow(vals$previewframe) > 1) {
        return(NULL)
      } else {
        return(NULL)
      }
      
      
      
    } else if(input$inputType == "supScout") {
      
      if(nrow(vals$sspreviewframe) == 1) {
        if(nrow(vals$ssframe) == 0) {
          vals$ssframe <- rbind(vals$ssframe, vals$sspreviewframe[1, ])
          
          saveSSframe()
          
          vals$sspreviewframe <- data.frame()
          updateTextAreaInput(session, "dataInput", value = "")
          return(NULL)
        } else {
          repeatFound <- FALSE
          
          for(row in 1:length(vals$ssframe$matchNum)) {
            if(vals$ssframe$matchNum[row] == vals$sspreviewframe$matchNum[1]) {
              repeatFound <- TRUE
              showModal(repeatModal())
            } 
          }
          
          if(repeatFound == FALSE) {
            vals$ssframe <- rbind(vals$ssframe, vals$sspreviewframe[1, ])
            
            saveSSframe()
            vals$sspreviewframe <- data.frame()
            updateTextAreaInput(session, "dataInput", value = "")
          }
        }
      }
      
    }
  })
  
  observeEvent(input$noData, {
    vals$previewframe <- data.frame()
    updateTextAreaInput(session, "dataInput", value = "")
  })
  
  observeEvent(input$deleteFiles, {
    
    showModal(deleteModal())
  })
  
  observeEvent(input$confirmDelete, {
    vals$mainframe <- data.frame()
    if(file.exists(paste0(path, "mainframe.csv"))) {
      file.remove(paste0(path, "mainframe.csv"))
    }
    removeModal()
  })
  
  deleteModal <- function() {
    modalDialog(
      tagList(actionButton("confirmDelete", "Yes"),
      title = "Are you sure you want to delete all data?"
      )
    )
  }
  
  repeatModal <- function() {
    modalDialog(
      tagList(
        h4("This looks like repeat data. Are you sure you want to add another entry to the system?"),
        actionButton("confirmApplyRepeat", "Yes")
      ),
      title = "Repeat Data?"
    )
  }
  
  output$preview <- renderDT(datatable(vals$previewframe, extensions = "FixedColumns", 
                                       options = list(scrollX = TRUE, paging = FALSE),
                                       selection = "single"))
  output$ssPreview <- renderDT(datatable(vals$sspreviewframe, extensions = "FixedColumns", 
                                         options = list(scrollX = TRUE, paging = FALSE),
                                         selection = "single"))
  
  # Teams Page
  
  observeEvent(input$enterSearch, {
    teamNum <- input$pickTeamNum
    
    picPath <- paste0(path, "Pictures\\", teamNum, ".png")
    picAlt <- paste0(path, "Pictures\\", teamNum, ".JPG")
    nopePath <- paste0(path, "Pictures\\nope.png")
    
    if(file.exists(picPath)) {
      output$teamPhoto <- renderImage(list(src = picPath, width = 280), deleteFile = FALSE)
    } else if(file.exists(picAlt)) {
      output$teamPhoto <- renderImage(list(src = picAlt, width = 280), deleteFile = FALSE)
    } else {
      output$teamPhoto <- renderImage(list(src = nopePath, width = 280), deleteFile = FALSE)
    }
    
    
    vals$searchframe <- data.frame(teamNum = c(),
                                 matchNum = c(),
                                 alliance = c(),
                                 driveStation = c(),
                                 startLocation = c(),
                                 preload = c(),
                                 mobility = c(),
                                 autoPickups = c(),
                                 autoFailedPickups = c(),
                                 autoCones = c(),
                                 autoCubes = c(),
                                 autoBalance = c(),
                                 communityPickups = c(),
                                 neutralPickups = c(), 
                                 singlePickups = c(),
                                 doublePickups = c(),
                                 teleopCones = c(),
                                 teleopCubes = c(),
                                 shuttle = c(),
                                 teleopBalance = c(),
                                 buddyClimb = c(),
                                 driver = c(),
                                 scoutName = c(),
                                 comments = c(),
                                 
                                 scoredT = c(),
                                 scoredA = c(),
                                 scoredCones = c(),
                                 scoredCubes = c(),
                                 scoredTCones = c(),
                                 scoredTCubes =  c(),
                                 scoredACones = c(),
                                 scoredACubes = c(),
                                 totalPickups = c(),
                                 pointsT = c(),
                                 pointsA = c(),
                                 pointsE = c(),
                                 pointsTotal = c(),
                                 
                                 scoredLowT = c(),
                                 scoredMidT = c(),
                                 scoredHighT = c(),
                                 scoredLowA = c(),
                                 scoredMidA = c(),
                                 scoredHighA = c(),
                                 
                                 ct = c()
    )
    
    vals$searchcalcframe = data.frame(matchNum = c(),
                                    autoBalance = c(),
                                    scoredA = c(),
                                    scoredLowA = c(),
                                    scoredMidA = c(),
                                    scoredHighA = c(),
                                    mobility = c(),
                                    scoredT = c(),
                                    scoredLowT = c(),
                                    scoredMidT = c(), 
                                    scoredHighT = c(),
                                    teleopBalance = c(),
                                    comments = c()
    )
    
    indexes <- which(vals$mainframe$teamNum == teamNum)
    
    
    if(length(indexes > 0)) {
      for(i in 1:length(indexes)) {
        idx <- indexes[i]
        
        vals$searchframe <- rbind(vals$searchframe, vals$mainframe[idx, ])
      }
    }
    
    
    output$searchframeout <- renderDT(datatable(vals$searchframe, extensions = "FixedColumns", selection = "single",
                                              options = list(scrollX = TRUE, paging = FALSE, scrollY = "240px",
                                                             fixedColumns = list(leftColumns = 3))))
    
    output$teamautonscoring <- renderTable(vals$autonScoring)
    output$teamteleopscoring <- renderTable(vals$teleopScoring)
    
    observeEvent(input$searchframeout_rows_selected, {
      clearAFrame("team")
      clearTFrame("team")
      
      
      
      row <- vals$searchframe[input$searchframeout_rows_selected, ]
      
      aCones <- unlist(strsplit(toString(row$autoCones), ","))
      aCubes <- unlist(strsplit(toString(row$autoCubes), ","))
      
      tCones <- unlist(strsplit(toString(row$teleopCones), ","))
      tCubes <- unlist(strsplit(toString(row$teleopCubes), ","))
      
      
      if(aCones[1] != "NA" && aCones[1] != "" && !(is.na(aCones[1]))) {
        addScores(aCones, "a", "cone", "team")
      }
      
      if(aCubes[1] != "NA" && aCubes[1] != "" && !(is.na(aCubes[1]))) {
        addScores(aCubes, "a", "cube", "team")
      }
      
      if(tCones[1] != "NA" && tCones[1] != "" && !(is.na(tCones[1]))) {
        addScores(tCones, "t", "cone", "team")
      }
      
      if(tCubes[1] != "NA" && tCubes[1] != "" && !(is.na(tCubes[1]))) {
        addScores(tCubes, "t", "cube", "team")
      }
    })
    
    observeEvent(input$clearteamscoringDTs, {
      clearAFrame("team")
      clearTFrame("team")
    })
    
    output$searchCalcData <- renderDT(datatable(vals$searchcalcframe, options = list(scrollX = TRUE, scrollY = "220px",
                                                                                 paging = FALSE)))
    
    
    if(nrow(vals$searchframe) > 0) {
      for(row in 1:nrow(vals$searchframe)) {
        df <- data.frame(matchNum = numeric(1),
                         scoredA = numeric(1),
                         scoredLowA = numeric(1),
                         scoredMidA = numeric(1),
                         scoredHighA = numeric(1),
                         mobility = logical(1),
                         autoBalance = character(1),
                         scoredT = numeric(1),
                         scoredLowT = numeric(1),
                         scoredMidT = numeric(1), 
                         scoredHighT = numeric(1),
                         teleopBalance = character(1),
                         comments = character(1)
        )
        
        
        df$matchNum[1] <- vals$searchframe$matchNum[row]
        df$scoredA[1] <- vals$searchframe$scoredA[row]
        df$scoredLowA[1] <- vals$searchframe$scoredLowA[row]
        df$scoredMidA[1] <- vals$searchframe$scoredMidA[row]
        df$scoredHighA[1] <- vals$searchframe$scoredHighA[row]
        df$mobility[1] <- vals$searchframe$mobility[row]
        df$autoBalance[1] <- vals$searchframe$autoBalance[row]
        df$scoredT[1] <- vals$searchframe$scoredT[row]
        df$scoredLowT[1] <- vals$searchframe$scoredLowT[row]
        df$scoredMidT[1] <- vals$searchframe$scoredMidT[row]
        df$scoredHighT[1] <- vals$searchframe$scoredHighT[row] 
        df$teleopBalance[1] <- vals$searchframe$teleopBalance[row]
        df$comments[1] <- vals$searchframe$comments[row]
        
        vals$searchcalcframe <- rbind(vals$searchcalcframe, df)
        
      }
    }
    

    aagrow <- which(vals$aagframe$teamNum == teamNum)
    teamrow <- which(vals$teamframe$teamNum == teamNum)
    
        
    output$autoBSuccesses <- renderText(paste0("Balance Successes: ", vals$aagframe$aBalS[aagrow]))
    output$autoBFails <- renderText(paste0("Balance Fails: ", vals$aagframe$aBalF[aagrow]))
    output$mobilitySuccessRate <- renderText(paste0("Mobility Rate: ", vals$aagframe$mRate[aagrow], "%"))
    
    output$autoScoreHigh <- renderText(paste0("Scored High: ", vals$aagframe$sHa[aagrow]))
    output$autoScoreMid <- renderText(paste0("Scored Mid: ", vals$aagframe$sMa[aagrow]))
    output$autoScoreLow <- renderText(paste0("Scored Low: ", vals$aagframe$sLa[aagrow]))
    
    output$teleopMean <- renderText(paste0("Average Scored: ", vals$aagframe$aSt[aagrow]))
    output$teleopMedian <- renderText(paste0("Score Median: ", vals$aagframe$tMed[aagrow]))
    output$teleopMax <- renderText(paste0("Max Scored: ", vals$aagframe$tMax[aagrow]))
    output$teleopDeviation <- renderText(paste0("Score Deviation: ", vals$aagframe$sDev[aagrow]))
    
    output$scoringLocs <- renderText("Intake Percentages:")
    output$pcNeut <- renderText(paste0("Neutral Zone: ", vals$aagframe$pcNeut[aagrow], "%"))
    output$pcCom <- renderText(paste0("Community: ", vals$aagframe$pcCom[aagrow], "%"))
    output$pcSingle <- renderText(paste0("Single Substation: ", vals$aagframe$pcSing[aagrow], "%"))
    output$pcDouble <- renderText(paste0("Double Substation: ", vals$aagframe$pcDoub[aagrow], "%"))
    
    output$teleopMin <- renderText(paste0("Min: ", vals$aagframe$tMin[aagrow]))
    output$teleopMinG0 <- renderText(paste0("Min (no 0): ", vals$aagframe$tMinG0[aagrow]))
    
    output$height <- renderText("Scoring Row:")
    output$high <- renderText(paste0("High: ", vals$aagframe$tHigh[aagrow], "%"))
    output$mid <- renderText(paste0("Mid: ", vals$aagframe$tMid[aagrow], "%"))
    output$low <- renderText(paste0("Low: ", vals$aagframe$tLow[aagrow], "%"))
    output$type <- renderText("Scored Types:")
    output$cone <- renderText(paste0("Cones: ", vals$aagframe$tCones[aagrow], "%"))
    output$cube <- renderText(paste0("Cubes: ", vals$aagframe$tCubes[aagrow], "%"))
    
    output$engages <- renderText(paste0("Engages: ", vals$aagframe$balE[aagrow]))
    output$docks <- renderText(paste0("Docks: ", vals$aagframe$balD[aagrow]))
    output$fails <- renderText(paste0("Fails: ", vals$aagframe$balF[aagrow]))
    output$parks <- renderText(paste0("Parks: ", vals$aagframe$balP[aagrow]))
    output$nas <- renderText(paste0("N/As: ", vals$aagframe$balN[aagrow]))
    
    output$BC <- renderText(paste0("BC: ", vals$teamframe$BC[teamrow], "%"))
  })
  
  
  
  
  
  
  
  # Matches Page
  
  observeEvent(input$entermatchSearch, {
    vals$matchsearchframe <- data.frame(teamNum = c(),
                                        matchNum = c(),
                                        alliance = c(),
                                        driveStation = c(),
                                        startLocation = c(),
                                        preload = c(),
                                        mobility = c(),
                                        autoPickups = c(),
                                        autoFailedPickups = c(),
                                        autoCones = c(),
                                        autoCubes = c(),
                                        autoBalance = c(),
                                        communityPickups = c(),
                                        neutralPickups = c(), 
                                        singlePickups = c(),
                                        doublePickups = c(),
                                        teleopCones = c(),
                                        teleopCubes = c(),
                                        shuttle = c(),
                                        teleopBalance = c(),
                                        buddyClimb = c(),
                                        driver = c(),
                                        scoutName = c(),
                                        comments = c(),
                                        
                                        scoredT = c(),
                                        scoredA = c(),
                                        scoredCones = c(),
                                        scoredCubes = c(),
                                        scoredTCones = c(),
                                        scoredTCubes =  c(),
                                        scoredACones = c(),
                                        scoredACubes = c(),
                                        totalPickups = c(),
                                        pointsT = c(),
                                        pointsA = c(),
                                        pointsE = c(),
                                        pointsTotal = c(),
                                        
                                        scoredLowT = c(),
                                        scoredMidT = c(),
                                        scoredHighT = c(),
                                        scoredLowA = c(),
                                        scoredMidA = c(),
                                        scoredHighA = c(),
                                        
                                        ct = c()
    )
    
    s <- as.integer(input$matchsearch)
    
    if(is.null(s) || is.null(vals$mainframe)) {
      return(NULL)
    }
    
    for(newrow in 1:nrow(vals$mainframe)) {
      if(vals$mainframe$matchNum[newrow] == s) {
        vals$matchsearchframe <- rbind(vals$matchsearchframe, vals$mainframe[newrow, ])
      }
    }
    
    searchVal <- as.integer(input$search)
    
    mIndex <- which(vals$teamframe$matchNum == searchVal)
    
    
  })
  
  observe({
    matchsearchDT <- datatable(vals$matchsearchframe, options = list(scrollX = TRUE, scrollY = "260px", paging = FALSE),
                               selection = "single")
    
    output$matchsearchDT <- renderDT(matchsearchDT)
    
    observeEvent(input$clearmatchscoringDTs, {
      clearAFrame("match")
      clearTFrame("match")
    })
    
    observeEvent(input$matchsearchDT_rows_selected, {
      clearAFrame("match")
      clearTFrame("match")
      
      
      row <- vals$matchsearchframe[input$matchsearchDT_rows_selected, ]
      
      aCones <- unlist(strsplit(toString(row$autoCones), ","))
      aCubes <- unlist(strsplit(toString(row$autoCubes), ","))
      
      tCones <- unlist(strsplit(toString(row$teleopCones), ","))
      tCubes <- unlist(strsplit(toString(row$teleopCubes), ","))
      
      
      if(aCones[1] != "NA") {
        addScores(aCones, "a", "cone", "match")
      }
      
      if(aCubes[1] != "NA") {
        addScores(aCubes, "a", "cube", "match")
      }
      
      if(tCones[1] != "NA") {
        addScores(tCones, "t", "cone", "match")
      }
      
      if(tCubes[1] != "NA") {
        addScores(tCubes, "t", "cube", "match")
      }
    })
  })
  
  
  output$matchautonScoring <- renderTable(vals$matchautonScoring)
  output$matchteleopScoring <- renderTable(vals$matchteleopScoring)
  
  
  
  
  
  
  
  # Competition Page
  
  output$mainframeOutput <- renderDT(datatable(vals$mainframe[order(vals$mainframe$matchNum), ],
                                               extensions = "FixedColumns",
                                               options = list(scrollX = TRUE, scrollY = "540px",
                                                              fixedColumns = list(leftColumns = 3),
                                                                              paging = FALSE)))
  
  
  # Superscout Page
  
  output$ssframeOutput <- renderDT(datatable(vals$ssframe, options = list(scrollX = TRUE, scrollY = "540px",
                                                                          paging = FALSE)))
  
  
  # Qualitative Page
  
  
  
  
  # Graphs Page
  # Y = points in auton
  # X = points in teleop
  # color = ABT
  
  output$plot1 <- 
    renderPlot({
      ggplot(vals$teamframe, aes(x = aSt, y = aSa, label = teamNum)) +
        ggrepel::geom_label_repel()+
        labs(x='Average Score Teleop',y='Average Score Auton.')+
        ggtitle("Score plot")+
        scale_size(trans = "reverse",
                   range = c(5, 10))
    })
  
  output$plot2 <- 
    renderPlot({
      vals$mainframe %>%
        group_by(teamNum)%>%
        summarise(avg_Cone=mean(scoredTCones+scoredACones,na.rm=T),
                  avg_Cube=mean(scoredTCubes+scoredACubes,na.rm=T))%>%
        ggplot(aes(x=avg_Cone,y=avg_Cube,label=teamNum) ) +
        ggrepel::geom_label_repel(position = 'jitter')+
        ggtitle("cone vs cube")
    })
  
  output$plot3 <- 
    renderPlot({
      vals$mainframe %>%
        group_by(teamNum)%>%
        summarise(avg_pickups=mean(totalPickups+as.numeric(!is.na(preload)),na.rm=T),
                  avg_placed=mean( (scoredTCones+
                                      scoredACones+
                                      scoredTCubes+
                                      scoredACubes),na.rm=T),
                  batting_avg=avg_placed/avg_pickups) %>%
        ggplot(aes(x=avg_placed,y=avg_pickups,label=teamNum,fill=batting_avg) ) +
        ggrepel::geom_label_repel(position = 'jitter')+
        labs(y='Pickups + preload')+
        scale_color_gradient2(low = 'red',high = 'blue',midpoint = 0.6,aesthetics = "fill")+
        geom_abline(slope=1,intercept = 0)+
        labs(caption = 'formula total placed/(total pickups + preload)')
    })
  
  output$plot4 <- 
    renderPlot({
      vals$mainframe  %>%
        group_by(teamNum)%>%
        summarise(avg_Driv=mean(driver,na.rm=T),
                  avg_pickups=mean(totalPickups+as.numeric(!is.na(preload)),na.rm=T)
        ) %>% 
        ggplot(aes(label =reorder(factor(teamNum),avg_Driv),x= avg_Driv,y=avg_pickups)) +
        ggrepel::geom_label_repel(force=1,max.overlaps=20)+
        coord_flip()  ##+
      ##               ggtitle("Driver skill vs pickups")  
    })
  
  output$plot5 <- 
    renderPlotly({
      plot_ly(vals$teamframe, x = ~aSt, y = ~aSa, type = 'scatter',
              mode = 'text',
              name = 'Scouted Average Score Teleop vs Average Score Auton',
              text=~teamNum)
    })
  
  
  # Match Planner Page
  
  observe({
    updateSelectInput(session, "selectedMatch",
                      choices = vals$matchesourTeam$matches)
    updateSelectInput(session, "pickTeamNum",
                      choices = vals$teamframe$teamNum)
  })
  
  observeEvent(input$selectedMatch, {
    if(vals$winnersCalculated) {
      updatePlannerTable(input$selectedMatch)
      
      winPC <- vals$scheduleframe$winChances[as.integer(input$selectedMatch)]
      winTeam <- vals$scheduleframe$predictedWinner[as.integer(input$selectedMatch)]
      
      index <- findOurMatchIndex(input$selectedMatch)
      alliance <- vals$matchesourTeam$alliance[index]
      station <- vals$matchesourTeam$station[index]
      
      scores <- calculatePredScore(input$selectedMatch)
      
      redScore <- as.integer(scores[1])
      blueScore <- as.integer(scores[2])
      
      if(alliance != winTeam) {
        winPC <- 100 - winPC
      }
      
      if(alliance == "r") {
        alliance <- "Red"
      } else if(alliance == "b") {
        alliance <- "Blue"
      }
      
      # col 13 should be hidden
      
      output$winChanceourTeam <- renderText(paste("Win Chance: ", winPC, "%", sep = ""))
      output$driverStation <- renderText(paste("Driver Station: ", station))
      output$predictedScore <- renderText(paste("Predicted Score: ", redScore, " - ", blueScore, sep = ""))
      output$alliance <- renderText(paste("Alliance:", alliance))
      
      observe({
        output$plannertable <- renderDT({
          datatable(
            vals$plannerframe,
            options = list(columnDefs = list(list(visible = FALSE, targets = c(13)))),
            selection = "single"
          ) %>% formatStyle(
            1, 13,
            color = styleEqual(c("r", "b"), c("red", "blue"))
          )
        })
      })
      
      observeEvent(input$plannertable_rows_selected, {
        teamNum <- vals$plannerframe$teamNum[input$plannertable_rows_selected]
        
        aagrow <- which(vals$aagframe$teamNum == teamNum)
        teamrow <- which(vals$teamframe$teamNum == teamNum)
        
        if(length(aagrow) > 0) {
          
          output$PautoBSuccesses <- renderText(paste0("Balance Successes: ", vals$aagframe$aBalS[aagrow]))
          output$PautoBFails <- renderText(paste0("Balance Fails: ", vals$aagframe$aBalF[aagrow]))
          output$PmobilitySuccessRate <- renderText(paste0("Mobility Rate: ", vals$aagframe$mRate[aagrow], "%"))
          
          output$PautoScoreHigh <- renderText(paste0("Scored High: ", vals$aagframe$sHa[aagrow]))
          output$PautoScoreMid <- renderText(paste0("Scored Mid: ", vals$aagframe$sMa[aagrow]))
          output$PautoScoreLow <- renderText(paste0("Scored Low: ", vals$aagframe$sLa[aagrow]))
          
          output$PteleopMean <- renderText(paste0("Average Scored: ", vals$aagframe$aSt[aagrow]))
          output$PteleopMedian <- renderText(paste0("Score Median: ", vals$aagframe$tMed[aagrow]))
          output$PteleopMax <- renderText(paste0("Max Scored: ", vals$aagframe$tMax[aagrow]))
          output$PteleopDeviation <- renderText(paste0("Score Deviation: ", vals$aagframe$sDev[aagrow]))
          
          output$PteleopMin <- renderText(paste0("Min: ", vals$aagframe$tMin[aagrow]))
          output$PteleopMinG0 <- renderText(paste0("Min (no 0): ", vals$aagframe$tMinG0[aagrow]))
          
          output$PscoringLocs <- renderText("Intake Percentages:")
          output$PpcNeut <- renderText(paste0("Neutral Zone: ", vals$aagframe$pcNeut[aagrow], "%"))
          output$PpcCom <- renderText(paste0("Community: ", vals$aagframe$pcCom[aagrow], "%"))
          output$PpcSingle <- renderText(paste0("Single Substation: ", vals$aagframe$pcSing[aagrow], "%"))
          output$PpcDouble <- renderText(paste0("Double Substation: ", vals$aagframe$pcDoub[aagrow], "%"))
          
          output$Pheight <- renderText("Scoring Row:")
          output$Phigh <- renderText(paste0("High: ", vals$aagframe$tHigh[aagrow], "%"))
          output$Pmid <- renderText(paste0("Mid: ", vals$aagframe$tMid[aagrow], "%"))
          output$Plow <- renderText(paste0("Low: ", vals$aagframe$tLow[aagrow], "%"))
          output$Ptype <- renderText("Scored Types:")
          output$Pcone <- renderText(paste0("Cones: ", vals$aagframe$tCones[aagrow], "%"))
          output$Pcube <- renderText(paste0("Cubes: ", vals$aagframe$tCubes[aagrow], "%"))
          
          output$Pengages <- renderText(paste0("Engages: ", vals$aagframe$balE[aagrow]))
          output$Pdocks <- renderText(paste0("Docks: ", vals$aagframe$balD[aagrow]))
          output$Pfails <- renderText(paste0("Fails: ", vals$aagframe$balF[aagrow]))
          output$Pparks <- renderText(paste0("Parks: ", vals$aagframe$balP[aagrow]))
          output$Pnas <- renderText(paste0("N/As: ", vals$aagframe$balN[aagrow]))
          
          output$PBC <- renderText(paste0("BC: ", vals$teamframe$BC[teamrow], "%"))
          
        }
      })
      
    }
    
  })
  
  
  
  # Functions Page
  
  observeEvent(input$getWinChances, {
    
    getWinChances()
    
  })
  
  observeEvent(input$updateData, {
    
    if(file.exists(paste0(path, "mainframe.csv"))) {
      vals$mainframe <- read.csv(paste0(path, "mainframe.csv"))
    }
    
    if(file.exists(paste0(path, "teamframe.csv"))) {
      vals$teamframe <- read.csv(paste0(path, "teamframe.csv"))
    }
    
    if(file.exists(paste0(path, "schedule.csv"))) {
      vals$scheduleframe <- read.csv(paste0(path, "schedule.csv"))
    }
    
    if(file.exists(paste0(path, "ssframe.csv"))) {
      vals$ssframe <- read.csv(paste0(path, "ssframe.csv"))
    }
    
    if(file.exists(paste0(path, "teammatches.csv"))) {
      vals$teammatchesframe <- read.csv(paste0(path, "teammatches.csv"))
    }
  })
  
  observeEvent(input$resetEPAs, {
    vals$teamframe$EPAi <- defaultEPA
    saveTeamframe()
    showNotification("EPA's retrieved")
  })
  
  observeEvent(input$pullStatboticsEPAs, {
    updateStatboticsEPAs()
    saveTeamframe()
  })
  
  observeEvent(input$saveData, {
    
    time <- as.POSIXlt(Sys.time())
    
    mon <- time$mon + 1
    d <- time$mday
    
    h <- time$hour
    m <- as.character(time$min)
    
    ftime <- paste0(mon, "-", d, "_", h, ".", m)
    
    foldername <- paste0("scoutingdata", "_", ftime)
    
    folderpath <- paste0(path, foldername, "\\")
    
    dir.create(paste0(path, foldername))
    
    write.csv(vals$mainframe, paste0(folderpath, "mainframe.csv"), row.names = FALSE)
    write.csv(vals$scheduleframe, paste0(folderpath, "schedule.csv"), row.names = FALSE)
    write.csv(vals$teamframe, paste0(folderpath, "teamframe.csv"), row.names = FALSE)
    write.csv(vals$ssframe, paste0(folderpath, "ssframe.csv"), row.names = FALSE)
    write.csv(vals$teammatchesframe, paste0(folderpath, "teammatches.csv"), row.names = FALSE)
    
    showNotification("Data Saved!", type = "message")
    
  })
  
  observeEvent(input$calcSSData, {
    if(vals$teammatchesFound || file.exists(paste0(path, "teammatches.csv"))) {
      calcSSValues()
      saveSSframe()
    }
  })
  
  observeEvent(input$calcAAG, {
    calcAAGVals()
  })
  
  observeEvent(input$recalcVals, {
    recalcValues()
    saveTeamframe()
  })
  
  observeEvent(input$findTeamMatches, {
    getTeamMatches()
    saveTeamMatchFrame()
  })
  
  observeEvent(input$testConnection, {
    
  })
  
  observeEvent(input$recalcMatchVals, {
    recalcMatchValues()
    saveMainframe()
  })
  
  observeEvent(input$generateReports, {
    
    withProgress(
      message = "Generating Reports",
      value = 0,
      {
        for(t in 1:nrow(vals$teamframe)) {
          tNum <- vals$teamframe$teamNum[t]
          
          incProgress(amount = 1/nrow(vals$teamframe), detail = paste0("Generating report for ", tNum))
          
          render("teamReport.Rmd", output_file = paste0(path, "Reports\\", tNum, "report.pdf"))
          
        }
      }
    )
    
  })
  
  
  # Stats Page

  # possible stats to show:
  # teamNum, matchesPlayedi, EPAi, matchesPlayed, EPA, ECT, aPPG, SEf, aSt, aSa, aS, BC
  
  stats_to_show <- c("teamNum", "ECT", "aPPG", "aSa", "aSt", "aS", "BC")
  
  observe({
  if(input$showstats) {
    output$statsData <- renderDT(datatable(vals$teamframe, 
           options = list(scrollX = TRUE, scrollY = "540px", paging = FALSE)))
  } else{
    output$statsData <- renderDT(datatable(vals$teamframe[,stats_to_show], 
           options = list(scrollX = TRUE, scrollY = "540px", paging = FALSE))) 
    }})
  
  
  
  
  
  # Schedule Page
  observe({
    output$matchScheduleDT <- renderDT({
      datatable(
        vals$scheduleframe,
        extensions = "FixedColumns",
        options = list(scrollX = TRUE, scrollY = "540px", paging = FALSE),
        selection = "single"
      ) %>% formatStyle(
        9, 10,
        color = styleEqual(c("r", "b", "even"), c("red", "blue", "gray"))
      )
    })
  })
  
  
  
  
  

    
}




shinyApp(ui, server)