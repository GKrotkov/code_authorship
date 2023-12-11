install.packages(c("tidyr","rattle","ggplot2","data.table","dplyr","magrittr","stringr",
                   "readxl","devtools","basictabler","openxlsx","readr","tidytext"))
library(devtools)
library(rattle)
library(tidyr)
library(data.table)
library(dplyr)
library(ggplot2)
library(magrittr)
library(stringr)
library(readxl)
library(basictabler)
library(openxlsx)
library(readr)
library(tidytext)
getwd()
setwd("/Users/zachstrennen/Downloads/October 2022 Major Minor Report/")
ds1 <- read.csv("First Major-Table 1.csv",skip = 1)
ds2 <- read.csv("Second Major-Table 1.csv",skip = 1)
ds3 <- read.csv("Third Major-Table 1.csv",skip = 1)
ds4 <- read.csv("Second Major Other Schools-Table 1.csv",skip = 3)
ds5 <- read.csv("First Minor-Table 1.csv",skip = 1)
ds6 <- read.csv("Second Minor-Table 1.csv",skip = 1)
ds7 <- read.csv("Third Minor-Table 1.csv",skip = 1)
ds8 <- read.csv("Minors Other Schools-Table 1.csv",skip = 3)

common <- intersect(colnames(ds1), colnames(ds2))
ds <- rbind(ds1[common],ds2[common])
common <- intersect(colnames(ds), colnames(ds3))
ds <- rbind(ds[common],ds3[common])
common <- intersect(colnames(ds1), colnames(ds2))
ds <- rbind(ds1[common],ds2[common])
common <- intersect(colnames(ds), colnames(ds3))
ds <- rbind(ds[common],ds3[common])
common <- intersect(colnames(ds), colnames(ds4))
ds <- rbind(ds[common],ds4[common])
common <- intersect(colnames(ds), colnames(ds5))
ds <- rbind(ds[common],ds5[common])
common <- intersect(colnames(ds), colnames(ds6))
ds <- rbind(ds[common],ds6[common])
common <- intersect(colnames(ds), colnames(ds7))
ds <- rbind(ds[common],ds7[common])
common <- intersect(colnames(ds), colnames(ds8))
ds <- rbind(ds[common],ds8[common])
glimpse(ds)

ds<- filter(ds, FIRST.MAJOR == c("History","Classical Civilization","Art History","Medieval & Renaissance Studies") |
         SECOND.MAJOR == c("History","Classical Civilization","Art History","Medieval & Renaissance Studies") |
         FIRST.MINOR == c("History","Classical Civilization","Art History","Medieval & Renaissance Studies") |
         SECOND.MINOR == c("History","Classical Civilization","Art History","Medieval & Renaissance Studies"))

ds<-filter(ds, NAME!="")



ds <- transmute(ds, NAME, ID,EMAIL.ADDRESS,CLASS,FIRST.MAJOR,SECOND.MAJOR,FIRST.MINOR,SECOND.MINOR,PRIMARY.ADVISOR)

write.csv(ds,"/Users/zachstrennen/Downloads/students1.csv", row.names = FALSE)

