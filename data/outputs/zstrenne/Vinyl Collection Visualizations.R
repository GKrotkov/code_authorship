install.packages(c("tidyr","rattle","ggplot2","data.table","dplyr","magrittr","stringr",
                   "readxl","devtools","basictabler","openxlsx","readr","tidytext",
                   "scales"))
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
library(scales)
getwd()
setwd("/Users/zachstrennen/Downloads/")
ds <- read.csv("Music - Albums-8.csv")

glimpse(ds)

# Histogram of album duration in minutes
ggplot(ds, aes(x=Duration)) +
  geom_histogram(bins=28,aes(y=..density..), colour="black", fill="white") +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0),legend.position="none") +
  geom_vline(aes(xintercept=mean(Duration)),color="blue", linetype="dashed", size=1) +
  geom_density(alpha=.2, fill="#FF6666",outline.type="full") +
  labs(x="Duration (minutes)",
       y="Density")

# Histogram of duration split by type of vinyl
ggplot(ds, aes(x=Duration)) +
  geom_histogram(bins=40,aes(y=..density..), colour="black", fill="white") +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0),legend.position="none") +
  geom_vline(aes(xintercept=mean(Duration)),color="blue", linetype="dashed", size=1) +
  facet_wrap(~ Format) +
  geom_density(alpha=.2, fill="#FF6666",outline.type="full") +
  labs(x="Duration (minutes)",
       y="Density")

# Histogram of years it took to produce vinyl after albums release date
ggplot(ds, aes(x=Vinyl.Release.Year-Album.Release.Year)) +
  geom_histogram(bins=50,aes(y=..density..), colour="black", fill="white") +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0),legend.position="none") +
  geom_vline(aes(xintercept=mean(Vinyl.Release.Year-Album.Release.Year)),color="blue", linetype="dashed", size=1) +
  geom_density(alpha=.2, fill="#FF6666",outline.type="full") +
  labs(x="Years to Produce",
       y="Density")


# Highest frequency of artists
table1 <- table(ds$Artist)
simple <- as.data.frame(table1)
simple <- filter(simple,rank(desc(Freq))<=16)
ggplot(simple, aes(x=reorder(Var1,Freq),y=Freq)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(x="Artist",
       y="Frequency")

# Densities of album releases and vinyl releases
ds1 <- transmute(ds,year = Album.Release.Year,Release='Album Release')
ds2 <- transmute(ds,year = Vinyl.Release.Year,Release='Vinyl Release')
ds3 <- rbind(ds1,ds2)
ggplot(ds3, aes(x=year,fill=Release)) +
  geom_density(alpha=.2,outline.type="full") +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0)) +
  labs(x="Year",
       y="Density")

# Plot of vinyl release year vs. album release year
ggplot(ds, aes(x=Album.Release.Year, y=Vinyl.Release.Year)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0),legend.position="none") +
  geom_smooth(method = "loess", alpha = .15,fullrange=TRUE) +
  labs(x="Album Release Year",
       y="Vinyl Release Year")

# Density of album durations split by format
ggplot(ds, aes(x=Duration,fill=Format)) +
  geom_density(alpha=.2,outline.type="full") +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0)) +
  labs(x="Duration (minutes)",
       y="Density")

#Density of album release year
ggplot(ds, aes(x=Album.Release.Year,fill='red')) +
  geom_density(alpha=.2,outline.type="full") +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0)) +
  labs(x="Album Release Year",
       y="Density")

# Density of album release year by format
ggplot(ds, aes(x=Album.Release.Year,fill=Format)) +
  geom_density(alpha=.2,outline.type="full") +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0)) +
  labs(x="Album Release Year",
       y="Density")

#density of album release year by format
ggplot(ds, aes(x=Vinyl.Release.Year,fill=Format)) +
  geom_density(alpha=.2,outline.type="full") +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0)) +
  labs(x="Vinyl Release Year",
       y="Density")

# Histogram of track counts
ggplot(ds, aes(x=Track.Count)) +
  geom_histogram(bins=28,aes(y=..density..), colour="black", fill="white") +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0),legend.position="none") +
  geom_vline(aes(xintercept=mean(Track.Count)),color="blue", linetype="dashed", size=1) +
  geom_density(alpha=.2, fill="#FF6666",outline.type="full") +
  labs(x="Track Count",
       y="Density")

#Density of track counts by format
ggplot(ds, aes(x=Track.Count,fill=Format)) +
  geom_density(alpha=.2,outline.type="full") +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0)) +
  labs(x="Track Count",
       y="Density")

# Plot of track count vs. duration colored by format
ggplot(ds, aes(x=Duration, y=Track.Count,color=Format)) +
  geom_point() +
  geom_smooth(method = "loess", alpha = .15,fullrange=TRUE,color="black") +
  labs(x="Duration (minutes)",
       y="Track Count")

# Frequencies of certain features
table(ds$Format)
table(ds$Vinyl.Release.Year)
table(ds$Album.Release.Year)