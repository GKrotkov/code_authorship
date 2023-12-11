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

setwd("/Users/zachstrennen/Downloads/")
ds <- read.csv("CensusData_forClass.csv")
ds

means <- ds %>% group_by(ACADEMIC_PERIOD) %>% 
  summarise(mean=mean(GPA,na.rm = TRUE),
            .groups = 'drop')

ggplot(means, aes(x=ACADEMIC_PERIOD, y=mean)) +
  geom_point() +
  geom_smooth(method = "loess", alpha = .15,fullrange=TRUE,color="black")


means <- ds %>% group_by(ACADEMIC_YEAR) %>% 
  summarise(mean=mean(GPA,na.rm = TRUE),
            .groups = 'drop')

ggplot(means, aes(x=ACADEMIC_YEAR, y=mean)) +
  geom_point() +
  geom_smooth(method = "loess", alpha = .15,fullrange=TRUE,color="black")

means <- ds %>% group_by(School_Desc) %>% 
  summarise(mean=mean(GPA,na.rm = TRUE),
            .groups = 'drop')

ggplot(means, aes(x=School_Desc, y=mean)) +
  geom_point() +
  geom_smooth(method = "loess", alpha = .15,fullrange=TRUE,color="black") +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0)) 

means <- ds %>% group_by(Term) %>% 
  summarise(mean=mean(GPA,na.rm = TRUE),
            .groups = 'drop')

ggplot(means, aes(x=Term, y=mean)) +
  geom_point() +
  geom_smooth(method = "loess", alpha = .15,fullrange=TRUE,color="black") +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0)) 

means <- ds %>% group_by(ACADEMIC_PERIOD) %>% 
  summarise(mean=mean(GPA,na.rm = TRUE),
            .groups = 'drop')

ggplot(means, aes(x=ACADEMIC_PERIOD, y=mean)) +
  geom_point() +
  geom_smooth(method = "loess", alpha = .15,fullrange=TRUE,color="black")

nrow(ds)

#retention rate proportions for each year and school
#full-time part-time
#athletic non-athletic
#difference between college and school is spiritan section
#by major facet for each year retention proportion

means <- ds %>% group_by(ACADEMIC_PERIOD) %>% 
  summarise(mean=mean(GPA,na.rm = TRUE),
            .groups = 'drop')
means

counts <- ds %>% 
  count(ACADEMIC_YEAR, CollegeRetainedAY)

counts
counts <- counts %>% group_by(ACADEMIC_YEAR) %>% mutate(year_sum = sum(n))

counts <- counts %>% mutate(proportion_stayed = n/year_sum)

counts %<>% filter(ACADEMIC_YEAR != 2023)

ggplot(counts, aes(x=CollegeRetainedAY, y=proportion_stayed)) +
  geom_point() +
  geom_smooth(method = "loess", alpha = .15,fullrange=TRUE,color="black") +
  facet_wrap(~ ACADEMIC_YEAR) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0,size=5)) 


#isolate this by country
left <- filter(counts,CollegeRetainedAY=="")
left

ggplot(left, aes(x=ACADEMIC_YEAR, y=proportion_stayed)) +
  geom_point() +
  geom_smooth(method = "loess", alpha = .15,fullrange=TRUE,color="black") +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0,size=5)) +
  labs(x="Academic Year",y="Proportion Left")

#Look at eac individual school. Isolate most normal ones.

#Separate grad from undergrad

#undergraduate retention for all

#withdrawal year

ds <- read.csv("CensusData_forClass_sheet1.csv")
ds

ds %<>% filter(IRP_LEVEL == "UG")
ds %<>% filter(IRP_Class == "FR")
counts <- ds %>% 
  count(ACADEMIC_YEAR, School, CollegeRetainedAY)
counts

counts <- counts %>% group_by(ACADEMIC_YEAR) %>% mutate(year_sum = sum(n))
counts
c ounts <- counts %>% mutate(proportion_stayed = n/year_sum)

counts %<>% filter(ACADEMIC_YEAR != 2022)
counts %<>% filter(ACADEMIC_YEAR != 2023)
counts %<>% filter(CollegeRetainedAY != "")
counts %<>% filter(CollegeRetainedAY != "")
counts %<>% filter(CollegeRetainedAY != "00")
counts %<>% filter(CollegeRetainedAY != "0")

ggplot(counts, aes(x=CollegeRetainedAY, y=proportion_stayed)) +
  geom_point() +
  facet_wrap(~ ACADEMIC_YEAR) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0,size=5))

ggplot(counts, aes(x=CollegeRetainedAY, y=proportion_stayed)) +
  geom_bar(stat="identity")+
  facet_wrap(~ ACADEMIC_YEAR)+
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0,size=5))









