install.packages(c("tidyr","rattle","ggplot2","data.table","dplyr","magrittr","stringr",
"readxl","devtools","basictabler","openxlsx","readr","tidytext","lmerTest"))
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
library(lmerTest)

#Load in data
setwd("/Users/zachstrennen/Downloads/")
ds <- read.csv("CensusData_forClass_sheet1.csv")

#Clean P1 type of labeling
ds$IRP_Class = replace(ds$IRP_Class,ds$IRP_Class == "P1","FR")
ds$IRP_Class = replace(ds$IRP_Class,ds$IRP_Class == "P3","JR")
ds$IRP_Class = replace(ds$IRP_Class,ds$IRP_Class == "PPY1","FR")
ds$IRP_Class = replace(ds$IRP_Class,ds$IRP_Class == "PPY2","SO")
ds$IRP_Class = replace(ds$IRP_Class,ds$IRP_Class == "P2","SO")
ds$IRP_Class = replace(ds$IRP_Class,ds$IRP_Class == "P4","SR")

#Exclude dataset to undergrads fall and spring
ds %<>% filter(IRP_LEVEL == "UG")
ds %<>% filter(IRP_Class == "FR" | IRP_Class == "SO" | IRP_Class == "JR" | IRP_Class == "SR")
ds %<>% filter(Semester != "Summer")
ds %<>% filter(Semester == "Fall" | Semester == "Spring")

#Clean living hall data
ds$Building_Desc = replace(ds$Building_Desc,ds$Building_Desc == "ASSUMP","Assumption Hall")
ds$Building_Desc = replace(ds$Building_Desc,ds$Building_Desc == "BROTTI","Brottier Hall")
ds$Building_Desc = replace(ds$Building_Desc,ds$Building_Desc == "TOWERA","Duquesne Towers A")
ds$Building_Desc = replace(ds$Building_Desc,ds$Building_Desc == "TOWERB","Duquesne Towers B")
ds$Building_Desc = replace(ds$Building_Desc,ds$Building_Desc == "TOWERC","Duquesne Towers C")
ds$Building_Desc = replace(ds$Building_Desc,ds$Building_Desc == "STANNE","Saint Ann Hall East")
ds$Building_Desc = replace(ds$Building_Desc,ds$Building_Desc == "STANNW","Saint Ann Hall West")
ds$Building_Desc = replace(ds$Building_Desc,ds$Building_Desc == "STMART","Saint Martin Hall")
ds$Building_Desc = replace(ds$Building_Desc,ds$Building_Desc == "VICKRY","Vickroy Hall")
ds$Building_Desc = replace(ds$Building_Desc,ds$Building_Desc == NA,"OTHER")
ds$Building_Desc = replace(ds$Building_Desc,ds$Building_Desc == "","OTHER")


ds$STATE_PROVINCE = replace(ds$STATE_PROVINCE,ds$STATE_PROVINCE == "","OTHER")
ds$CITIZENSHIP_DESC = replace(ds$CITIZENSHIP_DESC,ds$CITIZENSHIP_DESC == "","OTHER")

ds %<>% filter(is.na(GPA) == FALSE)
ds %<>% filter(GPA != "")

ds %<>% mutate(Retained = case_when(CollegeRetainedAP=="" ~ FALSE,CollegeRetainedAP!="" ~ TRUE))

ds %<>% filter(ACADEMIC_YEAR != 2022)
ds %<>% filter(ACADEMIC_YEAR != 2023)
ds %<>% filter(ACADEMIC_YEAR != 2008)
ds %<>% filter(ACADEMIC_YEAR != 2009)
ds %<>% filter(ACADEMIC_YEAR != 2010)

ds %<>% filter(School != "")
ds %<>% filter(School != "")
ds %<>% filter(School != "00")
ds %<>% filter(School != "0")
ds %<>% filter(School != "UP")
ds %<>% filter(School != "SPIR")


clean <- ds



#TESTING FOR EDUCATION RES LIFE


ds <- clean
ds <- filter(ds, School == "ED")
ds %<>% filter(IRP_Class == "FR")


ds2 <- filter(ds,Building_Desc != "Saint Ann Hall East")
ds2 <- filter(ds2,Building_Desc != "Saint Ann Hall West")
ds2 <- filter(ds2,Building_Desc != "Saint Martin Hall")

counts <- ds %>% 
  count(ACADEMIC_YEAR, School, Retained, Semester)
#Create a column of sums regardless of retention
counts <- counts %>% group_by(ACADEMIC_YEAR, School, Semester) %>% mutate(year_sum = sum(n))
#Calculate proportion retained
counts <- counts %>% mutate(proportion_stayed = n/year_sum)
Retained1 <- filter(counts, Retained == TRUE)
Retained1 %<>% mutate(group = "Control")


counts <- ds2 %>% 
  count(ACADEMIC_YEAR, School, Retained, Semester)
#Create a column of sums regardless of retention
counts <- counts %>% group_by(ACADEMIC_YEAR, School, Semester) %>% mutate(year_sum = sum(n))
#Calculate proportion retained
counts <- counts %>% mutate(proportion_stayed = n/year_sum)
Retained2 <- filter(counts, Retained == TRUE)
Retained2 %<>% mutate(group = "Test")

Retained <- rbind(Retained1, Retained2)

Retained
filter(Retained, Semester == "Spring") %>%
  ggplot(aes(x=ACADEMIC_YEAR, y=proportion_stayed,color=group)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0,size=5))


Retained <- filter(Retained, Semester == "Spring")
t.test(proportion_stayed ~ group, data =Retained)









#LIBERAL ARTS - saint annes st martin



ds <- clean


ds <- clean
ds <- filter(ds, School == "AR")
ds %<>% filter(IRP_Class == "FR")

ds
ds2 <- filter(ds,Building_Desc != "Saint Ann Hall East")
ds2 <- filter(ds2,Building_Desc != "Saint Ann Hall West")
ds2 <- filter(ds2,Building_Desc != "Saint Martin Hall")

ds <- filter(ds,Building_Desc == "Saint Ann Hall East" | Building_Desc == "Saint Ann Hall West" | "Saint Martin Hall")


counts <- ds %>% 
  count(ACADEMIC_YEAR, School, Retained, Semester)
#Create a column of sums regardless of retention
counts <- counts %>% group_by(ACADEMIC_YEAR, School, Semester) %>% mutate(year_sum = sum(n))
#Calculate proportion retained
counts <- counts %>% mutate(proportion_stayed = n/year_sum)
Retained1 <- filter(counts, Retained == TRUE)
Retained1 %<>% mutate(group = "Saints")
Retained1

counts <- ds2 %>% 
  count(ACADEMIC_YEAR, School, Retained, Semester)
#Create a column of sums regardless of retention
counts <- counts %>% group_by(ACADEMIC_YEAR, School, Semester) %>% mutate(year_sum = sum(n))
#Calculate proportion retained
counts <- counts %>% mutate(proportion_stayed = n/year_sum)
Retained2 <- filter(counts, Retained == TRUE)
Retained2 %<>% mutate(group = "Others")

Retained <- rbind(Retained1, Retained2)

Retained
filter(Retained, Semester == "Spring") %>%
  ggplot(aes(x=ACADEMIC_YEAR, y=proportion_stayed,color=group)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0,size=5))


#Retained <- filter(Retained, Semester == "Spring")
Retained
t.test(proportion_stayed ~ group, data =Retained)









#LIBERAL ARTS - commuters



ds <- clean


ds <- clean
ds <- filter(ds, School == "AR")
#ds %<>% filter(IRP_Class == "FR")

ds
ds2 <- filter(ds,Building_Desc != "OTHER")

ds <- filter(ds,Building_Desc == "OTHER")


counts <- ds %>% 
  count(ACADEMIC_YEAR, School, Retained, Semester)
#Create a column of sums regardless of retention
counts <- counts %>% group_by(ACADEMIC_YEAR, School, Semester) %>% mutate(year_sum = sum(n))
#Calculate proportion retained
counts <- counts %>% mutate(proportion_stayed = n/year_sum)
Retained1 <- filter(counts, Retained == TRUE)
Retained1 %<>% mutate(group = "Commuters")
Retained1

counts <- ds2 %>% 
  count(ACADEMIC_YEAR, School, Retained, Semester)
#Create a column of sums regardless of retention
counts <- counts %>% group_by(ACADEMIC_YEAR, School, Semester) %>% mutate(year_sum = sum(n))
#Calculate proportion retained
counts <- counts %>% mutate(proportion_stayed = n/year_sum)
Retained2 <- filter(counts, Retained == TRUE)
Retained2 %<>% mutate(group = "Others")

Retained <- rbind(Retained1, Retained2)



Retained$group <- factor(Retained$group, levels = c("Others", "Commuters"))
filter(Retained, Semester == "Spring") %>%
  ggplot(aes(x=ACADEMIC_YEAR, y=proportion_stayed,color=group)) +
  geom_point() +
  geom_smooth(aes(col=group),method="lm",se=FALSE) +
  theme(axis.text.x = element_text(angle = 0, vjust = .5, hjust=.5,size=10),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) +
  scale_color_discrete(labels = c("Living On Campus","Commuter")) +
  scale_y_continuous(label=function(x) {return(paste(x*100,"%",sep=""))}) +
  labs(x="Year",
       y="Average Retention Rate",
       title="Average Retention Rate at the End of the Academic\nYear for Students in the School of Liberal Arts",
       subtitle = "Commuters vs. Students Living on Campus",
       color = "Residency Status")


0.8403908794788274 + 0.8501628664495114 + 0.8306188925081434 + 0.8306188925081434 + 0.8436482084690554 + 0.85
ggplot(Retained,aes(x=ACADEMIC_YEAR, y=proportion_stayed,color=group)) +
geom_point() +
geom_smooth(aes(col=group),method="lm",se=FALSE)+
theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0,size=5))

#Retained <- filter(Retained, Semester == "Fall")
Retained
t.test(proportion_stayed ~ group, data =Retained)







#BUSINESS - vickroy


ds <- clean


ds <- clean
ds <- filter(ds, School == "BU")
ds %<>% filter(IRP_Class != "FR")

ds
ds2 <- filter(ds,Building_Desc != "Vickroy Hall")

ds <- filter(ds,Building_Desc == "Vickroy Hall")


counts <- ds %>% 
  count(ACADEMIC_YEAR, School, Retained, Semester)
#Create a column of sums regardless of retention
counts <- counts %>% group_by(ACADEMIC_YEAR, School, Semester) %>% mutate(year_sum = sum(n))
#Calculate proportion retained
counts <- counts %>% mutate(proportion_stayed = n/year_sum)
Retained1 <- filter(counts, Retained == TRUE)
Retained1 %<>% mutate(group = "Vickroy")
Retained1

counts <- ds2 %>% 
  count(ACADEMIC_YEAR, School, Retained, Semester)
#Create a column of sums regardless of retention
counts <- counts %>% group_by(ACADEMIC_YEAR, School, Semester) %>% mutate(year_sum = sum(n))
#Calculate proportion retained
counts <- counts %>% mutate(proportion_stayed = n/year_sum)
Retained2 <- filter(counts, Retained == TRUE)
Retained2 %<>% mutate(group = "Others")

Retained <- rbind(Retained1, Retained2)

Retained
filter(Retained, Semester == "Fall") %>%
  ggplot(aes(x=ACADEMIC_YEAR, y=proportion_stayed,color=group)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0,size=5))


#Retained <- filter(Retained, Semester == "Fall")
Retained
t.test(proportion_stayed ~ group, data =Retained)








#Health Science - st annes


ds <- clean


ds <- clean
ds <- filter(ds, School == "HS")
ds %<>% filter(IRP_Class != "FR")

ds
ds2 <- filter(ds,Building_Desc != "Saint Ann Hall East")
ds2 <- filter(ds2,Building_Desc != "Saint Ann Hall West")

ds <- filter(ds,Building_Desc == "Saint Ann Hall East" | Building_Desc == "Saint Ann Hall West")


counts <- ds %>% 
  count(ACADEMIC_YEAR, School, Retained, Semester)
#Create a column of sums regardless of retention
counts <- counts %>% group_by(ACADEMIC_YEAR, School, Semester) %>% mutate(year_sum = sum(n))
#Calculate proportion retained
counts <- counts %>% mutate(proportion_stayed = n/year_sum)
Retained1 <- filter(counts, Retained == TRUE)
Retained1 %<>% mutate(group = "st anne")
Retained1

counts <- ds2 %>% 
  count(ACADEMIC_YEAR, School, Retained, Semester)
#Create a column of sums regardless of retention
counts <- counts %>% group_by(ACADEMIC_YEAR, School, Semester) %>% mutate(year_sum = sum(n))
#Calculate proportion retained
counts <- counts %>% mutate(proportion_stayed = n/year_sum)
Retained2 <- filter(counts, Retained == TRUE)
Retained2 %<>% mutate(group = "Others")

Retained <- rbind(Retained1, Retained2)

Retained
filter(Retained, Semester == "Fall") %>%
  ggplot(aes(x=ACADEMIC_YEAR, y=proportion_stayed,color=group)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0,size=5))


#Retained <- filter(Retained, Semester == "Fall")
Retained
t.test(proportion_stayed ~ group, data =filter(Retained, Semester == "Spring"))









#music - commuters



ds <- clean


ds <- clean
ds <- filter(ds, School == "MU")
#ds %<>% filter(IRP_Class == "FR")

ds
ds2 <- filter(ds,Building_Desc != "OTHER")

ds <- filter(ds,Building_Desc == "OTHER")


counts <- ds %>% 
  count(ACADEMIC_YEAR, School, Retained, Semester)
#Create a column of sums regardless of retention
counts <- counts %>% group_by(ACADEMIC_YEAR, School, Semester) %>% mutate(year_sum = sum(n))
#Calculate proportion retained
counts <- counts %>% mutate(proportion_stayed = n/year_sum)
Retained1 <- filter(counts, Retained == TRUE)
Retained1 %<>% mutate(group = "Commuters")
Retained1

counts <- ds2 %>% 
  count(ACADEMIC_YEAR, School, Retained, Semester)
#Create a column of sums regardless of retention
counts <- counts %>% group_by(ACADEMIC_YEAR, School, Semester) %>% mutate(year_sum = sum(n))
#Calculate proportion retained
counts <- counts %>% mutate(proportion_stayed = n/year_sum)
Retained2 <- filter(counts, Retained == TRUE)
Retained2 %<>% mutate(group = "Others")

Retained <- rbind(Retained1, Retained2)

Retained
filter(Retained, Semester == "Fall") %>%
  ggplot(aes(x=ACADEMIC_YEAR, y=proportion_stayed,color=group)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0,size=5))


#Retained <- filter(Retained, Semester == "Fall")
Retained
t.test(proportion_stayed ~ group, data =Retained)

r1 <- filter(Retained, group == "Commuters")
r2 <- filter(Retained, group == "Others")

chisq.test(Retained$group, Retained$proportion_stayed)






0.9022801302931596 +0.8697068403908795 +0.8664495114006515 + 0.8697068403908795 + 0.8599348534201955 + 0.8762214983713354 + 0.8631921824104235





