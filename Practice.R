rm(list=ls())

# install and load necessary packages install.packages("PackageName")
library(tidyverse)
#library(dplyr)
#library(tidyr)
library(xlsx)

#choose the wd
setwd(choose.dir())
getwd()

#get the data
Complete=read.csv("Jacob's Orius Data Sheet 25-May-2020 Complete2020.csv", header = TRUE, stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM") #added fileEncoding="UTF-8-BOM" to remove "i.." from first column header https://stackoverflow.com/questions/24568056/rs-read-csv-prepending-1st-column-name-with-junk-text/24568505
head(Complete)

#rename frist column header to TreatmentName using dplyr's rename()
# Complete %>% rename(TreatmentName = ?..TreatmentName)
  #using base functions
    #names(Complete)[names(Complete) == "ï..TreatmentName"] = "TreatmentName"
  #or using the location of the column
    #names(Complete)[1] = "TreatmentName"
    #colnames(Complete)

    
#make Complete into a tibble
tbl_df(Complete)


dim(Complete)
# Filter out squished or MIA pairs 
Complete=filter(Complete, 
                DaysMAlive2020 != "#VALUE!",
                DaysFAlive2020 != "#VALUE!"
                ) 
dim(Complete)
#make DaysAlive numeric
Complete$DaysMAlive2020=as.numeric(Complete$DaysMAlive2020)
Complete$DaysFAlive2020=as.numeric(Complete$DaysFAlive2020)
#then filter out '0 days alive'
Complete = filter(Complete, 
                  DaysMAlive2020 > 0, 
                  DaysFAlive2020 > 0
                  )
dim(Complete) 

# Preoviposition period for pairs that didnt oviposit are negative numbers (because the last opiposition is 0 so StartOfMatingPeriod - 1-Jan = big negative number), but they should equal 0, so fix:
hist(as.numeric(Complete$PreOvipositionPeriod))
Complete$PreOvipositionPeriod[Complete$PreOvipositionPeriod<1]=0
dim(Complete)
hist(as.numeric(Complete$PreOvipositionPeriod))
#Post-oviposition for same pairs are very large numbers so
hist(as.numeric(Complete$PostOvipositionPeriod))
Complete$PostOvipositionPeriod[Complete$PostOvipositionPeriod>1000]=0
dim(Complete)
hist(as.numeric(Complete$PostOvipositionPeriod))
#some pairs have eggs found after female died but thats impossible?? it is possible if the egg was laid, the female died and then later the bean was examined and replaced, though some are longer than that layover period

#change date header to col
      #dates are as a header but should be a data column, so use gather() to make into columns called Date and NumberOfEggs 
      #DateasCol=Complete %>% gather(Date, NumberOfEggs, X1:X135 )
      #head(DateasCol)
      #dim(DateasCol)
      #DateNumberasCol = DateasCol %>% separate(Date, c("Xs","DateNumber"), sep = "(?<=[A-Za-z])(?=[0-9])")       #"(?<=[A-Za-z])(?=[0-9])" from https://stackoverflow.com/questions/9756360/split-character-data-into-numbers-and-letters
      #dim(DateNumberasCol)
      #head(DateNumberasCol)
      #eggsovertimegraph = ggplot(DateNumberasCol, aes(x = DateNumber, y = NumberOfEggs, color = DateNumberasCol$UniqueName)) +
      #  geom_point() +
      #  geom_line()
      #eggsovertimegraph



# Longevity


#visualization
hist(as.numeric(Complete$DaysMAlive2020))
hist(as.numeric(Complete$DaysFAlive2020))


#need to combine DaysMAlive and DaysFAlive (or just use SexColumn datasheet) by...

#1. using SexColumn datasheet, apply same filters (no MIA or 0DaysAlive)
#  Complete=read.csv("Jacob's Orius Data Sheet 25-May-2020 Sex Column2020.csv", header = TRUE, stringsAsFactors=FALSE)
#  head(Complete)
#  #make Complete into a tibble
#  tbl_df(Complete)
#  # Filter out squished or MIA pairs 
#  dim(Complete)
#  Complete=filter(Complete, 
#                  DaysAlive2020 != "#VALUE!"
#                  ) 
#  dim(Complete)
#  #make DaysAlive numeric
#  Complete$DaysAlive2020=as.numeric(Complete$DaysAlive2020)
#  #then filter out 0s
#  Complete = filter(Complete, 
#                    DaysAlive2020 > 0 
#  )
#  dim(Complete)     
#
#  hist(as.numeric(Complete$DaysAlive2020))
#  boxplot(Complete$DaysAlive2020~Complete$TreatmentName)
#  #SEXCOLUMN IS REMOVING INDIVIDUALS THAT DIDN'T LIVE 1 DAY (n=517 after filtering), NOT PAIRS THAT HAD AN INDIVIDUAL THAT DIDN'T LIVE 1 DAY (n=498 after filter), SO NEED TO USE COMPLETE 

#2. using Complete by combining DaysMAlive2020 and DaysFAlive2020 using gather
dim(Complete)
Complete=Complete %>% gather(Sex, DaysAlive2020, c(DaysFAlive2020, DaysMAlive2020) )
dim(Complete)


#visualization of DaysAlive
hist(as.numeric(Complete$DaysAlive2020))
boxplot(Complete$DaysAlive2020~Complete$TreatmentName)


#perform ANOVA
LongevityANOVA=aov(Complete$DaysAlive2020~Complete$TreatmentName)

#perform Tukey HSD post hoc test
#summary(LongevityANOVA)
#LongevityTukey=TukeyHSD(LongevityANOVA)
#LongevityTukey
#plot(LongevityTukey)

  #or using agricolae package (from https://rpubs.com/aaronsc32/post-hoc-analysis-tukey)
  #install.packages("agricolae")
  library("agricolae")
  LongevityTukey2=HSD.test(LongevityANOVA, trt = 'Complete$TreatmentName', group = TRUE)
  LongevityTukey2


#graph results usign ggplot2
library("ggplot2")
ggplot(Complete, aes(x= as.factor(Complete$TreatmentName), y= Complete$DaysAlive2020)) + 
  geom_boxplot() +
  xlab("Treatment")



# Fecundity


#visualization
hist(as.numeric(Complete$TotalEggs))

ggplot(Complete, aes(x= as.factor(Complete$TreatmentName), y= Complete$TotalEggs)) + 
  geom_boxplot() +
  xlab("Treatment")
        
regressioneggsdays=lm(Complete$TotalEggs~Complete$DaysFAlive2020)
summary(regressioneggsdays)
ggplot(Complete, aes(x= DaysFAlive2020, y= TotalEggs)) + 
  geom_point() +
  geom_smooth(method=lm)
#There is a correlation between TotalEggs and DaysFAlive2020, so control for days alive by expressing fecundity as a rate?
  #Problem: there are some pairs which lived a long time, but produced very few eggs, possibly because the male died early, or more likely because the beans were reused when there were no eggs found but if an egg was missed and then found later it could result in this error

#Fecundity as rate 
Complete$Eggsperfemaleperday = Complete$TotalEggs/Complete$DaysFAlive2020
hist(Complete$Eggsperfemaleperday)
ggplot(Complete, aes(x= as.factor(TreatmentName), y= Eggsperfemaleperday)) + 
  geom_boxplot() +
  xlab("Treatment")

#perform ANOVA and Tukey HSD on Eggs/Female/Day
FecundityANOVA=aov(Complete$Eggsperfemaleperday~Complete$TreatmentName)
FecundityTukeyHSD=HSD.test(FecundityANOVA, trt = 'Complete$TreatmentName', group = TRUE)
FecundityTukeyHSD



# Pre/Oviposition Period


#visualization
#Preoviposition period
hist(as.numeric(Complete$PreOvipositionPeriod))

ggplot(Complete, aes(x= as.factor(TreatmentName), y= PreOvipositionPeriod)) + 
  geom_boxplot() +
  xlab("Treatment")

#perform ANOVA and Tukey HSD on Preoviposition Period
PreoviANOVA=aov(Complete$PreOvipositionPeriod~Complete$TreatmentName)
PreoviTukeyHSD=HSD.test(PreoviANOVA, trt = 'Complete$TreatmentName', group = TRUE)
PreoviTukeyHSD


#Oviposition period
hist(as.numeric(Complete$OvipositionPeriod))

ggplot(Complete, aes(x= as.factor(TreatmentName), y= OvipositionPeriod)) + 
  geom_boxplot() +
  xlab("Treatment")

#perform ANOVA and Tukey HSD on Oviposition Period
OviANOVA=aov(Complete$OvipositionPeriod~Complete$TreatmentName)
OviTukeyHSD=HSD.test(OviANOVA, trt = 'Complete$TreatmentName', group = TRUE)
OviTukeyHSD

#this is for my test branch