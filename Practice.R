rm(list=ls())

# install and load necessary packages install.packages("PackageName")
library(tidyverse)
#library(dplyr)
#library(tidyr)
library(xlsx)
library(ggplot2)

#choose the wd
setwd("C:/Users/Jacob/Documents/Harrow 2020/Supplimental Lighting Experiments/Supplemental-Lighting-Git")
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
Complete=tbl_df(Complete)



# Data Cleanup



# Filter out squished or MIA pairs 
dim(Complete)
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

# Summary stats for DaysAlive2020 (Mean, SD, SE, n)
Longevitysummary = Complete %>% summarise(Mean = mean(DaysAlive2020), SD = sd(DaysAlive2020), SE = sd(DaysAlive2020)/sqrt(length(DaysAlive2020)), n = length(DaysAlive2020))
GroupedComplete = Complete %>% group_by(TreatmentName, Sex)
GroupedLongevitysummary = GroupedComplete %>% summarise(Mean = mean(DaysAlive2020), SD = sd(DaysAlive2020), SE = sd(DaysAlive2020)/sqrt(length(DaysAlive2020)), n = length(DaysAlive2020))
GroupedLongevitysummary

#visualization of DaysAlive
hist(as.numeric(Complete$DaysAlive2020)) # all values in DaysAlive
ggplot(Complete, aes(x = DaysAlive2020, fill = Sex)) + # separated by Sex (from https://www.r-graph-gallery.com/histogram_several_group.html)
  geom_histogram()
ggplot(Complete, aes(x = DaysAlive2020, color = Sex, fill = Sex)) + # separated by Sex and Treatment (from https://www.r-graph-gallery.com/histogram_several_group.html)
  geom_histogram() +
  xlab("Days Alive") +
  ylab("Frequency") +
  facet_wrap(~factor(Complete$TreatmentName, levels = c("S", "W", "HPS", "HB", "HR", "LB", "LR")), ncol = 3, scales = "fixed") 
    # I dont know why ~ is important but it is
    # order the facets by using the factor function on TreatmentName, which identifies that vector as a factor and then specifies the order of the levels using levels =, from https://stackoverflow.com/questions/15116081/controlling-order-of-facet-grid-facet-wrap-in-ggplot2 
    # also see https://www.rdocumentation.org/packages/ggplot2/versions/1.0.0/topics/facet_wrap
    # could separate the sexes as well by adding facet_wrap(~Complete$TreatmentName + Sex)
  # Could've tried to create multiple plots using a loop but doesnt work because Treatment is a column in long form, not multiple columns in wide form, though couldve tried (https://stackoverflow.com/questions/9315611/grid-of-multiple-ggplot2-plots-which-have-been-made-in-a-for-loop using gridExtra package: https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.htmltry) 
    # plots = list()
    # uniquetreatments = unique(Complete$TreatmentName)
    # for (i in uniquetreatments) {
    #  plots[[i]] = 
    #  ggplot(Complete, aes(x = filter(DaysAlive2020 == i), color = Sex, fill = Sex)) +
    #    geom_histogram() +
    #    ylab("Frequency")
    #}
    #library(gridExtra)
    #grid.arrange(plots, ncol = 3)
boxplot(Complete$DaysAlive2020~Complete$TreatmentName)
boxplot(Complete$DaysAlive2020~Complete$TreatmentName+Complete$Sex)
ggplot(Complete, aes(y = DaysAlive2020, x = TreatmentName, color = Sex, fill = Sex)) +
  geom_boxplot() +
  xlab("Treatment")

# ANOVA
LongevityANOVA=aov(Complete$DaysAlive2020~Complete$TreatmentName + Complete$Sex + Complete$TreatmentName:Complete$Sex)
summary(LongevityANOVA)

# Tukey HSD post hoc tests
  #summary(LongevityANOVA)
  #LongevityTukey=TukeyHSD(LongevityANOVA)
  #LongevityTukey
  #plot(LongevityTukey)
#or using agricolae package (from https://rpubs.com/aaronsc32/post-hoc-analysis-tukey)
#install.packages("agricolae")
library("agricolae")
LongevityTukeyTreatment = HSD.test(LongevityANOVA, trt = 'Complete$TreatmentName', group = TRUE)
LongevityTukeyTreatment

# Means plot to visualize interaction effect
Longevitymeansplot = 
  ggplot(data= GroupedLongevitysummary, aes(y = Mean, x = factor(TreatmentName, levels = c("S", "W", "HPS", "HB", "HR", "LB", "LR")), group = Sex, col = Sex, fill = Sex)) + # (from http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization)
  geom_line() + # to colour code by Sex, could add color = GroupedLongevitysummary$Sex within geom_line() 
  geom_point() +
  xlab("Treatment") +
  ylab("Mean Longevity (days)")
# geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE)) # to give error bars of +-SE
Longevitymeansplot
# Thus, there is an interaction effect in the Low intensity Treatments, where males live unusually longer than females

# Boxplot with significant groups using multcompView package instead of agricolae (from https://www.r-graph-gallery.com/84-tukey-test)
# Or extract the groups from agricolae tukey test myself, then use geom_text to put onto graph (modified from many links: https://intellipaat.com/community/16343/how-to-put-labels-over-geombar-for-each-bar-in-r-with-ggplot2
                                                                                                                        # https://stackoverflow.com/questions/6455088/how-to-put-labels-over-geom-bar-in-r-with-ggplot2
                                                                                                                        # https://stackoverflow.com/questions/48029549/labeling-individual-boxes-in-a-ggplot-boxplot
                                                                                                                        # https://www.researchgate.net/post/R_How_to_add_labels_for_significant_differences_on_boxplot_ggplot2
                                                                                                                        # https://stackoverflow.com/questions/23328582/add-multiple-labels-on-ggplot2-boxplot
Grouplabels = tbl_df(LongevityTukeyTreatment$groups["groups"])            # extract the significant groups from the tukey test and make it a tibble
Grouplabels$TreatmentName = row.names(LongevityTukeyTreatment$groups)     # create a column with the TreatmentNames from the row names
Max = LongevityTukeyTreatment$means["Max"]                                # extract the maximum value from the tukey test
Maxtbl = as_tibble(Max, rownames = "TreatmentName")                       # make the maximum values into a tibble with a column for TreatmentNames
Grouplabels = merge(Grouplabels, Maxtbl, by = "TreatmentName")            # merge the two tibbles by TreatmentName
Grouplabels$aboveMax = Grouplabels$Max + 10                               # create a new column for placement above the max value
Grouplabels
ggplot(Complete, aes(y = DaysAlive2020, x = TreatmentName)) +             # plot 
  geom_boxplot(aes(color = Sex, fill = Sex)) +                            # need to put Sex aes in the boxplot() so that the geom_text() isn't trying to label across Sex??
  xlab("Treatment") +
  geom_text(data = Grouplabels, aes(x = TreatmentName, y = aboveMax, label = groups)) # apply the labels from the tibble







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

