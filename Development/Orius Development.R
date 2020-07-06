# Prep --------------------------------------------------------------------

rm(list=ls())

# install and load necessary packages install.packages("PackageName")
library(tidyverse)
#library(dplyr)
#library(tidyr)
library("agricolae")
library("ggplot2")
library("gplots")


# Choose the wd
setwd(choose.dir())
#setwd("./Openrefined data") # "." refers to the file that this script is already in
getwd()


# Get Openrefined Data ----------------------------------------------------

# Development Rate
Rate = tbl_df(read.csv("Development Rate Openrefined.csv", header = TRUE))
head(Rate)
summary(Rate)
# Development Rate with Nymphal Instars in 'long' format
Longrate = tbl_df(read.csv("Development Rate Openrefined Long.csv", header = TRUE))

# % Mortality
Mort = tbl_df(read.csv("Development Mortality Openrefined.csv", header = TRUE))
head(Mort)
summary(Mort)

# Tibial Length
Tibia = tbl_df(read.csv("Development Tibial Length Openrefined.csv", header = TRUE))
head(Tibia)
summary(Tibia)


# Development Rate Analysis -----------------------------------------------

# measures of spread
summary(Rate)

# Histograms
hist(Rate$Total.Development..days.)

# Summary stats (Mean, SD, SE, n)
# just Total Development (days)
Rate %>% summarise(Mean = mean(Total.Development..days.), SD = sd(Total.Development..days.), SE = sd(Total.Development..days.)/sqrt(length(Total.Development..days.)), n = length(Total.Development..days.))
# then grouped by Block and Treatment
Groupedrate = Rate %>% group_by(Block, Treatment)
Groupedsummary = Groupedrate %>% summarise(Mean = mean(Total.Development..days.), SD = sd(Total.Development..days.), SE = sd(Total.Development..days.)/sqrt(length(Total.Development..days.)), n = length(Total.Development..days.))
Groupedsummary
#then for All Nymphal Instars NOT WORKING (trying from https://dplyr.tidyverse.org/reference/group_by.html and https://community.rstudio.com/t/summarise-multiple-columns-using-multiple-functions-in-a-tidy-way/8645)
Groupedrate %>% summarise(across(c("N1d":"Total.Development..days.")), .funs = (Mean = mean(), SD = sd(), SE = sd()/sqrt()/length(), n = length()))

# Boxplot
boxplot(Rate$Total.Development..days. ~ Rate$Treatment, xlab = "Treatment", ylab = "Total Development (days)")
# or using ggplot
ggplot(Rate, aes(x= Treatment, y= Total.Development..days.)) + 
  geom_boxplot() +
  xlab("Treatment") +
  ylab("Total Development (days)")

# ? make Nymphal Instar Development Time into long format?

# ANOVA
TotaldevANOVA= aov(Rate$Total.Development..days. ~ Rate$Treatment)
summary(TotaldevANOVA)

# Tukey HSD
TotaldevTukey = HSD.test(TotaldevANOVA, trt = 'Rate$Treatment', group = TRUE)
TotaldevTukey

# ? is there a difference between Blocks? ie. use 2-way anova with Block and Treatment including interaction effect (from http://www.sthda.com/english/wiki/two-way-anova-test-in-r)
Totaldev2wayANOVA = aov(Rate$Total.Development..days. ~ Rate$Block + Rate$Treatment + Rate$Block:Rate$Treatment)
summary(Totaldev2wayANOVA)
TotaldevTukeyBlock = HSD.test(Totaldev2wayANOVA, trt = 'Rate$Block', group = TRUE)
TotaldevTukeyBlock
TotaldevTukeyTreatment = HSD.test(Totaldev2wayANOVA, trt = 'Rate$Treatment', group = TRUE)
TotaldevTukeyTreatment
boxplot(Rate$Total.Development..days. ~ Rate$Block + Rate$Treatment, xlab = "Treatment", ylab = "Total Development (days)")
# make a means plot to illustrate any interaction effect
level_order <- c('W', 'S', 'HPS', 'HB', 'HR', 'LB', 'LR')
meansplot = 
  ggplot(data = Groupedsummary, aes(y = Mean, x = factor(Treatment, levels = level_order), group = Block, col = Block)) + # (from http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization)
  geom_line() + # to colour code by Block, could add color = Groupedsummary$Block within geom_line() 
  geom_point() +
  ylab("Mean Total Development (days)")
  # geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE)) # to give error bars of +-SE
meansplot
# Yes there is a sig diff, but some Treatments don't having samples in both Blocks (W, LR)

# Boxplot with significant groups (as in Longevity)
Grouplabels = tbl_df(TotaldevTukeyTreatment$groups["groups"])            # extract the significant groups from the tukey test and make it a tibble for easier manipulation (as a dataframe was causing problems)
Grouplabels$TreatmentName = row.names(TotaldevTukeyTreatment$groups)     # create a column with the TreatmentNames from the row names
Max = TotaldevTukeyTreatment$means["Max"]                                # extract the maximum value from the tukey test
Maxtbl = as_tibble(Max, rownames = "TreatmentName")                       # make the maximum values into a tibble with a column for TreatmentNames (rather than just inserting the Max dataframe into Grouplabels tibble because then its still a dataframe str within the tibble which caused problems)
Grouplabels = merge(Grouplabels, Maxtbl, by = "TreatmentName")            # merge the two tibbles by TreatmentName
Grouplabels$aboveMax = Grouplabels$Max + 1                               # create a new column for placement above the max value
Grouplabels
absMax = max(Grouplabels$Max)
Treatmentlevelsorder = c("S", "W", "HPS", "HB", "HR", "LB", "LR")
ggplot(Rate, aes(y = Total.Development..days., x = factor(Treatment, levels = Treatmentlevelsorder))) + # plot, with Treatments ordered
  geom_boxplot(aes(color = factor(Block))) + 
  xlab("Treatment") +
  ylab("Total Development (days)") +
  geom_text(data = Grouplabels, aes(x = TreatmentName, y = aboveMax, label = groups)) # apply the labels from the tibble
    # to put labels all at same height, y = absMax + absMax*0.05

# Normality and Assumptions

# Repeat for all Nymphal Instars

# ? Combine into multi-boxplot?


# Percent Mortality Analysis ----------------------------------------------

Groupedmort = Mort %>% group_by(Trt)
Mortblockcombined = Groupedmort %>% summarise(TotNo = sum(TotNo), NoD = sum(NoD))
Mortblockcombined
Percentmortcombined = Mortblockcombined %>% mutate(NoA = TotNo-NoD, PercentJuvenileMortality = (NoD/TotNo)*100)
Percentmortcombined

ggplot(Percentmortcombined, aes(y = PercentJuvenileMortality, x = Trt)) +
  geom_bar(stat = "identity") +
  xlab("Treatment") +
  ylab("Percent Mortality")

# ? what stat test, chi squared??


# Tibial Lengths Analysis -------------------------------------------------


