# Prep --------------------------------------------------------------------

rm(list=ls())

# install and load necessary packages install.packages("PackageName")
library(tidyverse)
#library(dplyr)
#library(tidyr)
library("agricolae")
library("ggplot2")

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
Longrate = Rate = tbl_df(read.csv("Development Rate Openrefined Long.csv", header = TRUE))

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
Groupedrate %>% summarise(Mean = mean(Total.Development..days.), SD = sd(Total.Development..days.), SE = sd(Total.Development..days.)/sqrt(length(Total.Development..days.)), n = length(Total.Development..days.))
#then for All Nymphal Instars NOT WORKING
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
TotaldevTukeyHSD=HSD.test(TotaldevANOVA, trt = 'Rate$Treatment', group = TRUE)
TotaldevTukeyHSD

# ? is there a difference between Blocks? ie. use 2-way anova with Block and Treatment
TotaldevANOVA2 = aov(Rate$Total.Development..days. ~ Rate$Block + Rate$Treatment)
summary(TotaldevANOVA2)
TotaldevTukeyHSD2 = HSD.test(TotaldevANOVA2, trt = 'Rate$Block', group = TRUE)
TotaldevTukeyHSD2
boxplot(Rate$Total.Development..days. ~ Rate$Block + Rate$Treatment, xlab = "Treatment", ylab = "Total Development (days)")
# Yes, but this may be due to low sample size and some Treatments not having samples in both Blocks

# Boxplot with Sig. Groups

# Normality and Assumptions

