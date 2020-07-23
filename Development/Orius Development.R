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
setwd("C:/Users/Jacob/Documents/Harrow 2020/Supplimental Lighting Experiments/Supplemental-Lighting-Git/Development")
#setwd("./Openrefined data") # "." refers to the file that this script is already in
getwd()


# Get Openrefined Data ----------------------------------------------------
# OpenRefine Lesson: http://datacarpentry.org/OpenRefine-ecology-lesson/

# Development Rate
Dev = tbl_df(read.csv("Consolidated-Orius-Development-JB2020-Raw-Development-Openrefined.csv", header = TRUE))
head(Dev)

DevAdults = Dev %>% filter(Fate == "A")

# Tibial Length


# Development Rate Analysis -----------------------------------------------

# measures of spread
summary(Dev)

# Transformations
  # Rate$Total.Development..days. = sqrt(Rate$Total.Development..days.)
    # sqrt transformation results in nearly identical results to untransformed data
  # Rate$Total.Development..days. = log(Rate$Total.Development..days.)
    # Log transformation results in nearly identical results to untransformed data

# Summary stats for Development (Mean, SD, SE, n)
Tdevsummary = DevAdults %>% group_by(Block, Treatment, Sex) %>% summarise(Mean = mean(TotalDevelopment), SD = sd(TotalDevelopment), SE = sd(TotalDevelopment)/sqrt(length(TotalDevelopment)), n = length(TotalDevelopment))
Tdevsummary
  # for Total and all Nymphal Instars (from https://dplyr.tidyverse.org/articles/colwise.html#multiple-functions-1trying, also https://dplyr.tidyverse.org/reference/group_by.html and https://community.rstudio.com/t/summarise-multiple-columns-using-multiple-functions-in-a-tidy-way/8645 )
  summarystatfunctions = list(
    Mean = ~ mean(.x), 
    SD = ~ sd(.x), 
    SE = ~ sd(.x)/sqrt(length(.x)), 
    n = ~ length(.x)
  )
  Devsummaryallnymphalinstars = DevAdults %>% summarise(across(.cols = N1 : TotalDevelopment, .fns = summarystatfunctions, .names = "{col}.{fn}"))
  Devsummaryallnymphalinstars
  # or usea  loop

# Histograms 
Treatmentlevelsorder = c("S", "W", "HPS", "HB", "HR", "LB", "LR")
ggplot(DevAdults, aes(x = TotalDevelopment, color = factor(Sex), fill = factor(Sex))) +
  geom_histogram() +
  xlab("Total Development (days)") +
  ylab("Frequency") +
  theme_classic() +
  facet_wrap(~factor(Block) + factor(Treatment, levels = Treatmentlevelsorder), ncol = 3, scales = "fixed")
# then for all Nymphal Instars
  # ? need nymphal instar in long form?

# ? make Nymphal Instar Development Time into long format?

# multi-ANOVA including interaction effects (from http://www.sthda.com/english/wiki/two-way-anova-test-in-r)
TdevANOVA= aov(DevAdults$TotalDevelopment ~ DevAdults$Block + DevAdults$Treatment + DevAdults$Sex + DevAdults$Block:DevAdults$Treatment + DevAdults$Block:DevAdults$Sex + DevAdults$Treatment:DevAdults$Sex)
summary(TdevANOVA)
  # no significant difference between sexes so combined, and no interaction effects

# Tukey HSD
TdevTukeyTreatment = HSD.test(TdevANOVA, trt = 'DevAdults$Treatment', group = TRUE)
TdevTukeyTreatment

# make a means plot to illustrate any interaction effect
level_order <- c('W', 'S', 'HPS', 'HB', 'HR', 'LB', 'LR')
Tdevmeansplot = 
  ggplot(data = Tdevsummary, aes(y = Mean, x = factor(Treatment, levels = level_order), group = Block, col = Block)) + # (from http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization)
  geom_line() + 
  geom_point() +
  ylab("Mean Total Development (days)")
  # geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE)) # to give error bars of +-SE
Tdevmeansplot

# Boxplot with significant groups
Grouplabels = tbl_df(TdevTukeyTreatment$groups["groups"])
Grouplabels$Treatment = row.names(TdevTukeyTreatment$groups)
Max = TdevTukeyTreatment$means["Max"]
Maxtbl = as_tibble(Max, rownames = "Treatment")
Grouplabels = merge(Grouplabels, Maxtbl, by = "Treatment")
Grouplabels$aboveMax = Grouplabels$Max + 1
Grouplabels
absMax = max(Grouplabels$Max)
Treatmentlevelsorder = c("S", "W", "HPS", "HB", "HR", "LB", "LR")
ggplot(DevAdults, aes(y = TotalDevelopment, x = factor(Treatment, levels = Treatmentlevelsorder))) +
  geom_boxplot(aes(color = factor(Block))) + 
  xlab("Treatment") +
  ylab("Total Development (days)") +
  geom_text(data = Grouplabels, aes(x = Treatment, y = aboveMax, label = groups)) # apply the labels from the tibble
    # to put labels all at same height, y = absMax + absMax*0.05

# Assumptions
  # individuals are randomly sampled (randomly selected from the colony)
  # samples are independently selected ie. not paired across the treatments
  # subjects were independently selected (not grabbing multiple individuals at once)
  # Normal distribution of each populaiton (ie under each treatment)
    # Histograms
    # Shapiro-Wilk or Kolmogorov-Smirnov (Lilliefors) tests
      library("rstatix")
      DevAdults %>% group_by(Block, Sex) %>% shapiro_test(TotalDevelopment) # from https://www.datanovia.com/en/lessons/normality-test-in-r/
    # Q-Q Plots
      ggplot(DevAdults, aes(sample = TotalDevelopment, color = factor(Block))) + # from https://ggplot2.tidyverse.org/reference/geom_qq.html or https://www.datanovia.com/en/lessons/ggplot-qq-plot/
        stat_qq() + stat_qq_line() +
        xlab("Theoretical") +
        ylab("Sample") +
        facet_wrap(~factor(Treatment, levels = Treatmentlevelsorder), ncol = 3, scales = "fixed")
  # equal variance across populations "homogeneity of variance" "homoscedasticity" 
    # Levene's test
      # leveneTest(DevAdults$TotalDevelopment ~ DevAdults$Treatment * factor(DevAdults$Block), center = median)
      DevAdults %>% levene_test(formula = TotalDevelopment ~ Treatment * factor(Block) * factor(Sex), center = median) # from https://www.datanovia.com/en/lessons/homogeneity-of-variance-test-in-r/
        # ? does using all three x factors work?



# Repeat for all Nymphal Instars
  # ? need in Long form?

# ? Combine into multi-boxplot?



# Percent Mortality Analysis ----------------------------------------------

Groupedmort = Mort %>% group_by(Trt)
Mortblockcombined = Groupedmort %>% summarise(TotNo = sum(TotNo), NoD = sum(NoD))
Mortblockcombined
Percentmortcombined = Mortblockcombined %>% mutate(NoA = TotNo-NoD, PercentJuvenileMortality = (NoD/TotNo)*100, PercentSurvival = (NoA/TotNo)*100)
Percentmortcombined

ggplot(Percentmortcombined, aes(y = PercentJuvenileMortality, x = Trt)) +
  geom_bar(stat = "identity") +
  xlab("Treatment") +
  ylab("Percent Mortality")

# Chi-squared Test of Independence ie. is the number of Dead individuals in/dependent on Treatment? from https://www.r-bloggers.com/chi-squared-test/

# need to make a contingency table of Treamtent as rows, and Alive vs. Dead as columns
  # first remove the escaped and unknown individuals  
    Mortalityminusescaped = filter()

  # then use table() function to make a contingency table from categorical data
    Mortalitycontingency = Percentmortcombined %>% select(Trt, NoA, NoD)

# Chi-squared Test
chisq.test(Mortalitycontingency, correct = FALSE) # correct = FALSE so Yates continuity correction is NOT applied, as per Lecture 16 of Biostats


# Tibial Lengths Analysis -------------------------------------------------


