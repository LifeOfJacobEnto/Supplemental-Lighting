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
Tdevsummary = DevAdults %>% group_by(Block, Treatment, Sex) %>% summarise(Instar = "Total", Mean = mean(TotalDevelopment), SD = sd(TotalDevelopment), SE = sd(TotalDevelopment)/sqrt(length(TotalDevelopment)), n = length(TotalDevelopment))
N1summary = DevAdults %>% group_by(Block, Treatment, Sex) %>% summarise(Instar = "N1", Mean = mean(N1), SD = sd(N1), SE = sd(N1)/sqrt(length(N1)), n = length(N1))
N2summary = DevAdults %>% group_by(Block, Treatment, Sex) %>% summarise(Instar = "N2", Mean = mean(N2), SD = sd(N2), SE = sd(N1)/sqrt(length(N2)), n = length(N2))
N3summary = DevAdults %>% group_by(Block, Treatment, Sex) %>% summarise(Instar = "N3", Mean = mean(N3), SD = sd(N3), SE = sd(N1)/sqrt(length(N3)), n = length(N3))
N4summary = DevAdults %>% group_by(Block, Treatment, Sex) %>% summarise(Instar = "N4", Mean = mean(N4), SD = sd(N4), SE = sd(N1)/sqrt(length(N4)), n = length(N4))
N5summary = DevAdults %>% group_by(Block, Treatment, Sex) %>% summarise(Instar = "N5", Mean = mean(N5), SD = sd(N5), SE = sd(N1)/sqrt(length(N5)), n = length(N5))
DevAdultssummary = rbind(Tdevsummary, N1summary, N2summary, N3summary, N4summary, N5summary)
DevAdultssummarywide = DevAdultssummary %>% gather(Statistic, Value, Mean:n) %>% spread(key = Instar, value = Value)
# ^ is easiest, but could also try:
  # or Total and all Nymphal Instars (from https://dplyr.tidyverse.org/articles/colwise.html#multiple-functions-1trying, also https://dplyr.tidyverse.org/reference/group_by.html and https://community.rstudio.com/t/summarise-multiple-columns-using-multiple-functions-in-a-tidy-way/8645 )
  # or use a loop

# Histograms 
Treatmentlevelsorder = c("S", "W", "HPS", "HB", "HR", "LB", "LR")
ggplot(DevAdults, aes(x = TotalDevelopment, color = factor(Sex), fill = factor(Sex))) +
  geom_histogram() +
  xlab("Total Development (days)") +
  ylab("Frequency") +
  theme_classic() +
  facet_wrap(~factor(Block) + factor(Treatment, levels = Treatmentlevelsorder), ncol = 3, scales = "fixed")

# multi-ANOVA including interaction effects (from http://www.sthda.com/english/wiki/two-way-anova-test-in-r)
TdevANOVA= aov(DevAdults$TotalDevelopment ~ DevAdults$Block + DevAdults$Treatment + DevAdults$Sex + DevAdults$Block:DevAdults$Treatment + DevAdults$Block:DevAdults$Sex + DevAdults$Treatment:DevAdults$Sex)
summary(TdevANOVA)
  # no significant difference between Sex so combined, and no interaction effects

# Tukey HSD
TdevTukeyTreatment = HSD.test(TdevANOVA, trt = 'DevAdults$Treatment', group = TRUE)
TdevTukeyTreatment

# make a means plot to illustrate any interaction effect
level_order <- c('W', 'S', 'HPS', 'HB', 'HR', 'LB', 'LR')
Tdevmeansplot = 
  ggplot(data = Tdevsummary, aes(y = Mean, x = factor(Treatment, levels = level_order), group = Block, col = Block)) + # (from http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization)
  geom_line() + 
  geom_point() +
  theme_classic() +
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
  theme_classic() +
  geom_text(data = Grouplabels, aes(x = Treatment, y = aboveMax, label = groups)) # apply the labels from the tibble
    # to put labels all at same height, y = absMax + absMax*0.05
# ? separate Sex by using texture for Block?

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
      ggplot(DevAdults, aes(sample = TotalDevelopment, color = factor(Sex))) + # from https://ggplot2.tidyverse.org/reference/geom_qq.html or https://www.datanovia.com/en/lessons/ggplot-qq-plot/
        stat_qq() + stat_qq_line() +
        xlab("Theoretical") +
        ylab("Sample") +
        theme_classic() +
        facet_wrap(~factor(Block) + factor(Treatment, levels = Treatmentlevelsorder), ncol = 3, scales = "fixed")
  # equal variance across populations "homogeneity of variance" "homoscedasticity" 
    # Levene's test
      DevAdults %>% levene_test(formula = TotalDevelopment ~ factor(Block) * Treatment * Sex, center = median) # from https://www.datanovia.com/en/lessons/homogeneity-of-variance-test-in-r/
        # ? does using all three factors work? Or group_by(Block, Sex) , or just group_by(Block) because Sex is not sig diff


# Repeat for all Nymphal Instars
  N1ANOVA= aov(DevAdults$N1 ~ DevAdults$Block + DevAdults$Treatment + DevAdults$Sex + DevAdults$Block:DevAdults$Treatment + DevAdults$Block:DevAdults$Sex + DevAdults$Treatment:DevAdults$Sex)
  N2ANOVA= aov(DevAdults$N2 ~ DevAdults$Block + DevAdults$Treatment + DevAdults$Sex + DevAdults$Block:DevAdults$Treatment + DevAdults$Block:DevAdults$Sex + DevAdults$Treatment:DevAdults$Sex)
  N3ANOVA= aov(DevAdults$N3 ~ DevAdults$Block + DevAdults$Treatment + DevAdults$Sex + DevAdults$Block:DevAdults$Treatment + DevAdults$Block:DevAdults$Sex + DevAdults$Treatment:DevAdults$Sex)
  N4ANOVA= aov(DevAdults$N4 ~ DevAdults$Block + DevAdults$Treatment + DevAdults$Sex + DevAdults$Block:DevAdults$Treatment + DevAdults$Block:DevAdults$Sex + DevAdults$Treatment:DevAdults$Sex)
  N5ANOVA= aov(DevAdults$N5 ~ DevAdults$Block + DevAdults$Treatment + DevAdults$Sex + DevAdults$Block:DevAdults$Treatment + DevAdults$Block:DevAdults$Sex + DevAdults$Treatment:DevAdults$Sex)
  summary(N1ANOVA)
  summary(N2ANOVA)
  summary(N3ANOVA)
  summary(N4ANOVA)
  summary(N5ANOVA)
  N1TukeyTreatment = HSD.test(N1ANOVA, trt = 'DevAdults$Treatment', group = TRUE)
  N2TukeyTreatment = HSD.test(N2ANOVA, trt = 'DevAdults$Treatment', group = TRUE)
  N3TukeyTreatment = HSD.test(N3ANOVA, trt = 'DevAdults$Treatment', group = TRUE)
  N4TukeyTreatment = HSD.test(N4ANOVA, trt = 'DevAdults$Treatment', group = TRUE)
  N5TukeyTreatment = HSD.test(N5ANOVA, trt = 'DevAdults$Treatment', group = TRUE)
  N1TukeyTreatment$groups
  N2TukeyTreatment$groups
  N3TukeyTreatment$groups
  N4TukeyTreatment$groups
  N5TukeyTreatment$groups
  
  # or make a function
    BoxplotSig = function(Instar, title = "Title") {
      ANOVA = aov(Instar ~ DevAdults$Block + DevAdults$Treatment + DevAdults$Sex + DevAdults$Block:DevAdults$Treatment + DevAdults$Block:DevAdults$Sex + DevAdults$Treatment:DevAdults$Sex)

      TukeyTreatment = HSD.test(ANOVA, trt = 'DevAdults$Treatment', group = TRUE)

      Grouplabels = tbl_df(TukeyTreatment$groups["groups"])
      Grouplabels$Treatment = row.names(TukeyTreatment$groups)
      Max = TukeyTreatment$means["Max"]
      Maxtbl = as_tibble(Max, rownames = "Treatment")
      Grouplabels = merge(Grouplabels, Maxtbl, by = "Treatment")
      Grouplabels$aboveMax = Grouplabels$Max + 1
      absMax = max(Grouplabels$Max)
      Treatmentlevelsorder = c("S", "W", "HPS", "HB", "HR", "LB", "LR")
      ggplot(DevAdults, aes(y = Instar, x = factor(Treatment, levels = Treatmentlevelsorder))) +
        geom_boxplot(aes(color = factor(Block))) +
        xlab("Treatment") +
        ylab("Development Time (days)") +
        ggtitle(title) +
        theme_classic() +
        geom_text(data = Grouplabels, aes(x = Treatment, y = aboveMax, label = groups))
    }


# ? Combine into multi-boxplot?
      # ? need Instar in long form?



# Percent Nymphal Mortality Analysis ----------------------------------------------
# Chi-squared Test of Independence ie. is the number of Dead individuals independent (p > alpha 0.05) or dependent(p < alpha 0.05) on Treatment? from https://www.r-bloggers.com/chi-squared-test/
  Mortalitycontingency = table(Dev$Treatment, Dev$Fate)
    # ? could use ftable(Dev$Block, Dev$Sex, Dev$Treatment, Dev$Fate) to separate by Block and Sex (maybe without Sex, since no sig diff), but doens't work for Chi-squared test, look into Mantel-Haenszel Chi-sqaured Test, from https://www.datacamp.com/community/tutorials/contingency-tables-r
  chisq.test(Mortalitycontingency, correct = FALSE) # correct = FALSE so Yates continuity correction is NOT applied, as per Lecture 16 of Biostats
   # ? repeat for each Treatment separated by Block and Sex using filter
    
# Barchart of %Mortality to visualize the significant difference
  # ? or percent stacked bar chart of %Survival and %Mortality from https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html or https://www.datanovia.com/en/blog/how-to-create-a-ggplot-stacked-bar-chart-2/

# ? combine treatments W and W2
    
# Tibial Lengths Analysis -------------------------------------------------

# ? add the two measurements Dana made, see email
