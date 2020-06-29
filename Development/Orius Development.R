# Prep

rm(list=ls())

# install and load necessary packages install.packages("PackageName")
library(tidyverse)
#library(dplyr)
#library(tidyr)
library(xlsx)

#choose the wd
setwd(".") # "." refers to the file that this script is already in
getwd()


# Get data and cleanup

# Development Rate
Rate = tbl_df(read.csv("Development Rate raw csv.csv", header = TRUE, fileEncoding="UTF-8-BOM"))
head(Rate)
summary(Rate)

# % Mortality
Mort = tbl_df(read.csv("Development Mortality raw csv.csv", header = TRUE, fileEncoding="UTF-8-BOM"))
head(Mort)
summary(Mort)

# Tibial Length
Tibia = tbl_df(read.csv("Development Tibial Length raw csv.csv", header = TRUE, fileEncoding="UTF-8-BOM"))
head(Tibia)
summary(Tibia)
