
######################
# Brian J. Enquist
# R for CHAMBASA - Peru Campaign
# BIEN traits for Chambasa
#####################

# Read in file
setwd("/Users/benquist/R_CHAMBASA/")
data.in <- read.csv(file = "compiled_traits_Peru.csv")
head(data.in)
tail (data.in)
summary(data.in)
sapply(data.in, class)
#number of tree observations in CHAMBASA plots
dim(data.in)
#number of genus-level traits
summary(data.in$d13C)
summary(data.in$SLA)
summary(data.in$WoodDensity)
summary(data.in$LCC)
summary(data.in$LNC)
summary(data.in$LPC)
library(dplyr)
library(plyr)
library(reshape2)
melted <- melt(data.in, id.vars=c("Plot", "WoodDensity"))
ddply(melted, c("Plot", "WoodDensity"), summarise,
      mean = mean(value), sd = sd(value),
      sem = sd(value)/sqrt(length(value)))


# this is the script way of installing packages
install.packages ("ggplot2", dependencies = TRUE)
install.packages("dplyr")
install.packages("plyr")
install.packages("ggthemes")
install.packages("reshape2")

#load libraries
library(ggplot2)
library(reshape2)
library(ggthemes)
library(plyr)
library(MASS)

#histograms - all individuals
hist(data.in$d13C)
hist(data.in$WoodDensity)
hist(data.in$Height)
hist(data.in$SLA)
logSLA <- log10(data.in$SLA)
hist(data.in$Height)
hist(data.in$LNC)
hist(data.in$LPC)

data.plots.in  <- data.in |
  filter(cut=="Plots") |
  select(SLA, WoodDenisity, LNC)
head(data.plots.in )


data.in %>%                                        # Start with the 'diamonds' dataset
  filter(cut == "Plot") %>%                        # Then, filter down to rows where cut == Ideal
  ggplot(aes(x=color,y=price)) +                     # Then, plot using ggplot
  geom_boxplot() 
