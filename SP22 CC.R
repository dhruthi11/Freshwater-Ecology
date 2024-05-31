# Mandavilli - Canopy Cover
# July 2023

# Set working directory:
setwd("~/Desktop/")

# Load packages:
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(car)
library(ggpubr)
library(FSA)
library("readxl")


# Canopy Cover Plots ------------------------------------------------------

# load file
canopy <- read_excel("SP22_CC.xlsx")
canopy <- canopy %>% mutate(Quality = Quality/10)

# order weeks
canopy$Week <- factor(canopy$Week, levels=c('WA', 'WB', 'WC', 'W1', 'W2', 'W3', 'W4','W5','W6'))

# plot canopy cover
canopy_cover <- ggplot(canopy, aes(x=Week, y=Percentage, color=Trap)) +
  geom_point(aes(alpha=Quality, size=1.2))+
  scale_size_continuous(guide = "none")+
  facet_wrap(~Pool, ncol=5)+
  geom_line(aes(group=Trap))+
  theme_classic()+
  ylab("Canopy Cover (%)")
canopy_cover

#devtools::install_github("thomasp85/patchwork")
#library(patchwork)
#canopy_cover / weekly_temps


#https://stackoverflow.com/questions/48940187/inserting-an-extra-plot-into-faceted-grid-ggplot2

plot <- ggplot()+
  geom_point(data=canopy, aes(x=Week, y=Percentage, color=Trap, alpha=Quality, size=1.2))+
  facet_wrap(~Pool, ncol=5)+
  geom_line(data=canopy, aes(x=Week, y=Percentage, color=Trap, group=Trap))+
  scale_size_continuous(guide = "none")+
  theme_classic() +
  scale_y_continuous(name = 'Canopy Cover (%)') +
  labs(title = 'Canopy Cover Data', x = 'Week')
plot


# Temperature Graphs ------------------------------------------------------

temps <- read.csv("Pinn_Temps.csv")

# order weeks
temps$Week <- factor(temps$Week, levels=c('WA', 'WB', 'WC', 'W1', 'W2', 'W3', 'W4','W5','W6'))

weekly_temps <- ggplot(temps, aes(x=Week, y=Temps, color=Metric)) +
  geom_point(aes(size=1))+
  scale_size_continuous(guide = "none")+
  geom_line(aes(group=Metric))+
  scale_color_manual(values=c("chocolate1", "firebrick2"))+
  theme_classic() +
  ylab("Temperature (°F)")
weekly_temps


# Dual Axis Plotting: Average Temps ------------------------------------------------------
avg_temps <- read.csv("average_temps.csv")
levels=c('WA', 'WB', 'WC', 'W1', 'W2', 'W3', 'W4','W5','W6')

# first, set the limits of the two y-axes
# primary y-axis = precipitation
# secondary y-axis = temperature
ylim.prim <- c(0, 80)   # % canopy cover
ylim.sec <- c(0, 70)    # temperature

# transformation for scaling each axis
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

# data=date table
# month = x-axis

ggplot(canopy, aes(Week, Percentage)) +
  geom_point(data=canopy, aes(x=Week, y=Percentage, color=Trap, alpha=Quality, size=1.2)) + # for the canopy cover data
  scale_size_continuous(guide = "none")+
  facet_wrap(~Pool, ncol=5)+
  geom_line(data=canopy, aes(x=Week, y=Percentage, color=Trap, group=Trap))+
  geom_line(data=avg_temps, aes(group=Metric), color = "darkred")+
  geom_line(aes(y = a + avg_temps$Percentage*b), color = "red") + # for the temp data
  geom_point(data=avg_temps, aes(size=1.1), shape=4)+
  theme_classic() +
  scale_y_continuous("Canopy Cover (%)", sec.axis = sec_axis(~ (. - a)/b, name = "Average Temperature (°F)")) +
  scale_x_discrete("Week", levels)

# first, set the limits of the two y-axes
# primary y-axis = precipitation
# secondary y-axis = temperature
#ylim.prim <- c(0, 100)   # precipitation
#ylim.sec <- c(0, 35)    # temperature

# transformation for scaling each axis
#b <- diff(ylim.prim)/diff(ylim.sec)
#a <- ylim.prim[1] - b*ylim.sec[1]) 

# data=canopy table
# week = x-axis

#ggplot(data, aes(month, precipitation)) +
#  geom_col() + # for the precip data
#  geom_line(aes(y = a + temperature*b), color = "red") + # for the temp data
#  scale_y_continuous("Precipitation", sec.axis = sec_axis(~ (. - a)/b, name = "Temperature")) +
#  scale_x_continuous("Month", breaks = 1:12)



# Dual Axis Plotting: Maximum Temps ---------------------------------------
max_temps <- read.csv("max_temps.csv")
levels=c('WA', 'WB', 'WC', 'W1', 'W2', 'W3', 'W4','W5','W6')

# first, set the limits of the two y-axes
# primary y-axis = precipitation
# secondary y-axis = temperature
ylim.prim <- c(0, 80)   # % canopy cover
ylim.sec <- c(0, 90)    # temperature

# transformation for scaling each axis
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

# data=date table
# month = x-axis

ggplot(canopy, aes(Week, Percentage)) +
  geom_point(data=canopy, aes(x=Week, y=Percentage, color=Trap, alpha=Quality, size=1.2)) + # for the canopy cover data
  scale_size_continuous(guide = "none")+
  facet_wrap(~Pool, ncol=5)+
  geom_line(data=canopy, aes(x=Week, y=Percentage, color=Trap, group=Trap))+
  geom_line(data=max_temps, aes(group=Metric), color = "darkred")+
  geom_line(aes(y = a + max_temps$Percentage*b), color = "red") + # for the temp data
  geom_point(data=max_temps, aes(size=1.1), shape=4)+
  theme_classic() +
  scale_y_continuous("Canopy Cover (%)", sec.axis = sec_axis(~ (. - a)/b, name = "Maximum Temperature (°F)")) +
  scale_x_discrete("Week", levels)


# All Combined ------------------------------------------------------------
max_temps <- read.csv("max_temps.csv")
levels=c('WA', 'WB', 'WC', 'W1', 'W2', 'W3', 'W4','W5','W6')

# first, set the limits of the two y-axes
# primary y-axis = precipitation
# secondary y-axis = temperature
ylim.prim <- c(0, 80)   # % canopy cover
ylim.sec <- c(0, 90)    # temperature

# transformation for scaling each axis
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

# data=date table
# month = x-axis

ggplot(canopy, aes(Week, Percentage)) +
  geom_point(data=canopy, aes(x=Week, y=Percentage, color=Trap, alpha=Quality, size=1.2)) + # for the canopy cover data
  scale_size_continuous(guide = "none")+
  facet_wrap(~Pool, ncol=5)+
  geom_line(data=canopy, aes(x=Week, y=Percentage, color=Trap, group=Trap))+
  geom_line(data=max_temps, aes(group=Metric), color = "darkred")+
  geom_line(data=avg_temps, aes(group=Metric), color = "darkorange2")+
  geom_line(aes(y = a + max_temps$Percentage*b), color = "red") + # for the temp data
  geom_point(data=max_temps, aes(size=1.1), shape=8)+
  geom_point(data=avg_temps, aes(size=1.1), shape=4)+
  theme_classic() +
  scale_y_continuous("Canopy Cover (%)", sec.axis = sec_axis(~ (. - a)/b, name = "Temperatures (°F)")) +
  scale_x_discrete("Week", levels)


# Significant Differences -------------------------------------------------

#Shapiro-Wilk normality test
orange_agressionsc <- data %>% filter(focal_morph == "o") %>% pull(agg_score)
orange_agressionsc
normality_O <- shapiro.test(orange_agressionsc)
normality_O

#Levene Test for variance
levene_agsc <- leveneTest(agg_score ~ focal_morph, data = data)
levene_agsc

#ANOVA
anova_agsc <- anova(Week ~ Percentage, data = canopy)
anova


