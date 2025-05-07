# Load packages -----------------------------------------------------------

library(tidyverse)
library(skimr)
library(readxl) # for reading Excel 

# Load data ---------------------------------------------------------------

weight_df <- read_csv('weight.csv')
# if your have Excel file, i.e. weight.xlsx 
# read_excel('weight.xlsx')


# Summary statistics ------------------------------------------------------

skim(weight_df)

# Simple linear regression ------------------------------------------------

# simple linear regression: outcome variable weight, predictor variable height

m1 <- lm(weight ~ height, data = weight_df)

# view the main results
summary(m1)


# multiple linear regression predicting weight from height AND age

m2 <- lm(weight ~ height + age, data = weight_df)
summary(m2)


# t-test ------------------------------------------------------------------

m3 <- t.test(height ~ gender, data = weight_df)
m3

# Correlation coefficient -------------------------------------------------

m4 <- cor.test(~ weight + height, data = weight_df)
m4

# Scatterplot -------------------------------------------------------------

ggplot(weight_df, aes(x = height, y = weight)) + 
  geom_point(size = 0.5)

ggplot(weight_df, aes(x = height, y = weight, colour = gender)) + 
  geom_point(size = 0.5)

ggplot(weight_df, aes(x = height, y = weight, colour = gender)) + 
  geom_point(size = 0.5) +
  theme(legend.position = 'top')

ggplot(weight_df, aes(x = height, y = weight, colour = gender)) + 
  geom_point(size = 0.5) +
  theme_classic()


# Factorial Anova ---------------------------------------------------------

# two way ANOVA
m5 <- aov(weight ~ gender * factor(race), data = weight_df)
summary(m5)



