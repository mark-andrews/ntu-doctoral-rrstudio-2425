library(tidyverse)
library(skimr)
library(emmeans)

weight_df <- read_csv('data/weight.csv')

glimpse(weight_df)
skim(weight_df)

# Selecting columns with "select" -----------------------------------------

weight_df2 <- select(weight_df, height, weight, gender, age)

select(weight_df, starts_with('wei'))
select(weight_df, contains('eig'))
select(weight_df, gender:age)

select(weight_df, where(is.numeric))

# filtering rows with "filter" --------------------------------------------

filter(weight_df, gender == 'Male')
filter(weight_df, age < 35)
filter(weight_df, gender == 'Male', age < 35) # Male AND under 35
filter(weight_df, gender == 'Male' | age < 35)

# t test on height and gender ---------------------------------------------

result_1 <- t.test(height ~ gender, data = weight_df)


# Linear regression -------------------------------------------------------

result_2 <- lm(weight ~ height, data = weight_df)
summary(result_2)

result_3 <- lm(weight ~ height + age, data = weight_df)


# ANOVA -------------------------------------------------------------------

result_4 <- aov(weight ~ group, data = PlantGrowth)
summary(result_4)

emmeans(result_4, specs = pairwise ~ group)
