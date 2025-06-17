library(tidyverse)

blp_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntu-doctoral-rrstudio-2425/refs/heads/main/data/blp-trials-short.txt")


# Subset columns using "select" -------------------------------------------

select(blp_df, participant, lex, spell, resp, rt)
select(blp_df, id = participant, lex, item = spell, resp, rt)

select(blp_df, participant:rt)

# select by index
select(blp_df, 1, 2, 3, 4, 5)
select(blp_df, 1:5)

# select columns by their starting characters
select(blp_df, starts_with('r'))
select(blp_df, participant, starts_with('r'))

# select columns by their ending characters
select(blp_df, ends_with('t'))
select(blp_df, ends_with('rt'))

# select columns by whether its name contains "rt"
select(blp_df, contains('rt'))

select(blp_df, matches('^r')) # ^ signifies the start of the string
select(blp_df, matches('t$')) # $ signifies the end of the string
select(blp_df, matches('^r|t$'))
select(blp_df, matches('^rt|rt$'))

select(blp_df, matches('^r.*t$')) # begins with "r" AND ends with "t" 
select(blp_df, matches('^rt.*rt$')) # begins with "rt" AND ends with "rt" 

select(blp_df, matches('^.+rt.+$'))

select(blp_df, where(is.numeric))
select(blp_df, where(is.character))

# Renaming with "rename" --------------------------------------------------

select(blp_df, id = participant, rt = rt.raw)
rename(blp_df, id = participant)

rename_with(blp_df, toupper)

# Selecting rows with "slice" ---------------------------------------------

slice(blp_df, 1:5)
slice(blp_df, 1, 5, 56, 101:105)
slice_head(blp_df, n = 5)
slice_head(blp_df, n = 15)


# Filter, aka select rows, with "filter" ----------------------------------

filter(blp_df, lex == 'N') # rows where the stimulus type "lex" was 'N'
filter(blp_df, resp == 'N') # rows where the response "resp" was 'N'

filter(blp_df, rt < 1000)
filter(blp_df, rt <= 1000)
filter(blp_df, rt >= 500, rt <= 1000)
filter(blp_df, rt >= 500 | rt <= 1000)
filter(blp_df, rt.raw >= 500 | rt.raw <= 1000)

filter(blp_df, lex == resp)


# Create new variables with "mutate" --------------------------------------

mutate(blp_df, accuracy = lex == resp)
mutate(blp_df, accuracy = lex == resp, .after = participant)
mutate(blp_df, accuracy = lex == resp, .before = participant)

mutate(blp_df, spell = as.factor(spell))
mutate(blp_df, 
       lex = as.factor(lex), 
       spell = as.factor(spell),
       resp = as.factor(resp)
)

mutate(blp_df, across(where(is.character), as.factor))

mutate(blp_df, fast_rt = rt.raw < 800)
mutate(blp_df, fast_rt = rt.raw < median(rt.raw))

mutate(blp_df, 
       rt_speed = case_when(
         rt.raw < 600 ~ 'fast',
         rt.raw > 1000 ~ 'slow',
         rt.raw >= 600 & rt.raw <= 1000 ~ 'medium'
       )
)
mutate(blp_df, 
       rt_speed = case_when(
         rt.raw < 600 ~ 'fast',
         rt.raw > 1000 ~ 'slow',
         TRUE ~ 'medium'
       )
)

summarise(blp_df, 
          rt_avg = mean(rt.raw),
          rt_median = median(rt.raw),
          rt_sd = sd(rt.raw))
          
summarise(blp_df, 
          rt_avg = mean(rt.raw),
          rt_median = median(rt.raw),
          rt_sd = sd(rt.raw),
          .by = lex)
  
blp_df2 <- mutate(blp_df, accuracy = lex == resp)
  
summarise(blp_df2, 
          rt_avg = mean(rt.raw),
          rt_median = median(rt.raw),
          rt_sd = sd(rt.raw),
          .by = accuracy)

summarize(blp_df2, 
          rt_avg = mean(rt.raw),
          rt_median = median(rt.raw),
          rt_sd = sd(rt.raw),
          .by = accuracy)


# Understanding the pipe, i.e. this "|>" or "%>%" -------------------------

primes <- c(2, 3, 5, 7, 11, 13)
log(primes)
sqrt(log(primes))
sum(sqrt(log(primes)))
sqrt(sum(sqrt(log(primes))))

# do the above but with pipes

primes |> log() # same as log(primes)
primes |> log() |> sqrt() |> sum() |> sqrt()

mutate(blp_df, accuracy = lex == resp) |> 
  summarise(rt_avg = mean(rt.raw),
            rt_median = median(rt.raw),
            rt_sd = sd(rt.raw),
            .by = accuracy)

# Pipeline example
# 1) read in the raw data from the web
# 2) create a new variable, accuracy
# 3) calculate means and stdevs of rt's for accurate and inaccurate trials

read_csv("https://raw.githubusercontent.com/mark-andrews/ntu-doctoral-rrstudio-2425/refs/heads/main/data/blp-trials-short.txt") |> 
  mutate(accuracy = lex == resp) |> 
  select(participant, accuracy, rt = rt.raw) |> 
  summarise(avg = mean(rt), stdev = sd(rt), .by = accuracy)
  

# Data Viz ----------------------------------------------------------------

ggplot(blp_df, aes(x = rt.raw)) +
  geom_histogram(binwidth = 100, colour = 'white', fill = 'grey50') +
  xlim(0, 2500) +
  xlab('Reaction time') +
  theme_classic()

blp_df |> 
  mutate(accuracy = lex == resp) |> 
  ggplot(aes(x = rt.raw, fill = accuracy)) +
  geom_histogram(binwidth = 100, colour = 'white') +
  xlim(0, 2500) +
  xlab('Reaction time') +
  theme_classic()

blp_df |> 
  mutate(accuracy = if_else(lex == resp, 'accurate', 'inaccurate')) |> 
  ggplot(aes(x = rt.raw, fill = accuracy )) +
  geom_histogram(binwidth = 100, colour = 'white', position = 'dodge') +
  xlim(0, 2500) +
  labs(x = 'Reaction time', fill = 'Accurate') +
  theme_classic()

blp_df |> 
  mutate(accuracy = if_else(lex == resp, 'accurate', 'inaccurate')) |> 
  ggplot(aes(x = rt.raw, fill = accuracy )) +
  geom_histogram(binwidth = 100, colour = 'white', position = 'identity', alpha = 0.75) +
  xlim(0, 2500) +
  labs(x = 'Reaction time', fill = 'Accurate') +
  theme_classic()

blp_df |> 
  mutate(accuracy = if_else(lex == resp, 'accurate', 'inaccurate')) |> 
  ggplot(aes(x = rt.raw, fill = accuracy )) +
  geom_histogram(binwidth = 100, colour = 'white', position = 'identity', alpha = 0.75) +
  xlim(0, 2500) +
  labs(x = 'Reaction time', fill = 'Accurate') +
  theme_classic() + 
  facet_wrap(~accuracy)


# GLM ---------------------------------------------------------------------

smoking_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntu-doctoral-rrstudio-2425/refs/heads/main/data/smoking.csv") |> 
  mutate(smoker = cigs > 0)

# What we could do but should not do is this ...
# lm(smoker ~ age, data = smoking_df)

# Instead we can do a logistic regression
M_1 <- glm(smoker ~ age, data = smoking_df, family = binomial())
summary(M_1)

smoking_df2 <- tibble(age = c(20, 25, 50, 80))
predict(M_1, newdata = smoking_df2)
predict(M_1, newdata = smoking_df2, type = 'response')

library(modelr)
add_predictions(smoking_df2, M_1, type = 'response')

add_predictions(smoking_df2, M_1, type = 'response') |> 
  ggplot(aes(x = age, y = pred)) + 
  geom_line(colour = 'red') +
  geom_point() +
  labs(x = 'Age (in years)', y = 'Probability of being a smoker')

# multiple logistic regression
M_2 <- glm(smoker ~ age + educ + lincome, 
           data = smoking_df, 
           family = binomial())

summary(M_2)

logLik(M_1)
logLik(M_2)

-2 * logLik(M_1)
deviance(M_1)
deviance(M_2)

anova(M_1, M_2, test = 'Chisq') # null hypothesis test; counterpart of F-test

M_3 <- glm(smoker ~ age + educ, 
           data = smoking_df, 
           family = binomial())


anova(M_3, M_2, test = 'Chisq')

M_4 <- glm(smoker ~ educ + age + cigpric + white + lincome + restaurn,
           data = smoking_df,
           family = binomial())

summary(M_4)
M_5 <- step(M_4) # forward-backward stepwise regression


# Mixed effects models in R -----------------------------------------------

library(lme4)
sleepstudy

ggplot(sleepstudy, aes(x = Days, y = Reaction)) + geom_point() + facet_wrap(~Subject) + geom_smooth(method = 'lm', se = F)

M_6 <- lmer(Reaction ~ Days + (Days|Subject), data =sleepstudy)
summary(M_6)

