
# Data wrangling ----------------------------------------------------------

library(tidyverse)

# select
# filter
# mutate
# summarize
# the pipe: %>% |>

blp_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntu-doctoral-rrstudio-2425/refs/heads/main/data/blp-trials-short.txt")

select(blp_df, participant, spell, rt)
select(blp_df, id = participant, item = spell, rt)

select(blp_df, 1, 3, 5)
select(blp_df, participant, 3, 5)
  
select(blp_df, lex:rt)
select(blp_df, 2:5)
select(blp_df, participant, 3:6)

select(blp_df, starts_with('r'))
select(blp_df, participant, starts_with('r'))

select(blp_df, ends_with('t'))
select(blp_df, ends_with('rt'))

select(blp_df, contains('rt'))

select(blp_df, matches('^rt|rt$'))
select(blp_df, matches('^r.*t$')) # starts with "r" and ends with "t" and anything in between

select(blp_df, where(is.numeric))


# Renaming ----------------------------------------------------------------

rename(blp_df, id = participant)
select(blp_df, id = participant)

rename_with(blp_df, toupper)


# filtering rows with filter ----------------------------------------------

filter(blp_df, rt < 900)
filter(blp_df, rt > 900)
filter(blp_df, rt >= 900)
filter(blp_df, rt >= 500 | rt <= 600)

filter(blp_df, lex == resp)

# quick look at slice
slice(blp_df, 45)
slice(blp_df, 45:57)

# mutate ------------------------------------------------------------------

blp_df2 <- mutate(blp_df, accuracy = lex == resp)
print(blp_df2, n = Inf)

mutate(blp_df, accuracy = lex == resp, .after = participant)

mutate(blp_df, lex = as.factor(lex))

# recode three variables as factor vectors
mutate(blp_df, 
       lex = as.factor(lex), 
       spell = as.factor(spell), 
       resp = as.factor(resp))

mutate(blp_df, across(where(is.character), as.factor))

mutate(blp_df, fast_rt = rt.raw <= 600)

mutate(blp_df, 
       speed = case_when(
         rt.raw <= 600 ~ 'fast',
         rt.raw >= 800 ~ 'slow',
         rt.raw > 600 & rt.raw < 800 ~ 'medium'
       )
)
mutate(blp_df, 
       speed = case_when(
         rt.raw <= 600 ~ 'fast',
         rt.raw >= 800 ~ 'slow',
         TRUE ~ 'medium'
       )
)

mutate(blp_df, quartile_rt = ntile(rt.raw, n = 4))


# Summarizing with summarize ----------------------------------------------

summarize(blp_df, mean_rt = mean(rt.raw), std_rt = sd(rt.raw))

library(skimr)
skim(blp_df)

summarise(blp_df2, 
          mean_rt = mean(rt.raw), 
          std_rt = sd(rt.raw), 
          .by = accuracy)


# The pipe ----------------------------------------------------------------

primes <- c(2, 3, 5, 7, 11, 13, 17)
log(primes)
sqrt(log(primes))
sum(sqrt(log(primes)))
log(sum(sqrt(log(primes))))

log(primes)
primes %>% log()
primes |> log()

primes |> log() |> sqrt() |> sum() |> log()


# Data cleaning with the pipe ---------------------------------------------

blp_df |> 
  mutate(accuracy = lex == resp) |> 
  select(id = participant, accuracy, rt = rt.raw) |> 
  drop_na() |> 
  summarize(mean_rt = mean(rt), std_rt = sd(rt), .by = accuracy)


read_csv("https://raw.githubusercontent.com/mark-andrews/ntu-doctoral-rrstudio-2425/refs/heads/main/data/blp-trials-short.txt")|> 
  mutate(accuracy = lex == resp) |> 
  select(id = participant, accuracy, rt = rt.raw) |> 
  drop_na() |> 
  summarize(mean_rt = mean(rt), std_rt = sd(rt), .by = accuracy)


# Data viz ----------------------------------------------------------------

weight_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntu-doctoral-rrstudio-2425/refs/heads/main/data/weight.csv")


ggplot(weight_df, 
       mapping = aes(x = weight)
) + geom_histogram(binwidth = 2.5, colour = 'white')


ggplot(weight_df, 
       mapping = aes(x = weight, fill = gender)
) + geom_histogram(binwidth = 2.5, colour = 'white')

ggplot(weight_df, 
       mapping = aes(x = weight, fill = gender)
) + geom_histogram(binwidth = 2.5, position = 'dodge')

ggplot(weight_df, 
       mapping = aes(x = weight, fill = gender)
) + geom_histogram(binwidth = 2.5, position = 'identity', alpha = 0.6)


# Logistic regression -----------------------------------------------------


smoking_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntu-doctoral-rrstudio-2425/refs/heads/main/data/smoking.csv") |> 
  mutate(smoker = cigs > 0)

# we don't do this ...
# lm(smoker ~ age, data = smoking_df)

M_1 <- glm(smoker ~ age, data = smoking_df, family = binomial())

summary(M_1)

smoking_df2 <- tibble(age = seq(15, 90, by = 5))
library(modelr)
smoking_df2 |> add_predictions(M_1)

smoking_df2 |> add_predictions(M_1) |> mutate(prob = plogis(pred))

smoking_df2 |>
  add_predictions(M_1) |>
  mutate(prob = plogis(pred)) |> 
  ggplot(aes(x = age, y = prob)) + geom_line()


exp(coef(M_1)) # odds ratio


# Multiple logistic regression --------------------------------------------

M_2 <- glm(smoker ~ age + educ + lincome, 
           data = smoking_df,
           family = binomial())

summary(M_2)

logLik(M_1) 
logLik(M_2)
logLik(M_1) * -2
logLik(M_2) * -2
deviance(M_1)
deviance(M_2)
summary(M_1)

anova(M_1, M_2) # Model comparison of M_1 and M_2

M_3 <- glm(smoker ~ age + educ + lincome, 
           data = smoking_df,
           family = binomial())

M_4 <- glm(smoker ~ age + educ, 
           data = smoking_df,
           family = binomial())

anova(M_1, M_4) # compare M_1 and M_4
anova(M_2, M_4) # compare M_2 and M_4


# Linear mixed effects models ---------------------------------------------

library(lme4)

ggplot(sleepstudy,
       aes(x = Days, y = Reaction, colour = Subject)
) + geom_point() + 
  facet_wrap(~Subject) +
  stat_smooth(method= 'lm', se = FALSE) +
  theme(legend.position = 'none')

M_5 <- lmer(Reaction ~ Days + (Days|Subject), 
            data = sleepstudy)

summary(M_5)


# stepwise regression
m <- glm(smoker ~ educ + cigpric + white + age + income, data = smoking_df,
         family = binomial())

m_best <- step(m)



