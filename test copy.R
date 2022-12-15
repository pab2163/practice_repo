library(emmeans)
library(ggplot2)
library(lme4)
library(tidyverse)


fiber = fiber
data = expand.grid(participantID = 1:100, condition = c('Familiar', 'Unfamiliar'), trial = 1:5, stringsAsFactors = FALSE)

data = mutate(data,
              emotionValencecenter = round(runif(n = nrow(data), min = -4, max = 4)),
              z = -5 + emotionValencecenter + emotionValencecenter^2 + -5*emotionValencecenter*(condition=='Familiar'),
              pr = 1/(1+exp(z)),
              MEAMreport = rbinom(nrow(data), 1, pr))



ggplot(data = data, aes(x = factor(emotionValencecenter), y = MEAMreport, color = condition)) +
    stat_summary()



m <- glmer(data=data, MEAMreport ~ poly(emotionValencecenter, 2, raw=TRUE)*condition + (emotionValencecenter*condition|participantID),
          family = binomial(link = 'logit'))
summary(m)

# same as summary()
emtrends(m, ~ condition, var = "emotionValencecenter", at = list(emotionValencecenter = 0), max.degree = 2, delta.var = 8/1000)

# does not match summary()
emtrends(m, ~ condition, var = "emotionValencecenter")


ggplot(fiber, aes(x = diameter, y = strength)) + 
    geom_point()





fiber$diameter_z = scale(fiber$diameter)
fiber$l = rbinom(nrow(fiber), 13, 0.5)

fiber.lm <- lm(strength ~ poly(diameter_z,2,raw = TRUE)*machine + (diameter_z | l), data=fiber)
summary(fiber.lm)

# same as summary()
emtrends(fiber.lm, ~ machine, var = "diameter_z", at = list(diameter_z = 0))

# does not match summary()
emtrends(fiber.lm, ~ machine, var = "diameter_z")


ggplot(fiber, aes(x = diameter, y = strength)) + 
    geom_point()
