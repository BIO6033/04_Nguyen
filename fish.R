library(ggplot2)
library(tidyverse)
library(magrittr)
library(brms)

#https://www.kaggle.com/aungpyaeap/fish-market

fish <- read_csv("Fish.csv")
glimpse(fish)

fish %>% 
  ggplot(aes(x = Height, y = Weight)) + geom_point() +
  facet_wrap(~Species)

names(fish)

fish %<>% #v assigned to fish without rewriting it
  mutate(v = (Height/2)^2)
glimpse(fish)


fish_regression <- lm(Weight ~ v, data=fish)
summary(fish_regression)

fish %>% 
  mutate(sci_predict = predict(fish_regression, type = "response")) %>% 
  ggplot(aes(x = v, y = Weight)) + geom_point() + 
  geom_line(aes(y = sci_predict)) +
  facet_wrap(~Species)



fish_regression_h <- lm(Weight ~ I((Height/2)^2), data=fish)
summary(fish_regression_h)

fish %>% 
  mutate(sci_predict = predict(fish_regression_h, type = "response")) %>% 
  ggplot(aes(x = Height, y = Weight)) + geom_point() + 
  geom_line(aes(y = sci_predict)) 

fishtubes <- bf(Weight ~ a*(Height/2)^2,
   a~1 + (1|Species),
   nl = TRUE)

get_prior(fishtubes, data = fish)
