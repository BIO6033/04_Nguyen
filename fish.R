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
   a ~ 1 + (1|Species),
   nl = TRUE)

get_prior(fishtubes, data = fish)

fish_prior <- c(
  prior(cauchy(0,2), class = "sigma"),
  prior(normal(2,2), class = "b", coef = "Intercept", nlpar = "a"),
  prior(cauchy(0,2), class = "sd", nlpar = "a")
  )

ft_model_prior <- brm(fishtubes, data = fish, prior = fish_prior, sample_prior = "only")

library(tidybayes)

fish %>% 
  add_predicted_draws(ft_model_prior, n = 5) %>% 
  ggplot(aes(x = Height, y = .prediction)) + 
  geom_point(alpha = 0.4) + 
  facet_wrap(~.draw)


## fit the model for real
ft_model <- brm(fishtubes, data = fish, prior = fish_prior, sample_prior = "yes")

fake_fish <- expand_grid(Height = seq(min(fish$Height), to = max(fish$Height), length.out = 200),
                         Species = unique(fish$Species))

fake_fish %>% 
  add_predicted_draws(ft_model, n = 500) %>% 
  ggplot(aes(x = Height, y = .prediction)) + 
  stat_lineribbon() + 
  geom_point(aes(y = Weight), data = fish) + 
  scale_fill_brewer(palette = "Greens") +
  facet_wrap(~Species)


## the range of the x is so different between panels! 

narrow_range_fish <- fish %>% 
  group_by(Species) %>% 
  summarize(min = min(Height),
            max = max(Height)) %>% 
  mutate(Height = map2(min, max, ~ seq(from = .x, to = .y, length.out = 100))) %>% 
  unnest(Height)


narrower_range_predicted_fish <-  narrow_range_fish %>% 
  add_predicted_draws(ft_model, n = 400)

narrower_range_predicted_fish %>% 
  ggplot(aes(x = Height, y = .prediction)) + 
  stat_lineribbon() + 
  geom_point(aes(y = Weight), data = fish, pch = 21, fill = "Orange") + 
  scale_fill_brewer(palette = "Greens") +
  facet_wrap(~Species, scales = "free")

## not a bad model, but still improvements are possible! for example, negative
## predictions are very possible


# simplify code with functions! -------------------------------------------

plot_model_predictions <- function(model, df = narrow_range_fish){
  narrower_range_predicted_fish <-  df %>% 
    add_predicted_draws(model, n = 400)
  
  narrower_range_predicted_fish %>% 
    ggplot(aes(x = Height, y = .prediction)) + 
    stat_lineribbon() + 
    geom_point(aes(y = Weight), data = fish, pch = 21, fill = "Orange") + 
    scale_fill_brewer(palette = "Greens") +
    facet_wrap(~Species, scales = "free_y")
}




# gamma -------------------------------------------------------------------
fishtubes_gamma <- bf(Weight ~ a * (Height/2)^2,
                        a ~ 1 + (1|Species),
                        nl = TRUE, 
                        # specify the family -- NOTE THE LINK
                        family = Gamma(link = "identity")
)


get_prior(fishtubes_gamma, data = fish)

fish_prior_gamma <- c(
  prior(gamma(0.01,0.01),class="shape"),
  prior(normal(0, 0.5), class = "b", coef = "Intercept", nlpar = "a"),
  prior(normal(0,2), class = "sd", nlpar = "a")
)


#oops! there is one weight of zero!

fish_nozero <- fish %>% 
  filter(Weight > 0)
nrow(fish_nozero)
nrow(fish)

gamma_fish_fit <- brm(fishtubes_gamma, data = fish_nozero, prior = fish_prior_gamma, sample_prior = "yes")

plot_model_predictions(gamma_fish_fit) + 
  labs(y = "Weight (g)", x = "Height")

ggsave("fish_predictions_mixed.png", height = 7, width = 8)

# lognormal distribution --------------------------------------------------



fishtubes_lognorm <- bf(Weight ~ a * (Height/2)^2,
                a ~ 1 + (1|Species),
                nl = TRUE, 
                # specify the family
                family = lognormal(link = "identity")
                )

get_prior(fishtubes_lognorm, data = fish)

fish_prior_lnorm <- c(
  prior(exponential(7), class = "sigma"),
  prior(normal(0, 0.5), class = "b", coef = "Intercept", nlpar = "a"),
  prior(cauchy(0,2), class = "sd", nlpar = "a")
)


lognorm_model_prior <- brm(fishtubes_lognorm, data = fish_nozero, prior = fish_prior_lnorm, sample_prior = "only")

lognorm_model_fit <- brm(fishtubes_lognorm, data = fish_nozero, prior = fish_prior, sample_prior = "yes")

## not a great fit at all! 

plot_model_predictions(gamma_fish_fit)


narrower_range_predicted_fish <-  narrow_range_fish %>% 
  add_predicted_draws(lognorm_model_fit, n = 400)

narrower_range_predicted_fish %>% 
  ggplot(aes(x = Height, y = .prediction)) + 
  stat_lineribbon() + 
  geom_point(aes(y = Weight), data = fish, pch = 21, fill = "Orange") + 
  scale_fill_brewer(palette = "Greens") +
  facet_wrap(~Species, scales = "free") + 
  scale_y_log10()


# the model struggles a little ! There are a couple of things to try
