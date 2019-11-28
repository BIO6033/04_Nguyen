library(ggplot2)
library(tidyverse)
library(magrittr)
library(lubridate)
library(mgcv)

#mise en situation: Je suis employé de la ville de MTL. Je vais utiliser les données d'utilisation
#de piste cyclable de 2018 prédire l'utilisation en 2019 pour l'allocation appropriée
#des ressources pour la saisons 2019


# lecture et nettoyage des données ----------------------------------------

velo <- read.csv("velo2017.csv")

velo_cc <- velo %>% 
  select(-X) %>% 
  gather(piste, nb, -Date) %>% 
  group_by(Date) %>% 
  summarise(sum = sum(nb, na.rm = TRUE)) %>% 
  mutate(mois = month(Date)) %>% 
  group_by(mois) %>% 
  summarise(total = sum(sum, na.rm = TRUE))
  
  #mutate(mois = as.numeric(substr(Date, start = 1, stop = 2)))

velo_cc

# visualisation graphique -------------------------------------------------

ggplot(velo_cc)+
  geom_bar(mapping = aes(x = mois, y = total), stat = "identity")


# courbe avec GAM ---------------------------------------------------------

mod_gam1 <- gam(total ~ s(mois, bs="cr"), data=velo_cc)
summary(mod_gam1)

velo_cc %>% 
  mutate(sci_predict = predict(mod_gam1, type = "response")) %>% 
  ggplot() + geom_bar(mapping = aes(x = mois, y = total), stat = "identity") + 
  geom_line(aes(x = mois, y = sci_predict))


# courbe avec GLM - poisson ---------------------------------------------------------

velo_glm <- glm(total ~ mois, data = velo_cc, family = "poisson")

velo_predict <- velo_cc %>% 
  mutate(new_y = predict(velo_glm, type = "response"),
         pred_y = rpois(length(new_y), new_y))

velo_cc %>% 
  ggplot() + 
  geom_bar(aes(x = mois, y = total), stat = "identity") +
  geom_line(aes(x = mois, y= pred_y), data = velo_predict, col = "green")

# courbe avec GLM - gaussian ---------------------------------------------------------

velo_glm <- glm(total ~ mois, data = velo_cc, family = "gaussian")

velo_predict <- velo_cc %>% 
  mutate(new_y = predict(velo_glm, type = "response"),
         pred_y = rnorm(length(new_y), new_y))

velo_cc %>% 
  ggplot() + 
  geom_bar(aes(x = mois, y = total), stat = "identity") +
  geom_line(aes(x = mois, y= pred_y), data = velo_predict, col = "green")

# Application de la courbe aux données de 2018 -------------------------------

velo_2018 <- read.csv("velo2018.csv")

velo_2018_cc <- velo_2018 %>% 
  select(-X) %>% 
  gather(piste, nb, -Date) %>% 
  group_by(Date) %>% 
  summarise(sum = sum(nb, na.rm = TRUE)) %>% 
  mutate(mois = month(Date)) %>% 
  group_by(mois) %>% 
  summarise(total = sum(sum, na.rm = TRUE))

velo_2018_cc

ggplot(velo_2018_cc)+
  geom_bar(mapping = aes(x = mois, y = total), stat = "identity")

velo_2018_cc %>% 
  mutate(sci_predict = predict(mod_gam1, type = "response")) %>% 
  ggplot() + geom_bar(mapping = aes(x = mois, y = total), stat = "identity") + 
  geom_line(aes(x = mois, y = sci_predict))

#ooh la courbe est vraiment off.... :(

#https://www.guru99.com/r-aggregate-function.html
