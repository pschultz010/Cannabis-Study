# Pete Schultz

library(performance)
library(see)
library(tidyverse)
library(dplyr)
library(magrittr)
library(olsrr)
library(car)
library(broom) 
library(kableExtra)
library(scales)
library(gridExtra)
library(stargazer)
library(webshot)
library(performance)

strains <- read_csv("strains.csv")

strains %>% ggplot(aes(x=thc, y=rating)) +
  geom_point() + geom_jitter() +
  geom_smooth(method=lm, se=TRUE) +
  labs(x = "THC", y = "Rating")

# rating per type
strains$type <- factor(strains$type)
strains %>% filter(type != 'Edible') %>% ggplot(aes(x = type, y = rating)) + 
  geom_boxplot()

table(strains$type)

strains %>% filter(type != 'Edible') %>% ggplot(aes(x=type, y=calming_to_energizing)) +
  geom_boxplot() +
  geom_smooth(method=lm, se=TRUE) +
  labs(x = "Type", y = "Calm to Energy")


