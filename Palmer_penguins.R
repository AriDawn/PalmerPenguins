## Dataset come from penguins in palmerpenguins package. 
## The dataset contains different body measurement of three species of penguins: Chintrap, Gentoo and Adelie.
## These penguins come from three islands in the Palmer Archipelago, Antarctica

## loading packages
library(tidyverse)
library(palmerpenguins)
library(janitor)
library(ggpubr)

##Looking at the data before analysis
head(penguins)
str(penguins)

## Cleaning data to prepare for analysis
## rename all columns of penguins data to lowercase
## Clean name is use to clean the data so that they only left with numbers, letters and underscores
penguins1 <- rename_with(penguins, tolower)

penguins_clean <- clean_names(penguins1) %>% 
  drop_na()

## Checking number of sex for each species to see if there is bias in the data
penguins_clean %>% 
  group_by(species) %>%
  count(sex)


## Finding the mean, max and min of bill and flipper length grouping them by species
penguins_clean %>% 
  group_by(species) %>% 
  summarise(mean_bill_length_mm = mean(bill_length_mm), 
            max_bill_length_mm = max(bill_length_mm), 
            min_bill_length_mm = min(bill_length_mm))

penguins_clean %>% 
  group_by(species) %>% 
  summarise(mean_flipper_length_mm = mean(flipper_length_mm), 
            max_flipper_length_mm = max(flipper_length_mm), 
            min_flipper_length_mm = min(flipper_length_mm))


## Interestingly only the species Adelie are in the three island Biscoe, Dream and Torgersen.
## we will see if island location affect the size of penguins
penguins_clean %>% 
  filter(species == 'Adelie') %>% 
  group_by(island) %>% 
  summarise(mean_body_mass_g = mean(body_mass_g))


## visualization

ggplot(data=penguins_clean) +
  geom_bar(mapping = aes(x = species, fill = species)) +
  labs(title = "Number of Penguins for Different Species", y = "number of penguins",
       caption = "Data collected by Dr. Kristen Gorman")


ggplot(data=penguins_clean, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species)) +
  geom_smooth(method = "lm") +
  stat_regline_equation(label.y=5700, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y=5500, aes(label = ..rr.label..)) +
  labs(title = "Palmer Penguins: Body Mass Vs Flipper Length", 
       subtitle = "sample of three penguin species",
       caption = "Data collected by Dr. Kristen Gorman")


ggplot(data=penguins_clean, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species)) +
  geom_smooth(method = "lm") +
  facet_wrap(~species) +
  stat_regline_equation(label.y=5700, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y=5500, aes(label = ..rr.label..))+
  labs(title = "Palmer Penguins: Body Mass Vs Flipper Length", 
       subtitle = "sample of three penguin species",
       caption = "Data collected by Dr. Kristen Gorman")


ggplot(data=penguins_clean, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species)) +
  geom_smooth(method = "lm") +
  facet_wrap(~sex) +
  stat_regline_equation(label.y=5700, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y=5500, aes(label = ..rr.label..))+
  labs(title = "Palmer Penguins: Body Mass Vs Flipper Length", 
       subtitle = "sample of three penguin species seperated by sex",
       caption = "Data collected by Dr. Kristen Gorman")