# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2022-05-24')
tuesdata <- tidytuesdayR::tt_load(2022, week = 21)

sevens <- tuesdata$sevens
fifteens <- tuesdata$fifteens


# Libraries ---------------------------------------------------------------


library(tidyverse)
library(ggplot2)
  


# Start -------------------------------------------------------------------


quince <- fifteens %>% select(date) %>% 
  mutate(year = as.numeric(format(date, '%Y'))) %>% 
  group_by(year) %>% 
  count(year) %>% 
  ungroup() %>% mutate(tipo="Fifteens")
  
siete <-   sevens %>% select(date) %>% 
  mutate(year= as.numeric(format(date, '%Y'))) %>% 
  group_by(year) %>% 
  count(year) %>% 
  mutate(tipo="Sevens") %>% 
  ungroup()

base <- siete %>% full_join(quince) 

tidytuesday21 <- base %>% ggplot(mapping = aes(x=year, y=n, color=tipo))+
  geom_line(aes(linetype=as.factor(tipo)))+
  facet_wrap( ~tipo, nrow = 2, scales = "free")+
  scale_x_continuous(n.breaks = 20)  +
  scale_y_continuous(n.breaks = 10)+
  labs(title = "Count of games per year of variants of rugby along the time"
       , caption = "Elaboration: Aracely Heredia", x = "", 
       y = "Number of games per year", color = "Type of game")

ggsave(plot = tidytuesday21, filename = "tidytuesday21.png", width = 	8*4496/2400,height = 4*4496/2400)



  
