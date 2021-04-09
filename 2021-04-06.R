
# Functions ---------------------------------------------------------------
print_descr <- function(x){
  min = min(x, na.rm = TRUE)
  max = max(x, na.rm = TRUE)
  mean = mean(x, na.rm = TRUE)
  stdev = sd(x, na.rm = TRUE)
  print(tibble("min" = min,
               "max" = max,
               "M" = mean,
               "SD" = stdev))
}


# Load packages and data --------------------------------------------------
library(tidyverse)
library(magrittr)
library(descr)
library(rstatix)
library(here)

root_proj_path <- here()
setwd(root_proj_path)

raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/vegetable_oil.csv")

# Attempt to tidy data for MYS and USA --------------------------------------------
mys <- raw %>% 
  filter(., code == "MYS") # selects only data for MYS

mys %>% 
  drop_na(production) %>% 
  count(crop_oil, wt = production)

p.my <- mys %>% 
  filter(., crop_oil == c("Palm", "Palm kernel", "Coconut (copra)")) %>% 
  ggplot(., aes(x = year, y = production)) +
  geom_point(aes(color = crop_oil))

usa <- raw %>% 
  filter(., code == "USA")

usa %>% 
  drop_na(production) %>% 
  count(crop_oil, wt = production)

p.usa <- usa %>% 
  filter(., crop_oil == c("Soybean","Maize", "Cottonseed")) %>%
  ggplot(., aes(x = year, y = production, color = crop_oil)) +
  geom_point()
