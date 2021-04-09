
# Load packages and data --------------------------------------------------
library(tidyverse)
library(magrittr)
library(descr)
library(rstatix)
library(here)

root_proj_path <- here()
setwd(root_proj_path)

raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/vegetable_oil.csv")

# Plot for Malaysia --------------------------------------------
mys <- raw %>% 
  filter(., code == "MYS") # selects only data for MYS

mys %<>%
  rowwise() %>% 
  mutate(prodmil = production/1e6)

mys %>% 
  drop_na(prodmil) %>% 
  count(crop_oil, wt = prodmil)

p.my <- mys %>%
  drop_na(prodmil) %>% 
  filter(., crop_oil == c("Palm", "Palm kernel", "Coconut (copra)")) %>% 
  ggplot(., aes(x = year, y = prodmil, color = crop_oil)) +
  geom_point() +
  theme_bw() +
  labs(x = "", y = "Oil production in million tonnes", color = "") +
  scale_x_continuous(breaks = seq(1961, 2014, 5),
                     labels = seq(1961, 2014, 5))


# Plot for USA ------------------------------------------------------------
usa <- raw %>% 
  filter(., code == "USA")

usa %<>% 
  rowwise() %>% 
  mutate(prodmil = production/1e6)

usa %>% 
  drop_na(prodmil) %>% 
  count(crop_oil, wt = prodmil)
  
p.usa <- usa %>%
  drop_na(prodmil) %>% 
  filter(., crop_oil == c("Soybean", "Maize", "Cottonseed")) %>% 
  ggplot(., aes(x = year, y = prodmil, color = crop_oil)) +
  geom_point() +
  theme_bw() +
  labs(x = "", y = "Oil production in million tonnes", color = "") +
  scale_x_continuous(breaks = seq(1961, 2014, 5),
                     labels = seq(1961, 2014, 5)) +
  scale_y_continuous(breaks = seq(0, 20, 5),
                     limits = c(0, 20),
                     labels = seq(0, 20, 5))

