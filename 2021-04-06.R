
# Load packages and data --------------------------------------------------
library(tidyverse)
library(magrittr)
library(descr)
library(rstatix)
library(here)
library(ggpubr)
library(wesanderson)

root_proj_path <- here()
setwd(root_proj_path)

raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/vegetable_oil.csv")

# Plot for Malaysia --------------------------------------------
mys <- raw %>% 
  filter(., code == "MYS") # selects only data for MYS

mys %<>%
  rowwise() %>% 
  mutate(prodmil = production/10^6)

mys %>% 
  drop_na(prodmil) %>% 
  count(crop_oil, wt = prodmil)

p.my <- mys %>%
  group_by(crop_oil) %>% 
  filter(., crop_oil == c("Palm", "Palm kernel", "Coconut (copra)")) %>% 
  ggplot(., aes(x = year, y = prodmil, color = crop_oil)) +
  geom_point() +
  scale_color_manual(values = wes_palette(name = "Cavalcanti1")) +
  theme_bw() +
  theme(legend.position = c(.2, .9)) +
  theme(legend.background = element_blank()) +
  labs(x = "", y = "Oil production in million tonnes", color = "",
       title = "Malaysia") +
  scale_x_continuous(breaks = seq(1961, 2014, 5),
                     labels = seq(1961, 2014, 5)) +
  scale_y_continuous(breaks = seq(0, 20, 5),
                     limits = c(0, 20),
                     labels = seq(0, 20, 5))


# Plot for USA ------------------------------------------------------------
usa <- raw %>% 
  filter(., code == "USA")

usa %<>% 
  rowwise() %>% 
  mutate(prodmil = production/10^6)

usa %>% 
  drop_na(prodmil) %>% 
  count(crop_oil, wt = prodmil)
  
p.usa <- usa %>%
  group_by(crop_oil) %>% 
  filter(., crop_oil == c("Soybean", "Maize", "Cottonseed")) %>% 
  ggplot(., aes(x = year, y = prodmil, color = crop_oil)) +
  geom_point() +
  theme_bw() +
  scale_color_manual(values = wes_palette(n = 3, name = "Darjeeling1")) +
  theme(legend.position = c(.2, .9)) +
  theme(legend.background = element_blank()) +
  labs(x = "", y = "", color = "", title = "USA") +
  scale_x_continuous(breaks = seq(1961, 2014, 5),
                     labels = seq(1961, 2014, 5)) +
  scale_y_continuous(breaks = seq(0, 20, 5),
                     limits = c(0, 20),
                     labels = seq(0, 20, 5))


# Arrange plots -----------------------------------------------------------
p <- ggarrange(p.my, p.usa,
          nrow = 1,
          ncol = 2)
## Need to learn how to use facets...  
  
annotate_figure(p,
                top = text_grob("Top three crops used in vegetable oil production in Malaysia and the US",
                face = "bold",
                size = 16),
                bottom = text_grob("Data: Our World in Data | Plot: @sarakyeo",
                                   hjust = 1, x = 1, size = 10))
