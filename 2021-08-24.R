
# Load packages, set wd, read data ----------------------------------------
library(tidyverse)
library(magrittr)
library(here)
library(summarytools)
library(ggpubr)
library(patchwork)
library(rstatix)
library(psych)
library(knitr)

root_proj_path <- here()
setwd(root_proj_path)

lemurs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')


# Create figure with lemur weight and birth type --------------------------
lemurs %>% 
  select(birth_type, birth_institution) %>% 
  freq()

# Examine only lemurs that are wild-born
lemurs %>% 
  filter(birth_type == "WB") %>% 
  freq(birth_institution)

# Examine weight of lemurs
lemurs %>% 
  freq(weight_g)

# ANOVA: y = weight_g, x = birth_type
lemurs %>% 
  anova_test(weight_g ~ birth_type,
             detailed = TRUE,
             effect.size = "pes") # F(2, 82606) = 749.241, p < .001

p <- lemurs %>% 
  ggplot(., aes(x = birth_type, y = weight_g)) +
  scale_y_continuous(name = "Weight (g)") + 
  scale_x_discrete(name = "", labels=c("Captive born",
                                                 "Wild-born",
                                                 "Unknown")) +
  theme_bw() +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)

annotate_figure(p,
                top = text_grob("Mean weight of lemurs by birth type",
                                face = "bold",
                                size = 16),
                bottom = text_grob("Data: Duke Lemur Center | Plot: @sarakyeo",
                                   hjust = 1, x = 1, size = 10))

