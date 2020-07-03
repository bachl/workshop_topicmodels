## ---- packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, stm, stminsights, tidytext, quanteda, lubridate, knitr, tictoc, furrr, oolong)
theme_set(theme_bw()) # ggplot theme

tibble(package = c("R", sort(pacman::p_loaded()))) %>% 
  mutate(version = map_chr(package, ~as.character(pacman::p_version(package = .x)))) %>% 
  knitr::kable()
