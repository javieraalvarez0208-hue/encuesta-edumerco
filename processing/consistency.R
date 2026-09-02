# 0. Identification ---------------------------------------------------

# Title: Consistency code of EDUMERCO data
# Institution: EDUMER
# Responsible: Researcher

# Executive Summary: This script contains the code to consistency analyses of the EDUMERCO data
# Date: February 6, 2025

# 1. Packages  -----------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjmisc, 
               here,
               sjlabelled,
               psych)


options(scipen=999)
rm(list = ls())


# 2. Data -----------------------------------------------------------------

load(here("input/data/original/completas-270125.RData"))

glimpse(data)

# 3. Processing -----------------------------------------------------------

# select ----

db <- data %>% 
  select(screen_age, screen_education) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as_tibble()

# recode and transform ----

frq(db$screen_education)

db$screen_education <- sjlabelled::set_labels(db$screen_education, 
                       labels = c("Básica incompleta" = 1, 
                                  "Básica completa" = 2,
                                  "Media incompleta" = 3, 
                                  "Media completa" = 4, 
                                  "Técnica superior incompleta" = 5, 
                                  "Técnica superior completa" = 6,
                                  "Universitaria incompleta" = 7, 
                                  "Universitaria completa" = 8,
                                  "Estudios de posgrado (magíster o doctorado)" = 9))


frq(db$screen_age) # ok

# 4. Consistency ----------------------------------------------------------

db %>% 
  filter(screen_age <= 24) %>% 
  frq(screen_education) # no hay menores de 24 con estudios de posgrado

db %>% 
  filter(screen_age <= 21) %>% 
  frq(screen_education) # hay 12 casos de 18 a 21 años con universitaria completa

db$screen_education[db$screen_education == 8 & db$screen_age <= 21] <- NA

psych::describeBy(db$screen_age, group = db$screen_education)

# 5. Save and export ------------------------------------------------------

save(db, file = here("input/data/proc/bbdd_edumerco.RData"))
