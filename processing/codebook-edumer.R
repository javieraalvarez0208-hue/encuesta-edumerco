#*******************************************************************************************************************
#
# 0. Identification ---------------------------------------------------
# Title: Data preparation and code book for EDUMER Survey Wave 1
# Author: Andreas Laffert            
# Overview: EDUMER Survey Wave 1         
# Date: 13-066-2024            
#
#******************************************************************************************************************

# 1. Packages ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjlabelled, 
               sjmisc, 
               rio,
               here, 
               codebook)

options(scipen=999)
rm(list = ls())

# 2. Data --------------------------------------------------------------

db <- rio::import(file = here("input","data", "original", "310524_BDD_edumer.sav")) %>% 
  as_tibble()

names(db)

db_or_label <- sjlabelled::get_label(db)

# 3. Processing -----------------------------------------------------------

# 3.1 Students data -------------------------------------------------------

# Select ----
db_st <- db[,c(1:105)] %>% 
  select(-starts_with("T_d")) # only students variables and eliminate names



# Colnames ----
names(db_st)

db_st <- db_st %>% 
  rename(id_estudiante = SbjNum,
         fecha = Date,
         consentimiento = ACEPTA,
         nivel_estudiante = Nivel_def,
         asignacion = aleatorio)

names(db_st)
glimpse(db_st)

names(which(colSums(is.na(db_st)) > 0))

# Manipulation ----

## ID 
frq(db_st$id_estudiante)
get_label(db_st$id_estudiante)
db_st$id_estudiante <- sjlabelled::set_label(db_st$id_estudiante, label = "Identificador único estudiante")

## Fecha
frq(db_st$fecha)
class(db_st$fecha)
db_st$fecha <- lubridate::as_datetime(db_st$fecha)
get_label(db_st$fecha)

## Consentimiento
frq(db_st$consentimiento)
get_label(db_st$consentimiento)
db_st$consentimiento <- sjlabelled::set_label(db_st$consentimiento, label = "Autorización adulto para participación de menor de edad en encuesta")
get_labels(db_st$consentimiento)

## d2
frq(db_st$d2)
get_label(db_st$d2)
db_st$d2 <- sjlabelled::set_label(db_st$d2, label = "Establecimiento eduacional")
get_labels(db_st$d2)

## d3_def
frq(db_st$d3_def)

db_st$d3_def <- factor(tolower(db_st$d3_def), 
                       levels = c("6a", "6b", "6c", "7a", "7b", 
                                  "1a", "1b", "1c", "2a", "2b", "2c"),
                       labels = c("6a", "6b", "6c", "7a", "7b", 
                                  "1a", "1b", "1c", "2a", "2b", "2c"))

db_st$d3_def <- sjlabelled::set_label(db_st$d3_def, label = "Curso al que pertenece")

## nivel_estudiante
frq(db_st$nivel_estudiante)


