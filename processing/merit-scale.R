# 0. Identification ---------------------------------------------------

# Title: Merit-scale code of EDUMERCO data
# Institution: EDUMER
# Responsible: Researcher

# Executive Summary: This script contains the code for evaluate de merit scale in EDUMERCO data
# Date: February 6, 2025

# 1. Packages  -----------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjmisc, 
               sjPlot,
               here,
               lavaan,
               psych,
               corrplot,
               ggdist,
               patchwork,
               sjlabelled,
               semTools,
               gtools,
               RColorBrewer,
               skimr,
               naniar)


options(scipen=999)
rm(list = ls())


# 2. Data -----------------------------------------------------------------

load(here("input/data/original/completas-270125.RData"))

glimpse(data)

# 3. Processing -----------------------------------------------------------

# select ----

db <- data %>% 
  select(perc_effort = merit_01, 
         perc_talent = merit_02, 
         perc_rich_parents = merit_03,
         perc_contact = merit_04, 
         pref_effort = merit_05, 
         pref_talent = merit_06, 
         pref_rich_parents = merit_07, 
         pref_contact = merit_08) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as_tibble()

# recode and transform ----

frq(db$perc_effort)
frq(db$perc_talent)
frq(db$perc_rich_parents)
frq(db$perc_contact)

frq(db$pref_effort)
frq(db$pref_talent)
frq(db$pref_rich_parents)
frq(db$pref_contact)

labels1 <- c("Muy en desacuerdo" = 1, 
             "En desacuerdo" = 2, 
             "De acuerdo" = 3, 
             "Muy de acuerdo" = 4, 
             "No sabe" = 5, 
             "No responde" = 6)

db <- db %>% 
  mutate_at(.vars = (1:8),.funs = ~ sjlabelled::set_labels(., labels = labels1))

db <- db %>% 
  mutate(
    across(
      .cols = -c(id),
      .fns = ~ set_na(., na = c(5,6))
    )
  )

# missings ----

colSums(is.na(db))

prop_miss(db)*100

miss_var_summary(db)

miss_var_table(db)

miss_case_table(db)

vis_miss(db) + theme(axis.text.x = element_text(angle=80))

db <- na.omit(db)

# 4. Analysis -------------------------------------------------------------


# descriptive

t1 <- db %>% 
  select(-id) %>% 
  skim() %>% 
  yank("numeric") %>% 
  as_tibble() %>% 
  mutate(range = paste0("(",p0,"-",p100,")")) %>% 
  mutate_if(.predicate = is.numeric, .funs = ~ round(.,2)) %>% 
  select("Variable" = skim_variable,"Mean"= mean, "SD"=sd, "Range" = range, "Histogram"=hist) 

t1 %>% 
  kableExtra::kable(format = "markdown")

theme_set(theme_ggdist())
colors <- RColorBrewer::brewer.pal(n = 4, name = "RdBu")

a <- db %>% 
  select(starts_with("perc")) %>% 
  sjPlot::plot_likert(geom.colors = colors,
                      title = c("a. Percepciones"),
                      geom.size = 0.8,
                      axis.labels = c("Esfuerzo", "Talento", "Padres ricos", "Contactos"),
                      catcount = 4,
                      values  =  "sum.outside",
                      reverse.colors = F,
                      reverse.scale = T,
                      show.n = FALSE,
                      show.prc.sign = T
  ) +
  ggplot2::theme(legend.position = "none")

b <- db %>% 
  select(starts_with("pref")) %>% 
  sjPlot::plot_likert(geom.colors = colors,
                      title = c("b. Preferencias"),
                      geom.size = 0.8,
                      axis.labels = c("Esfuerzo", "Talento", "Padres ricos", "Contactos"),
                      catcount = 4,
                      values  =  "sum.outside",
                      reverse.colors = F,
                      reverse.scale = T,
                      show.n = FALSE,
                      show.prc.sign = T
  ) +
  ggplot2::theme(legend.position = "bottom")

likerplot <- a / b + plot_annotation(caption = paste0("Fuente: Elaboración propia en base a Encuesta EDUMERCO"," (n = ",dim(db)[1],")"
))

likerplot

# correlations

M <- psych::polychoric(db[c(1:8)])

diag(M$rho) <- NA

rownames(M$rho) <- c("A. Percepción Esfuerzo",
                     "B. Percepción Talento",
                     "C. Percepción Padres Ricos",
                     "D. Percepción Contactos",
                     "E. Preferencias Esfuerzo",
                     "F. Preferencias Talento",
                     "G. Preferencias Padres Ricos",
                     "H. Preferencias Contactos")

#set Column names of the matrix
colnames(M$rho) <-c("(A)", "(B)","(C)","(D)","(E)","(F)","(G)",
                    "(H)")

testp <- cor.mtest(M$rho, conf.level = 0.95)

#Plot the matrix using corrplot
corrplot::corrplot(M$rho,
                   method = "color",
                   addCoef.col = "black",
                   type = "upper",
                   tl.col = "black",
                   col = colorRampPalette(c("#E16462", "white", "#0D0887"))(12),
                   bg = "white",
                   na.label = "-") 


# cfa

model_cfa <- '
  perc_merit = ~ perc_effort + perc_talent
  perc_nmerit = ~ perc_rich_parents + perc_contact
  pref_merit = ~ pref_effort + pref_talent
  pref_nmerit = ~ pref_rich_parents + pref_contact
  '

m1 <- cfa(model = model_cfa, 
          data = db[,c(1:8)],
          estimator = "DWLS",
          ordered = T,
          std.lv = F) 

summary(m1, fit.measures = T, standardized = T) # works!

fitmeasures(m1, output = "matrix")[c("chisq","pvalue","df","cfi","tli",
                                        "rmsea","rmsea.ci.lower","rmsea.ci.upper",
                                        "srmr"),]
