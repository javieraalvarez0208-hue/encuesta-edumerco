# 0. Preparacion encuesta edumerco

# 1. cargar librerias ---------------------------------------------------------

#install.packages("pacman")
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)


# 2. cargar bbdd --------------------------------------------------------------
rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica


load("input/data/original/completas-270125.RData")
names(data)

# 3. seleccionar variables ----------------------------------------------------

proc_data <- data %>% select(starts_with("screen"), starts_with("carac"), starts_with("merit"), starts_with("des"))
                                                     
# 4. procesamiento de variables -----------------------------------------------

frq(proc_data$screen_sex) # 4 9 preferiría no responder
frq(proc_data$screen_education)
hist(proc_data$screen_age)


### a. descriptivo basico ----
frq(proc_data$merit_01) #Casos perdidos:
  #5 no sabe tiene 122 casos y 6 preferiria no responder 36 casos. 

### b. recodificacion ----

proc_data$carac_a04 <- recode(proc_data$carac_a04, "c(6,7)=NA")
proc_data$carac_a05 <- recode(proc_data$carac_a05, "c(9,10)=NA")
proc_data$carac_a06 <- recode(proc_data$carac_a06, "c(7,8)=NA")
proc_data$carac_a07 <- recode(proc_data$carac_a07, "c(4,5)=NA")
proc_data$carac_a08 <- recode(proc_data$carac_a08, "c(8,9)=NA")
proc_data$carac_a09 <- recode(proc_data$carac_a09, "c(4,5)=NA")

proc_data <- proc_data %>%
  mutate(across(c(carac_a13, carac_a14,carac_a15,carac_a16), 
                ~ recode(., "3=NA; 4=NA")))

sjmisc::frq(proc_data$carac_a13)

proc_data <- proc_data %>%
  mutate(across(starts_with("merit_"), ~ recode(., "5=NA; 6=NA")))

proc_data <- proc_data %>%
  mutate(across(c(des_01, des_02, des_03, des_07, des_08, des_09, des_10, des_11, des_12), 
                ~ recode(., "5=NA; 6=NA")))



### c. etiqutamiento ----
proc_data$merit_01 <- set_label(x = proc_data$merit_01,label = 
                                           "En Chile, las personas son recompensadas por sus esfuerzos")



### c. otros ajustes ----
proc_data$merit_02 <- set_label(x = proc_data$merit_02,label = 
                                                     "En Chile, las personas son recompensadas por su inteligencia y habilidad")


### c. otros ajustes ----
proc_data$merit_10 <- set_label(x = proc_data$merit_10,label = 
                                                     "En Chile, todas las personas obtienen lo que merecen")

# PERCEPCION FACTORES NO-MERITOCRACTICOS ---------------------------------------

### c. otros ajustes ----
proc_data$merit_09 <- set_label(x = proc_data$merit_09,label = 
                                                     "En Chile, todas las personas tienen las mismas oportunidades para salir adelante")

### c. otros ajustes ----
proc_data$merit_04 <- set_label(x = proc_data$merit_04,label = 
                                                              "En Chile, a quienes tienen buenos contactos les va mucho mejor en la vida")

### c. otros ajustes ----
proc_data$merit_03 <- set_label(x = proc_data$merit_03,label = 
                                                                 "En Chile, a quienes tienen padres ricos les va mucho mejor en la vida")

# PREFERENCIA MERITO SOCIAL ----------------------------------------------------

### c. otros ajustes ----
proc_data$merit_05 <- set_label(x = proc_data$merit_05,label = 
                                                 "Quienes más se esfuerzan deberían obtener mayores recompensas que quienes se esfuerzan menos")

### c. otros ajustes ----
proc_data$merit_06 <- set_label(x = proc_data$merit_06,label = 
                                                 "Quienes poseen más talento deberían obtener mayores recompensas que quienes poseen menos talento")


proc_data$social_merit_pref_ES <- set_label(x = proc_data$social_merit_pref_ES,label = 
                                                "Está bien que las personas más inteligentes y/o talentosas ganen más dinero, aun cuando requieran esforzarse menos para ello ")


#PREFERENCIA FACTORES NO MERITOCRÁTICOS ----------------------------------------

proc_data$merit_07 <- set_label(x = proc_data$merit_07,label = 
                                               "Está bien que quienes tienen padres ricos les vaya bien en la vida")


proc_data$merit_08 <- set_label(x = proc_data$merit_08,label = 
                                               "Está bien que quienes tienen buenos contactos les vaya bien en la vida")


# DESIGUALDAD ------------------------------------------------------------------

## just_educ ----

### a. descriptivo basico ----
frq(proc_data$des_01) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 3 casos, 99 preferiria no responder 0 casos.

### a. descriptivo basico ----
frq(proc_data$des_02) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 3 casos, 99 preferiria no responder 2 casos


### a. descriptivo basico ----
#frq(proc_data$just_pensiones) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 2 casos, 99 preferiria no responder 1 caso


# SOCIODEMOGRAFICOS ------------------------------------------------------------

## genero_ES ----

### a. descriptivo basico ----
frq(proc_data$screen_sex) #muestra con una mayoria de muejeres 48.52% y 
  #categoria otro: 4.52%. No tiene casos perdidos
proc_data$screen_sex <- recode(proc_data$screen_sex, "c(3,4)=NA")

### b. recodificacion ----
proc_data$screen_sex <- factor(proc_data$screen_sex, 
                                           levels=c(1,2),
                                           labels=c("Hombre","Mujer"))
frq(proc_data$screen_sex)


## libros_hogar ----
### a, descriptivo basico
frq(proc_data$carac_a06)

### b. recodificación ----
proc_data$carac_a06 <- recode(proc_data$carac_a06, "1=1;2=1; 3=2; 4=2;5=2;6=2")

proc_data$carac_a06 <- factor(proc_data$carac_a06, 
                                 levels=c(1,2),
                                 labels=c("Menos de 25 libros","Más de 25 libros"))

sjmisc::frq(proc_data$carac_a06)
### c. otros ajustes ---- 
sjmisc::frq(proc_data$screen_education)
proc_data$screen_education <- recode(proc_data$screen_education, "1=1; 2=1; 3=1; 4=1; 5=2; 6=2; 7=3; 8=3; 9=3")

proc_data$screen_education <- factor(proc_data$screen_education, 
                              levels=c(1,2,3),
                              labels=c("Educación media o menos", "Educación técnica superior","Educación universitaria o más"))


# 5. base procesada -----------------------------------------------------------
proc_data <-as.data.frame(proc_data)
stargazer(proc_data, type="text")

save(proc_data,file = "input/data/proc/data_proc.RData")
