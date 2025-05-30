pacman::p_load(tidyverse, #Conjunto de paquetes, sobre todo dplyr y ggplot2
               car, #Para recodificar
               haven,
               summarytools, #Para descriptivos
               sjmisc,
               psych,     # para Alfa de Chronbach
               dplyr, # Manipulacion datos
               sjPlot, # Tablas
               sjlabelled, #etiquetas
               kableExtra, #Tablas
               GGally, # Correlaciones
               corrplot, # Correlaciones
               psych) # Escalas

options(scipen = 999) # para desactivar notacion cientifica

rm(list = ls()) # para limpiar el entorno de trabajo



ELSOC_Long_2016_2023 <- read_dta("~/Trabajo-2-Pendola-Quezada/input/ELSOC_Long_2016_2023.dta")

ELSOC <- ELSOC_Long_2016_2023 %>% 
  filter(ola==6) %>%
  select(r13_ideol_01,
         c05_12,
         t10)


ELSOC_Limpia <- ELSOC %>% 
  set_na(., na = c(-666,-777,-888,-999)) %>% 
  na.omit()

ELSOC_Limpia <- ELSOC %>%
  mutate(
    r13_ideol_01 = as.numeric(r13_ideol_01),
    c05_12       = as.numeric(c05_12),
    t10          = as.numeric(t10))



ELSOC_Limpia$r13_ideol_01 <- car::recode(ELSOC_Limpia$r13_ideol_01, 
                                        "c(1,6)=0; c(2)=1; c(3)=2; c(4)=3; c(5)=4")

ELSOC_Limpia$c05_12 <- car::recode(ELSOC_Limpia$c05_12, 
                                         "c(1)=0; c(2)=1; c(3)=2; c(4)=3; c(5)=4")


ELSOC_Limpia$t10 <- car::recode(ELSOC_Limpia$t10, 
                                   "c(1)=0; c(2)=1; c(3)=2; c(4)=3; c(5)=4")


cor(ELSOC_Limpia)

psych::alpha(ELSOC_Limpia)

ELSOC_Limpia <- ELSOC_Limpia %>% 
  rowwise() %>% 
  mutate(MediosporPosicion = sum(r13_ideol_01,t10,c05_12))
summary(ELSOC_Limpia$MediosporPosicion)


hist(ELSOC_Limpia$MediosporPosicion,
     main = "Escala de Confianza en Medios segun Posicion Politica",
     xlab = "Promedio de respuestas (1 = No Confia, 5 = Confia Mucho)",
     col = "#607B8B", breaks = 5)

