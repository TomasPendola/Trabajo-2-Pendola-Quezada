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
  select(sexo_enc,
         edad_enc,
         c05_12,
         c06_01,
         c06_02,
         c06_03,
         c15,
         c16,
         t10)




ELSOC <- ELSOC %>%
  mutate(
    sexo_enc = case_when(
      sexo_enc == "Masculino" ~ 1,
      sexo_enc == "Femenino" ~ 2))


ELSOC <- ELSOC %>% select(Sexo=sexo_enc, Edad=edad_enc, Medios=c05_12, UDI=c06_01, DemocraciaCrist=c06_02, Comunistas=c06_03, PosicionPolitica=c15, Partido=c16, SeguridadBarrio=t10)



## Remover NA's
ELSOC <- ELSOC %>% 
  set_na(., na = c(-666,-777,-888,-999)) %>% 
  na.omit()


# Tabla de Correlacion
sjPlot::tab_corr(ELSOC, triangle = "lower")


M <- cor(ELSOC, use = "complete.obs")
M
corrplot.mixed(M)


sjPlot::plot_scatter(ELSOC, PosicionPolitica, SeguridadBarrio)



# Alpha de Combrach 

escala_PosicionPolitica<- ELSOC %>%
  select(PosicionPolitica, Medios)
resultado_alpha <- alpha(escala_PosicionPolitica)
resultado_alpha$total$raw_alpha


#Escala + Grafico
ELSOC <- ELSOC %>%
  mutate(escala_PosicionPolitica = (PosicionPolitica + Medios) / 2)


summary(ELSOC$escala_PosicionPolitica)


hist(ELSOC$escala_PosicionPolitica,
     main = "Escala de Confianza en los Medios de Comunicacion segun Posicion Politca",
     xlab = "Promedio de respuestas (1 = Nada, 5 = Mucha)",
     col = "#A8D5BA", breaks = 10)
