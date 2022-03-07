# TASA DE DESOCUPACION ----------------------------------

#librarys
library(eph)
library(tidyverse)

#limpiar ambiente de trabajo
rm(list = ls())


# Al barro! - Act. II -------------------------------

# 1. Cargen la Ãºltima EPH disponible en R
df_eph <- get_microdata(year = 2020, # especificar que año descargar 
                        trimester = 4, #Trimestre
                        type = "individual") %>% #Tipo de encuenta
  as.data.frame()

#2. Calculen la Tasa de DesocupaciÃ³n nacional

#Seleccion de variables y filtro de estado (Ocupados, desocupados, inactivos)
individuos <- df_eph %>% 
  select(REGION, PONDERA, CH04, ESTADO) %>% 
  filter(ESTADO == 1 | ESTADO == 2 | ESTADO == 3)

unique(individuos$ESTADO)

# Etiquetas a los valores
individuos <- individuos %>% 
  organize_labels(type = "individual") #Pegamos las etiquetas


individuos_p<- individuos %>% 
  #sumar el total de pob economicamente act
  summarise(PEA = sum(PONDERA[ESTADO==1 | ESTADO==2]),
            #sumar total desocupados
            desocupados=sum(PONDERA[ESTADO==3]),
            #calcular desocupados/pob activa*100 
            t_des= (desocupados/PEA)*100) 
individuos_p

#3. Comparen el resultado nacional con las regiones
individuos_region<- individuos %>% 
  group_by(REGION) %>% 
  summarise(PEA_r = sum(PONDERA[ESTADO==1 | ESTADO==2]),
            desocupados_r = sum(PONDERA[ESTADO==3]),
            t_desc_r=(desocupados_r/PEA_r)*100)

individuos_region

#4. Elijan un aglomerado a elecciÃ³n y calcule la tasa.
individuos_aglomerado <- df_eph %>% 
  select(AGLOMERADO, PONDERA, ESTADO) %>% 
  filter(ESTADO == 1 | ESTADO == 2 | ESTADO == 3) %>% 
  filter(AGLOMERADO==29)

desc_aglomerato<- individuos_aglomerado %>% 
  summarise(PEA_a = sum(PONDERA[ESTADO==1 | ESTADO==2]),
            #sumar total desocupados
            desocupados_a=sum(PONDERA[ESTADO==3]),
            #calcular desocupados/pob activa*100 
            t_des_aglom= (desocupados_a/PEA_a)*100) 

unique(individuos_aglomerado$ESTADO)

#5. Por Ãºltimo,escriban un parrafo resumiendo los puntos que considera sobresalientes. 
