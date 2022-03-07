# Actividad R con encuesta de consumos culturales calcular---------------------


#Librarys ---------------------------------------------------------------------
#install.packages(tidyverse)
#install.packages(readxl)
library(tidyverse)
library(readxl)

rm(list = ls())

# Encuesta de Consumos Culturales ---------------------------------------------
# No esta disponible para la descargar asique dejo el archivo adjunto
encuestaCC <- read_csv("UNTREF/datos/encc_2017.csv")
encuestaCC


#Filtramos los datos de interes para nuestro estudio
encuestaCC_filtro <- encuestaCC %>% 
  select(id, pondera_dem, region, sexo, edad, p5)

encuestaCC_filtro


# Que tipo de radio es escuchada por más personas, AM o FM? ------------------
# ¿Es igual en las regiones NOA y NEA? ---------------------------------------
      
unique(radio_N$p5)


radio_N<- encuestaCC_filtro %>% 
  # Filtrar region NOA y NEA
  filter(region == "NOA" | region=="NEA") %>% 
  # -> Limpiar datos NULL 
  filter(!is.na(p5)) %>% 
  # Agrupar por region y radio
  group_by(region) %>% 
  #Como estamos trabajando con una encuesta, para contar personas tenemos que 
  #sumar la variable de ponderacion -> pondera_dem
  summarise(oyentes_fm = sum(pondera_dem[p5=='FM']),
            oyentes_am =sum(pondera_dem[p5=='AM']),
           oyentes_ambas= sum(pondera_dem[p5=='AMBAS POR IGUAL']))
  
  radio_N
 
# ----------------------------------------------------------------------------

# La encuesta recoge informacion sobre diferentes consumos culturales. 
# Seleccionen aspecto de su interes y con lo incorporado hasta el momento:
    # Seleccionen las variables relevantes para ustedes.
    # Realicen algunn filtro para segmentar a la poblacion.
    # Realicen agrupacion y resumen de las variables de interes para describir el problema.




