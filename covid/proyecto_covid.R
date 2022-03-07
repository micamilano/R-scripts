#------------------PROYECTO COVID------------------

# Clean enviroment 
#rm(list = ls())

#LIBRARYS
  # install.packages(c("tidyverse", "eph", 
  #                 "funModeling", "readxl"))
  #install.packages("readr")
  #install.packages('dplyr')
  #install.packages('sf')
  #install.packages('Rcpp')
  library(readr)
  library(tidyverse)
  library(sf)
  library(readxl)


# Objetive -------------------------------------------------------------------
  # contagios de mujeres vs varones en CABA 
  # Objetener la tasa de contagios de COVID cada 100 mil habitantes entre los barrios de la provincia de Buenos Aires


#Carga de datos ---------------------------------------------------------------

#Cargamos archivo CSV de los datos de COVID en CABA y lo asignamos a la variable casos_covid_CABA
casos_covid_CABA <- read_csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/casos-covid-19/casos_covid19.csv")

#Observamos los campos de la tabla 
str(casos_covid_CABA)

#Cargamos archivo CSV con informacion poblacional por barrio y lo asignamos a variable poblacion_barrios
poblacion_barrios <- read.csv("https://raw.githubusercontent.com/flavioscargiali/desembarcandorensociales/master/barrios_poblacion%20-%20Hoja%201.csv", encoding = "UTF-8")
view(poblacion_barrios)


#Procesamiento de datos--------------------------------------------------------
# extraer casos + por barrio, comuna, genero, edad y clasificacion

unique(casos_covid_CABA$clasificacion)
unique(casos_covid_CABA$genero)
unique(casos_covid_CABA$barrio)


casos_confirmados <- casos_covid_CABA %>%
  #Filtramos datos por condicion 'Confirmado'
  filter(clasificacion=="confirmado") %>% 
  # Limpiamos datos NA
  filter(!is.na(genero)) %>% 
  #Agrupamos por comuna y genero
  group_by(barrio,genero) %>% 
  #Sumamos los casos por comuna y genero
  summarise(casos = n()) %>% 
  #Convertimos el resultado en una tabla
  as.data.frame()

#Ver rdo.
view(casos_confirmados)

# Transfromamor la tabla --------------------REVISAR SI SIRVE ESTO
casos_confirmados<- casos_confirmados %>% 
  pivot_wider(names_from = genero,
              values_from = casos) %>% 
  mutate(total_pob= femenino+masculino) %>% 
  mutate(porc_F = (femenino/total_pob)*100) %>% 
  mutate(porc_M = (masculino/total_pob)*100)

casos_confirmados$porc_F<-as.integer(casos_confirmados$porc_F)
casos_confirmados$porc_M<-as.integer(casos_confirmados$porc_M)



#Union de tablas casos_confrimados + poblacion_barrios 
barrios_pob_casos <- poblacion_barrios %>% 
  left_join(casos_confirmados)

view(barrios_pob_casos)


#Calculamos la tasa de contagios cada 100 mil habitantes por barrio
barrios_pob_casos <-barrios_pob_casos %>%
  #Calculamos la tasa
  mutate(tasa_100mil = (casos/Total) *100000) %>% 
  # Reemplazamos los valores perdidos (NA)
  mutate(tasa_100mil = replace_na(tasa_100mil,0)) 

# Imprimimos los casos
view(barrios_pob_casos)


# Creando mapa  -------------------------------------------------------------

#Creamos un objeto con los datos geograficos
barrios <- st_read("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson")
view(barrios)

#Unimos la tabla con los objetos geograficos
barrios_pob_casos <- barrios %>% 
  left_join(barrios_pob_casos, by=c("BARRIO"="barrio"))

view(barrios_pob_casos)

# Transformamos datos de variable COMUNA(character) a numeric
class(barrios_pob_casos$COMUNA)
barrios_pob_casos$COMUNA<- as.numeric(barrios_pob_casos$COMUNA) 

# Mapa de los barrios de la ciudad
ggplot()+
  geom_sf(data=barrios_pob_casos)

# Creamos objeto para poblacion FEMENINA ---------------------------------
barrios_pob_F <- barrios_pob_casos %>% 
  filter(genero == 'femenino')

# Mapa comparando la tasa de contagios de poblacion femenina 
# cada 100 mil habitantes en la ciudad
ggplot() +
  geom_sf(data = barrios_pob_F, aes(fill = tasa_100mil), color = NA)+
  scale_fill_distiller("Contagios pob. femenina c/100 mil hab.", palette = "Reds", direction = 1)



# Creamos el objeto comunas con la informacion espacial----------------------
comunas <- st_read("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/comunas/CABA_comunas.geojson")
class(comunas$COMUNAS)

# aplicamos transfromacion a los datos de variable COMUNA(character) a numeric
comunas$COMUNAS<- as.numeric(comunas$COMUNAS) 
#-----------------------------------------------------------------------------


# Armamos el mapa de tasa de contagios COVID en pob. femenina covid c/ 100 mil 
#habitantes por barrio y comuna de la CABA
ggplot() +
  geom_sf(data=barrios_pob_casos, aes(fill = tasa_100mil),color=NA) +
  geom_sf(data=comunas,color="black", fill=NA,size=0.5)+
  geom_sf_label(data = comunas, aes(label=COMUNAS),color="black", fill="white") +
  scale_fill_distiller("Contagios c/100 mil hab.",palette = "Reds",direction = 1) +
  labs(title = "Pob. femenina positiva de COVID en CABA", 
       y=NULL, x=NULL, 
       caption = "Fuente https://cdn.buenosaires.gob.ar/datosabiertos/") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size = 9, face = "italic"))

# Guardamos como .jpeg
ggsave(filename = "mapa_f_positivios_covid_caba.jpeg")



# Creamos objeto para poblacion MASCULINA ---------------------------------
barrios_pob_M <- barrios_pob_casos %>% 
  filter(genero == 'masculino')

# Mapa comparando la tasa de contagios de poblacion masculina 
# cada 100 mil habitantes en la ciudad
ggplot() +
  geom_sf(data = barrios_pob_M, aes(fill = tasa_100mil), color = NA)+
  scale_fill_distiller("Contagios pob. masculina c/100 mil hab.", palette = "green", direction = 1)

# Armamos el mapa de tasa de contagios COVID en pob. masculina covid c/ 100 mil 
#habitantes por barrio y comuna de la CABA
ggplot() +
  geom_sf(data=barrios_pob_casos, aes(fill = tasa_100mil),color=NA) +
  geom_sf(data=comunas,color="black", fill=NA,size=0.5)+
  geom_sf_label(data = comunas, aes(label=COMUNAS),color="black", fill="white") +
  scale_fill_distiller("Contagios c/100 mil hab.",palette = "green",direction = 1) +
  labs(title = "Pob. masculina positiva de COVID en CABA", 
       y=NULL, x=NULL, 
       caption = "Fuente https://cdn.buenosaires.gob.ar/datosabiertos/") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size = 9, face = "italic"))

# Guardamos como .jpeg
ggsave(filename = "mapa_m_positivios_covid_caba.jpeg")


