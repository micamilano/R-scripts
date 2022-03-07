#librarys
library(eph)
library(tidyverse)

#limpiar ambiente de trabajo
rm(list = ls())


#Cargar tabla de individuos y hogares
df_eph <- get_microdata(year = 2020, # especificar que año descargar 
                        trimester = 4, #Trimestre
                        type = "individual") %>% #Tipo de encuenta
          as.data.frame()

hogares <- get_microdata(year = 2020, #Año 
                         trimester = 4, #Trimestre
                         type = "hogar") %>% #Tipo
         as.data.frame()


hogares_seleccion<- hogares %>% 
  select(ANO4, AGLOMERADO,PONDERA, IV6, IV7, IV12_2, IX_TOT,IX_MEN10, IX_MAYEQ10) %>% 
  filter(AGLOMERADO==6| AGLOMERADO==5)


# Etiquetas a los valores
hogares_seleccion <- hogares_seleccion %>% 
  organize_labels(type = "hogar") #Pegamos las etiquetas

#filtrado
hogares_pna<- hogares_seleccion %>% 
  filter(AGLOMERADO==6)

hogares_agua<- hogares_pna %>% 
  select(PONDERA,IV6,IV7,IV12_2)

unique(hogares_agua$IV6 )
unique(hogares_agua$IV7 )

sum(hogares_agua$PONDERA)
# 99266 personas encuestadas


hogares_agua<- hogares_agua %>% 
  summarise(total_encuesta =sum(PONDERA),
            dentro= sum(PONDERA[IV6==1]),
            fuera= sum(PONDERA[IV6==3]),
            publica= sum(PONDERA[IV7==1])
  )
hogares_agua
#total_encuesta  dentro_casa  fuera   red publica
#         99266  98541         725    97997

#comprobar resultados
hogares_pub<- hogares_agua %>% 
  filter(IV7==1) %>% 
  summarise(total=sum(PONDERA))

hogares_pub  
#  total
#  97997


