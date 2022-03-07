# Activo las bibliotecas necesarias
library(data.table)
library(tidyverse)
library(lubridate)

vacunas<- DATOS_GUILLERMO_1_1_final %>% 
  select(Fecha,Departamento,Localidad,Dosis,Vacuna) %>% 
  filter(!Dosis==0)

names(vacunas)
class(vacunas$Dosis)

vacunas_conteo<- vacunas %>%
  select(Fecha,Dosis,Vacuna) %>% 
  group_by(Fecha,Vacuna) %>% 
  summarise(dosis=sum(Dosis))

#comprobando datos -------------------------------------------------------

#Total dosis
sum(vacunas$Dosis) #2704314

#dosis sputnik
sum(vacunas_conteo$dosis[vacunas_conteo$Vacuna=="SPUTNIK"]) #343851

sput<- vacunas %>% 
  filter(Vacuna=="SPUTNIK") %>% 
  select(Fecha,Dosis) %>% 
  mutate(Fecha = ymd(Fecha)) 

%>% 
  summarise(total=sum(Dosis[Vacuna=="SPUTNIK"])) #343851

unique(vacunas$Vacuna)

class(vacunas_conteo$Fecha)
vacunas_conteo$Fecha<- as.Date(vacunas_conteo$Fecha)

# GRAFICANDO --------------------------------------------------------------
ggplot(data = vacunas_conteo , aes(x = Fecha , y = dosis )) +
  geom_line(aes(color = vacunas_conteo$Vacuna ), linetype = "solid", size = .7) +
  geom_point(aes(color = vacunas_conteo$Vacuna)) +
  labs(x = "Fecha de envio", 
       y = "Cantidad de dosis",
       title = "Vacunas COVID",
       subtitle= "Enero 2021 - Diciembre 2021",
       caption= "Fuente Ministerio de Salud de ER")+
theme_minimal()+
theme(axis.title.y = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 8), 
      plot.title = element_text(hjust = 1, size = 16, face = "bold.italic")) +
  scale_x_date(date_breaks="1 months", limits = as.Date(c("2021-01-01","2021-12-31")), date_labels = "%b") +
  scale_y_continuous(limits = c(0,40500),
                     breaks = c(seq(0,40500,by=2500)))


ggplot(data = vacunas_conteo , aes(x = Fecha , y = dosis )) +
 geom_hi(aes(color = vacunas_conteo$Vacuna))+
  labs(x = "Fecha de envio", 
       y = "Cantidad de dosis",
       title = "Vacunas COVID",
       subtitle= "Enero 2021 - Diciembre 2021",
       caption= "Fuente Ministerio de Salud de ER")+
  theme_minimal()+
  theme(axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 8), 
        plot.title = element_text(hjust = 1, size = 16, face = "bold.italic")) +
  scale_x_date(date_breaks="1 months", limits = as.Date(c("2021-01-01","2021-12-31")), date_labels = "%b") +
  scale_y_continuous(limits = c(0,40500),
                     breaks = c(seq(0,40500,by=2500)))




