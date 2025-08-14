library("haven")
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)

setwd("C:\\Users\\natal\\OneDrive\\Documentos")


datos <- read_dta("TenderosFU03_Publica.dta")
poblacion <- read_excel("TerriData_Dim2_Sub3.xlsx")

poblacion <- poblacion #Limpiamos la base
poblacion <- poblacion[(poblacion$Año==2024),] #Filtramos por año
poblacion <- poblacion %>%
  filter(`Unidad de Medida` %in% c("Hombres", "Mujeres")) #Filtramos por hombres y mujeres
poblacion$`Dato Numérico` <- as.numeric(gsub(",", ".", gsub("\\.", "", poblacion$`Dato Numérico`))) #Quitamos puntos y comas


#1. Tarea 1
datos_municipio <- datos %>%
  group_by(Munic_Dept) %>%
  summarise( Municipio =first(Municipio), uso_internet = mean(uso_internet, na.rm = TRUE)) 

#. Tarea 2

tabla_actividades <- tibble(
  actividad = 1:11,
  nombre_actividad = c(
    "Tienda",
    "Comida preparada",
    "Peluqueria y belleza",
    "Ropa",
    "Otras variedades",
    "Papeleria y comunicaciones",
    "Vida nocturna",
    "Productos bajo inventario",
    "Salud",
    "Servicios",
    "Ferreteria y afines"
  )
)

act <- datos %>%
  group_by(Municipio) %>%
  summarise(across(actG1:actG11, sum, na.rm = TRUE)) 
act_long <- act %>%
  pivot_longer(cols = starts_with("actG"),
               names_to = "actividad") %>%
  mutate(actividad = str_remove(actividad, "actG")) %>%
  mutate(actividad = as.integer(actividad)) %>%
  left_join(tabla_actividades, by = "actividad")

# Tarea 3
actividad <- datos_municipio %>%
  left_join(act_long, by = c("Municipio"))
actividad <- actividad %>%
  select(Munic_Dept, Municipio, actG = actividad, actividad = nombre_actividad, internet = uso_internet )

#Tarea 4
poblacion_agg <- poblacion%>%
  mutate(codigo = as.numeric(`Código Entidad`)) %>%
  group_by(codigo) %>%
  summarise(poblacion = sum(`Dato Numérico`))

#Tarea 5

#Merge actividad con poblacion_agg

actividad <- actividad %>% #Para que no saque error por diferente formato
  mutate(Munic_Dept = as.character(Munic_Dept))

poblacion_agg <- poblacion_agg %>%
  mutate(codigo = as.character(codigo))

base_final1 <- actividad %>%
  inner_join(poblacion_agg, by = c("Munic_Dept" = "codigo"))

base_final1

