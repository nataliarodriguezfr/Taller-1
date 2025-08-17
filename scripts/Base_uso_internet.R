library(writexl)
library("haven")
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)



setwd("C:\\Users\\natal\\OneDrive\\Documentos")


tenderos <- read_dta("TenderosFU03_Publica.dta")
poblacion <- read_excel("TerriData_Dim2_Sub3.xlsx")

poblacion <- poblacion[(poblacion$Año==2024),] #Filtramos por año
poblacion <- poblacion %>%
  filter(`Unidad de Medida` %in% c("Hombres", "Mujeres")) #Filtramos por hombres y mujeres
poblacion$`Dato Numérico` <- as.numeric(gsub(",", ".", gsub("\\.", "", poblacion$`Dato Numérico`))) #Quitamos puntos y comas


#1. Tarea 1
datos_internet_municipio <- tenderos %>%
  group_by(Munic_Dept) %>%
  summarise( Municipio =first(Municipio), uso_internet = (mean(uso_internet, na.rm = TRUE)*100))

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

act_ancho <- tenderos %>%
  group_by(Municipio) %>%
  summarise(across(actG1:actG11, sum, na.rm = TRUE)) 
act_long <- act_ancho %>%
  pivot_longer(cols = starts_with("actG"),
               names_to = "actividad") %>%
  mutate(actividad = str_remove(actividad, "actG")) %>%
  mutate(actividad = as.integer(actividad)) %>%
  left_join(tabla_actividades, by = "actividad")

# Tarea 3
actividad <- datos_internet_municipio %>%
  left_join(act_long, by = c("Municipio"))
actividad <- actividad %>%
  select(Munic_Dept, Municipio, actG = actividad, actividad = nombre_actividad, internet = uso_internet )

#Tarea 4
poblacion_agg <- poblacion%>%
  mutate(codigo = as.numeric(`Código Entidad`)) %>%
  mutate(Municipio = as.character(Entidad)) %>%
  group_by(codigo, Municipio) %>%
  summarise(poblacion = sum(`Dato Numérico`))

#Tarea 5

#Merge actividad con poblacion_agg

actividad <- actividad %>% #Para que no saque error por diferente formato
  mutate(Munic_Dept = as.character(Munic_Dept))

poblacion_agg <- poblacion_agg %>%
  mutate(codigo = as.character(codigo))

base_final1 <- actividad %>%
  inner_join(poblacion_agg, by = c("Munic_Dept" = "codigo"))


#Tarea 5.2
arch1_opcion1 <- tenderos %>%
  group_by(Municipio) %>%
  summarise(total_tiendas = sum(actG1, na.rm = TRUE))  # actG1 es "Tienda"

arch2_opcion1 <- tenderos %>%
  filter(uso_internet == 1) %>%        # solo tiendas con internet
  group_by(Municipio) %>%
  summarise(tiendas_internet = sum(actG1, na.rm = TRUE)) %>%
  rename(Municipio = Municipio)

base_final_opcion1 <- inner_join(arch1_opcion1, arch2_opcion1, by = "Municipio")

base_final_opcion1 <- base_final_opcion1 %>%
  mutate(proporcion_internet = (tiendas_internet / total_tiendas)*100)


#Tarea 5.3

datos_clave <- tenderos %>% #Mantener variables clave
  mutate(Munic_Dept = as.character(Munic_Dept)) %>%
  select(Munic_Dept, Municipio, uso_internet, actG1:actG11)

datos_long <- datos_clave %>% #Pasar actividades de ancho a largo
  pivot_longer(cols = starts_with("actG"),
               names_to = "actividad",
               values_to = "valor") %>%
  filter(valor == 1)   # Solo tiendas que tienen esa actividad

tabla_actividades_opcion2 <- tibble( #Ponemos nombres a las actividades
  actividad = paste0("actG", 1:11),
  nombre_actividad = c(
    "Tienda",
    "Comida preparada",
    "Peluquería y belleza",
    "Ropa",
    "Otras variedades",
    "Papelería y comunicaciones",
    "Vida nocturna",
    "Productos bajo inventario",
    "Salud",
    "Servicios",
    "Ferretería y afines"
  )
)
datos_long <- datos_long %>% 
  left_join(tabla_actividades_opcion2, by = "actividad")


datos_long <- datos_long %>% #Agrupamos por municipios y actividad
  group_by(Munic_Dept,Municipio, nombre_actividad) %>%
  summarise(
    total = n(),
    con_internet = sum(uso_internet, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(prop_internet = (con_internet / total)*100)

poblacion_agg_opcion2 <- poblacion_agg %>% #Merge con poblacion
  mutate(codigo = as.character(codigo)) %>%
  select(codigo, poblacion)
base_final_opcion2 <- datos_long %>%
  left_join(poblacion_agg_opcion2, by = c("Munic_Dept" = "codigo"))

#--------------------Pasar a excel--------------------#

write_xlsx(base_final1, "C:/Users/natal/OneDrive - Universidad del rosario/Universidad/Segundo Semestre/Haciendo economía/Rstudio/Descargas/base_final1.xlsx")

write_xlsx(base_final_opcion1, "C:/Users/natal/OneDrive - Universidad del rosario/Universidad/Segundo Semestre/Haciendo economía/Rstudio/Descargas/base_final_opcion1.xlsx")

write_xlsx(base_final_opcion2, "C:/Users/natal/OneDrive - Universidad del rosario/Universidad/Segundo Semestre/Haciendo economía/Rstudio/Descargas/base_final_opcion2.xlsx")

