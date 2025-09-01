library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)
library(scales)

base <- read_excel("base_final1.xlsx")

#-------------------Grafica de barras---------------------#
base <- base %>%
  rename(municipio = Municipio.x)
base_muni <- base %>% #Creamos otra base para que quede más facil hacer la grafica
  group_by(municipio) %>%
  summarise(internet = mean(internet, na.rm = TRUE)) %>%
  arrange(desc(internet)) %>%
  mutate(municipio = factor(municipio, levels = municipio))


grafico_barras <- ggplot(base_muni, aes(x = municipio, y = internet)) +
  geom_col(fill = "steelblue") +
  labs(title = "Proporción de acceso a internet por municipio",
       x = "Municipio", y = "Proporción de acceso a internet") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


#-------------------Grafico de dispersion-------------------------#


# Graficar
grafico_dispersion <- ggplot(base, aes(x = internet, y = poblacion, label = municipio)) +
  geom_point(color = "steelblue", size = 5, alpha = 0.7) +  # puntos
  geom_text(vjust = -0.8, size = 3) + # etiquetas con nombre de municipio
  labs(
    x = "Proporción de internet (%)",
    y = "Población",
    title = "Municipios por conectividad y población"
  ) +
  scale_y_continuous(labels = comma) +   # Para que muestre los numeros
  theme_minimal()
  
#--------------------Guardar--------------------------#

ggsave("grafico_dispersion.png", plot = grafico_dispersion, width = 8, height = 6, dpi = 300)
ggsave("grafico_barras.png", plot = grafico_barras, width = 8, height = 6, dpi = 300)
