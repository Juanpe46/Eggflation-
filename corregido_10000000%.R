install.packages("knitr")
library(readxl)
library(tidyverse)
library(ggplot2)
library(dslabs)
library(ggthemes)
library(lubridate)
library(knitr)

#importando la data
data <- read_excel("D:/Usuario/Slade/OneDrive/Escritorio/CARLOS/Universidad/Semestres/Semestre 8/Electiva Introduccion a la Ciencia de Datos/Entrega Final/DATA/Copia de data_frame.xlsx")

#ordenando la data

#Eliminando todos los NA
data <- data %>% 
  filter(!is.na(...3)) %>% 
  .[-1,] %>% 
  select(-1)

#Cambiando la columna de año por un repetido del año por cada mes 
#(12 veces 2018, 12 veces 2019...)
data$ano <- rep(c(2018:2022), each = 12)

#Se reajusta la columna de año
data <- data %>% 
  relocate(ano) 


#se ajustan los nombres para mejor comprensión
names(data) <- c("ano", "mes", "huevo12", "diesel", "maiz", "electricidad")

# cambiando las clases de los datos 
data$ano <- as.numeric(data$ano)
data$huevo12 <- as.numeric(data$huevo12)
data$diesel <- as.numeric(data$diesel)
data$maiz <- as.numeric(data$maiz)
data$electricidad <- as.numeric(data$electricidad)


#redondeando 
data <- data %>% 
  mutate_if(is.numeric,
            round,
            digits = 3)


#uniendo ano y mes
data <- data %>%
  mutate(fecha = seq(as.Date("2018-01-01"), as.Date("2022-12-01"), by = "month"))

data<- data %>% 
  relocate(fecha)

head(data)


## Analisis grafico de los Datos:


# GRAFICO 1. PRECIO DEL HUEVO 2018-2022

data %>% 
  ggplot(aes(x = fecha, y = huevo12))+
  geom_line(color = "red", size = 1.5)+
  ggtitle("Precio docena de huevos 2018-2022")+
  xlab("Años")+
  ylab("Precio Huevos de Gallina")+
  theme_classic()+
  theme(plot.title = element_text(color = "black", size = 17, face = "bold", hjust = 0.5),
        axis.title = element_text(color = "black", size = 16),
        axis.text = element_text(color = "black", size = 14))

# GRAFICO 2. PRECIO DEL HUEVO + FACTORES QUE AFECTAN EL COSTO DEL MISMO

data2 <- data %>% 
  mutate(maiz_kg = 10*data$maiz/1000, electricidad_10kw = 10*data$electricidad)

data2 %>%
  ggplot(aes(x = fecha)) +
  geom_line(aes(y = huevo12, color = "Huevos"), size = 1.5) +
  geom_line(aes(y = diesel, color = "Diesel"), size = 1) +
  geom_line(aes(y = maiz_kg, color = "maiz_kg"), size = 1) +
  geom_line(aes(y = electricidad_10kw, color = "electricidad_10kw"), size = 1) + 
  labs(title = "Costos de producción del huevo vs precio del huevo 2018-22",
       x = "Años",
       y = "Precios $",
       color = "Variables") +
  scale_color_manual(values = c("green", "blue", "red", "purple")) +
  theme_economist_white()+
  theme(plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5),
        axis.title = element_text(color = "black", size = 12),
        axis.text = element_text(color = "black", size = 10))

# GRAFICA 3

pollo <- read_excel("D:/Usuario/Slade/OneDrive/Escritorio/CARLOS/Universidad/Semestres/Semestre 8/Electiva Introduccion a la Ciencia de Datos/Entrega Final/DATA/pollo.xls")

names(pollo) <- c("fecha", "pollo")

class(pollo$fecha)
class(data2$fecha)

pollo$fecha <- as.Date(pollo$fecha)



DF_completo <- merge(data2, pollo, by = "fecha", all.x = TRUE)




DF_completo %>%
  ggplot(aes(x = fecha)) +
  geom_line(aes(y = huevo12, color = "Huevos"), size = 1.5) +
  geom_line(aes(y = pollo, color = "Pollo"), size = 1) +
  labs(title = "Precio Huevos, Pollo (USA) 2018-2023",
       x = "Años",
       y = "Precios $",
       color = "Variable") +
  scale_color_manual(values = c("Huevos" = "red", "Pollo" = "blue")) +  
  theme_classic() + 
  theme(plot.title = element_text(color = "black", size = 17, face = "bold", hjust = 0.5),
        axis.title = element_text(color = "black", size = 16),
        axis.text = element_text(color = "black", size = 14))


# REGRESION LINEAL

rm(list = ls)
variables_explicativas <- c("diesel", "maiz", "electricidad")

datos_regresion2 <- DF_completo[, c(variables_explicativas, "huevo12")]

#  modelo de regresion lineal
modelo_def2 <- lm(huevo12 ~ diesel + maiz + electricidad, data = datos_regresion2)

# resumen del modelo
summary(modelo_def2)

#-----------------
# SEGUNDA OPCION REGRESION

DF_completo$dhuevo12 <- c(0, diff(DF_completo$huevo12))
DF_completo$ddiesel <- c(0, diff(DF_completo$diesel))
DF_completo$dmaiz <- c(0, diff(DF_completo$maiz))
DF_completo$delectricidad <- c(0, diff(DF_completo$electricidad))

variables_explicativas <- c("ddiesel", "dmaiz", "delectricidad")

datos_regresion3 <- DF_completo[, c(variables_explicativas, "dhuevo12")]

#  modelo de regresion lineal
modelo_def3 <- lm(dhuevo12 ~ ddiesel + dmaiz + delectricidad, data = datos_regresion3)

# resumen del modelo
summary(modelo_def3)

#--------------
# TERCERA OPCION REGRESION

DF_completo$ddhuevo12 <- c(0,0, diff(diff(DF_completo$huevo12)))
DF_completo$dddiesel <- c(0,0, diff(diff(DF_completo$diesel)))
DF_completo$ddmaiz <- c(0,0, diff(diff(DF_completo$maiz)))
DF_completo$ddelectricidad <- c(0,0, diff(diff(DF_completo$electricidad)))

variables_explicativas <- c("dddiesel", "ddmaiz", "ddelectricidad")

datos_regresion4 <- DF_completo[, c(variables_explicativas, "ddhuevo12")]

#  modelo de regresion lineal
modelo_def4 <- lm(ddhuevo12 ~ dddiesel + ddmaiz + ddelectricidad, data = datos_regresion4)

# resumen del modelo
summary(modelo_def4)

# GRAFICA DE LA REGRESION 

# GRAFICA DIESEL
ggplot(datos_regresion2, aes(x = diesel, y = huevo12)) +
  geom_point() +                    
  geom_smooth(method = "lm", se = FALSE, color = "blue") +   
  labs(title = "Regresion Lineal Diesel",
       x = "Precio Promedio Diesel",
       y = "Precio Promedio Huevos")

# GRAFICA MAIZ
ggplot(datos_regresion2, aes(x = maiz, y = huevo12)) +
  geom_point() +                    
  geom_smooth(method = "lm", se = FALSE, color = "red") +   
  labs(title = "Regresion Lineal Maiz",
       x = "Precio Promedio Maiz",
       y = "Precio Promedio Huevos")

#GRAFICA ELECTRICIDAD
ggplot(datos_regresion2, aes(x = electricidad, y = huevo12)) +
  geom_point() +                    
  geom_smooth(method = "lm", se = FALSE, color = "blue") +   
  labs(title = "Regresion Lineal Electricidad",
       x = "Precio Promedio Electricidad/Kw",
       y = "Precio Promedio Huevos")

