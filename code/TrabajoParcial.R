#seteo environment
setwd("D:/JOSHUA/R/Trabajo Parcial") 

#librerias usadas
library(scales)
library(ggplot2)
library(dplyr)
library(lubridate)

#cargamos el csv con nombre de columnas y que las cadenas se consideren factors
df_bookings <- read.csv("D:/JOSHUA/R/Trabajo Parcial/hotel_bookings.csv", header = TRUE, stringsAsFactors = FALSE)

#creamos un dataframe donde haremos todos los cambios
df2 <- df_bookings

#mostramos el total de columnas
print(paste("Total de columnas: ", nrow(df_bookings)))
print("Nombre de columnas: ")
#el nombre de las columnas
colnames(df2)
#aplicamos la funcion clss para ver las clases de cada variable
sapply(df2, class)
#pequeño resumen estadistico de las variables
str(df2)
summary(df2)
        
#eliminamos las columnas que no vamos a necesitar para el analisis
df2 <- df2[, c(-2, -15, -16, -24, -25)]
colnames(df2)

#añadimos una columna que nos servira de identificador unico para fila
df2$indice <- 1:nrow(df2)

#funcion para ver las columnas con NA
sin_valor <- function(x){
  sum = 0
  for (i in 1:ncol(x)){
    #recorremos cada columna y sumamos los na en cada una
    cat("En la columna",colnames(x[i]),"total de valores NA:",colSums(is.na(x[i])),"\n")
  }
}
sin_valor(df2)

#vemos que valores tienen na en la columna que sus valores na eran mayor a 0
df2$indice[is.na(df2$children)]

#eliminamos las filas que contengan esos valores
df2<-df2[!(df2$indice==40601),]
df2<-df2[!(df2$indice==40668),]
df2<-df2[!(df2$indice==40680),]
df2<-df2[!(df2$indice==41161),]

#guardamos los valores que contienen null 
valores_con_null <- which(df2=="NULL",arr.ind = TRUE)
#los reemplazamos por NA
df2[valores_con_null]<-NA

#creamos una funcion para caluclar la moda
moda = getmode <- function(x){
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x,uniqv)))]
}

#reemplazamos los valores faltantes por la moda de la columna
moda_country = moda(df2$country)
df2$country[is.na(df2$country)] = moda_country


#coonvertimos los valores que son 1 a Yes y 0 a No
df2$is_repeated_guest <- as.factor(ifelse(df2$is_repeated_guest==1, "Yes", "No"))

#vemos las boxplots de las variables numericas y que necesiten reemplazar sus outliers
boxplot(df2$lead_time) 
boxplot(df2$days_in_waiting_list) 
boxplot(df2$adr) 

#reescalamos las columnas que lo necesitan usando la transofmracion lineal
#reescalamos a valores entre 0 y 1
df2$lead_time.rescales <- rescale((df2$lead_time))
df2$days_in_waiting_list.rescales <- rescale((df2$days_in_waiting_list))
df2$adr.rescales <- rescale((df2$adr))

#creamos una funcion que reemplaza los outliers para que esten dentro del rango
fix_outliers <- function(x, removeNA = TRUE){
  #Calculamos los quanz|tiles 1) por arriba del 5% y por debajo del 95%
  quantiles <- quantile(x, c(0.05, 0.95), na.rm = removeNA)
  x[x<quantiles[1]] <- mean(x, na.rm = removeNA)
  x[x>quantiles[2]] <- median(x, na.rm = removeNA)
  x
}

df2$lead_time.rescales<-(fix_outliers(df2$lead_time.rescales))
df2$adr.rescales<-(fix_outliers(df2$adr.rescales))

boxplot(df2$adr.rescales,main="adr")
boxplot(df2$lead_time.rescales,main="lead_time")

#analisis de consultas-------------------------------------------------------------------------------

#Pregunta a - ¿Cuántas reservas se realizan por tipo de hotel?
#separamos las reservas por hotel
df2.hotel1 <- df2[df2$hotel=="Resort Hotel",]
df2.hotel2 <- df2[df2$hotel=="City Hotel",]

#mostramos la suma de cada hotel
print(paste("Numero de reservas en Resort Hotel: ", nrow(df2.hotel1)))
print(paste("Numero de reservas en City Hotel: ", nrow(df2.hotel2)))

#Pregunta b - ¿Está aumentando la demanda con el tiempo?
#separamos las reservas por año
table(df2$arrival_date_year)
#mostramos el grafico 
barplot(table(df2$arrival_date_year), main="Reservas por Año",names=c("2015","2016","2017"))

#Pregunta c - ¿Cuando se producen las temporadasd de reservas: alta, media y baja?
# Calculamos la cantidad de reservas por mes
reservas_por_mes <- aggregate(df2$stays_in_week_nights + df2$stays_in_weekend_nights, by = list(Month = df2$arrival_date_month),FUN = sum)

# Creamos un índice para ordenar los meses correctamente
meses_ordenados<- factor(reservas_por_mes$Month, levels = unique(reservas_por_mes$Month))

# Ordenamos los datos por mes
reservas_por_mes <- reservas_por_mes[order(meses_ordenados), ]

# gráfico de barras para la visualización
barplot(reservas_por_mes$x, 
        names.arg = reservas_por_mes$Month,
        col = "skyblue",
        main = "Cantidad de Reservas por Mes",
        xlab = "Mes",
        ylab = "Cantidad de Reservas")

# Identificamos las temporadas (alto y bajo)
alto <- quantile(reservas_por_mes$x, 0.75)  
media <-quantile(reservas_por_mes$x, 0.50)
bajo <- quantile(reservas_por_mes$x, 0.25)  

# Marcamos las temporadas en el gráfico
abline(h = alto, col = "red", lwd = 2, lty = 2)
abline(h = media, col = "black", lwd = 2, lty = 2)
abline(h = bajo, col = "blue", lwd = 2, lty = 2)

# se crea la leyenda
legend("topright", legend = c("Alta", "Media", "Baja"), col = c("red", "black", "blue"), lwd = 2, lty = 2)

# Calculamos los cuartiles
quartiles <- quantile(reservas_por_mes$x, probs = c(0.25, 0.5, 0.75))

# Identificamos las temporadas (alta, media y baja)
bajo <- min(reservas_por_mes$x)
medio <- quartiles[2]
alto <- quartiles[3]

# Identificamos los meses correspondientes a cada temporada
temporada_baja <- reservas_por_mes$Month[reservas_por_mes$x < medio]
temporada_media <- reservas_por_mes$Month[reservas_por_mes$x >= medio & reservas_por_mes$x < alto]
temporada_alta <- reservas_por_mes$Month[reservas_por_mes$x >= alto]

# Mostramos los resultados
print("Temporada baja:")
print(temporada_baja)
print("Temporada media:")
print(temporada_media)
print("Temporada alta:")
print(temporada_alta)


#Pregunta d - ¿Cuando es menor la demanda de reservas?
# Calculamos la cantidad total de reservas
total_reservas <- sum(reservas_por_mes$x)

# Calculamos el porcentaje de reservas para cada mes
porcentaje_reservas <- (reservas_por_mes$x / total_reservas) * 100
# Creamos un vector con los porcentajes de demanda para cada mes
porcentajes <- porcentaje_reservas

# Identificamos el mes de menor demanda (temporada baja)
menor_demanda <- reservas_por_mes$Month[which.min(porcentajes)]
porcentajemenor <- min(porcentajes)

# Creamos un gráfico de línea para mostrar la demanda a lo largo de los meses
plot(porcentajes, type = "o", col = "gray", pch = 16,
     main = "Porcentaje de Demanda por Mes", 
     xlab = "Mes", 
     ylab = "Porcentaje de Demanda",
     xlim = c(0, length(porcentajes) + 1),
     ylim = c(0, max(porcentajes) + 10))

# Resaltamos el mes de menor demanda con un punto y un texto
points(which.min(porcentajes), porcentajemenor, col = "blue", pch = 16)
text(which.min(porcentajes), porcentajemenor + 1, 
     paste0(round(porcentajemenor, 1), "%"), col = "blue", pos = 3)

# hacemos la leyenda
legend("topright", legend = paste("Mes de menor demanda:", menor_demanda), fill = "blue", title = "Temporada Baja", cex = 0.8)

# Calculamos el número de reservas para el porcentaje señalado
num_demanda <- (porcentajemenor / 100) * total_reservas

# Mostramos el mes de menor demanda y el número de reservas correspondientes
print("Mes de menor demanda:")
print(paste(menor_demanda, "-", round(porcentajemenor, 1), "%"))
print("Número de reservas correspondientes a este porcentaje es:")
print(round(num_demanda))


#PRegunta e - ¿Cuántas reservas incluyen niños/bebés?
# Creamos un nuevo dataframe con los datos necesarios
datos_grafico <- data.frame(
  Categoria = c("Con Niños o Bebés", "Sin Niños ni Bebés"),
  Cantidad = c(sum(df2$children > 0 | df2$babies > 0),
               sum(df2$children == 0 & df2$babies == 0))
)

#Graficamos la cantidad de reservas por niños
ggplot(datos_grafico, aes(x = Categoria, y = Cantidad, fill = Categoria)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Reservas con Niños o Bebés",
    y = "Número de Reservas",
    fill = "Categoría"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

#Pregunta f - ¿Es importante contar con espacios de estacionamiento?
#Saber si necesita un parqueo
df2$required_car_parking_spaces <- as.numeric(df2$required_car_parking_spaces)
df2$required_car_parking_spaces <- as.factor(ifelse(df2$required_car_parking_spaces>=1, "Yes", "No"))


barplot(table(df2$required_car_parking_spaces), main="Cuantas personas necesitan parking",  xlab="Respuestas",  ylab="Cantidad",col=c("red","green"),names=c("No necesitan","Si necesitan"))

#Pregunta g - ¿En qué meses del año se producen más cancelaciones de reserva?  
#Convertimos la columna a tipo date
df2$reservation_status_date<- as.Date(df2$reservation_status_date)

# Extrae el mes de cada fecha de cancelación
df2 <- df2 %>%
  mutate(mes_cancelacion = month(reservation_status_date))

# Realiza el conteo de cancelaciones por mes
cancelaciones_por_mes <- df2 %>%
  group_by(mes_cancelacion) %>%
  summarize(total_cancelaciones = n())

# Ordena los resultados por el total de cancelaciones en orden descendente
cancelaciones_por_mes <- cancelaciones_por_mes %>%
  arrange(desc(total_cancelaciones))

# Imprime el resultado
print(cancelaciones_por_mes)

ggplot(cancelaciones_por_mes, aes(x = factor(mes_cancelacion), y = total_cancelaciones, fill = factor(mes_cancelacion))) +
  geom_bar(stat = "identity") +
  labs(x = "Mes de Cancelación", y = "Total de Cancelaciones") +
  scale_x_discrete(labels = month.name) + # Etiquetas de los meses
  theme_minimal() + # Estilo del gráfico
  ggtitle("Cancelaciones de Reserva por Mes") + # Título del gráfico
  scale_fill_discrete(name = "Mes de Cancelación") # Cambiar la leyenda de colores

#guardamos el dataset que nos quedo en un csv
write.csv(df2,'hootel_bookings_df_limpio',row.names = TRUE)
