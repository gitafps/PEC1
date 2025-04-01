# Ejercicio 3: Análisis exploratorio de los datos

# En primera instancia exploraré la organización de los datos.
# Las dimensiones:
dim(assay(sumex))

# Los nombres de las variables y su estructura:
colnames(assay(sumex))
str(assay(sumex))

# Y las primeras filas:

head(assay(sumex))

# Luego, para obtener una primera idea de las variables
# y su distribución se puede hacer un resumen estadístico:

summary(assay(sumex))

# para añadir gráficos al análisis explorativo usaré el paquete ggplot2 & reshape2
library(ggplot2)
library(reshape2)

# para graficar los datos es más conveniente trabajar
# con un dataframe. Si no lo tuvieramos aún podríamos
# crearlo a partir del assay data del SummarizedExp.,
# pero como creamos el Sum.Exp. a partir de un dataframe,
# podemos simplemente usar los datos originales.

# podemos ver la distribución de los niveles de metabolitos
df <- melt(cachexia)
ggplot(df, aes(x = value)) +
  geom_histogram(bins = 10, fill = "yellow") +
  labs(x = "Nivel de metabolitos", y = "Frecuencia")

# podemos hacer lo mismo pero separado por grupos:
# primero (para facilitar el trabajo) asociaré las muestras 
# de los pacientes caquexicos y control a dos variables diferentes:
# (la condición del paciente se indica en la columna Muscle.loss)
cach <- assay(sumex)[, "Muscle.loss"] == "cachexic"
cont <- assay(sumex)[, "Muscle.loss"] == "control"

# crear subsets según el grupo
cach2 <- assay(sumex)[cach, colnames(assay(sumex)) != "Muscle.loss"]
cont2 <- assay(sumex)[cont, colnames(assay(sumex)) != "Muscle.loss"]

# preparar datos para el analisis como antes
dfcach <- as.data.frame(cach2)
dfcont <- as.data.frame(cont2)
dfcach2 <- melt(dfcach)
dfcont2 <- melt(dfcont)

# hacer gráficos
ggplot(dfcach2, aes(x = value)) +
  geom_histogram(bins = 10, fill = "orange") +
  labs(x = "Nivel de metabolitos", y = "Frecuencia")
ggplot(dfcont2, aes(x = value)) +
  geom_histogram(bins = 10, fill = "blue") +
  labs(x = "Nivel de metabolitos", y = "Frecuencia")
  
# podemos hacer un análisis comparativo entre los dos grupos 
# para los 10 primeros metabolitos (también se podría hacer
# para todos los metabolitos pero generaríamos demasiados plots).
# Así que estos primeros 10 servirán de ejemplo.
# Primero hay que crear un subset de los 10 primeros metabolitos
# y los datos correspondientes:

nombres_meta10 <- colnames(assay(sumex))[1:10]
meta10 <- sumex[, nombres_meta10]

# verificamos las dimensiones del subset
dim(assay(meta10))

# convertir en dataframe y preparar datos para análisis por grupo
dataf <- as.data.frame(assay(meta10))
df2 <- melt(dataf)
grupo2 <- df2$Muscle.loss

# hacer los plots de comparación entre los dos grupos
ggplot(df2, aes(x = grupo2, y = value, fill = grupo2)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Control vs. Caquexia",
       x = "Grupo", y = "Nivel de metabolito") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))