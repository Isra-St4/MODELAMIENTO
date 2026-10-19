
#Empiezo llamando a las librerias
library(ggplot2) 
library(dplyr) 
library(ggpubr) 

data<- read.csv("music.csv",header=T,dec=".", sep=",")
#Ahora voy a filtrar las músicas mas populares, Espotify usa una escala de popularidad  del 0-100 donde la categoria de 80-100 son considerado hits
hits<-data$popularity>=80
data1<-data[hits,]

#Como ya filtre las musicas mas populares creo un dataframe extrictamente con las variables a usar 
#Dataframe
df<- data.frame(Tempo=data1$tempo,Bailabilidad=data1$danceability)

#creado de df voy a revisar si hay datos faltantes 
summary(df)
#Aunq no hay datos faltantes se nota que tempo y bailabilidd en 0 como minimo son valores anómalos ya que una musica con 0 bpm no exixten o al menos no debrian esta ren el hit 
anomalos<-df%>% filter(Tempo==0|Bailabilidad==0)
nrow(anomalos)

#ahora como se observa una cancion tiene el valor de 0 
df1<-df%>% filter(Tempo!=0|Bailabilidad!=0)
nrow(df1) # ahor esta eliminado el valor anomalo
#verifico nuevamente el summary 
summary(df1)
#correlacion
cor(df1$Tempo,df1$Bailabilidad)
glimpse(df1)

#MODELO
modelo <- lm(Bailabilidad~Tempo,data=df1)
summary(modelo)

residuos <- modelo$residuals
shapiro.test(residuos)

#Grafico
ggqqplot(residuos,ylab="Residuos del Modelo", xlab = "Valores Teóricos")



#Grafico residuos frente a los valores ajustados 
plot(modelo$fitted.values,modelo$residuals,
     main = "Gráfico de residuos vs Valores ajustados",
     xlab="Valores Ajustados (Predicciones del modelo)",
     ylab = "Residuos (Errores)",
     col="steelblue", pch=16)
abline(h = 0, col = "red", lty = 2, lwd = 2)

# Extraemos los coeficientes 
coeficientes <- modelo$coefficients
intercepto <- coeficientes[1]
pendiente <- coeficientes[2]


ecuacion <- paste("y = ", round(pendiente, 4), "x + ", round(intercepto, 2))

# gráfico del modelo
ggplot(df1, aes(x = Tempo, y = Bailabilidad)) + 
  geom_point(color = "steelblue", alpha = 0.5) +   # Puntos de las canciones
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, col = '#EB1780') + # Línea de tendencia
  theme_light() +
  labs(x = "Tempo (BPM)", 
       y = "Bailabilidad", 
       title = "Regresión Lineal: Bailabilidad vs. Tempo (Hits de Spotify)") +
  annotate("text", x = 120, y = 0.2, label = ecuacion, color = "black", size = 4, fontface = "bold")
