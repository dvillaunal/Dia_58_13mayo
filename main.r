## ----Protocolo, eval=FALSE, include=TRUE---------------------------------------------------
## "Protocolo:
## 
##  1. Daniel Felipe Villa Rengifo
## 
##  2. Lenguaje: R
## 
##  3. Tema: Calculo de media, moda y mediana con R (realice al menos dos ejercicios que requieran cargar archivos externos, leer y procesar la información del archvo leído, y guardar las respuestas a los ejercicios  en archivos independientes)
## 
##  4. Fuentes:
##     https://areatutorial.com/r-media-mediana-y-moda/"


## ------------------------------------------------------------------------------------------
# Cargamos la base de datos:
basemedia <- read.csv(file = "Basemedia.csv", header = T, sep = ",", dec = ".")

basemedia <- data.frame(basemedia)

# Convertimos en factor la 1ra columna:
basemedia$Materia <- as.factor(basemedia$Materia)

library(dplyr)
# Ahora separamos las materias:
baseF <- filter(basemedia, basemedia$Materia == levels(basemedia$Materia)[1])

baseI <- filter(basemedia, basemedia$Materia == levels(basemedia$Materia)[2])

baseL <- filter(basemedia, basemedia$Materia == levels(basemedia$Materia)[3])

baseM <- filter(basemedia, basemedia$Materia == levels(basemedia$Materia)[4])

baseQ <- filter(basemedia, basemedia$Materia == levels(basemedia$Materia)[5])

# Ahora calculamos su media de cada una de las materias:
meanF <- mean(baseF$Nota)
meanI <- mean(baseI$Nota)
meanL <- mean(baseL$Nota)
meanM <- mean(baseM$Nota)
meanQ <- mean(baseQ$Nota)

#Ahora Guardamos en una tabla cada una: con sus respectivos nombres:

resultadomedia <- data.frame(
  "Materia" = levels(basemedia$Materia),
  # Redondeo con round a dos cifras decimales:
  "Promedio_(NotaFinal)" = round(c(meanF, meanI, meanL, meanM, meanQ), 2)
)

## Exportamos:
write.csv(resultadomedia, file = "ResultadoMedia.csv", sep = ",", dec = ".", row.names = F)


## ------------------------------------------------------------------------------------------
# Cargamos la base de datos:
basemitad <- read.table(file = "BaseMediana.txt", header = T, sep = ",", dec = ".")

## Convertimos en Factor la 1ra columna:
basemitad$Medicamento <- as.factor(basemitad$Medicamento)

## Calculemos la mitad o mediana y el medicamento que se selecciona:
median(basemitad$n)
"Vemos que la definición los dice que saca la mediana una vez los datos esten de menor a mayor"

"primero organizaremos la base de datos"
basemitad <- arrange(basemitad, basemitad$n)

#una vez organizada veremos que el termino 50 es es que cumple el requisto de la mediana:

## Creamos la tabla que contedra la mediana:
n50 <- as.character(basemitad$Medicamento[50])

ResultadoMediana <- data.frame(
  "Medicamento" = n50,
  "Mediana_(numero de pastillas generadas)" = median(basemitad$n)
)

## Exportamos nuestro resultado:
write.table(ResultadoMediana, file = "ResultadoMediana.txt", sep = ",", dec = ".", row.names = F)


## ------------------------------------------------------------------------------------------
moda <- function(x) {
  # Esta función retorna un valor numerico en el cual
  # el más repetido de un conteo de un vector o columna:
  return(as.numeric(names(which.max(table(x)))))
}


## ------------------------------------------------------------------------------------------
# Importamos la base de datos:
basemoda <- read.csv(file = "BaseModa.csv", sep = ",", header = T)

## Convertimos en data frame y en factores las dos columnas:
basemoda <- data.frame(basemoda)

basemoda$Departamento <- as.factor(basemoda$Departamento)

basemoda$Marca <- as.factor(basemoda$Marca)


## ------------------------------------------------------------------------------------------
## Generemos una tabla haceindo un conteo de las dos variables:
conteomoda <- data.frame(with(basemoda, table(Departamento, Marca)))

print(conteomoda)

## Ahora separamos las tablas por marcas:
baseche <- filter(conteomoda, conteomoda$Marca == "Chevrolet")
baseford <- filter(conteomoda, conteomoda$Marca == "Ford")
basekia <- filter(conteomoda, conteomoda$Marca == "Kia")
baseMazda <- filter(conteomoda, conteomoda$Marca == "Mazda")
baserenault <- filter(conteomoda, conteomoda$Marca == "Renault")
basetoyota <- filter(conteomoda, conteomoda$Marca == "Toyota")

# Exportemos la base ford:
write.table(baseford, file = "BaseFord.txt", sep = ",", dec = ".", row.names = F)

## Ahora con la función moda sacaremos la mayor frecuencia de cada uno:
che <- moda(baseche$Freq)
ford <- moda(baseford$Freq)
kia <- moda(basekia$Freq)
mazda <- moda(baseMazda$Freq)
renault <- moda(baserenault$Freq)
toyota <- moda(basetoyota$Freq)


"Como podemos ver los datos que mas se repiten son los de 0 en todas las ocaciones"

#Exportemos un resultado como pureba:
modadata <- data.frame(
  "Marca" = "Ford",
  "Moda" = ford
)
write.csv(modadata, file = "ResultadoModa.txt", sep = ",", dec=".", row.names = F)

