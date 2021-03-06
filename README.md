```{r Protocolo, eval=FALSE, include=TRUE}
"Protocolo:

 1. Daniel Felipe Villa Rengifo

 2. Lenguaje: R

 3. Tema: Calculo de media, moda y mediana con R (realice al menos dos ejercicios que requieran cargar archivos externos, leer y procesar la información del archvo leído, y guardar las respuestas a los ejercicios  en archivos independientes)

 4. Fuentes:
    https://areatutorial.com/r-media-mediana-y-moda/"
```

# Media:

La media, también conocida como promedio, es el valor que se obtiene al dividir la suma de un conglomerado de números entre la cantidad de ellos.

## Sintaxis

La sintaxis básica para calcular la media en R es:
    
    mean(x, trim = 0, na.rm = FALSE, ...)
    
A continuación se muestra la descripción de los parámetros utilizados:

+ `X` es el vector de entrada.

+ `na.rm` se utiliza para eliminar los valores perdidos del vector de entrada.

## Ejercicio

Vamos a calcular la media de cada una de las materias de un estudiante

```{r}
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
```

# Mediana

La mediana es un conjunto es un valor que se encuentra a la mitad de los otros valores, es decir, que al ordenar los número de menor a mayor, éste se encuentra justamente en medio entre los que están por arriba.

## Sintaxis

La sintaxis básica para calcular la mediana en R es:

    median(x, na.rm = FALSE)

A continuación se muestra la descripción de los parámetros utilizados:

+ `X` es el vector de entrada.

+ `na.rm` se utiliza para eliminar los valores perdidos del vector de entrada.

## Ejercicio:

Vamos a calcular la mitad de la produccion de una planta que que fabrica medicamentos:

Medicamento: Medicamento fabricado
n: N° de pastillas generadas (1-10, tableta convencional)

```{r}
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
```


# Moda

La moda es el valor que tiene más ocurrencias en el conjunto de datos. Al igual que la media y la mediana, una moda puede tener datos numéricos y de caracteres.

R no dispone de una función en su paquete base que nos permita calcular la moda. La función mode devuelve el tipo o modo de almacenamiento de un objeto. Hay múltiples formas de calcular la moda haciendo uso de otras funciones de R.

## Sintaxis

Vamos a calcular apenas una de los tipos de moda:

### Estimación unimodal discreta

Una manera sencilla de calcular la moda en R es usar la siguiente función:
```{r}
moda <- function(x) {
  # Esta función retorna un valor numerico en el cual
  # el más repetido de un conteo de un vector o columna:
  return(as.numeric(names(which.max(table(x)))))
}
```

    
## Ejercicio

En la siguiente base de datos veremos cual es el departamento que registra una moda en los autos en el país con su respectiva marca:

+ `Marca`: Marca de auto vendida

```{r}
# Importamos la base de datos:
basemoda <- read.csv(file = "BaseModa.csv", sep = ",", header = T)

## Convertimos en data frame y en factores las dos columnas:
basemoda <- data.frame(basemoda)

basemoda$Departamento <- as.factor(basemoda$Departamento)

basemoda$Marca <- as.factor(basemoda$Marca)
```

```{r}
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
```