
library(readxl)
Datosssm <- read_excel("Databases/Tabla_Pequena_Filtrada.xlsx")
datos <- Datosssm[,colSums(is.na(Datosssm))==0] 
datos <- datos[,-c(1,2)] 
datos <- data.frame(datos)
apply(datos, 2, function(x) class(x))


## Convertir a vaariables dummy
datos$P5502 <- factor(datos$P5502)   ## autualmente .. 3 viuda, 5 soltera
datos$P6081 <- factor(datos$P6081)  # padre vive en el hogar
datos$P6083 <- factor(datos$P6083) # la madre vive en este hogar
datos$P6080 <- factor(datos$P6080) # a que cultura pertenece
datos$P1896 <- factor(datos$P1896) # satisfaccion con el ingreso
datos$P1897 <- factor(datos$P1897) # satisfaccion con la salud
datos$P1898 <- factor(datos$P1898) # satisfecho con el nivel de seguridad
datos$P1899 <- factor(datos$P1899) # satisfecho con el trabajo
datos$P1901 <- factor(datos$P1901) # feliz el dia de ayer
datos$P1902 <- factor(datos$P1902) # tranquilo el dia de ayer
datos$P1903 <- factor(datos$P1903) # preocupado el dia de ayer
datos$P1904 <- factor(datos$P1904) # triste el dia de ayer
datos$P1905 <- factor(datos$P1905) # las cosas que hace en su vida valen la pena?
datos$P1910 <- factor(datos$P1910) # utiliza _____ computador de escritorio (en cualquier lugar)?
datos$P1911 <- factor(datos$P1911) # utiliza _____ portatil (en cualquier lugar)?
datos$P1912 <- factor(datos$P1912) # utiliza _____ tablet (en cualquier lugar)?
datos$P1084 <- factor(datos$P1084) # utiliza _____ intenet ?
datos$P1083S3 <- factor(datos$P1083S3) # utiliza _____ redes sociales ?
datos$P1082 <- factor(datos$P1082) # utiliza _____ celular ?
datos$P804 <- factor(datos$P804) # radio
# Variable respuesta
datos$P1895 <- factor(datos$P1895) # Nivel de satisfaccion


# Formula con las variables seleccionadas.
formula2 <- as.formula("P1895 ~ P6040 + P1896 + P1897 + P1898 + P1901 + P1902 + P1905 + N_HIJOS + N_NIETOS + P1084 + P1083S3")


#Clasificador con Bosques Aleatorios (Random Forests Classifier)  *****
library(randomForest)
clasificadorRF <- randomForest(formula2, data = datos, ntree = 250)
#pred_valid_RF <- predict(clasificadorRF, newdata = datos)
#matrizConfusion <- table(datos$P1895, pred_valid_RF)
#matrizConfusion




# predicción*** PARA LA APLICACIÖN ****

datos2 <- data.frame(P6040,
P1896,
P1897,
P1898,
P1901,
P1902,
P1905,
N_HIJOS,
N_NIETOS,
P1084,
P1083S3)


#preprando el data frame
datos2 <- data.frame(datos[3,])

# relizado la predicción
pred_valid_RF <- predict(clasificadorRF, newdata = datos2)

# lo que se debe mostrar en la app 
print(pred_valid_RF)



