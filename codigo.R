
# Leyendo la base de datos-------------------------------------
library(readxl)
Datosssm <- read_excel("Databases/Tabla_Pequena_Filtrada.xlsx")
#View(Tabla_Pequena_Filtrada)
#View(Datosssm)
## Dimension de la base de datos........................................... 
dim(Datosssm)
summary(Datosssm)
names(Datosssm2)
## Verifiquemos los datos faltantes........................................
apply(Datosssm, 2, function(x) sum(is.na(x)))



#....................................................................................
## ---------------------- Nueva Base sin datos faltantes----------------------------
#...................................................................................

## Escojamos las variables que esten completas.........................
Datosssm2 <- Datosssm[,colSums(is.na(Datosssm))==0] 
Datosssm2 <- Datosssm2[,-c(1,2)] 
apply(Datosssm2, 2, function(x) sum(is.na(x)))
dim(Datosssm2)
## Dataframe con datos completos.....................................

names(Datosssm2)
Datosssm2 <-  as.data.frame(Datosssm2)
apply(Datosssm2, 2, function(x) class(x))



##************************************* Analisis desriptivo ******************************

## comp primera medida observemos el conjunto de datos(dimension) y sus mediciones

dim(Datosssm2)   # 901 resgistro correpondientes a cada madre soltera y 25 variables
str(Datosssm2)
summary(Datosssm2$P1895)


library(ggplot2)
bar <- table(Datosssm2$P1895,Datosssm2$P6040 )
bar <- as.data.frame(bar)
tail(bar)

data=data.frame(Nivel_de_Satisfaccion = bar$Var1 , Edad = bar$Var2, Frecuencia= bar$Freq)


View(data)

### Haciendo grafico de barras 

#pdf(file = "barras2.pdf", height = 4.5, width = 6.5)
ggplot(data, aes(x=Nivel_de_Satisfaccion, y=Frecuencia)) +
  ggtitle ("Distribución del nivel de satisfacción de las madres solteras")+
  geom_bar(stat = "identity", fill="steelblue")+
  geom_text(aes(label=bar$Freq), vjust=1.6, color="white", size=3.5)+
  theme_minimal()
#dev.off()


##### #####     Haciendo boxplot

#Datosssm2$P1895[Datosssm2$P1895==8]
pdf(file= "D:/boxplot1.pdf", height = 4.5, width = 6.6)
boxplot(Datosssm2$P1895, main = "Distribución del nivel de satisfacción de las madres solteras")
#dev.off()


######  hagamos un grafico de la distribucion de los hijos

pdf(file = "barrasnumerodehijos.pdf", height = 4.5, width = 6.5)
barplot(table(Datosssm2$N_HIJOS), xlab = "Número de hijos", ylab = "cantidad")
dev.off()


#### hagamos un grafico con la distribución del número de nietos

pdf(file = "barrasnumerodenietos1.pdf", height = 4.5, width = 6.5)
barplot(table(Datosssm2$N_NIETOS), xlab = "Número de nietos", ylab = "cantidad")
dev.off()

pdf(file = "usoderedes.pdf", height = 4.5, width = 6.5)
barplot(table(Datosssm2$P1083S3), col = c("brown1", "cornflowerblue"), names.arg = c("NO", "SI"), xlab = "Uso de redes sociales", ylab = "cantidad")
dev.off()


table(Datosssm2$P5502)





data$Edad <- as.numeric_version(data$Edad)
data$Edad <- as.numeric(data$Edad)
min(data$Edad)
max(data$Edad)
######## Haciendo un grafico de barras apiladas al 100% 


data$Edad[data$Edad <= 30] <- "entre 19 y 30 años "
data$Edad[data$Edad <= 50]<- "entre 31 y 50 años"
data$Edad[data$Edad <= 70]<- "entre 51 y 70 años"
data$Edad[data$Edad <= 94 ] <- "mayor a 71 años"

#Datosssm2$P1895
#Datosssm2$P6040

data$Nivel_de_Satisfaccion <- as.numeric_version(data$Nivel_de_Satisfaccion)
data$Nivel_de_Satisfaccion <- as.numeric(data$Nivel_de_Satisfaccion)
min(data$Nivel_de_Satisfaccion)

data$Nivel_de_Satisfaccion[data$Nivel_de_Satisfaccion == 0] <- "3.Nivel de satisfacción bajo"
data$Nivel_de_Satisfaccion[data$Nivel_de_Satisfaccion == 1] <- "3.Nivel de satisfacción bajo"
data$Nivel_de_Satisfaccion[data$Nivel_de_Satisfaccion == 2] <- "3.Nivel de satisfacción bajo"
data$Nivel_de_Satisfaccion[data$Nivel_de_Satisfaccion == 3] <- "3.Nivel de satisfacción bajo"

data$Nivel_de_Satisfaccion[data$Nivel_de_Satisfaccion == 4] <- "2.Nivel de satisfacción regular"
data$Nivel_de_Satisfaccion[data$Nivel_de_Satisfaccion == 5] <- "2.Nivel de satisfacción regular"
data$Nivel_de_Satisfaccion[data$Nivel_de_Satisfaccion == 6] <- "2.Nivel de satisfacción regular"
data$Nivel_de_Satisfaccion[data$Nivel_de_Satisfaccion == 7] <- "2.Nivel de satisfacción regular"


data$Nivel_de_Satisfaccion[data$Nivel_de_Satisfaccion == 8] <- "1.Nivel de satisfacción bueno"
data$Nivel_de_Satisfaccion[data$Nivel_de_Satisfaccion == 9] <- "1.Nivel de satisfacción bueno"
data$Nivel_de_Satisfaccion[data$Nivel_de_Satisfaccion == 10] <- "1.Nivel de satisfacción bueno"


# write.csv(tb, file = "D:/Datos.csv")

ggplot(data = data, aes(x=Edad, y= Frecuencia, fill=Nivel_de_Satisfaccion))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("darkgreen", "purple","black"))



ggplot(data = data, aes(x=Edad, y= Frecuencia, fill=Nivel_de_Satisfaccion)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("darkgreen", "purple","black"))



ggplot(data = data, aes(x=Edad, y= Frecuencia, fill=Nivel_de_Satisfaccion)) + 
  geom_bar(stat="identity", position=position_fill()) +
  scale_fill_manual(values=c("darkgreen", "purple","black"))



ggplot(data = data, aes(x=Edad, y= Frecuencia, fill=Nivel_de_Satisfaccion)) + 
  geom_bar(stat="identity", position=position_stack()) +
  scale_fill_manual(values=c("darkgreen", "purple","black"))



ggplot(data = data, aes(x=Edad, y= Frecuencia, fill=Nivel_de_Satisfaccion)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("darkgreen", "purple","black"))



## Hagamos modelos con variables dummy........................................................


#Datosssm2$P6020 <- factor(Datosssm2$P6020, levels = c("2"))     ## sexo 
Datosssm2$P5502 <- factor(Datosssm2$P5502)   ## autualmente .. 3 viuda, 5 soltera
#Datosssm2$P6081 <- factor(Datosssm2$P6081)  # padre vive en el hogar
#Datosssm2$P6083 <- factor(Datosssm2$P6083) # la madre vive en este hogar
Datosssm2$P6080 <- factor(Datosssm2$P6080) # a que cultura pertenece
Datosssm2$P1896 <- factor(Datosssm2$P1896) # satisfaccion con el ingreso
Datosssm2$P1897 <- factor(Datosssm2$P1897) # satisfaccion con la salud
Datosssm2$P1898 <- factor(Datosssm2$P1898) # satisfecho con el nivel de seguridad
Datosssm2$P1899 <- factor(Datosssm2$P1899) # satisfecho con el trabajo
Datosssm2$P1901 <- factor(Datosssm2$P1901) # feliz el dia de ayer
Datosssm2$P1902 <- factor(Datosssm2$P1902) # tranquilo el dia de ayer
Datosssm2$P1903 <- factor(Datosssm2$P1903) # preocupado el dia de ayer
Datosssm2$P1904 <- factor(Datosssm2$P1904) # triste el dia de ayer
Datosssm2$P1905 <- factor(Datosssm2$P1905) # las cosas que hace en su vida valen la pena?

Datosssm2$P1910 <- factor(Datosssm2$P1910) # utiliza _____ computador de escritorio (en cualquier lugar)?
Datosssm2$P1911 <- factor(Datosssm2$P1911) # utiliza _____ portatil (en cualquier lugar)?
Datosssm2$P1912 <- factor(Datosssm2$P1912) # utiliza _____ tablet (en cualquier lugar)?
Datosssm2$P1084 <- factor(Datosssm2$P1084) # utiliza _____ intenet ?
Datosssm2$P1083S3 <- factor(Datosssm2$P1083S3) # utiliza _____ redes sociales ?
Datosssm2$P1082 <- factor(Datosssm2$P1082) # utiliza _____ celular ?
#Datosssm2$P769 <- factor(Datosssm2$P769) # ¿con que frecuencia utiliza ____ teléfono celular?
#Datosssm2$P804 <- factor(Datosssm2$P804) # radio

# Variable respuesta
#Datosssm2$P1895 <- factor(Datosssm2$P1895) # radio

boxplot(Datosssm2$P1895, main = "Diagrama de caja de Nivel de satisfacción de las madres solteras")
h <- table(Datosssm2$P1895)
barplot(h, main = "Distribución del nivel de satisfación de las madres solteras")

vect <-  as.matrix(Datosssm2$P1895)


barplot(Datosssm2$P1895, main="Car Distribution", 
        xlab="Number of Gears")



## Hagamos nuestro primer modelo con lm (minimos cuadrados)
mod0 <- lm(P1895~P6040+P5502+P6080+P1896+P1897+P1898+P1899+P1901+P1902+P1903+P1904+P1905+N_HIJOS+N_NIETOS+P1910+P1911+P1912+P1084+P1083S3+P1082, Datosssm2)
summary(mod0)








# con las variables seleccionadas ****

mod00 <- lm(P1895 ~ P1896 + P1897 + P1898 + P1901 + P1902 + P1905 + N_HIJOS + N_NIETOS + N_HIJOS:N_NIETOS, Datosssm2)
summary(mod00)



library("hydroGOF")
mse(Datosssm2$P1895, mod0$fitted.value)

AIC(mod0)
sum()
  
mod0$fitted.values
cor(Datosssm2$P1895, mod0$fitted.values)
plot(Datosssm2$P1895, mod0$fitted.values)

## vamos la matriz de diseño 
model.matrix(mod0)


#-------Paso1) prueba marginal bondad de ajuste variable explicativa (P1895) nivel de satisfacción  ---------------


hist(Datosssm2$P1895)

barplot(Datosssm2$P1895)

densidad <- density(Datosssm2$P1895)
plot(densidad)
p<-fitDist(Datosssm2$P1895,type="realplus")    # prueba de bondad de ajuste 
p$fits   #  para ver las mejores distrinbuciones ajustadas 

## histograma de la variable respuesta (distribucion de la variable respuesta)
with(Datosssm2, hist(P1895))

hist(Datosssm2$P1895,freq=T, col="lightcyan",
     main="Nivel de satisfacción en la vida de las madres solteras ",xlab="",ylab="Densidad")




###---------------------------- paso2) definir el horizonte del modelo -----------------------------------

FORM1 <- as.formula("~(N_HIJOS+N_NIETOS+P6040)^2+P6040+P5502+P6080+P1896+P1897+P1898+P1899+P1901+P1902+P1903+P1904+P1905+N_HIJOS+N_NIETOS+P1910+P1911+P1912+P1084+P1083S3+P1082")   # definimos todas las variables solas e interacciones entre ellas




### ---------------------------Haciendo el primer modelo con gamlss---------------------------------


library(gamlss)
mod1 <- gamlss(P1895~.,
               data = datos)


xtable(summary(mod1))
Rsq(mod1)


library("broom")
salida <- tidy(summary(mod1))
View(salida)
str(salida$Pr...t..)
salida <- data.frame(salida)



#-------------------------------------------------Paso3 ) seleccion de variables stepGAICAll.A---------------------------------------------



# Hagamos un proceso de seleccion de variables usando stepgaicall

mod2<- stepGAIC(mod1, scope=list(lower=~1, upper=FORM1), direction = 'both')



mod2$anova
summary(mod2)
Rsq(mod2)

round(coef(mod2),2) 


# Varibles seleccionadas..

#Step:  AIC= 3884.46 

#P1895 ~ P1896 + P1897 + P1898 + P1901 + P1902 + P1905 + N_HIJOS + N_NIETOS + N_HIJOS:N_NIETOS





############################ Hagamos un modelo multinomial###################################
#############################################################################################
#############################################################################################

Mydata <-  Datosssm2[, c("P1895", "P1896", "P1897", "P1898", "P1901", "P1902", "P1905", "N_HIJOS", "N_NIETOS", "P1084", "P1083S3")]
plot(Mydata)


library(gamlss)
library(MASS)

require(nnet)
modmultinom<- multinom(P1895 ~ P1896 + P1897 + P1898 + P1901 + P1902 + P1905 + N_HIJOS + N_NIETOS + P1084 + P1083S3 + N_HIJOS:N_NIETOS,
                  data=Datosssm2, trace=FALSE)



summary(modmultinom)

length(modcf$fitted.values)
length(Datosssm2$P1895)


pre<-predict(modmultinom, newdata = Datosssm2[,c("P1896","P1897","P1898","P1901","P1902","P1905","N_HIJOS","N_NIETOS", "P1084", "P1083S3")])
length(pre)



plot(Datosssm2$P1895, pre,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(Datosssm2$P1895, pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))





a <- Datosssm2[,c("P1896","P1897","P1898","P1901","P1902","P1905","N_HIJOS","N_NIETOS", "P1084", "P1083S3")]
dim(a)

valoresingresados <- data.frame(
  P1896 = as.factor(c(10)),
  P1897 = as.factor(c(10)),
  P1898 = as.factor(c(10)),
  P1901 = as.factor(c(10)),
  P1902 = as.factor(c(10)),
  P1905 = as.factor(c(10)),
  N_HIJOS = 1,
  N_NIETOS =1)

satisfaccion_calculada2 <- predict(modmultinom, newdata = valoresingresados)
satisfaccion_calculada2


class(valoresingresados)




tabladeresultados <- cbind(pre,Datosssm2$P1895)
View(tabladeresultados)

contador <- 0
for (i in 1:nrow(tabladeresultados)){
  if (pre[i]==Datosssm2$P1895[i]){
    contador = contador + 1
    
  }
    
  
}

aciertos <- contador/nrow(tabladeresultados)



pre[c(5:10)]
Datosssm2$P1895[0:10]


# comprobacion del modelo

obs<-Datosssm2$P1895
pre<-predict(modcf, type="class")
cont<-0

for (i in 1:length(Datosssm2)){
  if (pre[i]==obs[i])
    cont=cont+1
  else
    cont=cont
}
cont
tocc<- cont/length(Datosssm2)
tocc



## seleccionn de variables con el modelo multinom








###-----------------------------------------Logit multinom----------------------------

library(gml)

regresion2 = glm(P1895 ~ P1896 + P1897 + P1898 + P1901 + P1902 + P1905 + N_HIJOS + N_NIETOS + N_HIJOS:N_NIETOS, family =multinom(), data = Datosssm2)
summary(regresion2)








###------------------------------





