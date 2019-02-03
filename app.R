library(shiny)
set.seed(123)
server <- function(input, output) {
  
  library(readxl)
  Datosssm <- read_excel("Databases/Tabla_Pequena_Filtrada.xlsx")
  datos <- Datosssm[,colSums(is.na(Datosssm))==0] 
  datos <- datos[,-c(1,2)] 
  datos <- data.frame(datos)
  apply(datos, 2, function(x) class(x))
  ## Convertir a vaariables dummy
  datos$P1896 <- factor(datos$P1896) # satisfaccion con el ingreso
  datos$P1897 <- factor(datos$P1897) # satisfaccion con la salud
  datos$P1898 <- factor(datos$P1898) # satisfecho con el nivel de seguridad
  datos$P1901 <- factor(datos$P1901) # feliz el dia de ayer
  datos$P1902 <- factor(datos$P1902) # tranquilo el dia de ayer
  datos$P1905 <- factor(datos$P1905) # las cosas que hace en su vida valen la pena?
  datos$P1084 <- factor(datos$P1084) # utiliza _____ intenet ?
  datos$P1083S3 <- factor(datos$P1083S3) # utiliza _____ redes sociales ?
  # Variable respuesta
  datos$P1895 <- factor(datos$P1895) # Nivel de satisfaccion
  
  # otro subset que se necesita
  datos <- data.frame(datos[,c("P1895","P6040", "P1896", "P1897", "P1898", "P1901", "P1902", "P1905", "P1084","P1083S3", "N_HIJOS", "N_NIETOS")])
  
  
  # Formula con las variables seleccionadas.
  formula2 <- as.formula("P1895 ~ P6040 + P1896 + P1897 + P1898 + P1901 + P1902 + P1905 + N_HIJOS + N_NIETOS + P1084 + P1083S3")
  
  #Clasificador con Bosques Aleatorios (Random Forests Classifier)  *****
  library(randomForest)
  clasificadorRF <- randomForest(formula2, data = datos, ntree = 250)
  #pred_valid_RF <- predict(clasificadorRF, newdata = datos)
  #matrizConfusion <- table(datos$P1895, pred_valid_RF)
  #matrizConfusion
  
  
  out <- reactive({
    
    # predicción*** PARA LA APLICACIÖN **** ( aqui hay que insertar el id de las etiquetas en cada objeto html) en lugar de los valores particulares.
    
    datos2 <- data.frame(P6040 = input$P6040, 
                         P1896 = input$P1896,
                         P1897 = input$P1897,
                         P1898 = input$P1898,
                         P1901 = input$P1901,
                         P1902 = input$P1902,
                         P1905 = input$P1905,
                         N_HIJOS = input$N_HIJOS,
                         N_NIETOS = input$N_NIETOS,
                         P1084 = input$P1084,
                         P1083S3 = input$P1083S3)
    # Este es un truco para igualas las clases y los niveles de cada variable. 
    xtest <- rbind(datos[1,-1] , datos2)
    xtest <- xtest[-1,]
    
  })
  
  
  
  output$satisfaccion <- renderText({
    pred_valid_RF <- predict(clasificadorRF, newdata = out())
  })
  
  
}

shinyApp(ui = htmlTemplate("www/index.html"), server)
