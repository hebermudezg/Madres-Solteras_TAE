library(shiny)
library(readxl)
library(DT)

## leyendo datos
Datosssm <- read_excel("./Tabla_Pequena_Filtrada.xlsx")

DatosFamilias <- read.csv("./Familias.csv",sep=";")



#...........................Base de datos sin datos faltantes...........
Datosssm2 <- Datosssm[,colSums(is.na(Datosssm))==0]


#.......................... convertir en data.frame..................
Datosssm2 <-  as.data.frame(Datosssm2)


##..........................Preparemos las variables dummy........................
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
# Variable respuesta............................................................
Datosssm2$P1895 <- factor(Datosssm2$P1895) # radio


#.............................haciendo modelo multinomial.......................
modmultinom<- multinom(P1895 ~ P1896 + P1897 + P1898 + P1901 + P1902 + P1905 + N_HIJOS + N_NIETOS + P1084 + P1083S3 + N_HIJOS:N_NIETOS,
                       data=Datosssm2, trace=FALSE)



#...........................
MadresCabezaHogar <- DatosFamilias[DatosFamilias$ORDEN == 1, c("LLAVEHOG","P1896","P1897","P1898","P1901","P1902",
                                                               "P1905","N_HIJOS","N_NIETOS","P1084","P1083S3")]



MadresCabezaHogar$P1084 <- paste(MadresCabezaHogar$P1084)
MadresCabezaHogar$P1083S3 <- paste(MadresCabezaHogar$P1083S3)
MadresCabezaHogar$P1083S3[MadresCabezaHogar$P1083S3 == ""] <- "0"
MadresCabezaHogarRaw <- MadresCabezaHogar
MadresCabezaHogar$P1083S3[MadresCabezaHogar$P1083S3 == "1"] <- "Si"
MadresCabezaHogar$P1083S3[MadresCabezaHogar$P1083S3 == "0"] <- "No"
MadresCabezaHogar$P1084[MadresCabezaHogar$P1084 == "1"] <- "Todos los días de la semana"
MadresCabezaHogar$P1084[MadresCabezaHogar$P1084 == "2"] <- "Al menos una vez a la semana"
MadresCabezaHogar$P1084[MadresCabezaHogar$P1084 == "3"] <- "Al menos una vez al mes"
MadresCabezaHogar$P1084[MadresCabezaHogar$P1084 == "4"] <- "Al menos una vez al año"
MadresCabezaHogar$P1084[MadresCabezaHogar$P1084 == "5"] <- "No utiliza internet"

MadresCabezaHogarRaw[] <- lapply(MadresCabezaHogarRaw, factor)
MadresCabezaHogarRaw$N_HIJOS <- as.double(MadresCabezaHogarRaw$N_HIJOS)
MadresCabezaHogarRaw$N_NIETOS <- as.double(MadresCabezaHogarRaw$N_NIETOS)


colnames(MadresCabezaHogar) <- c("Hogar","Satifaccion ingresos","Satisfaccion salud","Satisfaccion seguridad","Felicidad ayer",
                                 "Tranquilidad ayer","Satisfaccion con lo que hace","Hijos","Nietos","Uso de internet","Uso de redes sociales")

server <- function(input, output, session) {
  
  probabilidades <- eventReactive(input$calcular, {
    
    valoresingresados <- data.frame(
      P1896= as.factor(input$satisfaccion_ingreso),
      P1897= as.factor(input$satisfaccion_salud),
      P1898= as.factor(input$satisfaccion_seguridad),
      P1901= as.factor(input$satisfaccion_felicidad),
      P1902= as.factor(input$satisfaccion_tranquilidad),
      P1905= as.factor(input$satisfaccion_cosas_hace),
      N_HIJOS=input$numero_hijos,
      N_NIETOS=input$numero_nietos,
      P1084=as.factor(input$frecuencia_internet),
      P1083S3= as.factor(input$redes_sociales)
    )
    
    
    satisfaccion_calculada2 <- predict(modmultinom, newdata = valoresingresados)
    
    
    
  })
  
  
  output$satisfaccion_calculada <- renderText({
    Prob <- probabilidades()
    paste(Prob)
  })
  
  
  
  output$satisfaccion_clasificada <- renderText({
    f = probabilidades()
    P = as.numeric(levels(f))[f]
    if(P < 4) paste("Mala")
    else if(P < 8) paste("Regular")
    else paste("Buena")
  })
  
  
  
  output$tablaDatos <- DT::renderDT({
    datatable(MadresCabezaHogar, selection = 'single', class="compact", options = list(
      # autoWidth = TRUE,
      pageLength = 5,
      lengthMenu = c(5, 10, 20, 50, 100)
      # ,columnDefs = list(list(width = '40000px', targets = 1))
    ))
  })
  
  output$satisfaccion_seleccion_calculada <- renderText({
    if(length(input$tablaDatos_rows_selected)){
      hogar <- MadresCabezaHogarRaw[input$tablaDatos_rows_selected,1]
      seleccion <- MadresCabezaHogarRaw[MadresCabezaHogarRaw$LLAVEHOG == hogar,
                                        c("P1896","P1897","P1898","P1901","P1902","P1905","N_HIJOS","N_NIETOS","P1084","P1083S3")]
      p = predict(modmultinom, newdata = seleccion)
      paste(p)
    }
  })
  
  output$satisfaccion_seleccion_clasificada <- renderText({
    if(length(input$tablaDatos_rows_selected)){
      hogar <- MadresCabezaHogarRaw[input$tablaDatos_rows_selected,1]
      seleccion <- MadresCabezaHogarRaw[MadresCabezaHogarRaw$LLAVEHOG == hogar,
                                        c("P1896","P1897","P1898","P1901","P1902","P1905","N_HIJOS","N_NIETOS","P1084","P1083S3")]
      f = predict(modmultinom, newdata = seleccion)
      P = as.numeric(levels(f))[f]
      if(P < 4) paste("Mala")
      else if(P < 8) paste("Regular")
      else paste("Buena")
    }
  })
  
  output$networkFamilia <- renderForceNetwork({
    if(length(input$tablaDatos_rows_selected)){
      hogar <- MadresCabezaHogarRaw[input$tablaDatos_rows_selected,1]
      MyData_filtered = DatosFamilias[DatosFamilias$LLAVEHOG == hogar,]
      
      fathers = MyData_filtered[,c("P6081S1","ORDEN","P6020")]
      colnames(fathers) <- c("Parent", "Person","Genre")
      mothers = MyData_filtered[,c("P6083S1","ORDEN","P6020")]
      colnames(mothers) <- c("Parent","Person","Genre")
      
      parents = rbind(fathers,mothers)
      
      people = mothers[,c("Person","Genre")]
      
      people$Genre[people$Genre == 1] <- "Hombre"
      people$Genre[people$Genre == 2] <- "Mujer"
      
      parentChild = parents[!is.na(parents$Parent),c("Parent","Person")]
      if(length(parentChild$Parent)){
        parentChild$Parent = parentChild$Parent - 1
        parentChild$Person = parentChild$Person - 1
        parentChild$Value = 4
        
        forceNetwork(Links = parentChild, Nodes = people, Source = "Parent", Target = "Person", Value = "Value",
                     NodeID = "Person", Group = "Genre", arrows = TRUE, fontSize = 12, opacity = 1, legend = TRUE,
                     colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"), bounded = TRUE, zoom = TRUE)
      }
    }
  })
  
}  



