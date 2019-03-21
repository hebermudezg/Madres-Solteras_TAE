library(shiny)
library(networkD3)
#install.packages("networkD3")
#install.packages(DT)

material_page(
  #includeCSS("www/app.css"),
  aling = 'center',
  title = "StatiSoft", 
  nav_bar_color = "pink",   

#....................................Barra de navegacion..............................................
#.....................................................................................................


  material_side_nav(
    fixed = TRUE, 
    
    image_source = "img/LOGO.jpg",
    material_side_nav_tabs(
      side_nav_tabs = c(
        "Nivel de satisfacción" = "satisfaccion",
        "Estructura del hogar" = "estructura",
        #"Manual" = "manual",
        "Video" = "Video",
        "Sobre nosotros" = "Creadores" 
       ),
      icons = c("insert_chart", "explore", "explore", "explore")
      )
  ),

##..........................................................................................................



####                                          Pagina satisfaccion................................####
###.............................................................................................................


 material_side_nav_tab_content(
    side_nav_tab_id = "satisfaccion",
    tags$br(),
    
    
    material_row(
      material_column(
        align = 'center',
        width = 13,
        material_card(
          tags$h5("Ingrese la siguiente información, luego de clic en calcular para saber el nivel de satisfacción que tiene la madre soltera con su vida actualmente.",
                  style = "font-family: 'Arial';
                  font-size: 20px;
                  text-align: center;
                  color: #6E6E6E;")
          )
        )
      ),
  
    
#.......................................................................................................................  



## ............................ .......................Primera row...............................................

    material_row(
      
      
      ## Slider de satisfaccion con ingreso.....................
      
      material_column(
        width = 3,
        material_slider(
          input_id = "satisfaccion_ingreso",
          tags$label("¿Qué tan satisfecho(a) se siente con su ingreso?",
                     style = "font-family: Arial;
                     font-size: 15px;
                     color:#FF0080;"),
          min_value = 0,
          max_value = 10,
          initial_value = 5,
          color = "gray60"
          )
        ),
       #...................................................
      
      
 
      ## Slider de satisfaccion con la salud.......................
      
     material_column(
       width = 3,
       material_slider(
         input_id = "satisfaccion_salud",
         tags$label("¿Qué tan satisfecho(a) se siente con su salud?",
                    style = "font-family: Arial;
                    font-size: 15px;
                    color:#FF0080;"),
         min_value = 0,
         max_value = 10,
         initial_value = 5,
         color = "gray60"
         )
       ),
     #...........................................................
     
     
     ## Slider de satisfaccion con la seguridad.......................
     
     material_column(
       width = 3,
       material_slider(
         input_id = "satisfaccion_seguridad",
         tags$label("¿Qué tan satisfecho(a) se siente su nivel seguridad?",
                    style = "font-family: Arial;
                    font-size: 15px;
                    color: #FF0080;"),
         min_value = 0,
         max_value = 10,
         initial_value = 5,
         color = "gray60"
         )
       )
     #...........................................................
     
     
    ),    ### Aqui se cierra el  primera fila 


#........................................................................................


#..................................................Segunda row.........................................



material_row(
  
  ## Slider de que tan feliz se sintio ayer.......................
  
  material_column(
    width = 3,
    material_slider(
      input_id = "satisfaccion_felicidad",
      tags$label("¿Qué tan feliz se sintió el día de ayer?",
                 style = "font-family: Arial ;
                 font-size: 15px;
                 color: #FF0080;"),
      min_value = 0,
      max_value = 10,
      initial_value = 5,
      color = "gray60"
      )
    ),
  
  # Slider que tan tranquilo se sintio ayer ...............
  material_column(
    width = 3,
    material_slider(
      input_id = "satisfaccion_tranquilidad",
      tags$label("¿Qué tan tranquilo(a) se sintió el día de ayer?",
                 style = "font-family: Arial;
                 font-size: 15px;
                 color: #FF0080;"),
      min_value = 0,
      max_value = 10,
      initial_value = 5,
      color = "gray60"
      )
    ),
  #................................................................
  
  # Slider que tanto cosidera que las cosas que hace valen la pena ...............
  
  material_column(
    width = 3,
    material_slider(
      input_id = "satisfaccion_cosas_hace",
      tags$label("¿Qué tanto considera que las cosas que hace en su vida valen la pena?",
                 style = "font-family: Arial;
                 font-size: 15px;
                 color: #FF0080;"),
      min_value = 0,
      max_value = 10,
      initial_value = 5,
      color = "gray60"
      )
    )
  
),  # cierra material_row *****


  ##.....................................................tercera row................................
  
  
  
material_row(
  
  # Slider Frecuencia usa internet...................................................... ...............
  
  material_column(
    width =3,
    material_dropdown(
      input_id = "frecuencia_internet",
      label = "¿Con que frecuencia utiliza internet?",
      choices = c(
        "Todos los días de la semana" = 1,
        "Al menos una vez a la semana" = 2,
        "Al menos una vez al mes" = 3,
        "No utiliza internet" = 5
      ),
      selected = "5"
    )
  ),
  
  # Usa redes sociales...................................................... ...............
  
  material_column(
    width =3,
    material_dropdown(
      input_id = "redes_sociales",
      label = "¿Utiliza redes sociales?",
      choices = c(
        "Si" = 1,
        "No" = 0
       
      ),
      selected = 0
    )
  ),
  
  
  # Slider Hijos...................................................... ...............
  
  material_column(
    width =3,
  material_number_box(
    input_id = "numero_hijos",
    label = "Número de hijos",
    min_value = 1,
    max_value = 6,
    initial_value = 1,
    color = "dodgerblue"
    )
  ),
  
  
  # Slider Nietos....................................................................
  
  
  material_column(
    width =3,
    material_number_box(
      input_id = "numero_nietos",
      label = "Número de nietos",
      min_value = 0,
      max_value = 8,
      initial_value = 1,
      color = "dodgerblue"
    )
  )
),
  



##.............................................................................................................

## ..............Boton ...........................

    material_row(
      align = 'center',
      actionButton("calcular", "calcular", 
                   style="font-family: Arial;
                   font-size: 20px;
                   color: #FFFFFF;
                   background-color: #088A85;
                   border-color: #FF0080;
                   width: 20%;")
    ),
##...............................................



 #   material_row(
#      material_column(
 #       width = 12,
  #      align = 'center',
   #     material_card(
    #      title = "Su nivel de satisfacción con la vida es:"
     #   )
      #)
  #  ),



###..........................................Aqui se van a mostar los resultado...........................
    
    material_row(
      material_column(
        width = 13,
        align = 'center',
        material_card(
          title = "El nivel de satisfacción que tienes con tu vida actualmente",
          textOutput("satisfaccion_calculada"),
          tags$head(tags$style("#satisfaccion_calculada{
                               font-size: 25px;
                               color: #FF0080;
                               }"
                         )
          )
          )
          )
        ),


#................................. Mostar la clasificación....................................



material_row(
  material_column(
    width = 13,
    align = 'center',
    material_card(
      title = "Clasificación",
      textOutput("satisfaccion_clasificada"),
      tags$head(tags$style("#satisfaccion_clasificada{
                           font-size: 25px;
                           color: #FF0080;
                           }"
                         )
      )
      )
      )
  ),


#.................................................................


#....................................................................................................


#...................................... Comentarios al final de la pagina ........................................................


    material_row(
      material_column(
        width = 12,
        material_card(
          tags$h6("El resultado esta en una escala de 0 a 10. Donde 0 significa que se siente totalmente insatisfecho y 10 significa que se siente totalmente satisfecho",
                  style = "font-family: 'Arial';
                  font-weight: 500;
                  font-size: 15px;
                  text-align: center;
                  color: #616263;")
          )
        )
      )

),  # cierra la pagina principal 







####                                 Video................................................#####
###..............................................................................................



  material_side_nav_tab_content(
    side_nav_tab_id = "Video",
    material_row(
      material_column(
        width = 4,
        offset = 1,
        br(),
       tags$h6 ("Video"),
        HTML(paste0('<iframe width="800" height="400" src="https://www.youtube.com/embed/kU0lZ7-cl7w" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>'))
      
      )
    )
  ),





###                                                 Creadores..................................######
###.................................................................................................




  material_side_nav_tab_content(
    side_nav_tab_id = "Creadores",
    material_row(
      tags$h4("¿Quienes somos?",
              style = "font-family: 'Arial Narrow';
                      font-weight: 500;
                      text-align: center;
                      color: #616263;"
      ),
      br(),
      tags$style(HTML("
                      h6{
                        font-family: 'Arial';
                        font-size: 20px;              
                        text-align: center;
                        color: dodgerblue;
                        }"
                      
                      )),
      
      
      tags$h6("John Bryan Yepez Herrera - jbyepezh@unal.edu.co",
              style = "font-family: 'Arial';
              font-weight: 5;
              text-align: center;
              color: #616263;"
            ),
      
      
      
      tags$h6("Heber Esteban Bermúdez González - hebermudezg@unal.edu.co",
              style = "font-family: 'Arial';
              font-weight: 5;
              text-align: center;
              color: #616263;"
      ),
      
      
      tags$h6("Daniel Felipe Tamayo Garcia - dftamayog@unal.edu.co",
              style = "font-family: 'Arial';
              font-weight: 5;
              text-align: center;
              color: #616263;"
      ),
      
      
      tags$h6("Simon Zapata Gutierrez - sizapatagu@unal.edu.co",
              style = "font-family: 'Arial';
              font-weight: 5;
              text-align: center;
              color: #616263;"
      ),
      
      
      tags$h6("Nelson Smith Ordónez Martinez - neordoñezm@unal.edu.co",
              style = "font-family: 'Arial';
              font-weight: 5;
              text-align: center;
              color: #616263;"
      ),
    br(),
      
      tags$h6("Somos un grupo de Ingenieros y Estadísticos de la Universidad Nacional de Colombia, dueños de la empresa StatiSoft Corporation, que será una las mas importantes firmas de servicios profesionales del mundo, incluyendo auditoría, finanzas, contabilidad, estudios actuariales, asesoramiento y desarollo de software.",
              style = "font-family: 'Arial';
              font-weight: 5;
              text-align:justify;
              color: #000000;"
              )
    
      
      
    )
  ),

###                                                 estructura del hogar..................................######


material_side_nav_tab_content(
  side_nav_tab_id = "estructura",
  material_row(
    material_column(
      width = 4,
      offset = 1,
      br(),
      DT::dataTableOutput('tablaDatos')
    )
  ),
  
  material_row(
    material_column(
      width = 4,
      offset = 1,
      material_card(
        title = "Nivel de Satisfacción",
        textOutput("satisfaccion_seleccion_calculada"),
        tags$head(tags$style("#satisfaccion_seleccion_calculada{
                             font-size: 25px;
                             color: #FF0080;
                             }"
                              )
        )
        )
        ),
    material_column(
      width = 4,
      offset = 1,
      material_card(
        title = "Clasificación",
        textOutput("satisfaccion_seleccion_clasificada"),
        tags$head(tags$style("#satisfaccion_seleccion_clasificada{
                             font-size: 25px;
                             color: #FF0080;
                             }"
                              )
        )
        )
        )
        ),
  material_row(
    material_column(
      width = 8,
      offset = 1,
      forceNetworkOutput(outputId = "networkFamilia")
    )
  )
      )
  )

 #******************************************************* final del contenido*****************************************
#********************************************************************************************************************



