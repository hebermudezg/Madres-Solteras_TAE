library(shiny)
#install.packages("shiny")
library(shinymaterial)
#install.packages("shinymaterial")
#library(tidyverse)
#install.packages("tidyverse")
#library(stringr)
#install.packages("stringr")
#library(shinycssloaders)
#install.packages("shinycssloaders")
library(nnet)
#install.packages("nnet")
#library(raster)
#install.packages("raster")
#library(sp)
#install.packages("sp")
#library(car)
#install.packages("car")
#library(leaflet)
#install.packages("leaflet")
#library(rgdal)
#install.packages("rgdal")
require(colorRamps)
#install.packages("colorRamps")
library(dplyr)
#install.packages("dplyr")
require(MPV)
#install.packages("MPV")
require(MASS)
#install.packages("MASS")
library(readxl)
#install.packages("readxl")
#library(raster)
library(networkD3)

Datosssm <- read_excel("Tabla_Pequena_Filtrada.xlsx")

#devtools::install_github("ericrayanderson/shinymaterial")


## Funcion para el diseño tomado de la pagina:  https://ericrayanderson.github.io/shinymaterial/

git_refs <- function(){
  shiny::tagList(
    tags$a(target = "_blank",
           href = "https://github.com/ericrayanderson/shinymaterial_dashboard/blob/master/ui.R#L1", h3("ui.R")),
    "Includes shinymaterial functions: ",
    tags$ul(style = "font-family:monospace; display:block",
            tags$li("material_side_nav()",
                    "material_side_nav_tabs()",
                    "material_side_nav_tab_content()")
    ),
    tags$a(target = "_blank",
           href = "https://github.com/ericrayanderson/shinymaterial_dashboard/blob/master/server.R#L1", h3("server.R")),
    "Includes shinymaterial functions: ",
    tags$ul(style = "font-family:monospace; display:block",
            tags$li("material_spinner_show()",
                    "material_spinner_hide()")
    ),
    br(),
    br(),
    tags$a(
      target = "_blank",
      href = "https://ericrayanderson.github.io/shinymaterial/",
      "shinymaterial website"
    ) 
  )
}



