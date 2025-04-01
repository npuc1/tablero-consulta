library(shiny)
library(dplyr)
library(plotly)
library(openxlsx)
library(here)
library(tidyverse)
library(shinyjs)
library(DT)
library(spsComps)
library(data.table)
library(d3treeR)
library(treemap)
library(catmaply)
library(zip)
library(viridis)
library(shinycssloaders)
library(htmltools)
library(bslib)
library(echarts4r)
library(rintrojs)
library(glue)

base1 <- read.xlsx("base_expandida.xlsx", 1) 
base2 <- read.xlsx("Ejes objetivos.xlsx", 1)
mergedf <- merge(x = base1, y = base2, by = "No.Línea.de.Acción")

mergedf <- mergedf %>% 
  relocate(No.Línea.de.Acción, .after = No.Estrategia)

base_acciones<- mergedf

base_expandida <- mergedf %>% 
  filter(contiene_accion == 1) %>% 
  mutate(lab_est = str_c("Estrategia ", No.Estrategia)) # labels para treeMap

base_reporte <- mergedf %>% 
  mutate(texto.LA = str_remove(Línea.de.Acción, No.Línea.de.Acción)) # texto LA

ejes<-c("Todos","1","2","3","4")
objetivos<-c("Todos","1","2","3","4","5","6","7","8","9","10")
estrategias <- c("Todas", "1.1", "1.2", "2.1", "3.1", "3.2", "4.1", "4.2", "5.1", "6.1", "7.1", "7.2", "8.1", "9.1", "9.2", "10.1", "11.1", "11.2", "12.1", "12.2", "12.3", "13.1", "14.1", "15.1", "15.2", "16.1", "16.2", "17.1", "17.2", "18.1", "18.2", "19.1", "19.2", "19.3", "20.1", "20.2", "21.1", "22.1", "23.1", "23.2", "24.1", "24.2", "25.1", "25.2", "26.1", "27.1", "28.1", "29.1", "29.2", "30.1", "30.2", "30.3", "31.1", "31.2", "32.1", "33.1", "34.1", "35.1", "36.1", "36.2", "37.1", "37.2", "38.1", "39.1", "40.1")
linea<- c("Todas","1.1.1","1.1.2","1.1.3","1.2.1","1.2.2","2.1.1","2.1.2","2.1.3","3.1.1","3.1.2","3.1.3","3.2.1","3.2.2","4.1.1","4.1.2","4.2.1","4.2.2","5.1.1","5.1.2","5.1.3","6.1.1","7.1.1","7.1.2","7.1.3","7.2.1","7.2.2","8.1.1","8.1.2","9.1.1","9.1.2","9.2.1","9.2.2","10.1.1","10.1.2","10.1.3","11.1.1","11.1.2","11.1.3","11.2.1","11.2.2","12.1.1","12.1.2","12.2.1","12.3.1","12.3.2","13.1.1","13.1.2","13.1.3","14.1.1","14.1.2","14.1.3","15.1.1","15.1.2","15.1.3","15.2.1","15.2.2","15.2.3","16.1.1","16.2.1","16.2.2","17.1.1","17.1.2","17.1.3","17.2.1","18.1.1","18.1.2","18.1.3","18.2.1","18.2.2","19.1.1","19.1.2","19.1.3","19.2.1","19.3.1", "20.1.1","20.1.2", "20.1.3","20.2.1","20.2.2", "21.1.1","21.1.2","22.1.1","22.1.2","22.1.3","23.1.1","23.1.2","23.2.1","23.2.2","24.1.1","24.1.2", "24.2.1","24.2.2","25.1.1","25.2.1","25.2.2","25.2.3","26.1.1","26.1.2","26.1.3","27.1.1","27.1.2","27.1.3","28.1.1","28.1.2","28.1.3","29.1.1","29.1.2","29.2.1","29.2.2","30.1.1","30.1.2","30.2.1","30.2.2","30.3.1","30.3.2","31.1.1","31.1.2","31.1.3", "31.2.1","32.1.1","33.1.1","33.1.2","33.1.3","34.1.1","34.1.2","35.1.1", "36.1.1","36.1.2","36.1.3","36.2.1","36.2.2","36.2.3","37.1.1","37.1.2","37.2.1","37.2.2","38.1.1","39.1.1","40.1.1","40.1.2")

# indicadores

indicadores_data <- read.xlsx("base_indicadores.xlsx", 1)

# procesamiento redes

efectivas <- read.xlsx("base_red.xlsx", 1)

est <- efectivas %>% 
  group_by(Estrategia, Eje) %>% 
  summarise(value = n(),
            size = n(),
            symbol = "circle") %>% 
  mutate(Eje = str_c("Eje ", Eje),
         Estrategia = str_c("E ", Estrategia)) %>% 
  rename("grp" = Eje,
         "name" = Estrategia)

act <- efectivas %>% 
  group_by(Institución) %>% 
  summarise(value = n(),
            size = n(),
            symbol = "roundRect",
            grp = "Institución") %>% 
  rename("name" = Institución)

est$name <- as.character(est$name)

nodes_efe <- full_join(act, est)

edges_efe <- efectivas %>% 
  group_by(Institución, Estrategia) %>% 
  summarise(n = n()) %>% 
  select(c(1, 2)) %>% 
  mutate(Estrategia = str_c("E ", Estrategia)) %>% 
  rename("source" = Institución,
         "target" = Estrategia)

edges_efe$target <- as.character(edges_efe$target)

nodes_efe <- nodes_efe %>% 
  mutate(size = ifelse(value/10 < 1, value/10 + 1, value/10))

nodes_redu <- nodes_efe %>% 
  filter(value>=1)

# dataTreemap

dataTreemapFilter <- read.xlsx("treemapDataFilter.xlsx", 1) 

dataTreemap <- read.xlsx("treemapData.xlsx", 1) 

newNames <- c("Eje", "Objetivo", "Estrategia", "Nombre del indicador")

# info buttons

infoBtn <- function(id) {
  actionButton(id,
               label = "",
               icon = icon("question", style = "color: white;"),
               size = "",
               class = 'btn action-button btn-info btn-xs shiny-bound-input',
               style = "background-color: gray; border-color: gray; color: white; border-radius: 50%; height: 22px; width: 22px; padding: 0;"
  )
}

infoBtn2 <- function(id) {
  actionButton(id,
               label = "",
               icon = icon("question", style = "color: white;"),
               size = "",
               class = 'btn action-button btn-info btn-xs shiny-bound-input',
               style = "background-color: gray; border-color: gray; color: white; border-radius: 50%; height: 22px; width: 22px; padding: 0;"
  )
}

infoBtn3 <- function(id) {
  actionButton(id,
               label = "",
               icon = icon("question", style = "color: white;"),
               size = "",
               class = 'btn action-button btn-info btn-xs shiny-bound-input',
               style = "background-color: gray; border-color: gray; color: white; border-radius: 50%; height: 22px; width: 22px; padding: 0;"
  )
}