library(shiny)
library(dplyr)
library(plotly)
library(xlsx)
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

ind_data <- read.xlsx("base_ficha_indicadores.xlsx", 1)

data_long <- ind_data %>% 
  pivot_longer(cols = c(6:9),
               names_to = "col.name",
               values_to = "valor") %>% 
  mutate(estado = case_when(valor == 2 ~ "Sí",
                            valor == 1 ~ "En proceso",
                            valor == 0 ~ "No"),
         aspecto = case_when(col.name == "linea.base" ~ "¿Cuenta con línea base?",
                             col.name == "meta" ~ "¿Cuenta con una meta definida?",
                             col.name == "cambia" ~ "¿Se propondrán cambios al indicador?",
                             col.name == "medios" ~ "¿Cuenta con medios de verificación?",))
# utf8
dataTreemapFilter <- read.csv("treemapDataFilter.csv",
                              encoding = "UTF-8")
dataTreemap <- read.csv("treemapData.csv",
                        encoding = "UTF-8")

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

# ui

ui <- navbarPage(useShinyjs(),
                 tags$head(includeHTML(("google-head.html"))),
                 windowTitle = "TI-PNA Consulta",
                 tags$head(tags$link(rel = "shortcut icon", href = "favicon.png")),
                 title = HTML('<img src="logo_tablero.png" title=\"Tablero de Implementación\" style=\"float:right;width:375px;height:115px;padding-top:20px;\">'),
                 tags$script(
                 HTML("var header = $('.navbar > .container-fluid');
header.append('<div class=\"d-none d-lg-block\" style=\"float:right\"><a href=\"https://www.sesna.gob.mx/programa-implemetancion-pna/\"><img src=\"logo_pi.png\" title=\"Programa de Implementación de la Política Nacional Anticorrupción\" style=\"float:right;width:692px;height:80px;\"></div>');
    console.log(header)")
                 ),
                 tags$style(
                   '[data-value = "Nombre del eje"] {
                   font-size: 15px;
                   } .tab-pane {
                   width: 100%;
                   }
                   
                   [data-value = "Texto del objetivo"] {
                   font-size: 15px;
                   } .tab-pane {
                   width: 100%;
                   }
                   
                   [data-value = "Texto de la estrategia"] {
                   font-size: 15px;
                   } .tab-pane {
                   width: 100%;
                   }
                   
                   [data-value = "Texto de la línea de acción"] {
                   font-size: 15px;
                   } .tab-pane {
                   width: 100%;
                   }'),
                 tabsetPanel(tabPanel("Consulta gráfica",
                                      br(),
                                      fluidPage(
                                        theme = bs_theme(version = 4),
                                        sidebarLayout(
                                          sidebarPanel = sidebarPanel(
                                            width = 3,
                                            HTML("<h6><b>Primer Informe de Ejecución</b></h6>"),
                                            uiOutput("conditionalDesc"),
                                            uiOutput("conditionalInfoBtn"),
                                            uiOutput("conditionalHr"),
                                            uiOutput("conditionalTreeFilter"),
                                            hr(),
                                            downloadButton("descargaReporte2", ("Descargar Informe"))
                                          ),
                                          mainPanel(
                                            width = 9,
                                            div(class = "d-block d-lg-none",
                                                HTML("<br/>")),
                                            tabsetPanel(
                                              id = "panelesGrafica", # ID para accesar en reactive
                                              type = "pills",
                                              tabPanel("Por acción",
                                                       withSpinner(plotlyOutput("tree"))),
                                              tabPanel("Por indicador",
                                                       div(class = "d-block d-md-none",
                                                           HTML("<h7><b>Se recomienda visualizar esta gráfica en modo panorámico desde dispositivos móviles, o bien desde una computadora.</b></h7>")),
                                                       withSpinner(plotlyOutput("heat")),
                                                       withSpinner(htmlOutput("detallesTexto")),
                                                       withSpinner(dataTableOutput("info_indicador")),
                                                       div(br()),
                                                       downloadButton("descargaEstrategia", "Descargar ficha de la estrategia"),
                                                       downloadButton("descargaIndicador", "Descargar ficha del indicador"),
                                                       hr())
                                            )
                                          )
                                        )
                                      )),
                             tabPanel("Consulta tabla",
                                      br(),
                                      uiOutput("conditionalStyle"),
                                      fluidPage(
                                        sidebarLayout(
                                          sidebarPanel(
                                            width = 3,
                                            HTML("<h6><b>Primer Informe de Ejecución</b></h6>"),
                                            HTML("<h7>Se muestran las acciones reportadas por los actores participantes en el PI-PNA, de manera agregada o por acción individual.</h7>"),
                                            hr(),
                                            HTML("<h6><b>Filtros</b></h6>"),
                                            selectizeInput("filtereje", "Eje", ejes),
                                            selectizeInput("filterobj", "Objetivo",choices=NULL),
                                            selectizeInput("filterest", "Estrategia", choices=NULL),
                                            selectizeInput("filterLA", "Línea de acción", linea),
                                            actionButton("reset", "Restablecer filtros"),
                                            hr(),
                                            downloadButton("descargaReporte", ("Descargar Informe"))
                                          ),
                                          mainPanel(
                                            width = 9,
                                            div(class = "d-block d-lg-none",
                                                HTML("<br/>")),
                                            withSpinner(uiOutput("tabsetText")),
                                            tabsetPanel(
                                              id = "tipo-tabla",
                                              type = "pills",
                                              tabPanel("Actores PI-PNA",
                                                       HTML("<br>"),
                                                       withSpinner(dataTableOutput("dataTableInst"))),
                                              tabPanel("Base por acción", 
                                                       HTML("<br>"),
                                                       withSpinner(dataTableOutput("previewTable")))
                                            ),
                                            downloadButton("descargaTabla", "Descargar datos de tabla"),
                                            hr()
                                          )
                                        )
                                      )))
)


# Define server logic

server <- function(input, output, session) {
  
  # Reset button
  
  observeEvent(input$reset, {
    
    updateSelectizeInput(session,
                      "filtereje", 
                      choices = ejes,
                      selected = "Todos",
                      server = TRUE)
    
  })
  
  # Conditional sidebar consulta grafica (por elemento, generar dinamico todo el panel rompe el layout (bug?))
  # Descripción
  
  output$conditionalDesc <- renderUI({
    if(input$panelesGrafica == "Por acción") {
      
      HTML("<h7>Esta sección agrega las acciones reportadas por los actores de acuerdo con los elementos programáticos del Programa de Implementación de la Política Nacional Anticorrupción.</h7>")
      
    } else {
      HTML("<h7>Esta sección resume los aspectos más importantes del seguimiento a los indicadores del PI-PNA realizado a través del informe.</h7>")
    }
  })
  
  # Filtro
  
  output$conditionalTreeFilter <- renderUI({
    
    validate(
      need(input$panelesGrafica == "Por acción", "")
    )
    selectizeInput("filtro_act_tree", "Filtro por actor", c("Todos", sort(unique(base_acciones$Actores))))
    
  })
  
  # InfoButton
  
  output$conditionalInfoBtn <- renderUI({
    
    validate(
      need(input$panelesGrafica == "Por acción", "")
    )
    infoBtn2("exp_grafica") %>% 
      bsTooltip("La gráfica cuenta el número de acciones únicas reportadas por los actores, por lo que los totales pueden variar si el actor reportó la misma acción en diferentes líneas.",
                opacity = 0.7)
    
  })
  
  # hr
  
  output$conditionalHr <- renderUI({
    
    validate(
      need(input$panelesGrafica == "Por acción", "")
    )
    hr()
    
  })
  
  # Treemap output
  
  output$tree <- renderPlotly({
    
    validate(
      need(input$filtro_act_tree, "Generando visualización...")
    )
    
    if(input$filtro_act_tree == "Todos") {
      
      plot_ly(
        data = dataTreemap,
        type = "treemap",
        ids = ~id,
        labels = ~lab,
        customdata =  ~infoLA,
        parents = ~parent,
        values = ~valor,
        branchvalues = "total",
        maxdepth = 3,
        texttemplate = "%{label}<br><br>%{customdata}",
        hovertemplate = "%{label}<br>Número de acciones: %{value}<extra></extra>",
        marker = list(colors = ~color_code)
      ) %>% 
        config(locale = "es")
      
    } else {
      
      validate(
        need(nrow(dataTreemapFilter %>% filter(actor_filtro == input$filtro_act_tree)) > 0, 
             "No se reportaron acciones")
      )
      
      filteredDataTree <- dataTreemapFilter %>% 
        filter(actor_filtro == input$filtro_act_tree)
      
      ejes <- filteredDataTree %>% 
        group_by(parent) %>% 
        summarise(valor = sum(valor)) %>% 
        filter(grepl("Eje", parent)) %>%
        rename(id = parent) %>% 
        mutate(lab = id,
               parent = "PI-PNA",
               actor_filtro = "Eje",
               color_code = case_when(id == "Eje 1" ~ "#6ac72c",
                                      id == "Eje 2" ~ "#3a90c5",
                                      id == "Eje 3" ~ "#72588f",
                                      id == "Eje 4" ~ "#e14586"))
      
      filteredDataTree <- full_join(filteredDataTree, ejes)
      
      plot_ly(
        data = filteredDataTree,
        type = "treemap",
        ids = ~id,
        labels = ~lab,
        customdata = ~infoLA,
        texttemplate = "%{label}<br><br>%{customdata}",
        parents = ~parent,
        values = ~valor,
        branchvalues = "total",
        maxdepth = 3,
        marker = list(colors = ~color_code)
      )
      
    }
    
  })
  
  # Heatmap output
  
  output$heat <- renderPlotly({
    
    catmaply(
      data_long,
      x = indicador,
      y = aspecto,
      z = valor,
      legend_col = estado,
      color_palette = viridis(n = 3, 
                              direction = -1),
      hover_template = paste(nombre.indicador,
                             "<br><i>Clic para ver más detalles<i>",
                             "<extra></extra>"),
      source = "heatplot"
    ) %>% 
      layout(xaxis = list(type = "category",
                          title = "<b>Indicador</b>",
                          tickangle = 45),
             dragmode = "pan") %>% 
      config(locale = "es")
    
  })
  
  # output detalles
  
  output$detallesTexto <- renderUI({
    
    s <- event_data("plotly_click", source = "heatplot")
    noIndSelec <- s[["x"]]
    
    validate(
      need(length(noIndSelec) > 0,
           "Seleccione un indicador")
    )
    
    return(HTML("<h6><b>Detalles del indicador</b></h6>"))
    
  })
  
  output$info_indicador <- renderDataTable({
    
    # extraer datos clic
    
    s <- event_data("plotly_click", source = "heatplot")
    noIndSelec <- s[["x"]]
    
    req(length(noIndSelec) > 0)
    
    selecIndData <- transpose(ind_data %>% 
                                filter(indicador == noIndSelec) %>% 
                                select(2:5))
    
    rownames(selecIndData) <- newNames
    names(selecIndData) <- c(" ")
    
    datatable(selecIndData,
              options = list(dom = "t")) %>% 
      formatStyle(0, fontWeight = "bold")
    
  })
  
  # Table data agrupada
  
  groupedData <- reactive({
    
    if(input$filtereje == "Todos") {
      
      return(base_reporte %>% 
               group_by(Actores) %>% 
               summarise(acc_tot = sum(contiene_accion),
                         lineas_rep = length(unique(No.Línea.de.Acción[reporte == 1])),
                         est_rep = length(unique(No.Estrategia[reporte == 1])))
      )
      
    } else if (input$filtereje != "Todos" & input$filterobj == "Todos") {
      
      return(base_reporte %>% 
               filter(Eje == input$filtereje) %>%
               group_by(Actores) %>% 
               summarise(acc_tot = sum(contiene_accion),
                         lineas_rep = length(unique(No.Línea.de.Acción[reporte == 1])),
                         est_rep = length(unique(No.Estrategia[reporte == 1])))
      )
      
    }  
    
    else if (input$filtereje != "Todos" & input$filterobj != "Todos" & input$filterest == "Todas") {
      
      return(base_reporte %>% 
               filter(Objetivo.Específico == input$filterobj) %>%
               group_by(Actores) %>% 
               summarise(acc_tot = sum(contiene_accion),
                         lineas_rep = length(unique(No.Línea.de.Acción[reporte == 1])),
                         est_rep = length(unique(No.Estrategia[reporte == 1])))
      )
      
    }  
    
    
    else if (input$filtereje != "Todos" & input$filterobj != "Todos" & input$filterest != "Todas" & input$filterLA == "Todas") {
      
      return(base_reporte %>% 
               filter(No.Estrategia == input$filterest) %>%
               group_by(Actores) %>% 
               summarise(acc_tot = sum(contiene_accion),
                         lineas_rep = length(unique(No.Línea.de.Acción[reporte == 1])),
                         est_rep = length(unique(No.Estrategia[reporte == 1])))
      )
      
      
    }
    
    else if (input$filtereje != "Todos" & input$filterobj != "Todos" & input$filterest != "Todas" & input$filterLA != "Todas") {
      
      return(base_reporte %>% 
               filter(No.Línea.de.Acción == input$filterLA) %>%
               group_by(Actores) %>% 
               summarise(acc_tot = sum(contiene_accion),
                         lineas_rep = length(unique(No.Línea.de.Acción[reporte == 1])),
                         est_rep = length(unique(No.Estrategia[reporte == 1])))
      )
    }   
  })
  
  # Generación de filtros dinamicos 
  
  # Eje -> objetivo
  
  observeEvent(input$filtereje, {
    
    if(input$filtereje == "Todos") {
      
      
      updateSelectizeInput(session, "filterobj", choices = c("Seleccione un eje"),
                           server = TRUE)
      updateSelectizeInput(session, "filterest", choices = c("Seleccione un eje"),
                           server = TRUE)
      updateSelectizeInput(session, "filterLA", choices = c("Seleccione un eje"),
                           server = TRUE)
      
    }
    
    else if (input$filtereje !="Todos" ) {
      
      opcionesobj <- base_reporte %>%
        filter(Eje == input$filtereje)%>%
        pull (Objetivo.Específico) 
      
      updateSelectizeInput(session, "filterobj", choices = c("Todos",  opcionesobj ),
                           server = TRUE)
      updateSelectizeInput(session, "filterest", choices = c("Seleccione un Objetivo"),
                           server = TRUE)
      updateSelectizeInput(session, "filterLA", choices = c("Seleccione un Objetivo"),
                           server = TRUE)
      
    }
    
  })
  
  # Objetivo -> est
  
  observeEvent(input$filterobj , {
    
    if (input$filterobj == "Seleccione un eje") {
      
      updateSelectizeInput(session, "filterest", choices = c("Seleccione un eje"),
                           server = TRUE)
      updateSelectizeInput(session, "filterLA", choices = c("Seleccione un eje"),
                           server = TRUE)
    }
    
    else if (input$filterobj == "Todos") {
      
      
      updateSelectizeInput(session, "filterest", choices = c("Seleccione un objetivo" ),
                           server = TRUE) 
      updateSelectizeInput(session, "filterLA", choices = c("Seleccione un objetivo"),
                           server = TRUE)
      
    }
    
    else if (input$filterobj != "Todos") {
      
      
      opcionesEst<-base_reporte$No.Estrategia[base_reporte$Objetivo.Específico==input$filterobj]
      
      
      updateSelectizeInput(session, "filterest", choices = c("Todas",   opcionesEst ),
                           server = TRUE) 
      updateSelectizeInput(session, "filterLA", choices = c("Seleccione una estrategia"),
                           server = TRUE)
      
    }
    
  })
  
  # Estrategia -> LA
  
  observeEvent(input$filterest , {
    
    if (input$filterest== "Seleccione un eje") {
      
      updateSelectizeInput(session, "filterLA", choices = c("Seleccione un eje"),
                           server = TRUE)
    }
    
    else if (input$filterest== "Seleccione un objetivo") {
      
      updateSelectizeInput(session, "filterLA", choices = c("Seleccione un objetivo"),
                           server = TRUE)
    }
    
    else if (input$filterest== "Todas") {
      
      updateSelectizeInput(session, "filterLA", choices = c("Seleccione una estrategia"),
                           server = TRUE)
    }
    
    else if (input$filterest!= "Todas") {
      
      opcionesLA<-base_reporte$No.Línea.de.Acción[base_reporte$No.Estrategia==input$filterest]
      
      updateSelectizeInput(session, "filterLA", choices = c("Todas",   opcionesLA ),
                           server = TRUE) 
      
      
    }
    
  })
  
  # tabset dinamico para texto en jerarquia
  
  output$tabsetText <- renderUI({
    
    if(input$filtereje == "Todos") div() 
    
    else if(input$filterobj == "Todos") {
      
      tabsetPanel(
        type = "pills",
        tabPanel("Nombre del eje", HTML("<p style=\"font-size:8px;\"></p>"), htmlOutput("textoEje")),
        hr()
      )
      
    } else if(input$filterest == "Todas") {
      
      tabsetPanel(
        type = "pills",
        tabPanel("Nombre del eje", HTML("<p style=\"font-size:8px;\"></p>"), htmlOutput("textoEje")),
        tabPanel("Texto del objetivo", HTML("<p style=\"font-size:8px;\"></p>"), htmlOutput("textoObj")),
        hr()
      )
      
    } else if(input$filterLA == "Todas") {
      
      tabsetPanel(
        type = "pills",
        tabPanel("Nombre del eje", HTML("<p style=\"font-size:8px;\"></p>"), htmlOutput("textoEje")),
        tabPanel("Texto del objetivo", HTML("<p style=\"font-size:8px;\"></p>"), htmlOutput("textoObj")),
        tabPanel("Texto de la estrategia", HTML("<p style=\"font-size:8px;\"></p>"), htmlOutput("textoEst")),
        hr()
      )
      
    } else {
      
      tabsetPanel(
        type = "pills",
        tabPanel("Nombre del eje", HTML("<p style=\"font-size:8px;\"></p>"), htmlOutput("textoEje")),
        tabPanel("Texto del objetivo", HTML("<p style=\"font-size:8px;\"></p>"), htmlOutput("textoObj")),
        tabPanel("Texto de la estrategia", HTML("<p style=\"font-size:8px;\"></p>"), htmlOutput("textoEst")),
        tabPanel("Texto de la línea de acción", HTML("<p style=\"font-size:8px;\"></p>"), htmlOutput("textoLA")),
        hr()
      )
      
    }
    
  })
  
  # generacion texto dinamico
  
  txtEjeReactive <- reactive({
    
    filtro <- base_reporte %>% 
      filter(Eje == input$filtereje)
    
    return(filtro$Texto.del.eje[1])
    
  })
  
  txtObjReactive <- reactive({
    
    filtro <- base_reporte %>% 
      filter(Objetivo.Específico == input$filterobj)
    
    return(filtro$Texto.objetivo[1])
    
  })
  
  txtEstReactive <- reactive({
    
    filtro <- base_reporte %>% 
      filter(No.Estrategia == input$filterest)
    
    return(filtro$texto.estrategia[1])
    
  })
  
  txtLAReactive <- reactive({
    
    filtro <- base_reporte %>% 
      filter(No.Línea.de.Acción == input$filterLA)
    
    return(filtro$texto.LA[1])
    
  })
  
  # output texto
  
  output$textoEje <- renderText(paste0("<p style=\"font-size:16px;\">", {txtEjeReactive()}, "</p>"))
  
  output$textoObj <- renderText(paste0("<p style=\"font-size:14px;\">", {txtObjReactive()}, "</p>"))
  
  output$textoEst <- renderText(paste0("<p style=\"font-size:13px;\">", {txtEstReactive()}, "</p>"))
  
  output$textoLA <- renderText(paste0("<p style=\"font-size:13px;\">", {txtLAReactive()}, "</p>"))
  
  # conditional style
  
  output$conditionalStyle <- renderUI({ 
    
    color <- case_when(input$filtereje == "1" ~ "#6ac72c",
                       input$filtereje == "2" ~ "#3a90c5",
                       input$filtereje == "3" ~ "#72588f",
                       input$filtereje == "4" ~ "#e14586",
                       input$filtereje == "Todos" ~ "#847d7a")
    
    tags$style(HTML(paste0(".tabbable > .nav > li[class=active] > a[data-value='Nombre del eje'] {background-color:", color, "; color:white}",
                           ".tabbable > .nav > li[class=active] > a[data-value='Texto del objetivo'] {background-color:", color, "; color:white}",
                           ".tabbable > .nav > li[class=active] > a[data-value='Texto de la estrategia'] {background-color:", color, "; color:white}",
                           ".tabbable > .nav > li[class=active] > a[data-value='Texto de la línea de acción'] {background-color:", color, "; color:white}",
                           ".tabbable > ul[id='tipo-tabla'] > li[class=nav-item] > a[class='nav-link active'] {background-color:", color, "; color:white}")))
    
  })
  
  # output tabla instituciones
  
  output$dataTableInst <- renderDataTable({
    
    acc_tot_texto <- tags$span(
      "Acciones totales", 
      infoBtn("exp_acciones") %>% 
        bsTooltip("Número de acciones individuales reportadas por el actor a lo largo de todas las líneas de acción",
                  opacity = 0.7)
    ) %>% 
      as.character()
    
    datatable((groupedData()) %>% 
                rename("Estrategias reportadas" = est_rep,
                       "Líneas de acción reportadas" = lineas_rep,
                       !!acc_tot_texto:=acc_tot),
              selection = "none",
              rownames = FALSE,
              escape = FALSE,
              options = list(language = list(url = "https://cdn.datatables.net/plug-ins/1.10.21/i18n/Spanish.json"),
                             columnDefs = list(list(className = 'dt-center', targets = 1:3))))
  })
  
  # output tabla lista acciones
  
  previewData <- reactive({
    
    if(input$filtereje == "Todos") {
      return(base_expandida %>%
               select(c(31, 5, 10:15)) %>% 
               setnames("No.Estrategia", "Estrategia"))
      
    } 
    
    else if (input$filtereje != "Todos" & input$filterobj == "Todos") {
      return(base_expandida %>% 
               filter(Eje == input$filtereje)%>%
               select(c(31, 5, 10:15)) %>% 
               setnames("No.Estrategia", "Estrategia"))
    }
    
    else if (input$filtereje != "Todos" & input$filterobj != "Todos" & input$filterest == "Todas") {
      return(base_expandida %>% 
               filter(Objetivo.Específico == input$filterobj) %>%
               select(c(31, 5, 10:15)) %>% 
               setnames("No.Estrategia", "Estrategia"))
    }
    
    else if (input$filtereje != "Todos" & input$filterobj != "Todos" & input$filterest != "Todas" & input$filterLA == "Todas") {
      return(base_expandida %>% 
               filter(No.Estrategia == input$filterest) %>%
               select(c(31, 5, 10:15)) %>% 
               setnames("No.Estrategia", "Estrategia"))
    }
    
    else if (input$filtereje != "Todos" & input$filterobj != "Todos" & input$filterest != "Todas" & input$filterLA != "Todas") {
      return(base_expandida %>% 
               filter(No.Línea.de.Acción == input$filterLA) %>%
               select(c(31, 5, 10:15)) %>% 
               setnames("No.Estrategia", "Estrategia"))
    }   
  } )
  
  base_expandida = rename(base_expandida, c(`Línea de acción` = Línea.de.Acción,
                                            `Otros actores participantes` = Otros.actores.participantes,
                                            `Acción reportada` = Acción.reportada,
                                            `Fecha de inicio` = Fecha.de.inicio,
                                            `Fecha de término` = Fecha.de.término,
                                            Actor = Actores))
  
  output$previewTable <- renderDataTable({
    previewData()
  }, options = list(pageLength = 5,
                    language = list(url = "https://cdn.datatables.net/plug-ins/1.10.21/i18n/Spanish.json"),
                    scrollY = "500px"),
  rownames = FALSE,
  selection = "none")
  
  # Descarga datos zip
  
  nombrebase <- function() {
    if(input$filtereje == "Todos") paste0("base_acciones_", Sys.Date(), ".csv")
    else if(input$filterobj == "Todos") paste0("base_acciones_eje_", input$filtereje, "_", Sys.Date(), ".csv")
    else paste0("base_acciones_filtrada_", Sys.Date(), ".csv")
  }
  
  output$descargaTabla <- downloadHandler(filename = "descarga_tablero.zip",
                                          content = function(file){
                                            
                                            write.csv(previewData(),
                                                      file = nombrebase(),
                                                      row.names = FALSE,
                                                      fileEncoding = "latin1")
                                            
                                            zip::zip(file, files = c(nombrebase(), "diccionario_datos.xlsx"))
                                          })
  
  # Descarga informe (3 botones)
  
  output$descargaReporte <- downloadHandler(
    filename = "Primer Informe de ejecución del PI-PNA.pdf",
    content = function(file) {
      file.copy("download files/informe.pdf", file)
    }
  )
  
  output$descargaReporte2 <- downloadHandler( # ID cambia para corregir error cuando se duplicaba el botón en panel condicional
    filename = "Primer Informe de ejecución del PI-PNA.pdf",
    content = function(file) {
      file.copy("download files/informe.pdf", file)
    }
  ) 
  
  # Bloquear y desbloquear botones de descarga x indicador hasta que se seleccione uno
  
  shinyjs::disable("descargaIndicador")
  shinyjs::disable("descargaEstrategia")
  
  observeEvent(
    event_data("plotly_click", source = "heatplot"), {
      shinyjs::enable("descargaIndicador")
      shinyjs::enable("descargaEstrategia")
    }
  )
  
  # Descarga ficha indicador
  
  output$descargaIndicador <- downloadHandler(
    filename = function(file) {
      
      s <- event_data("plotly_click", source = "heatplot")
      noIndSelec <- s[["x"]]
      
      str_c("Ficha Indicador ", noIndSelec, ".pdf")
    },
    content = function(file) {
      
      s <- event_data("plotly_click", source = "heatplot")
      noIndSelec <- s[["x"]]
      
      file.copy(paste0("download files/fichas indicadores/",  noIndSelec, ".pdf"), file)
    }
  )
  
  # Descarga ficha estrategia (reproducir logica indicador cuando se manden los archivos, cambiar path)
  
  output$descargaEstrategia <- downloadHandler(
    filename = function(file) {
      
      s <- event_data("plotly_click", source = "heatplot")
      noIndSelec <- s[["x"]]
      
      str_c("1er Informe - Ficha Estrategia ", noIndSelec, ".pdf")
    },
    content = function(file) {
      
      s <- event_data("plotly_click", source = "heatplot")
      noIndSelec <- s[["x"]]
      
      file.copy(paste0("download files/fichas estrategia/",  noIndSelec, ".pdf"), file)
    }
  )
  
}

# Run the app
shinyApp(ui = ui, server = server)
