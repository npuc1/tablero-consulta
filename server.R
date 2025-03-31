# Define 1 logic

server <- function(input, output, session) {
  
  # tour start
  
  observe({
    introjs(session)
  })
  
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
    if(input$panelesGrafica == "Actividades") {
      
      HTML("<h7>Esta sección agrega las actividades reportadas por los actores de acuerdo con los elementos programáticos del Programa de Implementación de la Política Nacional Anticorrupción.</h7>")
      
    } else if(input$panelesGrafica == "Redes") {
      
      HTML("<h7>La gráfica representa el reporte de actividades de los actores a las Estrategias del PI-PNA. Un nodo más grande representa más actividades reportadas (para instituciones) o más actividades alineadas (para Estrategias).</h7>")
      
    } else if (input$panelesGrafica == "Indicadores") {
      
      HTML("Los indicadores se encuentran alineados a las 64 Estrategias del PI-PNA. De acuerdo con este documento, los indicadores permitirán la generación de mediciones históricas que derivarán en el establecimiento de metas por Estrategia más pertinentes y relevantes. El Segundo Informe de Ejecución, presentado en la Segunda Sesión Ordinaria del CC-SNA del 3 de mayo de 2024, contiene información con corte al 31 de diciembre de 2023.")
      
    }
  })
  
  # Filtro condicional tree
  
  output$conditionalTreeFilter <- renderUI({
    
    validate(
      need(input$panelesGrafica == "Actividades", "")
    )
    selectizeInput("filtro_act_tree", "Actor", c("Todos", sort(unique(base_acciones$Actores))))
    
  })
  
  # Filtro condicional velocímetro
  
  output$conditionalGaugeFilter <- renderUI({
    
    validate(
      need(input$panelesGrafica == "Indicadores", "")
    )
    selectInput("indicador_select", "Estrategia",
                choices = indicadores_data$indicador)
    
  })
  
  # Filtro y hr condicional tabla
  
  output$conditionalTableFilter <- renderUI({
    
    validate(
      need(input$tipo_tabla == "Por actividad", "")
    )
    selectizeInput("filtro_act_tabla", HTML("<b>Actor</b>"), c("Todos", sort(unique(base_acciones$Actores))))
    
  })
  
  
  output$conditionalHr2 <- renderUI({
    
    validate(
      need(input$tipo_tabla == "Por actividad", "")
    )
    hr()
    
  })
  
  
  # InfoButton
  
  output$conditionalInfoBtn <- renderUI({
    
    validate(
      need(input$panelesGrafica == "Actividades", "")
    )
    infoBtn2("exp_grafica") %>% 
      bsTooltip("La gráfica cuenta el número de actividades únicas reportadas por los actores, por lo que los totales pueden variar si el actor reportó la misma acción en diferentes líneas.",
                opacity = 0.7)
    
  })
  
  # hr
  
  output$conditionalHr <- renderUI({
    
    validate(
      need(input$panelesGrafica %in% c("Actividades", "Indicadores"), "")
    )
    hr()
    
  })
  
  # network output
  
  selected_element <- reactiveVal(NULL)
  
  output$network <- renderEcharts4r({
    e_charts() %>% 
      e_graph(layout = "force",
              animationDuration = 1500,
              color = list("#FFC300", "#6AC72C", "#2E74B5", "#76558D", "#DB397E"),
              label = list(show = TRUE,
                           position = "right",
                           fontSize = 16,
                           fontFamily = "monospace"),
              labelLayout = list(hideOverlap = TRUE),
              emphasis = list(focus = "adjacency",
                              scale = 1.3,
                              lineStyle = list(width = 3,
                                               opacity = 1)),
              selectedMode = TRUE,
              roam = TRUE,
              lineStyle = list(color = "target",
                               curveness = 0.2,
                               opacity = 0.2),
              force = list(edgeLength = 30,
                           repulsion = 45,
                           gravity = 0,
                           layoutAnimation = FALSE),
              title = list(text = "Texto prueba")) %>% 
      e_graph_nodes(nodes_redu, name, value, size, grp, symbol) %>%  
      e_graph_edges(edges_efe, source, target) %>% 
      e_tooltip() %>%
      e_on(
        query = "series.graph.nodes",
        handler = "function(params){
          console.log('Node clicked:', params);
          Shiny.setInputValue('selected_element', {type: 'node', data: params.data, dataType: params.dataType});
        }"
      ) %>%
      e_on(
        query = "series.graph.edges",
        handler = "function(params){
          console.log('Edge clicked:', params);
          Shiny.setInputValue('selected_element', {type: 'edge', data: params.data, dataType: params.dataType});
        }"
      ) # doble click select para la descarga diferenciada de edges y nodos
  })
  
  # observer descripción de base dinámica
  
  observeEvent(input$selected_element, {
    if (!is.null(input$selected_element)) {
      selected_element(input$selected_element)
      shinyjs::enable("descargaBaseRed")
      
      element_data <- input$selected_element$data
      element_type <- input$selected_element$dataType
      
      output$descrip_base <- renderText({
        
        if (element_type == "node") {
          paste(
            "Nodo:", if(!is.null(element_data$name)) element_data$name else "N/A", "\n",
            "No. Actividades:", if(!is.null(element_data$value)) element_data$value else "N/A"
          )
        } else if (element_type == "edge") {
          noAct <- efectivas %>% 
            filter(Institución == element_data$source) %>% 
            filter(Estrategia == sub("^..", "", element_data$target)) %>% 
            nrow()
          
          paste(
            "Institución:", if(!is.null(element_data$source)) element_data$source else "N/A", "\n",
            "Estrategia:", if(!is.null(element_data$target)) sub("^..", "", element_data$target) else "N/A", "\n",
            "No. Actividades:", noAct
          )
        } else {
          paste("Unknown element type:", element_type)
        }
      })
    }
  })
  
  shinyjs::disable("descargaBaseRed") # inicializa el botón deshabilitado
  
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
        hovertemplate = "%{label}<br>Número de actividades: %{value}<extra></extra>",
        marker = list(colors = ~color_code)
      ) %>% 
        config(locale = "es")
      
    } else {
      
      validate(
        need(nrow(dataTreemapFilter %>% filter(actor_filtro == input$filtro_act_tree)) > 0, 
             "No se reportaron actividades")
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
                           ".tabbable > ul[id='tipo_tabla'] > li[class=nav-item] > a[class='nav-link active'] {background-color:", color, "; color:white}")))
    
  })
  
  # output tabla instituciones
  
  output$dataTableInst <- renderDataTable({
    
    acc_tot_texto <- tags$span(
      "Actividades totales", 
      infoBtn("exp_acciones") %>% 
        bsTooltip("Se consideran los reportes realizados por las instituciones que hagan mencion a una actividad realizada.",
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
    
    validate(
      need(input$filtro_act_tabla, "Generando tabla...") 
    )
    
    if(input$filtro_act_tabla == "Todos") {
      
      if(input$filtereje == "Todos") {
        return(base_expandida %>%
                 select(c(31, 10, 11, 13, 20, 21, 14, 15)) %>% 
                 setnames("No.Estrategia", "Estrategia"))
        
      } 
      
      else if (input$filtereje != "Todos" & input$filterobj == "Todos") {
        return(base_expandida %>% 
                 filter(Eje == input$filtereje)%>%
                 select(c(31, 10, 11, 13, 20, 21, 14, 15)) %>% 
                 setnames("No.Estrategia", "Estrategia"))
      }
      
      else if (input$filtereje != "Todos" & input$filterobj != "Todos" & input$filterest == "Todas") {
        return(base_expandida %>% 
                 filter(Objetivo.Específico == input$filterobj) %>%
                 select(c(31, 10, 11, 13, 20, 21, 14, 15)) %>% 
                 setnames("No.Estrategia", "Estrategia"))
      }
      
      else if (input$filtereje != "Todos" & input$filterobj != "Todos" & input$filterest != "Todas" & input$filterLA == "Todas") {
        return(base_expandida %>% 
                 filter(No.Estrategia == input$filterest) %>%
                 select(c(31, 10, 11, 13, 20, 21, 14, 15)) %>% 
                 setnames("No.Estrategia", "Estrategia"))
      }
      
      else if (input$filtereje != "Todos" & input$filterobj != "Todos" & input$filterest != "Todas" & input$filterLA != "Todas") {
        return(base_expandida %>% 
                 filter(No.Línea.de.Acción == input$filterLA) %>%
                 select(c(31, 10, 11, 13, 20, 21, 14, 15)) %>% 
                 setnames("No.Estrategia", "Estrategia"))
      } 
      
    } else {
      
      if(input$filtereje == "Todos") {
        return(base_expandida %>%
                 filter(Actor == input$filtro_act_tabla) %>% 
                 select(c(31, 10, 11, 13, 20, 21, 14, 15)) %>% 
                 setnames("No.Estrategia", "Estrategia"))
        
      } 
      
      else if (input$filtereje != "Todos" & input$filterobj == "Todos") {
        return(base_expandida %>% 
                 filter(Actor == input$filtro_act_tabla) %>% 
                 filter(Eje == input$filtereje)%>%
                 select(c(31, 10, 11, 13, 20, 21, 14, 15)) %>% 
                 setnames("No.Estrategia", "Estrategia"))
      }
      
      else if (input$filtereje != "Todos" & input$filterobj != "Todos" & input$filterest == "Todas") {
        return(base_expandida %>% 
                 filter(Actor == input$filtro_act_tabla) %>% 
                 filter(Objetivo.Específico == input$filterobj) %>%
                 select(c(31, 10, 11, 13, 20, 21, 14, 15)) %>% 
                 setnames("No.Estrategia", "Estrategia"))
      }
      
      else if (input$filtereje != "Todos" & input$filterobj != "Todos" & input$filterest != "Todas" & input$filterLA == "Todas") {
        return(base_expandida %>% 
                 filter(Actor == input$filtro_act_tabla) %>% 
                 filter(No.Estrategia == input$filterest) %>%
                 select(c(31, 10, 11, 13, 20, 21, 14, 15)) %>% 
                 setnames("No.Estrategia", "Estrategia"))
      }
      
      else if (input$filtereje != "Todos" & input$filterobj != "Todos" & input$filterest != "Todas" & input$filterLA != "Todas") {
        return(base_expandida %>% 
                 filter(Actor == input$filtro_act_tabla) %>% 
                 filter(No.Línea.de.Acción == input$filterLA) %>%
                 select(c(31, 10, 11, 13, 20, 21, 14, 15)) %>% 
                 setnames("No.Estrategia", "Estrategia"))
      }
      
    }
    
  })
  
  base_expandida = rename(base_expandida, c(`Línea de acción` = Línea.de.Acción,
                                            `Otros actores participantes` = Otros.actores.participantes,
                                            `Actividad reportada` = Acción.reportada,
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
  
  # output indicadores 
  # gráfica
  
  output$gauge_title <- renderText({
    req(input$indicador_select)
    
    selected_title <- indicadores_data$titulo[indicadores_data$indicador == input$indicador_select]
    return(HTML(glue("<p style=\"font-size: 18px; font-weight: bold; text-align: center;\">{selected_title}</p>")))
  })
  
  output$gauge_chart <- renderEcharts4r({
    req(input$indicador_select)
    
    selected_value <- indicadores_data$valor[indicadores_data$indicador == input$indicador_select]
    selected_color100 <- indicadores_data$color100[indicadores_data$indicador == input$indicador_select]
    selected_color70 <- indicadores_data$color70[indicadores_data$indicador == input$indicador_select]
    selected_color30 <- indicadores_data$color30[indicadores_data$indicador == input$indicador_select]
    
    # value check
    
    if(is.na(selected_value)) {
      
      selected_exception <- indicadores_data$excepcion[indicadores_data$indicador == input$indicador_select]
      
      return(e_charts() %>%
               e_gauge(value = 0, 
                       name = str_c("Indicador ", input$indicador_select),
                       min = 0,
                       max = 100,
                       startAngle = 180,
                       endAngle = 0,
                       radius = "90%",
                       center = c("50%", "64%"),
                       itemStyle = list(color = "#f3f3f3"),
                       progress = list(show = TRUE, color = "#f3f3f3"),
                       axisLine = list(
                         lineStyle = list(
                           width = 35,
                           color = list(
                             c(0.3, "#f3f3f3"),
                             c(0.7, "#f3f3f3"),
                             c(1, "#f3f3f3")
                           )
                         )
                       ),
                       pointer = list(icon = "none"),
                       axisTick = list(distance = -40, length = 15, lineStyle = list(color = "#e0e0e0", width = 2)),
                       splitLine = list(distance = -40, length = 25, lineStyle = list(color = "#e0e0e0", width = 2)),
                       axisLabel = list(color = "#e0e0e0", distance = -20, fontSize = 15),
                       detail = list(
                         valueAnimation = TRUE, 
                         formatter = "{value}%", 
                         color = "#ffffff",
                         fontSize = 25,
                         fontWeight = "bold"
                       )
               ) %>%
               e_title(text = NULL,
                       subtext = selected_exception,
                       left = "center",
                       subtextStyle = list(fontSize = 14,
                                           fontWeight = "bold")) %>% 
               e_animation(duration = 1000))
    }
    
    # función color
    
    get_color <- function(value) {
      if (value <= 30) return(selected_color30)
      if (value <= 70) return(selected_color70)
      return(selected_color100)
    }
    
    color <- get_color(selected_value)
    
    e_charts() %>%
      e_gauge(value = selected_value, 
              name = str_c("Indicador ", input$indicador_select),
              min = 0,
              max = 100,
              startAngle = 180,
              endAngle = 0,
              radius = "90%",
              center = c("50%", "55%"),
              itemStyle = list(color = color),
              progress = list(show = TRUE, color = color),
              axisLine = list(
                lineStyle = list(
                  width = 35,
                  color = list(
                    c(0.3, selected_color30),
                    c(0.7, selected_color70),
                    c(1, selected_color100)
                  )
                )
              ),
              pointer = list(length = "65%",
                             width = 8,
                             itemStyle = list(color = color)),
              axisTick = list(distance = -40, length = 15, lineStyle = list(color = "#fff", width = 2)),
              splitLine = list(distance = -40, length = 25, lineStyle = list(color = "#fff", width = 2)),
              axisLabel = list(color = selected_color100, distance = -20, fontSize = 15),
              detail = list(
                valueAnimation = TRUE, 
                formatter = "{value}%", 
                color = selected_color100,
                fontSize = 25,
                fontWeight = "bold"
              )
      ) %>%
      e_animation(duration = 1000)
  })
  
  # descripción
  
  output$indicador_descripcion <- renderUI({
    req(input$indicador_select)
    
    selected_description <- indicadores_data$descripcion[indicadores_data$indicador == input$indicador_select]
    
    HTML(paste0(selected_description))
  })
  
  # Descarga datos zip
  
  output$descargaTabla <- downloadHandler(
    filename = paste0("base_actividades_", Sys.Date(), ".xlsx"),
    content = function(file){
      write.xlsx(previewData(),
                 file,
                 row.names = FALSE)
      }
    )
  
  output$descargaReporte <- downloadHandler(filename = "Reporte_Actividades_PI-PNA_2023.xlsx",
                                          content = function(file){
                                            file.copy("download files/reporte_actividades.xlsx", file)
                                           
                                          })
  
  
  # Descarga informe (2 botones)
  
  output$descargaInforme <- downloadHandler(
    filename = "Segundo Informe de ejecución del PI-PNA.pdf",
    content = function(file) {
      file.copy("download files/informe.pdf", file)
    }
  )
  
  output$descargaInforme2 <- downloadHandler( # ID cambia para corregir error cuando se duplicaba el botón en panel condicional
    filename = "Primer Informe de ejecución del PI-PNA.pdf",
    content = function(file) {
      file.copy("download files/informe.pdf", file)
    }
  ) 
  
  # Descarga alineación PI (2 botones misma lógica que el anterior)
  
  output$descargaPIBase <- downloadHandler( 
    filename = "Alineación del Programa de Implementación a la Política Nacional Anticorrupción.xlsx",
    content = function(file) {
      file.copy("download files/aliPIPNA.xlsx", file)
    }
  ) 
  
  output$descargaPIBase2 <- downloadHandler( 
    filename = "Alineación del Programa de Implementación a la Política Nacional Anticorrupción.xlsx",
    content = function(file) {
      file.copy("download files/aliPIPNA.xlsx", file)
    }
  ) 
  
  # modal descarga opciones
  
  observeEvent(input$descargaInst, {
    
    showModal(modalDialog(
      "Formato de la descarga:",
      br(),
      br(),
      downloadButton("instExcel", "Excel"),
      downloadButton("instPDF", "PDF"),
      footer = modalButton("Cerrar")
    ))
    
    
  })
  
  # download handler opcional
  
  output$instExcel <- downloadHandler( 
    filename = "Catalogo de instituciones.xlsx",
    content = function(file) {
      file.copy("download files/catInst.xlsx", file)
    }
  ) 
  
  output$instPDF <- downloadHandler( 
    filename = "Catalogo de instituciones.pdf",
    content = function(file) {
      file.copy("download files/catInst.pdf", file)
    }
  )
  
  # descarga ficha indicador
  
  output$descargaFicha <- downloadHandler(
    filename = function(file) {
      
      noIndSelec <- input$indicador_select
      str_c("Ficha del Indicador ", noIndSelec, ".pdf")
      
    },
    content = function(file) {
      
      noIndSelec <- input$indicador_select
      file.copy(paste0("download files/fichasInd/E",  noIndSelec, ".pdf"), file)
      
    }
  )
  
  # descarga ficha estrategia
  
  output$descargaEst <- downloadHandler(
    filename = function(file) {
      
      noIndSelec <- input$indicador_select
      str_c("Ficha de la Estrategia ", noIndSelec, ".pdf")
      
    },
    content = function(file) {
      
      noIndSelec <- input$indicador_select
      file.copy(paste0("download files/fichasEst/E",  noIndSelec, ".pdf"), file)
      
    }
  )
  
  # descarga datos indicador
  
  output$descargaBase <- downloadHandler(
    filename = function(file) {
      
      noIndSelec <- input$indicador_select
      str_c("Datos del Indicador ", noIndSelec, ".xlsx")
      
    },
    content = function(file) {
      
      noIndSelec <- input$indicador_select
      file.copy(paste0("download files/datos/",  noIndSelec, ".xlsx"), file)
      
    }
  )
  
  # descarga redes
  
  actSelecRedes <- reactiveVal(NULL)
  
  observeEvent(input$selected_element, {
    
    selected_element(input$selected_element)
    element_data <- input$selected_element$data
    element_type <- input$selected_element$dataType
    
    if(element_type == "edge") {
      
      actSelecRedes(efectivas %>% 
                      filter(Institución == element_data$source) %>% 
                      filter(Estrategia == sub("^..", "", element_data$target)))
      
    } else if(element_data$symbol == "circle") {
      
      actSelecRedes(efectivas %>% 
                      filter(Estrategia == sub("^..", "", element_data$name)))
      
    } else if (element_data$symbol == "roundRect") {
      
      actSelecRedes(efectivas %>% 
                      filter(Institución == element_data$name))
      
    }
    
  })
  
  output$descargaBaseRed <- downloadHandler(
    filename = paste0("base_actividades_", Sys.Date(), ".xlsx"),
    content = function(file) {
      write.xlsx(actSelecRedes(), 
                file, 
                row.names = FALSE)
    }
  )
  
}













