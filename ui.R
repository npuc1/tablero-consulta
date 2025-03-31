# ui

ui <- navbarPage(useShinyjs(),
                 introjsUI(),
                 tags$head(includeHTML(("google-head.html"))),
                 windowTitle = "TI-PNA Consulta",
                 tags$head(tags$link(rel = "shortcut icon", href = "favicon.png")),
                 tags$head(
                   tags$style(HTML("
    #gauge_chart {
      width: 100%;
      height: 400px;
    }
  "))
                 ),
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
                                            HTML("<h6><b>Informe de Ejecución del PI-PNA</b></h6>"),
                                            uiOutput("conditionalDesc"),
                                            uiOutput("conditionalInfoBtn"),
                                            uiOutput("conditionalHr"),
                                            uiOutput("conditionalTreeFilter"),
                                            uiOutput("conditionalGaugeFilter"),
                                            hr(),
                                            div(downloadButton("descargaInforme2", "Descargar 2° Informe")),
                                            div(br(), downloadButton("descargaPIBase2", "Descarga la alineación del PI-PNA")),
                                            div(br(), actionButton("descargaInst", "Descarga el catálogo de Instituciones participantes",
                                                                   icon = icon("download")))
                                          ),
                                          mainPanel(
                                            width = 9,
                                            div(class = "d-block d-lg-none",
                                                HTML("<br/>")),
                                            tabsetPanel(
                                              id = "panelesGrafica",
                                              type = "pills",
                                              tabPanel("Redes",
                                                       echarts4rOutput("network", height = "500%"),
                                                       hr(),
                                                       verbatimTextOutput("descrip_base"),
                                                       downloadButton("descargaBaseRed", "Descarga las actividades seleccionadas")),
                                              tabPanel("Indicadores",
                                                       br(),
                                                       fluidRow(
                                                         column(12, htmlOutput("gauge_title")),
                                                         column(12, echarts4rOutput("gauge_chart", height = "400px")),
                                                         column(12, withSpinner(htmlOutput("indicador_descripcion")))
                                                         ),
                                                       hr(),
                                                       downloadButton("descargaFicha", "Descargar ficha técnica"),
                                                       downloadButton("descargaEst", "Descargar ficha de estrategia"),
                                                       downloadButton("descargaBase", "Descargar datos")
                                                       ),
                                              tabPanel("Actividades",
                                                       withSpinner(plotlyOutput("tree", height = "500%")),
                                                       hr())
                                            )
                                          )
                                        ),
                                        div(br(),
                                            HTML('<p style="color: #999999; font-style: italic; font-size: 12px;">v2.2 <br> Actualizado 09/09/24</p>')) # fecha de última actualización
                                      )),
                             tabPanel("Consulta tabla",
                                      br(),
                                      uiOutput("conditionalStyle"),
                                      fluidPage(
                                        sidebarLayout(
                                          sidebarPanel(
                                            width = 3,
                                            HTML("<h6><b>Informe de Ejecución del PI-PNA</b></h6>"),
                                            HTML("<h7>Se muestran las actividades reportadas por los actores participantes en el PI-PNA, de manera agregada o por actividad individual.</h7>"),
                                            uiOutput("conditionalHr2"),
                                            uiOutput("conditionalTableFilter"),
                                            hr(),
                                            HTML("<h6><b>Elemento programático</b></h6>"),
                                            selectizeInput("filtereje", "Eje", ejes),
                                            selectizeInput("filterobj", "Objetivo",choices = NULL),
                                            selectizeInput("filterest", "Estrategia", choices = NULL),
                                            selectizeInput("filterLA", "Línea de acción", linea),
                                            actionButton("reset", "Restablecer filtros"),
                                            hr(),
                                            div(downloadButton("descargaInforme", "Descarga el 2° Informe")),
                                            div(br(), downloadButton("descargaPIBase", "Descarga la alineación del PI-PNA"))
                                          ),
                                          mainPanel(
                                            width = 9,
                                            div(class = "d-block d-lg-none",
                                                HTML("<br/>")),
                                            withSpinner(uiOutput("tabsetText")),
                                            tabsetPanel(
                                              id = "tipo_tabla",
                                              type = "pills",
                                              tabPanel("Por actividad", 
                                                       HTML("<br>"),
                                                       withSpinner(dataTableOutput("previewTable"))),
                                              tabPanel("Por actor",
                                                       HTML("<br>"),
                                                       withSpinner(dataTableOutput("dataTableInst")))
                                              
                                            ),
                                            div(br()),
                                            downloadButton("descargaTabla", "Descargar datos de tabla"),
                                            downloadButton("descargaReporte", "Descargar datos completos del reporte"),
                                            infoBtn3("exp_descargas") %>% 
                                              bsTooltip("La descarga de datos de tabla genera una base que contiene las actividades que se muestran en la tabla en este momento. La descarga de los datos del reporte genera la base de datos completa, incluidos aquellos reportes que no contienen actividades.",
                                                        opacity = 0.7),
                                            hr()
                                          )
                                        ),
                                        div(HTML('<p style="color: #999999; font-style: italic; font-size: 12px;">v2.2 <br> Actualizado 09/09/24</p>'))
                                      )))
)


