library(shiny)
library(shinydashboard)
library(dygraphs)
library(leaflet)
library(DT)

ui <- dashboardPage(title = "IIEG: Incidencia delictiva (SESNSP)", skin = "purple",
                    
                    dashboardHeader(title = "Área de selección",
                                    dropdownMenu(type = "notifications",
                                                 notificationItem("Fecha de actualización: 21-09-2020")
                                    )
                    ),
                    dashboardSidebar(collapsed = FALSE,
                      
                      
                      sidebarMenu(id = "sidebar",
                        menuItem("Gráficas", tabName = "graphs", icon = icon("chart-bar")),
                        menuItem("Datos", tabName = "data", icon = icon("list-alt"), badgeLabel = ".csv", badgeColor = "blue"),
                        conditionalPanel(condition = "input.sidebar == 'graphs'",
                        selectInput("crime", label = tags$h4("Delito", align = "center"),
                                    choices = c("Total",
                                                "Aborto",
                                                "Abuso de confianza",
                                                "Abuso sexual",
                                                "Acoso sexual",
                                                "Allanamiento de morada",
                                                "Amenazas",
                                                "Contra el medio ambiente",
                                                "Corrupción de menores",
                                                "Daño a la propiedad",
                                                "Delitos cometidos por servidores públicos",
                                                "Despojo",
                                                "Electorales",
                                                "Evasión de presos",
                                                "Extorsión",
                                                "Falsedad",
                                                "Falsificación",
                                                "Feminicidio",
                                                "Fraude",
                                                "Homicidio culposo",
                                                "Homicidio doloso",
                                                "Hostigamiento sexual",
                                                "Incesto",
                                                "Incumplimiento de obligaciones de asistencia familiar",
                                                "Lesiones culposas",
                                                "Lesiones dolosas",
                                                "Narcomenudeo",
                                                "Otros delitos contra el patrimonio",
                                                "Otros delitos contra la familia",
                                                "Otros delitos contra la sociedad",
                                                "Otros delitos del Fuero Común",
                                                "Otros delitos que atentan contra la libertad personal",
                                                "Otros delitos que atentan contra la libertad y la seguridad sexual",
                                                "Otros delitos que atentan contra la vida y la integridad corporal",
                                                "Otros robos",
                                                "Rapto",
                                                "Robo a casa habitación",
                                                "Robo a institución bancaria",
                                                "Robo a negocio",
                                                "Robo a transeúnte en espacio abierto al público",
                                                "Robo a transeúnte en vía pública",
                                                "Robo a transportista",
                                                "Robo de autopartes",
                                                "Robo de ganado",
                                                "Robo de maquinaria",
                                                "Robo de vehículo automotor",
                                                "Robo en transporte individual",
                                                "Robo en transporte público colectivo",
                                                "Robo en transporte público individual",
                                                "Secuestro",
                                                "Tráfico de menores",
                                                "Trata de personas",
                                                "Violación equiparada",
                                                "Violación simple",
                                                "Violencia de género en todas sus modalidades distinta a la violencia familiar",
                                                "Violencia familiar"),
                                    selected = "Total"),
                        conditionalPanel(condition = "input.tabs==1",
                          fluidRow(
                            column(width = 4,
                                   radioButtons("year", tags$h4("Año"),
                                              choices = c(2016:2020),
                                              selected = 2020)
                              ),
                            column(width = 4,    
                                 conditionalPanel(condition = "input.year < 2020",
                                                  radioButtons("month", tags$h4("Mes"),
                                                               choices = c("Enero", "Febrero",
                                                                           "Marzo", "Abril",
                                                                           "Mayo", "Junio",
                                                                           "Julio", "Agosto",
                                                                           "Septiembre", "Octubre",
                                                                           "Noviembre", "Diciembre",
                                                                           "Total"),
                                                               selected = "Enero")),
                                 
                                 conditionalPanel(condition = "input.year == 2020",
                                                  radioButtons("month2", tags$h4("Mes"),
                                                               choices = c("Enero", "Febrero",
                                                                           "Marzo", "Abril",
                                                                           "Mayo", "Junio",
                                                                           "Julio", "Agosto"),
                                                               selected = "Agosto"))
                            )
                          ),
                          conditionalPanel(condition = "input.month != 'Total' | input.year == 2020",
                                           selectInput("period", 
                                                       label = tags$h4("Periodo de comparación"),
                                                       choices = c("Mismo mes, año anterior",
                                                                   "Mes anterior"),
                                                       selected = "Mismo mes, año anterior")
                          ),
                              
                          conditionalPanel(condition = "input.month == 'Total' & input.year != 2020",
                                           selectInput("period2", 
                                                       label = tags$h4("Periodo de comparación", align = "center"),
                                                       choices = c("Año anterior"),
                                                       selected = "Año anterior")
                          )
                          ),
                          conditionalPanel(condition = "input.tabs==2",
                                
                            selectInput("state1", label = tags$h4("Entidades a comparar", align = "center"),
                                        
                                        choices = c("Aguascalientes", "Baja California",
                                                    "Baja California Sur", "Campeche",
                                                    "Coahuila", "Colima", "Chiapas",
                                                    "Chihuahua", "Ciudad de México",
                                                    "Durango", "Guanajuato", "Guerrero",
                                                    "Hidalgo", "Jalisco", "Estado de México",
                                                    "Michoacán", "Morelos", "Nayarit",
                                                    "Nuevo León", "Oaxaca", "Puebla",
                                                    "Querétaro", "Quintana Roo",
                                                    "San Luis Potosí", "Sinaloa", "Sonora",
                                                    "Tabasco", "Tamaulipas", "Tlaxcala",
                                                    "Veracruz", "Yucatán", "Zacatecas"),
                                        selected = "Jalisco"),
                            
                            selectInput("state2", 
                                        label = "",
                                        choices = c("Promedio nacional", "Aguascalientes", "Baja California",
                                                    "Baja California Sur", "Campeche",
                                                    "Coahuila", "Colima", "Chiapas",
                                                    "Chihuahua", "Ciudad de México",
                                                    "Durango", "Guanajuato", "Guerrero",
                                                    "Hidalgo", "Jalisco", "Estado de México",
                                                    "Michoacán", "Morelos", "Nayarit",
                                                    "Nuevo León", "Oaxaca", "Puebla",
                                                    "Querétaro", "Quintana Roo",
                                                    "San Luis Potosí", "Sinaloa", "Sonora",
                                                    "Tabasco", "Tamaulipas", "Tlaxcala",
                                                    "Veracruz", "Yucatán", "Zacatecas"),
                                        selected = "Promedio nacional")
                          )
                        )
                      )
                    ),
                    dashboardBody(
                      
                      tabItems(
                        tabItem(tabName = "graphs",
                                
                                fluidRow(
                                  tabBox(id = "tabs", width = 12, 
                                    tabPanel(title = "Incidencia", icon = icon("chart-bar"), value = 1,
                                             fluidRow(
                                               infoBox("Registros en Jalisco", textOutput("totalcrimes"), color = "maroon", icon = icon("folder")),
                                               infoBox("Tasa delictiva", textOutput("crimerate"), color = "green", icon = icon("users")),
                                               infoBox("Cambio porcentual", textOutput("crimechange"), color = "navy", icon = icon("percent"))
                                             ),
                                             fluidRow(
                                               column(6,
                                                tags$h3(textOutput("title1"), align = "center"),
                                                plotOutput("g1", height = "65rem"),
                                                helpText("La tasa es igual al número de carpetas o averiguaciones por cada cien mil habitantes. Se emplean las proyecciones de población a mitad de año del CONAPO.")
                                                ),
                                               column(6,
                                                      fluidRow(
                                                       tags$h3(textOutput("title2"), align = "center"),
                                                       leafletOutput("g2"),
                                                       helpText("El cambio porcentual se refiere a la cantidad adicional de carpetas o averiguaciones en el periodo seleccionado, como proporción del periodo de comparación.")
                                               ),
                                               fluidRow(
                                                 tags$h3(textOutput("title3"), align = "center"),
                                                 plotOutput("g4", height = "22rem")
                                               )
                                               )
                                              )
                                             ),
                                    tabPanel(title = "Serie", icon = icon("chart-line"), value = 2, 
                                             tags$h3(textOutput("title4"), align = "center"),
                                             dygraphOutput("g3"),
                                             helpText("El promedio nacional no contempla la entidad seleccionada, se calcula dividiendo por 31 el total de delitos ocurridos en los otros estados de la república."))
                                  )
                                )
                        ),
                        tabItem(tabName = "data",
                                helpText("La información fue obtenida del listado de incidencia delictiva del fuero común del ", tags$a("Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública",
                                                                                   href = "https://www.gob.mx/sesnsp", target = "_blank"), "y de las proyecciones de población a mitad de año del ",
                                         tags$a("Consejo Nacional de Población.", href = "https://www.gob.mx/conapo", target = "_blank")),

                                helpText(tags$h4("Incidencia delictiva (.csv)")),
                                downloadButton("download", "Descarga"),
                                tags$br(), 
                                tags$br(),
                                 DT::dataTableOutput("dtable"),
                                tags$br(),
                                helpText(tags$h4("Proyecciones poblacionales (.csv)")),
                                downloadButton("download2", "Descarga"),
                                tags$br(), 
                                tags$br(),
                                DT::dataTableOutput("dtable2")
                        )       
                      )
                    )
)
