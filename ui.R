# Pacotes ####
options(java.parameters = "-Xss2048k")
library(shiny)
suppressPackageStartupMessages(library(DT))
#library(plotly)
library(formattable)
library(readxl)
#library(plyr)
library(tibble)
library(tidyr)
suppressPackageStartupMessages(library(dplyr))
library(lazyeval)
suppressPackageStartupMessages(library(ggplot2))
library(ggthemes)
library(ggpmisc)
suppressPackageStartupMessages(library(xlsx))
library(rJava)
library(xlsxjars)
library(rmarkdown)

# UI ####

shinyUI(
  # Intro, taglists e error messages colors ####
  tagList(tags$style(HTML(".irs-single, .irs-bar-edge, .irs-bar{background: #00a90a}")), # this is actually .css; this changes the color for the sliders
          
          # Cor de todas as mensagens da funcao need
          tags$head(
            tags$style(HTML("
                            .shiny-output-error-validation {
                            color: #00a90a;
                            }
                            "))
            ),
          
          # cor das mensagens que eu especificar com "WRONG"
          tags$head(
            tags$style(HTML("
                            .shiny-output-error-WRONG {
                            color: red;
                            }
                            "))
            ),
          
          # cor das mensagens que eu especificar com "AVISO"
          tags$head(
            tags$style(HTML("
                            .shiny-output-error-AVISO {
                            color: orange;
                            }
                            "))
            ),
          
          
          
          
          navbarPage("App Cubagem",
                     
                     theme = "green_yeti2.css",
                     # theme = "green.css", # seleciona um tema contido na pasta www
                     # theme = shinythemes::shinytheme("paper"), # seleciona um tema utilizando pacote
                     
                     # Painel Intro ####          
                     tabPanel( "Intro" ,
                               fluidRow(
                                 column(5,
                                        includeMarkdown("about.md")
                                 ),
                                 column(6,
                                        img(contentType = "image/jpg",
                                            src="intro_picture.jpg",
                                            width = 770,
                                            #           height = 750)
                                            height = 856)
                                        
                                 )
                               ) # fluid row
                     ), # Painel Intro             
                     
                     
                     # Upload de dados ####
                     tabPanel("Importação",
                              sidebarLayout(
                                
                                sidebarPanel(
                                  
                                  h3("Dados"),
                                  
                                  radioButtons("df_select", 
                                               "Fazer o upload de um arquivo, ou utilizar o dado de exemplo?", 
                                               c("Fazer o upload", 
                                                 "Utilizar o dado de exemplo de Smalian", 
                                                 "Utilizar o dado de exemplo de Huber"), 
                                               selected = "Fazer o upload"),
                                  
                                  uiOutput("upload"), # tipos de arquivos aceitos
                                  hr(),
                                  uiOutput("upload_csv"), # tipos de arquivos aceitos
                                  uiOutput("upload_xlsx") # tipos de arquivos aceitos
                                  
                                  
                                ), # sidebarPanel
                                
                                mainPanel(
                                  DT::dataTableOutput("rawdata")
                                ) # mainPanel
                              ) # sidebarLayout
                     ),
                     
                     # Mapeamento ####
                     tabPanel("Mapeamento de variáveis",
                              fluidPage(
                                
                                #h1("Shiny", span("Widgets Gallery", style = "font-weight: 300"), 
                                h1("Definição dos nomes das variáveis", 
                                   style = "text-align: center;"),
                                br(),
                                
                                #  h4("Nesta aba serão indicados os nomes das colunas que serão utilizadas nas análises em todo o app"),
                                fluidRow(
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Diâmetro da seção"),
                                           p("Selecione o nome da variável referente à Diâmetro da seção:"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_di")
                                         )),
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Altura da seção"),
                                           p("Selecione o nome da variável referente à Altura da seção:"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_hi")
                                         )),
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Espessura da casca"),
                                           p("Selecione o nome da variável referente à Espessura da casca:"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_e_casca")
                                         ))
                                  
                                  
                                ), # fluidRow 1
                                
                                fluidRow(
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Comprimento da seção"),
                                           p("Selecione o nome da variável referente à comprimento da seção"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_comp_secao")
                                         )), # Coluna altura dominante
                                  
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Diâmetro (DAP)"),
                                           p("Selecione o nome da variável referente à DAP:"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_dap")
                                         )), # Coluna dap
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Altura total"),
                                           p("Selecione o nome da variável referente à Altura total:"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_ht")
                                         )) # Coluna ht
                                  
                                  
                                  
                                ),# fluidRow 2
                                
                                fluidRow(
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Árvore"),
                                           p("Selecione o nome da variável referente à Árvore:"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_arvore")
                                         )), # Coluna area.total
                                  
                                  
                                  column(4,
                                         wellPanel(
                                           h3("Estrato"),
                                           p("Selecione o nome da variável referente à Estrato:"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_estrato")
                                         )) # Coluna area.total
                                    )
                                
                              ) # fluidPage 
                              
                              
                     ),# tabPanel Mapeamento
                     
                     # tabPanel Preparação ####
                     tabPanel("Preparação dos dados", 
                              
                              
                              fluidPage(
                                
                                fluidRow(
                                  
                                  h1("Preparação dos dados",style = "text-align: center;"),
                                  br()
                                ),
                                
                                
                                fluidRow(
                                  
                                  sidebarPanel(
                                    h3("Variaveis para graficos de classe de diametro"),
                                    h4("Intervalo de classe"),
                                    numericInput("int.classe.dap", "Insira o intervalo de classe:", 5, 1, 50, 0.5),
                                    
                                    h4("Diâmetro mínimo"),
                                    numericInput("diam.min", "Insira o diâmetro mínimo:", 5, 1, 100, 1),
                                    
                                    h3("Converter diâmetro da seção de milímetro para centímetro?"),
                                    radioButtons("di_to_cm","Marcar 'Sim' caso o diametro da seção esteja em milímetro",c("Sim"=TRUE,"Nao"=FALSE), selected = FALSE, inline = TRUE),
                                    
                                    h3("Converter altura da seção de centímetro para metro?"),
                                    radioButtons("hi_to_m","Marcar 'Sim' caso a altura da seção esteja em centímetro",c("Sim"=TRUE,"Nao"=FALSE), selected = FALSE, inline = TRUE),
                              
                                    h3("Converter espessura da casca de milímetro para centímetro?"),
                                    radioButtons("e_casca_to_cm","Marcar 'Sim' caso a espessura da casca da seção esteja em milímetro",c("Sim"=TRUE,"Nao"=FALSE), selected = TRUE, inline = TRUE),
 
                                    h3("Transformar zero em NA"),
                                    radioButtons("zero_to_NA","Transformar zeros em variávies numéricas em NA? (recomendado)",c("Sim"=TRUE,"Nao"=FALSE), inline = TRUE),
                                    
                                    h3("Remover dados"),
                                    
                                    uiOutput("rm_data_var"),
                                    uiOutput("rm_data_level"),
                                    uiOutput("rm_vars"),
                                    uiOutput("consist_warning1")
                                    
                                    
                                    
                                  ),# sidebarPanel
                                  
                                  mainPanel( tabsetPanel(
                                    tabPanel("Dado pos preparação",
                                             shiny::htmlOutput("avisos_prep"),
                                             DT::dataTableOutput("prep_table"),
                                             hr(),
                                             tableOutput("teste")
                                             
                                    ),
                                    tabPanel("Dados inconsistentes",
                                             uiOutput("consist_warning2"),
                                             uiOutput("consist_table_help"),
                                             uiOutput("consist_choice"),
                                             DT::dataTableOutput("consist_table")
                                    )
                                    
                                  ))# mainPanel
                                  
                                  
                                )
                                
                              )
                              
                              
                              
                              
                     ), # tabPanel filtrar dados
                     # tabPanel cálculo do volume ####
                     tabPanel("cálculo do volume",
                              
                              fluidPage(
                                
                                h1("Cálculo do volume", style = "text-align: center;"),
                                br(),
                                
                                fluidRow(
                                  column( 3,  sliderInput("calc_vol_cd", 
                                                          label = "Selecione o numero de casas decimais:", 
                                                          min = 1, 
                                                          max = 9, 
                                                          value = 4,
                                                          step = 1)),
                                  
                                  column(3,
                                         radioButtons("data_vol_summary",
                                                      "Método de volume que deve ser utilizado:",
                                                      c("Smalian","Huber"), inline = TRUE)
                                  )
                                ),
                                
                                fluidRow(   
                                  tabsetPanel(
                                    tabPanel("Volume pelo método de Smalian",DT::dataTableOutput("tab_smalian") ), 
                                    tabPanel("Volume pelo método de Huber",DT::dataTableOutput("tab_huber") ), 
                                    tabPanel("Totalização das árvores",DT::dataTableOutput("tab_vol_arvore") ),
                                    selected="Totalização das árvores"
                                  )
                                )
                              )
                     ),# TabPanel cálculo do volume
                     
                     
                     
                     # tabPanel Analise descritiva ####
                     tabPanel("Analise descritiva",
                              
                              fluidPage(
                                h1("Análise descritiva", style = "text-align: center;"),
                                br(),
                                
                                fluidRow(
                                  
                                  column(3,
                                         radioButtons("graph_arvore_estrato",
                                                      "Gerar árvore média por estrato?",
                                                      c("Sim"=TRUE,"Nao"=FALSE), inline = TRUE),offset = 9
                                  )
                                ),
                                
                                fluidRow(
                                
                                tabsetPanel(
                                  
                                  tabPanel("Tabela da distribuição diamétrica"                 , DT::dataTableOutput("dd_geral_tab") ),
                                  tabPanel("Gráfico do Nº de indivíduos por classe diamétrica" , plotOutput("dd_graph_indv",height = "550px") ),
                                  tabPanel("Gráfico de VCC por classe diamétrica"              , plotOutput("dd_graph_vcc",height = "550px") ),
                                  tabPanel("Gráfico de VSC por classe diamétrica"              , plotOutput("dd_graph_vsc",height = "550px") ),
                                  tabPanel("Gráfico de G por classe diamétrica"                , plotOutput("dd_graph_G",height = "550px") ),
                                  tabPanel("Gráfico da forma média das árvores"                , plotOutput("graph_kozak" ,height = "550px", width = "700px" ))

                                )
                                )
                              )
                              
                     ), # tabPanel AD 
                     
                     # tabPanel Ajuste de Modelos Volumétricos ####
                     tabPanel("Ajuste de Modelos Volumétricos",
                              
                              fluidPage(
                                
                                h1("Ajuste de Modelos Volumétricos", style = "text-align: center;"),
                                br(),
                                fluidRow(
                                  
                                  column(3,
                                         radioButtons("ajuste_p_estrato",
                                                      "Ajustar modelos por estrato?",
                                                      c("Sim"=TRUE,"Nao"=FALSE), selected = FALSE,inline = TRUE),offset = 0
                                  )
                                ),
                                fluidRow(   
                                  tabsetPanel(
                                    tabPanel("Tabela de Coeficientes",DT::dataTableOutput("ajuste_vol_tab") ), 
                                    tabPanel("Gráficos de resíduo para volume com casca",plotOutput("graph_res_vcc_scatterplot"), plotOutput("graph_res_vcc_histogram"), plotOutput("graph_res_vcc_versus") ),
                                    tabPanel("Gráficos de resíduo para volume sem casca",plotOutput("graph_res_vsc_scatterplot"), plotOutput("graph_res_vsc_histogram"), plotOutput("graph_res_vsc_versus") )
                                  ) 
                                  )
                                )
                              ),# Ajuste de Modelos Volumétricos
                     
                     
                     
                     
                     # navbarMenu  Download ####
                     tabPanel("Download",
                              # Painel Download Tabelas ####
                              
                              fluidPage(
                                
                                
                                h1("Download dos resultados", style = "text-align: center;"),
                                br(),
                                
                                
                                tabsetPanel(
                                  tabPanel("Download de tabelas", 
                                           fluidPage(
                                             
                                             
                                             h2("Download de tabelas", style = "text-align: center;"),
                                             br(),
                                             
                                             fluidRow(
                                               column(
                                                 10
                                                 ,uiOutput("checkbox_df_download")
                                               )
                                               
                                             ),
                                             br(),
                                             
                                             fluidRow(column(3,downloadButton('downloadData', 'Baixar tabelas selecionadas'), offset=4)),
                                             br(),
                                             h3("Ou, para baixar todas as tabelas disponíveis, clique abaixo:"),
                                             fluidRow(
                                               column(3,downloadButton('downloadAllData', 'Baixar todas as tabelas'), offset=4)
                                             )
                                             
                                             
                                             
                                           )
                                  ), # download tabelas
                                  
                                  # Painel Download Graficos ####
                                  
                                  tabPanel("Download de graficos", 
                                           
                                           
                                           
                                           sidebarLayout(
                                             
                                             sidebarPanel(
                                               
                                               tags$style(type="text/css",
                                                          ".recalculating {opacity: 1.0;}"
                                               ),
                                               
                                               h3("Download de graficos"),
                                               
                                               selectInput("graph_d", "Escolha uma grafico:", 
                                                           choices = c(
                                                             "Indviduos por CC",
                                                             "VCC por CC",
                                                             "VSC por CC",
                                                             "G por CC",
                                                             "Forma media das arvores",
                                                             "Dispersao dos residuos VCC",
                                                             "Histograma dos residuos VCC",
                                                             "Obs vs est VCC",
                                                             "Dispersao dos residuos VSC",
                                                             "Histograma dos residuos VSC",
                                                             "Obs vs est VSC"
                                                           )),
                                               
                                               selectInput("graphformat",
                                                           "Escolha o formato do gráfico:",
                                                           choices = c("PNG" = ".png",
                                                                       "JPG" = ".jpg",
                                                                       "PDF" = ".pdf") ),
                                               
                                               downloadButton('downloadGraph', 'Download')
                                               
                                             ),
                                             mainPanel(
                                               plotOutput("graph_d_out",height = "550px")
                                             )
                                           )
                                  ) # download graficos
                                  
                                )       
                              ) # fluidPage
                     ) # final navbarMenu download ####    
                     # final da UI  ####    
          ) # navbarPage
            )#tagList
            ) # ShinyUI



