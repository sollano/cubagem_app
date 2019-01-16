# Pacotes ####
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
suppressPackageStartupMessages(library(ggpmisc))
library(openxlsx)
library(rmarkdown)
library(stringr)
library(googledrive)

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
          
          
          
          # Version ####
          navbarPage("App Cubagem 1.0.7",id="tab",
          # ####           
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
                                  
                                  radioButtons("df", 
                                               "Tipo da base de dados:", 
                                               choices = c("Dados em nivel de secao",
                                                           "Dados em nivel de arvore"),
                                               selected = "Dados em nivel de secao"),
                                  
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
                                           h3("*Diâmetro da seção"),
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
                                           h3("Comprimento da seção"),
                                           p("Selecione o nome da variável referente à comprimento da seção"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_comp_secao")
                                         ))

                                ), # fluidRow 1
                                
                                fluidRow(
                                  column(4,
                                         wellPanel(
                                           h3("Espessura da casca"),
                                           p("Selecione o nome da variável referente à Espessura da casca:"#, 
                                             #style = "font-family: 'Source Sans Pro';"
                                           ),
                                           uiOutput("selec_e_casca")
                                         )),
                                  
                                  
                                  column(4,
                                         wellPanel(
                                           h3("*Diâmetro (DAP)"),
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
                                           h3("*Árvore"),
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
                                    ), # fluidRow 3
                                
                                # Fluidrow opcional que so aparece na ui se o usuario selecionar o dado em nivel de arvore
                                # da a opcao do usuario selecionar a coluna do volume
                                 uiOutput("selec_vol") #FluidRow 4
                                
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
                                    
                                    h4("Diâmetro mínimo"),
                                    numericInput("diam.min", "Insira o diâmetro mínimo:", 0, 0, 100, 1),
                                    
                                    h4("Intervalo de classe"),
                                    numericInput("int.classe.dap", "Insira o intervalo de classe:", 5, 1, 50, 0.5),
                                    
                                    h3("Converter diâmetro da seção de milímetro para centímetro?"),
                                    radioButtons("di_to_cm","Marcar 'Sim' caso o diametro da seção esteja em milímetro",c("Sim"=TRUE,"Nao"=FALSE), selected = FALSE, inline = TRUE),
                                    
                                    h3("Converter altura da seção de centímetro para metro?"),
                                    radioButtons("hi_to_m","Marcar 'Sim' caso a altura da seção esteja em centímetro",c("Sim"=TRUE,"Nao"=FALSE), selected = FALSE, inline = TRUE),
                              
                                    h3("Converter espessura da casca de milímetro para centímetro?"),
                                    radioButtons("e_casca_to_cm","Marcar 'Sim' caso a espessura da casca da seção esteja em milímetro",c("Sim"=TRUE,"Nao"=FALSE), selected = TRUE, inline = TRUE),

                                    h3("Filtrar dados"),
                                    
                                    uiOutput("rm_data_var"),
                                    uiOutput("rm_data_level"),
                                    uiOutput("rm_vars")
                                    
                                    
                                    
                                  ),# sidebarPanel
                                  
                                  mainPanel( tabsetPanel(
                                    tabPanel("Dado pos preparação",
                                             shiny::htmlOutput("avisos_prep"),
                                             DT::dataTableOutput("prep_table"),
                                             hr(),
                                             tableOutput("teste")
                                             
                                    ),
                                    tabPanel("Consistência dos dados",
                                             
                                             radioButtons(
                                               "run_consist",
                                               h3("Deseja verificar a consistência dos dados?"),
                                               choices = c("Sim"=TRUE,"Nao"=FALSE),
                                               selected=FALSE,
                                               inline = TRUE,
                                               width = "200%"),
                                             p("Obs: A consistência requer que a variável DAP esteja mapeada. Recomenda-se mapear também a variável Altura."),
                                             
                                             uiOutput("consist_warning1"),
                                             uiOutput("consist_warning2"),
                                             uiOutput("consist_table_help"),
                                             uiOutput("consist_choice"),
                                             DT::dataTableOutput("consist_table")
                                    )
                                    
                                  ))# mainPanel
                                  
                                  
                                )
                                
                              )
                              
                              
                              
                              
                     ), # tabPanel filtrar dados
                     # tabPanel Cálculo do volume ####
                     tabPanel("Cálculo do volume",
                              
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
                                                      c("Sim"=TRUE,"Nao"=FALSE), selected = FALSE, inline = TRUE),offset = 9
                                  )
                                ),
                                
                                fluidRow(
                                
                                tabsetPanel(
                                  
                                  tabPanel("Tabela da distribuição diamétrica"                 , DT::dataTableOutput("dd_geral_tab") ),
                                  tabPanel("Gráfico do Nº de indivíduos por classe diamétrica" , plotOutput("dd_graph_indv",height = "550px") ),
                                  tabPanel("Gráfico de VCC por classe diamétrica"              , plotOutput("dd_graph_vcc",height = "550px") ),
                                  tabPanel("Gráfico de VSC por classe diamétrica"              , plotOutput("dd_graph_vsc",height = "550px") ),
                                  tabPanel("Gráfico volume com casca x volume sem casca"       , plotOutput("graph_vcc_x_vsc",height = "550px") ),
                                  tabPanel("Gráfico de G por classe diamétrica"                , plotOutput("dd_graph_G",height = "550px") ),
                                  tabPanel("Gráfico da forma média das árvores"                , plotOutput("graph_kozak" ,height = "550px", width = "700px" ))

                                )
                                )
                              )
                              
                     ), # tabPanel AD 
                     
                     # tabPanel Ajuste de modelos volumétricos ####
                     tabPanel("Ajuste de modelos volumétricos",
                              
                              fluidPage(
                                
                                h1("Ajuste de modelos volumétricos", style = "text-align: center;"),
                                br(),
                                fluidRow(
                                  
                                  column(3,
                                         radioButtons("ajuste_p_estrato",
                                                      "Ajustar modelos por estrato?",
                                                      c("Sim"=TRUE,"Nao"=FALSE), selected = FALSE,inline = TRUE)
                                         
                                  )
                                  ),
                                
                                fluidRow(column(6,uiOutput("aviso_ajuste"))),
                                
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
                                             helpText(
                                               "Ao clicar no botão de download, você se declara de acordo com os termos descritos",
                                               a(href="https://docs.google.com/document/d/1nvPcNTHCZJhuqsEYoHdYR9NVc44_AJuaHUynQwveVgk/edit?usp=sharing", "aqui"),
                                               "."
                                             ),
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
                                               helpText(
                                                 "Ao clicar no botão de download, você se declara de acordo com os termos descritos",
                                                 a(href="https://docs.google.com/document/d/1nvPcNTHCZJhuqsEYoHdYR9NVc44_AJuaHUynQwveVgk/edit?usp=sharing", "aqui"),
                                                 "."
                                               ),
                                               
                                               selectInput("graph_d", "Escolha um grafico:", 
                                                           choices = c(
                                                             "Indviduos por CC",
                                                             "VCC por CC",
                                                             "VSC por CC",
                                                             "VCC x VSC",
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



