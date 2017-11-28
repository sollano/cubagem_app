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

# Carregar dados de exemplo e funcoes ####

ex1 <- read.csv2("examples/exemplo_smalian.csv")
ex2 <- read.csv2("examples/exemplo_huber.csv")
source("funs/smaliancc.R"         , encoding="UTF-8")
source("funs/smaliansc.R"         , encoding="UTF-8")
source("funs/hubercc.R"           , encoding="UTF-8")
source("funs/hubersc.R"           , encoding="UTF-8")
source("funs/round_df.R"          , encoding="UTF-8")
source("funs/classe_diametro.R"   , encoding="UTF-8")
source("funs/htdapratio.R"        , encoding="UTF-8")
source("funs/consistency.R"       , encoding="UTF-8")
source("funs/xlsx.write.list.R"   , encoding="UTF-8")
source("funs/lm_table.R"          , encoding="UTF-8")
source("funs/inv.R"               , encoding="UTF-8")
source("funs/residuos_exp.R"      , encoding="UTF-8")
source("funs/cub_summary.R"       , encoding="UTF-8")
source("funs/curva_arvore_media.R", encoding="UTF-8")
source("funs/pow.R"               , encoding="UTF-8")


# Funcao para testar se uma variavel e numerica
# Sera utilizada dentro da funcao validate
check_numeric <- function(input, df, code){
  
  if(is.null(input) ){
    
    
  }else if(is.na(input)){
    
    
  }else if(input == ""){
    
  }else if(!is.null(input) && !is.numeric(df[[input]]) ){
    
    paste(code, "column must be numeric")
    
  }
  
}

# vectors for names ####

di_names <- c("di", "di_cc","d", "di (mm)", "di(cm)" )
hi_names <- c("hi", "hi (cm)", "hi (m)")
e_casca_names <- c("e_casca","espessura_casca", "e_casca (mm)", "e_casca (cm)")
comp_secao_names <- c("comp_secao")
CAP_names <- c("CAP","Cap","cap", "cbh", "Cbh","CBH","CBH_11","CAP(cm)","CAP(cm)","Cap (cm)","Cap(cm)")
DAP_names <- c("DAP","Dap","dap", "dbh", "Dbh","DBH","DBH_11","DAP(cm)","DAP(cm)","Dap (cm)","Dap(cm)")
HT_names <- c("HT_EST", "HT", "Ht", "ht","Htot","ALTURA","Altura","Altura_Total", "ALTURA_TOTAL","HT (m)","HT(m)","Ht (m)","Ht(m)","Altura Total (m)","Altura total(m)", "altura", "Altura", "ALTURA")
VCC_names <- c("VCC","Vcc", "vcc", "VOL", "Vol", "vol" ,"VOLUME", "Volume (m³)", "VOLUME (m³)", "VOL(m³)", "Volume(m³)", "VOLUME(m³)", "VOL(m³)")
arvore_names <- c("ARVORE", "Arvore", "arvore", "ARV", "Arv", "arv", "ARV.", "Arv.", "arv.","NP","Np","np","Árvore","ÁRVORE","árvore" )
estratos_names <- c("TALHAO", "Talhao", "talhao","COD_TALHAO","Cod_Talhao","cod_talhao", "COD.TALHAO", "Cod.Talhao","cod.talhao", "area.code", "Area.Code","AREA.CODE", "area_code","Area_Code","AREA_CODE")
# Server ####
shinyServer(function(input, output, session){
  
  # Importação ####
  
  #ui
  output$upload      <- renderUI({
    
    validate(need(input$df_select == "Fazer o upload", "" )  )
    
    list(    
      
      radioButtons("df", 
                   "Tipo da base de dados:", 
                   choices = c("Dados em nivel de secao",
                               "Dados em nivel de arvore"),
                   selected = "Dados em nivel de secao"),
      
      
      radioButtons("df_extension", 
                   "Informe o formato do arquivo:", 
                   choices = c(".csv (Valor separado por virgulas) ou .txt (arquivo de texto)",
                               ".xlsx (Excel)"),
                   selected = ".csv (Valor separado por virgulas) ou .txt (arquivo de texto)")
    )
  })
  output$upload_csv  <- renderUI({
    
    validate(need(input$df_select == "Fazer o upload" & input$df_extension == ".csv (Valor separado por virgulas) ou .txt (arquivo de texto)", "" )  )
    
    list(    
      
      radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
        inputId='sep',  #Id
        label='Separador:', # nome que sera mostrado na UI
        choices=c(Virgula=',', "Ponto e Virgula"=';', Tabulação='\t'), # opcoes e seus nomes
        selected=','), # valor que sera selecionado inicialmente
      
      radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
        inputId='dec', # Id
        label='Decimal:', # nome que sera mostrado na UI
        choices=c(Ponto=".", Virgula=","), # opcoes e seus nomes
        selected="."), # valor que sera selecionado inicialmente
      
      fileInput( # input de arquivos
        inputId = "file1", # Id
        
        label = "Selecione o arquivo: (.csv ou .txt)", # nome que sera mostrado na UI
        
        accept=c('text/csv', ".txt",'.csv'))
    )
    
    
  })
  output$upload_xlsx <- renderUI({
    
    validate(need(input$df_select == "Fazer o upload" & input$df_extension == ".xlsx (Excel)", "" )  )
    
    list(    
      # Selecionar numero da planilha
      numericInput(inputId = "sheet_n",
                   label   = "Número da planilha",
                   value   = 1,
                   min     = 1,
                   max     = 30,
                   step    = 1
      ),
      
      radioButtons(inputId = "mv_excel",label = "Valores ausentes", choices = c("Espaço vazio" = "", "NA" = "NA"), inline = T ),
      
      # input de arquivos
      fileInput( 
        inputId = "file2", # Id
        
        label = "Selecione o arquivo: (.xlsx)", # nome que sera mostrado na UI
        
        # So aceita .xlsx
        accept=c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                 '.xlsx')),
      
      
      div("Recomendamos o uso do formato .csv", style = "color:blue")
      
      
    )
    
    
  })
  
  #tabela
  upData <- reactive({ # Criamos uma nova funcao reactive. este sera o objeto filtrado, utilizado nos calculos
    
    # sera vazio caso nao seja selecionado "fazer o upload"
    validate(need(input$df_select == "Fazer o upload" , "" )  )
    
    # Salva o caminho do arquivo uploadado em um arquivo, dependendo do que o usuario selecionar
    if(input$df_extension == ".csv (Valor separado por virgulas) ou .txt (arquivo de texto)"){
      inFile <- input$file1
    }else if( input$df_extension == ".xlsx (Excel)"){
      inFile <- input$file2
    } # caso contrario, salvar o caminho do arquivo carregado em inFile
    
    # input$file1 sera NULL inicialmente. apos o usuario selecionar
    # e upar um arquivo, ele sera um data frame com as colunas
    # 'size', 'type', e 'datapath' . A coluna 'datapath' 
    # ira conter os nomes dos arquivos locais onde o dado pode ser encontrado
    
    # precisa do caminho do dado pra rodar os codigos a seguir
    req(inFile)
    
    if(input$df_extension != ".xlsx (Excel)")
    {
      raw_data <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, dec=input$dec,quote='"')
    } else {
      file.copy(inFile$datapath,
                paste(inFile$datapath, "xlsx", sep="."))
      raw_data <-  readxl::read_excel(paste(inFile$datapath, "xlsx", sep="."), input$sheet_n, na = input$mv_excel) 
      raw_data <- as.data.frame(raw_data)
    }
    
    # Carregamos o arquivo em um objeto
    
    
    raw_data # tabela final a ser mostrada. 
    
  })
  
  # rawData_ (com traco) sera o dado bruto sem filtro. Este dataframe sera utilizado em todo o app
  rawData_ <- reactive({
    
    # raw data, sera definido como o exemplo, ou o dado de upload, dependendo do usuario.
    # para evitar erros, caso seja selecionado "Fazer o upload" mas o dado ainda não tenha sido uploadado,
    # sera retornanado vazio
    switch(input$df_select, 
           "Fazer o upload" = if(is.null(input$file1) && is.null(input$file2)){return()}else{upData()},
           "Utilizar o dado de exemplo de Smalian" = ex1,
           "Utilizar o dado de exemplo de Huber" = ex2)
    
  })
  
  # render table
  output$rawdata <- DT::renderDataTable({ # renderizamos uma DT::DataTable
    
    validate(need(!is.null(rawData_()), "Please import a dataset"))
    
    # salvamos a funcao newData, que contem o arquivo carregado pelo usuario em um objeto
    data <- rawData_() 
    
    datatable(data,
              
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                  "}")
              )
    ) # Criamos uma DT::datatable com base no objeto
    
    # Este arquivo e reativo, e ira se alterar caso o usuario
    # aperte o botao input$columns
    
  })
  
  # Mapeamento ####
  
  # ui
  output$selec_di         <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.di", # Id
      NULL, # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = di_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  output$selec_hi         <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.hi", # Id
      NULL, # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = hi_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  output$selec_e_casca    <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.e_casca", # Id
      NULL, # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = e_casca_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  
  output$selec_comp_secao <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.comp_secao", # Id
      "Obs: Variável obrigatória para dados cubados pelo método de huber", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = comp_secao_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  output$selec_dap        <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.dap", # Id
      NULL, # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = DAP_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  output$selec_ht         <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.ht", # Id
      NULL, # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = HT_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  
  output$selec_arvore     <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.arvore", # Id
      NULL, # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      selected = arvore_names,     
      multiple=T,
      options = list(
        maxItems = 1,
        placeholder = 'selecione uma coluna abaixo'#,
        #onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  output$selec_estrato    <- renderUI({
    
    data <- rawData_()
    
    selectizeInput("col.estrato",
                   NULL, # nome que sera mostrado na UI
                   choices = names(data),
                   selected = estratos_names,
                   multiple = T,
                   options = list(
                    # maxItems = 1,
                     placeholder = 'Selecione uma coluna abaixo:'#,
                     #    onInitialize = I('function() { this.setValue(""); }')
                   ) # options    
    )# selectize
    
  })
  
  
  # Preparação ####
  # ui
  output$rm_data_var <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.rm_data_var", # Id
      "Selecione a coluna que se deseja filtrar:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      options = list(
        placeholder = 'selecione uma coluna abaixo',
        onInitialize = I('function() { this.setValue(""); }')
      ) # options    
    ) # selctize
    
    
  })
  output$rm_data_level <- renderUI({
    
    if( is.null(input$col.rm_data_var) || input$col.rm_data_var =="" ){
      
      opcoes <- NULL
      
    }else{
      
      data <- rawData_()
      
      opcoes <- levels(
        as.factor(
          data[,input$col.rm_data_var]))
    }
    
    selectizeInput("level.rm_data_level",
                   label = "Selecione o(s) nivel(s) que se deseja remover:",
                   choices = opcoes,
                   multiple = TRUE,
                   options = list(
                     placeholder = 'Selecione o(s) nivel(s) abaixo',
                     onInitialize = I('function() { this.setValue(""); }')
                   ) # options    
    )
    
    
    
  })
  output$rm_vars <- renderUI({
    
    data <- rawData_()
    
    selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
      "col.rm_vars", # Id
      "Selecione a(s) coluna(s) que se deseja remover:", # nome que sera mostrado na UI
      choices = names(data), # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
      multiple = TRUE,
      options = list(
        placeholder = 'selecione uma coluna abaixo',
        onInitialize = I('function() { this.setValue(" "); }')
      ) # options    
    ) # selctize
    
    
  })

  # tabela
  # rawData sera o dado utilizado durante o resto do app
  # as alteracoes feitas em 'preparacao' serao salvas aqui
  # caso nao seja feito nada, rawData sera identico a rawData_
  rawData <- reactive({
    
    data <- rawData_()
    nm <- varnames()
    
    # Antes de rodar as mensagens a seguir, um dado precisa ser importado
    validate(need(data,"please import a dataset"))
    validate(check_numeric(nm$dap, data, "dap"))
    validate(check_numeric(nm$ht, data, "ht"))
    validate(check_numeric(nm$di, data, "di"))
    validate(check_numeric(nm$hi, data, "hi"))
    # Aqui o dado nao ira rodar, caso essas condicoes sejam contrariadas
    # Elas serao mostradas em vermelho, devido a errorClass (definida no comeco da UI )
    #validate(
    #  need(is.numeric(data[[nm$dap]]), "dap column must be numeric"),
    # need(is.numeric(data[[nm$ht]]), "ht column must be numeric"), errorClass = "WRONG")
    
    
    # o primeiro if sera para remover as linhas
    
    # se o usuario nao selecionar nada, retorna o dado normal 
    # (isso faz com o que o dado original seja exibido logo que se entra na aba de filtrar),
    # caso contrario ele filtra o dado conforme o usuario seleciona as variaveis
    
    if( is.null(input$col.rm_data_var) || input$col.rm_data_var ==""){
      
      # esse if acima so foi feito dessa forma pois tentar adicionar ! nas condicoes acima
      # nao funcionou, por algum motivo.
      # portanto foi utilizado um if vazio com a condicao oposta a desejada,
      # e o resultado esperado dentro do else.
      
    }else{
      
      # remove linhas caso um nivel seja selecionado
      data <- data[!data[[input$col.rm_data_var]] %in% input$level.rm_data_level,]
      
      # data <- data %>% filter( ! .data[[input$col.rm_data_var]] %in% input$level.rm_data_level )
      
    }
    
    # A linha a seguir sera para remover uma ou mais colunas
    
    # se o usuario nao selecionar nada, uma coluna vazia e definida como nula,
    # ou seja, nao muda nada no dado.
    # por isso nao e necessario utilizar condicionais nesse caso
    
    data[, input$col.rm_vars] <- NULL
    
    
    # Aqui caso o usuario selecione, zeros serao transformados em NA nas variaveis numericas
    
    if(input$zero_to_NA){
      
      #ex1["HT"][ ex1["HT"] == 0 ] <- NA
      
      # Converter zero em NA quando a variavel tiver o seu nome definido
      if(nm$dap!=""){  data[nm$dap][ data[nm$dap] == 0 ] <- NA }
      if(nm$ht!= ""){  data[nm$ht ][ data[nm$ht ] == 0 ] <- NA }
      if(nm$di!= ""){  data[nm$di ][ data[nm$di ] == 0 ] <- NA }
      if(nm$hi!= ""){  data[nm$hi ][ data[nm$hi ] == 0 ] <- NA }
      
    }
    
    # converter valores
    if(is.null(nm$di)           || nm$di==""){}else if(nm$di_to_cm){data[[nm$di]]           <- data[[nm$di]]/10 }
    if(is.null(nm$hi)           || nm$hi==""){}else if(nm$hi_to_m){data[[nm$hi]]            <- data[[nm$hi]]/10 }
    if(is.null(nm$e_casca) || nm$e_casca==""){}else if(nm$e_casca_to_cm){data[[nm$e_casca]] <- data[[nm$e_casca]]/10 }
    
    
    # O if a seguir sera para remover linhas inconsistentes selecionadas pelo usuario
    
    # se o usuario nao selecionar nada, nada acontece
    # caso contrario ele filtra o dado conforme o usuario seleciona as variaveis
    
    if( ( is.null(input$consist_table_rows_selected) || input$consist_table_rows_selected == 0 || is.null(input$do_consist) || is.na(input$do_consist) || input$do_consist == "Nao" ) ){
      
      # esse if acima so foi feito dessa forma pois tentar adicionar ! nas condicoes acima
      # nao funcionou, por algum motivo.
      # portanto foi utilizado um if vazio com a condicao oposta a desejada,
      # e o resultado esperado dentro do else.
      
    }else{
      data_inconsist <- consist_fun()
      
      # Pega o numero da linha original (rowid) das linhas que o usuario selecionou na tabela (input$consist_table_rows_selected)
      insconsist_rows <- data_inconsist [input$consist_table_rows_selected, "rowid" ]
      
      # remove linhas inconsistentes
      data <- data[ -insconsist_rows ,  ]
    }
    
    data
    
  })

  # render
  output$prep_table <- DT::renderDataTable({
    
    validate(need(rawData(), "Please import a dataset"))
    
    data <- round_df(rawData(), 4)
    
    
    DT::datatable(data,
                  
                  options = list(
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                      "}")
                  )
    ) # Criamos uma DT::datatable com base no objeto
    
    
  })
  output$avisos_prep <- renderUI({
    data <- rawData_()
    nm <- varnames()
    
    # Essa parte do server ira gerar uma UI vazia, que gera avisos caso alguma condicao abaixo seja violada.
    #
    # Os erros so poderao ser mostrados se o usuario selecionar alguma coluna para ser removido
    req(input$col.rm_vars)
    
    # A seguir sao geradas uma mensagem de aviso para cada uma das variaveis que o usuario pode selecionar na aba
    # de mapeamento, caso elas tambem sejam selecionadas para serem removidas.
    # E utilizado %in% pois input$col.rm_vars pode ter mais de um nome (o usuario pode remover mais de uma variavel de uma vez)
    # e utilizado ! pois a condicao necessaria (que nao gera aviso) e que a variavel nao seja removida.
    # A cor da mensagem (laranja) e definada no argumento errorClass
    validate(
      need(! nm$dap %in% input$col.rm_vars, 
           "You just removed the 'dap' variable. This will prevent you from running some of the app's functions") , 
      need(! nm$ht %in% input$col.rm_vars, 
           "You just removed the 'ht' variable. This will prevent you from running some of the app's functions") , 
      need(! nm$vcc %in% input$col.rm_vars, 
           "You just removed the 'vcc' variable. This will prevent you from running some of the app's functions") ,
      need(! nm$vsc %in% input$col.rm_vars, 
           "You just removed the 'vsc' variable. This will prevent you from running some of the app's functions") ,
      need(! nm$estrato %in% input$col.rm_vars, 
           "You just removed the 'estrato' variable. This will prevent you from running some of the app's functions"),
      
      errorClass = "AVISO")
    
    # A errorClass AVISO foi criada no comeco da UI
    
  })
  # Set names ####
  varnames <- reactive({
    

    varnameslist <- list(
      di            = input$col.di,
      hi            = input$col.hi,
      e_casca       = input$col.e_casca,
      comp_secao    = input$col.comp_secao,
      dap           = input$col.dap,
      ht            = input$col.ht,
      arvore        = input$col.arvore,
      estrato       = input$col.estrato,
      IC.dap        = input$int.classe.dap,
      diam.min      = input$diam.min,
      di_to_cm      = input$di_to_cm,
      hi_to_m       = input$hi_to_m,
      e_casca_to_cm = input$e_casca_to_cm
    )

    
    # Os nomes nao selecionados serao salvos como NULL na lista,
    # estes sao entao convertidos para "", por conveniencia 
    #x <- data.frame(do.call(cbind, lapply(varnameslist, function(x){if(is.null(x)){x<-""}else{x} } )  ))    
    
    x <- lapply(varnameslist, function(x){if(is.null(x)){x<-""}else{x} } )   
    x
  })
  
  output$teste <- renderTable({
    varnames()
    
  })
  
  # Consistencia ####
  consist_fun <- reactive({
    
    data <- rawData_()
    
    # Aqui a funcao nao ira rodar, caso essas condicoes sejam contrariadas
    #  req(data, is.numeric(data[[input$col.dap]]),is.numeric(data[[input$col.ht]]) )
    validate(
      need(input$col.dap,""),
      need(input$col.ht,""),
      check_numeric(input$col.dap, data, "dap"),
      check_numeric(input$col.ht, data, "ht")  )
    
    #htdapratio(data, dap = input$col.dap, ht = input$col.ht) 
    consistency(data, dap = input$col.dap, ht = input$col.ht) 
  })
  output$consist_warning1 <- renderUI({
    # Essa aviso ira aparcer na UI caso consit_fun() nao seja nulo.
    # Esse objeto so nao sera nulo quando a funcao rodar, ou seja,
    # quando houverem dados inconsistentes.
    # Caso contrario a UI fica vazia, e nao aparece nada
    validate(need(is.null(consist_fun()), "Dados inconsistentes foram detectados" ), errorClass = "AVISO")
  })
  output$consist_warning2 <- renderUI({
    # Essa aviso ira aparcer na UI caso consit_fun() nao seja um objeto valido.
    # Esse objeto so  sera nulo quando a funcao rodar e gerar um resultado nulo.
    # Isso ocorre quando nao sao encontradas inconsistencias.
    # Caso contrario a UI fica vazia, e nao aparece nada
    validate(need(consist_fun(), "Não foram encontradas inconsistências" ) )
  })
  output$consist_choice <- renderUI({
    
    req(consist_fun())
    
    # Funcionando de forma semelhante a consist_warning,
    # se o objeto consist_fun() nao for nulo, ou seja,
    # se houverem dados a serem consistidos, essa UI ira aparecer, que da a ele a opcao de
    # remover ou nao as linhas da tabela em que ele clicou
    radioButtons("do_consist",
                 h4("Remover linhas selecionadas da tabela de dados inconsistentes?"), 
                 c("Sim","Nao"),
                 selected = "Nao",
                 inline = T)
    
  })
  output$consist_table_help <- renderUI({
    
    req(consist_fun())
    
    # Se houverem inconsistencias, essa UI ira aparecer, 
    # que gera um titulo e um texto de ajuda para a mesma
    
    list(
      h2("Dados inconsistentes:"),
      p("Analise os dados a seguir e clique nas linhas que desejar remover da analise."),
      p("Em seguida basta selecionar a opção 'Sim' àbaixo, e os dados serão removidos.")
      
    )
  })
  output$consist_table <- DT::renderDataTable({
    
    # Se o usuario quiser ver a tabela, e ela nao for nula,
    # nem a opcao de ver ela for nula, mostrar se nao, aviso
    validate(need(consist_fun(),""), errorClass = "AVISO" )
    
    #req(input$show_consist_table, input$show_consist_table == "Sim")
    
    consist_data <- round_df(consist_fun() , 2)
    
    datatable(consist_data,
              
              options = list(
                #             width = "200px",
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                  "}")
              )
    ) # Criamos uma DT::datatable com base no objeto
    
    
    
  })

  # Calculo de volume ####
  
  vol_smalian <- reactive({
    
    nm <- varnames()
    dados <- rawData()
    
    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(input$df == "Dados em nivel de secao", "Base de dados incompativel" ),
      need(nm$di,"Por favor mapeie a coluna referente a 'diametro da secao'  "),
      need(nm$hi,"Por favor mapeie a coluna referente a 'altura da secao'  "))
    
     if(nm$estrato != ""){
      group_arv <- c(nm$estrato, nm$arvore)
      
    }else{
      group_arv <- nm$arvore
      
    }
    
    tab <- smaliancc(
      df          = dados,
      di          = nm$di, 
      hi          = nm$hi, 
      .groups     = group_arv )

    if(nm$e_casca != ""){
      tab <- smaliansc(
        df          = tab,
        di          = nm$di, 
        hi          = nm$hi, 
        es          = nm$e_casca,
        .groups     = group_arv )
      
    }

    tab
    
  })
  output$tab_smalian <- DT::renderDataTable({
    
    data <- round_df(vol_smalian(), input$calc_vol_cd)
    
    datatable( data,
               rownames = F,
               options = list(searching = FALSE,
                              paging=TRUE,
                              ordering=TRUE,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )
    )
    
    
  })
  
  vol_huber <- reactive({
    
    nm <- varnames()
    dados <- rawData()
    
    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(input$df == "Dados em nivel de secao", "Base de dados incompativel" ),
      need(nm$di,"Por favor mapeie a coluna referente a 'diametro da secao'  "),
      need(nm$comp_secao,"Por favor mapeie a coluna referente a 'comprimento da secao'  "))
    
    if(nm$estrato != ""){
      group_arv <- c(nm$estrato, nm$arvore)
      
    }else{
      group_arv <- nm$arvore
      
    }
    
    tab <- hubercc(
      df          = dados,
      di          = nm$di, 
      comp_secao  = nm$comp_secao, 
      .groups     = group_arv,
      di_mm_to_cm = nm$di_to_cm)
    
    if(nm$e_casca != ""){
      tab <- hubersc(
        df          = tab,
        di          = nm$di, 
        comp_secao  = nm$comp_secao, 
        es          = nm$e_casca,
        .groups     = group_arv,
        di_mm_to_cm = nm$di_to_cm,
        es_mm_to_cm = nm$e_casca_to_cm)
      
    }
    
    tab
    
  })
  output$tab_huber <- DT::renderDataTable({
    
    data <- round_df(vol_huber(), input$calc_vol_cd)
    
    datatable( data,
               rownames = F,
               options = list(searching = FALSE,
                              paging=TRUE,
                              ordering=TRUE,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )
    )
    
    
  })
  
  vol_arvore <- reactive({
    
    nm <- varnames()
    

    # Switch que troca o dado que sera utilizado com base na selecao do usuario, na ui
    dados <- switch(input$data_vol_summary,
                    "Smalian" = vol_smalian(),
                    "Huber"   = vol_huber()  )
    
    validate(
      need(dados, "Por favor faça o calculo do volume utilizando o método desejado"),
      need(input$df == "Dados em nivel de secao", "Base de dados incompativel" ),
      need(nm$dap,"Por favor mapeie a coluna referente a 'DAP'  "),
      need(nm$ht,"Por favor mapeie a coluna referente a 'Altura'  "))
    
    # Utiliza o estrato no grupo, caso ele seja selecionado
    if(nm$estrato != ""){
      group_arv <- c(nm$estrato, nm$arvore)
      
    }else{
      group_arv <- nm$arvore
      
    }

    # Utiliza o volume com casca, caso seja calculado
    if(suppressWarnings(is.null(dados$VSC))){
      vsc_name <- ""
      
    }else{
      vsc_name <- "VSC"
      
    }
    
    tab <- cub_summary(
      df = dados, 
      dap = nm$dap, 
      ht = nm$ht,
      vcc = "VCC", 
      vsc = vsc_name, 
      .groups = group_arv)
    
  })
  output$tab_vol_arvore <- DT::renderDataTable({
    
    data <- round_df(vol_arvore(), input$calc_vol_cd)
    
    datatable( data,
               rownames = F,
               options = list(searching = FALSE,
                              paging=FALSE,
                              ordering=TRUE,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )
    )
    
    
  })

  # A partir daqui, o dado sera utilizado sera pelo switch dados_nivel_arvore ####
  
  dados_nivel_arvore <- reactive({
    
    req(input$df)
    
    # Se o dado for em nivel de arvore, a totalização de parcelas deve ser feita para que
    # NewData possa ser inserido em acs. Sem essa condição a ui gera mensagens de erro
    switch(input$df, 
           "Dados em nivel de secao" = if(is.null(vol_arvore()) ){return()}else{ vol_arvore()},
           "Dados em nivel de arvore" = rawData() )
    
    
  })
  
  # Distribuicoes e graficos ####
  
  dd_list <- reactive({
    
    nm <- varnames()
    dados <- dados_nivel_arvore()
    
    validate(
      need(dados, "Por favor faça o cálculo do volume ou o upload de uma base de dados em nível de arvore"),
      need(nm$dap,"Por favor mapeie a coluna referente a 'dap'  ") )
    
    lista <- list()

    lista[["dd_geral"]] <- classe_diametro(df = dados, 
                                           dap = "DAP",
                                           parcela = NA,
                                           area_parcela = NA, 
                                           ic = nm$IC.dap, 
                                           dapmin = nm$diam.min, 
                                           especies = NA, 
                                           volume = "VCC",
                                           rotulo.NI = NA ) %>%
                                           rename(VCC= volume)
    
    # Se o Volume sem casca for calculado, inclui-lo na tabela
    if(suppressWarnings(!is.null(dados$VSC))){
      lista[["dd_geral"]] <- lista[["dd_geral"]] %>% 
        mutate(VSC = classe_diametro(df = dados, 
                                     dap = "DAP",
                                     parcela = NA,
                                     area_parcela = NA, 
                                     ic = nm$IC.dap, 
                                     dapmin = nm$diam.min, 
                                     especies = NA, 
                                     volume = "VSC",
                                     rotulo.NI = NA ) %>%
                 rename(VSC= volume) %>%
                 pull(VSC) )
      
    }
    
    
    lista
  })
  output$dd_geral_tab <- DT::renderDataTable({
    
    g <- round_df(dd_list()[["dd_geral"]], 2)
    
    datatable( g,
               rownames = F,
               options = list(searching = FALSE,
                              paging=FALSE,
                              ordering=FALSE,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )
    )
    
    
  })
  
  dd_g1 <- reactive({
    
    g <- dd_list()[["dd_geral"]]
    #g$CC2 <-  sapply(g$CC , gsub, pattern= "[.]",replacement= "," )
    
    ggplot(g, aes(as.factor(CC),NumIndv)) +
      geom_bar(stat = "identity",color="black")+
      #   scale_y_continuous( expand=c(0,15) ) +
      ggthemes::theme_igray(base_family = "serif") +
      labs(x = "Centro de Classe de Diâmetro - CCD (cm)", y = "Nº de Individuos") + 
      geom_text(aes(label = round(NumIndv,1) ), position = position_dodge(0.9), vjust = -0.3, size = 6 ) + 
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title   = element_text(size = 26,face="bold"), 
        axis.text    = element_text(size = 22),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        strip.text.x = element_text(size = 22)   )
    
    
  })
  output$dd_graph_indv <- renderPlot({
    
    dd_g1()
    
    
  })
  dd_g2 <- reactive({
    
    g <- dd_list()[["dd_geral"]]
    
    ggplot(g, aes(as.factor(CC),VCC)) +
      geom_bar(stat = "identity",color="black")+
      #  scale_y_continuous( expand=c(0,15) ) +
      labs(x = "Centro de Classe de Diâmetro - CCD (cm)", y = "Volume com casca") + 
      ggthemes::theme_igray(base_family = "serif") +
      geom_text(aes(label = round(VCC,1) ), position = position_dodge(0.9), vjust = -0.3, size = 6 ) + 
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title   = element_text(size = 26,face="bold"), 
        axis.text    = element_text(size = 22),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        strip.text.x = element_text(size = 22)   )
    
  })
  output$dd_graph_vcc <- renderPlot({
    
    dd_g2()
    
  })
  dd_g3 <- reactive({
    
    g <- dd_list()[["dd_geral"]]
    
    ggplot(g, aes(as.factor(CC),VSC)) +
      geom_bar(stat = "identity",color="black")+
      #  scale_y_continuous( expand=c(0,15) ) +
      labs(x = "Centro de Classe de Diâmetro - CCD (cm)", y = "Volume sem casca") + 
      ggthemes::theme_igray(base_family = "serif") +
      geom_text(aes(label = round(VSC,1) ), position = position_dodge(0.9), vjust = -0.3, size = 6 ) + 
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title   = element_text(size = 26,face="bold"), 
        axis.text    = element_text(size = 22),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        strip.text.x = element_text(size = 22)   )
    
  })
  output$dd_graph_vsc <- renderPlot({
    
    dd_g3()
    
  })
  dd_g4 <- reactive({
    
    g <- dd_list()[["dd_geral"]] 
    
    ggplot(g, aes(as.factor(CC),G)) +
      geom_bar(stat = "identity",color="black")+
      # scale_y_continuous( expand=c(0,15) ) +
      labs(x = "Centro de Classe de Diâmetro - CCD (cm)", y = "Área Basal (G)") + 
      ggthemes::theme_igray(base_family = "serif") +
      geom_text(aes(label = round(G,1) ), position = position_dodge(0.9), vjust = -0.3, size = 6 ) + 
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title   = element_text(size = 26,face="bold"), 
        axis.text    = element_text(size = 22),
        axis.text.x = element_blank(),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        strip.text.x = element_text(size = 22)   )
    
  })
  output$dd_graph_G <- renderPlot({
    
    dd_g4()
    
  })
  kozak <- reactive({
    
    nm <- varnames()
    dados <- rawData()
    
    validate(
      need(dados, "Por favor faça o upload da base de dados"),
      need(input$df == "Dados em nivel de secao", "Base de dados incompativel" ),
      need(nm$di,"Por favor mapeie a coluna referente a 'diametro da secao'  "),
      need(nm$hi,"Por favor mapeie a coluna referente a 'altura da secao'  "))
    
    if(input$graph_arvore_estrato){
      grupo <- nm$estrato
    }else{
      grupo <- NULL
    }
    
    curva_arvore_media(df = dados, d = nm$di, dap = nm$dap, h = nm$hi, ht = nm$ht, facet = grupo)
    
  })
  output$graph_kozak <- renderPlot({
    
    kozak()
    
  })

  # Ajuste modelos de volume ####

  ajuste_vol <- reactive({
    
    nm <- varnames()
    dados <- dados_nivel_arvore()
    
    validate(
      need(dados, "Por favor faça o cálculo do volume ou o upload de uma base de dados em nível de arvore"),
      need(nm$dap,"Por favor mapeie a coluna referente a 'dap'  "),
      need(nm$ht,"Por favor mapeie a coluna referente a 'ht'  ")
      )
    
    # Ajustar por estrato caso o usuário deseje
    if(input$ajuste_p_estrato){
      grupo <- nm$estrato
    }else{
      grupo <- ""
    }
    
    # Ajustar modelo de schummacher
    tab <- bind_rows(
      lm_table(df=dados,modelo = log(VCC) ~  log(DAP) + log(HT), grupo ) %>% 
        mutate(Nome = "Schummacher  & Hall com casca",
               Modelo = "LN(VCC) = b0 + b1*LN(DAP) + b2*LN(HT) + e" ),

      lm_table(df=dados,modelo = log(VCC) ~  log(DAP), grupo )%>% 
        mutate(Nome = "Husch com casca",
               Modelo = "LN(VCC) = b0 + b1*LN(DAP) + e" ),
      
      lm_table(df=dados,modelo = log(VCC) ~  log(pow(DAP,2)*HT), grupo )%>% 
        mutate(Nome = "Spurr com casca",
               Modelo = "LN(VCC) = b0 + b1*LN(DAP²*HT) + e" )
      
      
    )
    # Se o Volume sem casca for calculado, ajustar modelo sem casca
    if(suppressWarnings(!is.null(dados$VSC))){
      
      tab <- bind_rows(tab,
                   lm_table(df=dados,modelo = log(VSC) ~  log(DAP) + log(HT), grupo ) %>% 
                     mutate(Nome = "Schummacher & Hall sem casca",
                            Modelo = "LN(VSC) = b0 + b1*LN(DAP) + b2*LN(HT) + e" ),
                   
                   lm_table(df=dados,modelo = log(VSC) ~  log(DAP), grupo )%>% 
                     mutate(Nome = "Husch sem casca",
                            Modelo = "LN(VSC) = b0 + b1*LN(DAP) + e" ),
                   
                   lm_table(df=dados,modelo = log(VSC) ~  log(pow(DAP,2)*HT), grupo )%>% 
                     mutate(Nome = "Spurr sem casca",
                            Modelo = "LN(VSC) = b0 + b1*LN(DAP²*HT) + e" )
                   
                   )
    }
    
    # Organizar a tabela
    tab <- 
    
    if(input$ajuste_p_estrato){
      
      tab <- tab %>% arrange(!!rlang::sym(grupo), Nome) %>% select(!!rlang::sym(grupo), Nome, Modelo, everything())
      
    }else{
      tab <-  tab %>% arrange(Nome) %>% select(Nome, Modelo, everything()) 
    }
   
    tab
     
  })
  output$ajuste_vol_tab <- DT::renderDataTable({
    g <- round_df(ajuste_vol(), 8)
    datatable( g,
               rownames = F,
               options = list(searching = FALSE,
                              paging=FALSE,
                              ordering=TRUE,
                              initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#00a90a', 'color': '#fff'});",
                                "}")
               )
    )
    
    
  })
 
  ajuste_vol_tab_est <- reactive({
    
    nm <- varnames()
    dados <- dados_nivel_arvore()
    
    validate(
      need(dados, "Por favor faça o cálculo do volume ou o upload de uma base de dados em nível de arvore"),
      need(nm$dap,"Por favor mapeie a coluna referente a 'dap'  "),
      need(nm$ht,"Por favor mapeie a coluna referente a 'ht'  ")
    )
    
    # Ajustar por estrato caso o usuário deseje
    if(input$ajuste_p_estrato){
      grupo <- nm$estrato
    }else{
      grupo <- ""
    }
    
    tab <- tibble(
      
      "VCC" = dados %>% pull(VCC),
      
      "Schummacher & Hall com casca" = dados %>% 
        lm_table(modelo = log(VCC) ~  log(DAP) + log(HT), 
                  grupo,
                  output = "est" ) %>% pull("est"),
      
      "Husch com casca" = dados %>% 
        lm_table( modelo = log(VCC) ~  log(DAP), 
                  grupo,
                  output = "est" ) %>% pull("est"),
      
      "Spurr com casca" = dados %>% 
        lm_table( modelo = log(VCC) ~  log(pow(DAP,2)*HT), 
                  grupo,
                  output = "est" ) %>% pull("est")
    )
    
    if(suppressWarnings(!is.null(dados$VSC))){
      
      tab <- tab %>% 
        mutate(
          "VSC" = dados %>% pull(VSC),
          
          "Schummacher & Hall sem casca" = dados %>% 
            lm_table(modelo = log(VSC) ~  log(DAP) + log(HT), 
                     grupo,
                     output = "est" ) %>% pull("est"),
          
          "Husch sem casca" = dados %>% 
            lm_table( modelo = log(VSC) ~  log(DAP), 
                      grupo,
                      output = "est" ) %>% pull("est"),
          
          "Spurr sem casca" = dados %>% 
            lm_table( modelo = log(VSC) ~  log(pow(DAP,2)*HT), 
                      grupo,
                      output = "est" ) %>% pull("est") 
          
        )
      
    }
    
    na.omit(tab)
    
    
  })

  vcc_scatter <- reactive({
    g <- ajuste_vol_tab_est()
    residuos_exp(g, "VCC", "Schummacher & Hall com casca", "Husch com casca", "Spurr com casca",type = "scatterplot" )
  })
  output$graph_res_vcc_scatterplot <- renderPlot({
    vcc_scatter()
  })
  
  vcc_hist <- reactive({
    g <- ajuste_vol_tab_est()
    residuos_exp(g, "VCC", "Schummacher & Hall com casca", "Husch com casca", "Spurr com casca",type = "histogram_curve" )
  })
  output$graph_res_vcc_histogram <- renderPlot({
    vcc_hist()
  })
  
  vcc_versus <- reactive({
    g <- ajuste_vol_tab_est()
    residuos_exp(g, "VCC", "Schummacher & Hall com casca", "Husch com casca", "Spurr com casca",type = "versus" )
  })
  output$graph_res_vcc_versus <- renderPlot({
    vcc_versus()
  })
  
  vsc_scatter <- reactive({
    g <- ajuste_vol_tab_est()
    residuos_exp(g, "VSC", "Schummacher & Hall sem casca", "Husch sem casca", "Spurr sem casca",type = "scatterplot" )
  })
  output$graph_res_vsc_scatterplot <- renderPlot({
    vsc_scatter()
  })
  
  vsc_hist <- reactive({
    g <- ajuste_vol_tab_est()
    residuos_exp(g, "VSC", "Schummacher & Hall sem casca", "Husch sem casca", "Spurr sem casca",type = "histogram_curve" )
  })
  output$graph_res_vsc_histogram <- renderPlot({
    vsc_hist()
  })
  
  vsc_versus <- reactive({
    g <- ajuste_vol_tab_est()
    residuos_exp(g, "VSC", "Schummacher & Hall sem casca", "Husch sem casca", "Spurr sem casca",type = "versus" )
  })
  output$graph_res_vsc_versus <- renderPlot({
  vsc_versus()
  })


  
  # Download tabelas ####
  
  output$checkbox_df_download <- renderUI({
    
    checkboxGroupInput("dataset", h3("Escolha uma ou mais tabelas, e clique no botão abaixo:"), 
                       choices =  c(
                         "Dados inconsistentes"    ,
                         "Dado utilizado"          ,
                         "Vol. por secao Smalian"  ,
                         "Vol. por secao Huber"    ,
                         "Totalizacao do volume"   ,
                         "Distribuicao diametrica" ,
                         "Tabela de coeficientes" 
                         ), inline = T )
    
    
  })
  
  list_of_df_to_download <- reactive({
    
    L <- list()
    
    if("Dados inconsistentes" %in% input$dataset ) {
      L[["Dados inconsistentes"]] <- try( consist_fun(), silent = T) 
    }
    
    if("Dado utilizado" %in% input$dataset ) {
      L[["Dado utilizado"]] <-  try(rawData(), silent = T)
    }

    if("Vol. por secao Smalian" %in% input$dataset ) {
      L[["Vol. por secao Smalian"]] <-  try(vol_smalian(), silent = T)
    }
    
    if("Vol. por secao Huber" %in% input$dataset ) {
      L[["Vol. por secao Huber"]] <-  try(vol_huber(), silent = T)
    }

    if("Totalizacao do volume" %in% input$dataset ) {
      L[["Totalizacao do volume"]] <-  try(dados_nivel_arvore(), silent = T)
    }
    
    if("Distribuicao diametrica" %in% input$dataset ) {
      L[["Distribuicao diametrica"]] <-  try(dd_list()[["dd_geral"]], silent=T)
    }
    
    if("Tabela de coeficientes" %in% input$dataset ) {
      L[["Tabela de coeficientes"]] <-  try(ajuste_vol(), silent = T)
    }

    L
    
  })
  list_of_df_all <- reactive({
    
    L <- list()
    
    L[["Dados inconsistentes"]] <- try( consist_fun(), silent = T) 
    L[["Dado utilizado"]]       <-  try(rawData(), silent = T)
    L[["Vol. por secao Smalian"]] <- try(vol_smalian(), silent = T)
    L[["Vol. por secao Huber"]] <- try(vol_huber(), silent = T)
    L[["Totalizacao do volume"]] <- try(dados_nivel_arvore(), silent = T)
    L[["Distribuicao diametrica"]] <-  try(dd_list()[["dd_geral"]], silent=T)
    L[["Tabela de coeficientes"]] <- try(ajuste_vol(), silent=T)
    L
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){"tabelas_app_cubagem.xlsx"},
    
    content = function(file){xlsx.write.list(file, list_of_df_to_download() )}
    
  )
  
  output$downloadAllData <- downloadHandler(
    filename = function(){"tabelas_app_cubagem.xlsx"},
    
    content = function(file){xlsx.write.list(file, list_of_df_all() )}
    
  )
  
  # Download graficos ####
  
  graphInput <- reactive({
    switch(input$graph_d,
           "Indviduos por CC"            = dd_g1(),
           "VCC por CC"                  = dd_g2(),
           "VSC por CC"                  = dd_g3(),
           "G por CC"                    = dd_g4(),
           "Forma media das arvores"     = kozak(),
           "Dispersao dos residuos VCC"  = vcc_scatter(),
           "Histograma dos residuos VCC" = vcc_hist(),
           "Obs vs est VCC"              = vcc_versus(),
           "Dispersao dos residuos VSC"  = vsc_scatter(),
           "Histograma dos residuos VSC" = vsc_hist(),
           "Obs vs est VSC"              = vsc_versus()         
    )
  })
  
  output$graph_d_out <- renderPlot({
    
    g <- graphInput()
    
    g
    
    
  }) 
  
  output$downloadGraph <- downloadHandler(
    filename = function() { 
      
      if(input$graphformat==".png")
      {
        paste(input$graph_d, '.png', sep='') 
      }
      else if(input$graphformat==".jpg")
      {
        paste(input$graph_d, '.jpg', sep='') 
      }
      else if(input$graphformat==".pdf")
      {
        paste(input$graph_d, '.pdf', sep='') 
      }
      
    },
    
    content = function(file) {
      
      ggsave(file, graphInput(), width = 12, height = 6 )
      
      
    }
  )
  # session end ####
  # session$onSessionEnded(function() {
  #  stopApp()
  #  q("no")
  #})
  # ####
  
  
  })



