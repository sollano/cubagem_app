print("aaaa")
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
suppressPackageStartupMessages(library(xlsx))
library(rJava)
library(xlsxjars)
library(rmarkdown)

# Carregar dados de exemplo e funcoes

ex1 <- read.csv2("examples/exemplo_smalian.csv")
ex2 <- read.csv2("examples/exemplo_huber.csv")
source("funs/smaliancc.R"      , encoding="UTF-8")
source("funs/smaliansc.R"      , encoding="UTF-8")
source("funs/hubercc.R"        , encoding="UTF-8")
source("funs/hubersc.R"        , encoding="UTF-8")
source("funs/round_df.R"       , encoding="UTF-8")
source("funs/classe_diametro.R", encoding="UTF-8")
source("funs/htdapratio.R"     , encoding="UTF-8")
source("funs/consistency.R"    , encoding="UTF-8")
source("funs/xlsx.write.list.R", encoding="UTF-8")
source("funs/lm_table.R"       , encoding="UTF-8")
source("funs/inv.R"            , encoding="UTF-8")
source("funs/residuos_exp.R"   , encoding="UTF-8")


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

di_names <- c("di", "di_cc","d")
hi_names <- c("hi")
e_casca_names <- c("e_casca","espessura_casca")
comp_secao_names <- c("comp_secao")
DAP_names <- c("DAP","Dap","dap", "dbh", "Dbh","DBH","DBH_11")
HT_names <- c("HT", "Ht", "ht","Htot","ALTURA","Altura","Altura_Total", "ALTURA_TOTAL")
arvore_names <- c("arvore", "ARVORE", "arv.", "ARVORES", "arvores", "Arvore", "Arvores")
estratos_names <- c("TALHAO", "Talhao", "talhao","COD_TALHAO","Cod_Talhao","cod_talhao", "COD.TALHAO", "Cod.Talhao","cod.talhao", "area.code", "Area.Code","AREA.CODE", "area_code","Area_Code","AREA_CODE")

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
                   selected = "Dados em nivel de arvore"),
      
      
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
      "Caso o dado não possua uma coluna de volume, este pode ser calculado na aba 'Preparação' ", # nome que sera mostrado na UI
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
      "col.di_cc", # Id
      "Caso o dado não possua uma coluna de volume, este pode ser calculado na aba 'Preparação' ", # nome que sera mostrado na UI
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
      "Caso o dado não possua uma coluna de volume, este pode ser calculado na aba 'Preparação' ", # nome que sera mostrado na UI
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
      "Caso o dado não possua uma coluna de volume, este pode ser calculado na aba 'Preparação' ", # nome que sera mostrado na UI
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
                     maxItems = 1,
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

    # Estimar altura
  output$ajust_ht_title <- renderUI({
    
    # precisa que o usuario tenha NAs na coluna de altura
    data <- rawData_()
    #req( any(is.na(data[[input$col.ht]])) )
    
    if(is.null(input$col.ht) || is.na(input$col.ht) || input$col.ht=="" ){
      
    }else if( !any(is.na(data[[input$col.ht]])) ) {
      return()
    }
    
    h3("Resíduos em porcentagem para os modelos hipsométricos",style = "text-align: center;")
    
    
  })
  output$ajust_ht <- renderUI({
    
    # precisa que o usuario tenha NAs na coluna de altura
    data <- rawData_()
    #req( any(is.na(data[[input$col.ht]])) )
    
    if(is.null(input$col.ht) || is.na(input$col.ht) || input$col.ht=="" ){
      
    }else if( !any(is.na(data[[input$col.ht]])) ) {
      return()
    }
    
    list(
      
      h3("Estimar altura das árvores não medidas"),
      
      h5("A altura será estimada utilizando um dos modelos hipsométricos abaixo. Resíduos para todos os modelos disponíveis são demonstrados graficamente ao lado:"),
      
      radioButtons("modelo_est_ht",
                   label = "Selecione o modelo para ser utilizado:",
                   choices = c(
                     "LN(HT) = b0 + b1 * 1/DAP + e",
                     "LN(HT) = b0 + b1 * LN(DAP) + e",
                     "LN(HT) = b0 + b1 * 1/DAP + b2 * LN(HD) + e"
                     
                   ) )      
      
      
    )
    
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
    }
    
    # Estimar HD
    if(is.null(input$est.hd) || is.null(input$col.estrato ) || input$col.estrato =="" || is.na(input$col.estrato ) || is.null(input$col.ht ) || input$col.ht =="" || is.na(input$col.ht ) || is.na(input$col.dap) || input$col.dap=="" || is.null(input$col.dap) ){
      
      
    }else if(is.null(input$col.hd) || input$col.hd=="" || is.na(input$col.hd) ){
      
      # esse if tem que ser separado do de cima, se nao da erro(sabe-se la por que)
      if(input$est.hd){
        
        data <- hdjoin(
          df     =  data,
          grupos =  nm$estrato, 
          HT     =  nm$ht, 
          DAP    =  nm$dap,
          OBS    =  nm$obs,
          dom    =  nm$cod.dom )  %>% 
          select(HD, everything() )
      }
    }
    
    # Estimar altura caso altura seja selecionada e possua NAs, ou seja, arvores nao medidas
    # Esse se evita mensagens de erro quando as colunas nao forem selecionadas
    if( is.null(input$col.ht) || input$col.ht=="" || is.na(input$col.ht) || is.na(input$col.dap) || input$col.dap=="" || is.null(input$col.dap) ||   is.null(input$modelo_est_ht) || input$modelo_est_ht=="" || is.na(input$modelo_est_ht) ){
      
      
    }else if( nm$ht!="" && any(is.na(data[[nm$ht]])) ){
      
      if(input$modelo_est_ht ==  "LN(HT) = b0 + b1 * 1/DAP + e" ){
        
        modelo_ht <- paste( "log(", nm$ht, ") ~ inv(", nm$dap ,")"  )
        
      }else if(input$modelo_est_ht ==  "LN(HT) = b0 + b1 * LN(DAP) + e" ){
        
        modelo_ht <- paste( "log(", nm$ht, ") ~ log(", nm$dap ,")"  )
        
      }else if(input$modelo_est_ht ==  "LN(HT) = b0 + b1 * 1/DAP + b2 * LN(HD) + e" ){
        
        modelo_ht <- paste( "log(", nm$ht, ") ~ inv(", nm$dap ,") + ", "log(", nm$hd ,")" )
      }
      
      
      data <- data %>%  
        lm_table(modelo_ht,output = "est" ) %>% 
        mutate( HT_EST = ifelse(is.na( .data[[nm$ht]] ), est, .data[[nm$ht]] ) ) %>% 
        select(HT_EST, everything(), -est )
      
    }
    
    
    
    # A seguir e feito o calculo do volume, caso o usuario nao insira uma variavel de volume e as variaveis necessarias para o calculo
    if( is.null(input$modelo_estvol) ||  is.null(input$col.dap)  || is.null(input$b0_estvol) || is.null(input$b1_estvol) || is.na(input$modelo_estvol) ||  is.na(input$col.dap)  || is.na(input$b0_estvol) || is.na(input$b1_estvol) || input$modelo_estvol =="" || input$col.dap ==""  || input$b0_estvol == "" || input$b1_estvol == ""  ){
      
      # esse if acima so foi feito dessa forma pois tentar adicionar ! nas condicoes acima
      # nao funcionou, por algum motivo.
      # portanto foi utilizado um if vazio com a condicao oposta a desejada,
      # e o resultado esperado dentro do else.
    }else{
      
      if(input$modelo_estvol == "LN(VFCC) = b0 + b1 * 1/DAP + e"){
        data$VOL <- exp( input$b0_estvol + 1/data[[input$col.dap]] * input$b1_estvol )
        data <- data %>% select(VOL, everything())
      }
      
      if(input$modelo_estvol == "VFCC = b0 + b1 * DAP + e"){
        data$VOL <- input$b0_estvol + data[[input$col.dap]] * input$b1_estvol
        data <- data %>% select(VOL, everything())
      }
      
      if(input$modelo_estvol == "VFCC = b0 + b1 * DAP² + e"){
        data$VOL <- input$b0_estvol + data[[input$col.dap]]^2 * input$b1_estvol
        data <- data %>% select(VOL, everything())
      }
      
      if(input$modelo_estvol == "VFCC = b0 + b1 * DAP + b2 * DAP² + e"){
        data$VOL <- input$b0_estvol + data[[input$col.dap]] * input$b1_estvol + data[[input$col.dap]]^2 * input$b2_estvol
        data <- data %>% select(VOL, everything())
      }
      
      if(input$modelo_estvol == "VFCC = b0 + b1 * LN(DAP) + e"){
        data$VOL <- input$b0_estvol + log(data[[input$col.dap]]) * input$b1_estvol
        data <- data %>% select(VOL, everything())
        
      }
      
      
      # modelos com b2 e ht precisam de mais uma condicao
      if( is.null(input$modelo_estvol) ||  is.null(input$col.ht)  ||  is.na(input$col.ht) || is.null(nm$ht.est)  ||  is.na(nm$ht.est) || is.na(input$b2_estvol) || input$col.ht ==""  || input$b2_estvol == "" ){
        
      }else if(input$modelo_estvol == "LN(VFCC) = b0 + b1 * LN(DAP) + b2 * LN(HT) + e"){
        data$VOL <- exp( input$b0_estvol + log(data[[input$col.dap]]) * input$b1_estvol + log(data[[nm$ht.est]]) * input$b2_estvol )
        data <- data %>% select(VOL, everything())
        
      }else  if(input$modelo_estvol == "VFCC = b0 + b1 * DAP + b2 * HT + e"){
        data$VOL <- input$b0_estvol + data[[input$col.dap]] * input$b1_estvol + data[[nm$ht.est]] * input$b2_estvol
        data <- data %>% select(VOL, everything())
      }else if(input$modelo_estvol == "VFCC = b0 * DAP^b1 * HT^b2 + e"){
        data$VOL <- input$b0_estvol * data[[input$col.dap]] ^ input$b1_estvol * data[[nm$ht.est]] ^ input$b2_estvol
        data <- data %>% select(VOL, everything())
      }
      
      
    }
    
    # A seguir e feito o calculo da estrutura vertical, caso o usuario nao tenha inserido uma variavel referente a mesma, e selecione que desja calcular
    if(!is.null(input$est.vert.calc) && !is.na(input$est.vert.calc) && input$est.vert.calc=="Sim"){
      
      data <- estrat_vert_souza(data, input$col.ht)
      
    }
    
    
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
  
  # Graficos de altura
  ht_graph <- reactive({
    
    req(input$col.ht, input$col.dap, !is.null(rawData()) )
    
    data <- rawData()
    nm <- varnames()
    
    if(is.null(input$col.ht) || is.na(input$col.ht) || input$col.ht=="" ){
      
      
    }else if( !any(is.na(data[[input$col.ht]])) ) {
      return()
    }
    
    data <- data %>%  filter( !is.na(.data[[nm$ht]]) )
    
    # Tentar Ajustar os modelos utilizando try, e salvar em uma lista,
    # junto com a altura observada
    lista <- list(
      data[!is.na(data[nm$ht]), nm$ht,drop=F],
      "LN(HT) = b0 + b1 * 1/DAP + e"               = try(lm_table(data, paste( "log(", nm$ht, ") ~ inv(", nm$dap ,")"  )                       , output = "est" )[["est"]], silent = T),
      "LN(HT) = b0 + b1 * LN(DAP) + e"             = try(lm_table(data, paste( "log(", nm$ht, ") ~ log(", nm$dap ,")"  )                       , output = "est" )[["est"]], silent = T),
      "LN(HT) = b0 + b1 * 1/DAP + b2 * LN(HD) + e" = try(lm_table(data, paste( "log(", nm$ht, ") ~ inv(", nm$dap ,") + ", "log(", nm$hd ,")" ) , output = "est" )[["est"]], silent = T)
    )
    
    # Criar um dataframe apenas com os modelos que ajustaram
    data2 <- as.data.frame(do.call(cbind,lista[!sapply(lista, is, "try-error")]))
    
    # Criar os graficos
    # suppressWarnings evita avisos quando um dos modelos nao for ajustado
    suppressWarnings(
      
      residuos_exp(data2, 
                   nm$ht, 
                   "LN(HT) = b0 + b1 * 1/DAP + e", 
                   "LN(HT) = b0 + b1 * LN(DAP) + e",
                   "LN(HT) = b0 + b1 * 1/DAP + b2 * LN(HD) + e", ncol = 2 )
      
    )
    
    
  })
  output$ht_plot <- renderPlot({
    
    ht_graph()
    
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
    
    #req(input$col.especies,input$col.parcelas, input$col.dap,input$col.ht,input$col.vcc, input$col.vsc,input$col.area.parcela,input$col.area.total, input$col.col.estrato,  input$col.est.vertical,input$col.est.interna)
    
    varnameslist <- list(
      di           = input$col.di,
      hi           = input$col.hi,
      e_casca      = input$e_casca,
      comp_secao   = input$comp_secao,
      dap          = input$col.dap,
      ht           = input$col.ht,
      arvore       = input$col.parcelas,
      estrato      = input$col.estrato,
      IC.dap       = input$int.classe.dap,
      diam.min     = input$diam.min
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
    consistency(data, dap = input$col.dap, ht = input$col.ht, parcela = input$col.parcelas) 
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
  
  
  
  
  

  
  
  
  
  
})



