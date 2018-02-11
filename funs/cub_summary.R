#' @export

cub_summary <- function(df, dap, ht, vcc, vsc, .groups){
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, parar
  if(  missing(df) || is.null(df) || is.na(df) || !is.data.frame(df) ){  
    stop("df not set", call. = F) 
  }
  
  # se dap nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(dap) || is.null(dap) || is.na(dap) || dap == "" || is.null(df[[dap]]) ){  
    stop("dap not set", call. = F) 
  }
  
  # se ht nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(ht) || is.null(ht) || is.na(ht) || ht == "" || is.null(df[[ht]]) ){  
    stop("ht not set", call. = F) 
  }
  
  # se vcc nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(vcc) || is.null(vcc) || is.na(vcc) || vcc == "" || is.null(df[[vcc]]) ){  
    stop("vcc not set", call. = F) 
  }
  
  # se vsc nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(vsc) || is.null(vsc) || is.na(vsc) || vsc == "" || is.null(df[[vsc]]) ){  
    df[["VSC"]] <- NA 
    vsc <- "VSC"
  }
  
  # se .groups nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(.groups) || is.null(.groups) || is.na(.groups) || .groups == "" || is.null(df[.groups]) ){  
    stop(".groups not set", call. = F) 
  }
  
  dap_name <- dap
  ht_name <- ht
  vcc_name <- vcc
  vsc_name <- vsc
  
  # funcao para transformar strings em symmbolos que o dplyr entende
  sym <- rlang::sym 
  dap_sym <- sym(dap) 
  ht_sym  <- sym(ht)
  vcc_sym <- sym(vcc)
  vsc_sym <- sym(vsc)
  
  # !! diz para o dplyr que voce esta lidando com simbolos ou strings
  
  # := e utilizado quando o nome da variavel nova dentro do pipe esta dentro de um objeto
  
  df  %>%                                     # define data frame utilizado
    na_if(0) %>%                              # Transforma zeros em NA
    group_by_at( vars(.groups) )  %>%         # definicao da chave
    summarize(                                # Funcao que compila os df
      !!dap_name := mean(!!dap_sym, na.rm = TRUE), # Media de DAP
      !!ht_name  := mean(!!ht_sym,  na.rm = TRUE), # media de HT
      AS          = pi * (!!sym(dap_name))^2 / 40000       , # Area Seccional
      !!vcc_name := sum(!!vcc_sym,  na.rm = TRUE), # Soma de volume com casca
      !!vsc_name := sum(!!vsc_sym,  na.rm = TRUE), # Soma de volume sem casca
      PORC_CASCA = (( (!!sym(vcc_name)) - (!!sym(vsc_name)) )/ (!!sym(vcc_name))  )*100    , # Porcentagem da casca
      #  VCIL    = AS *  (!!sym(ht_name)) ,
      FFCC       = (!!sym(vcc_name)) / (AS * (!!sym(ht_name)) )   , # Fator de forma com casca
      FFSC       = (!!sym(vsc_name)) / (AS * (!!sym(ht_name)) )   ) %>%     # Fator de forma sem casca
    mutate_at(                                # Funcao que cria novas variaveis utilizando as variaveis
      vars(FFCC, FFSC),                   # especificadas por vars
      funs(medio = mean)    ) %>%             # Fator de forma medio
    na_if(0) %>%                              # Se vsc nao for informado, variaveis que o utilizam serao 0, portanto, deve-se converte-las para NA, para depois remove-las
    select_if(Negate(anyNA))                  # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
  
}
