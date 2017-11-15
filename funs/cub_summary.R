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
  
  # funcao para transformar strings em symmbolos que o dplyr entende
  sym <- rlang::sym 
  dap <- sym(dap) 
  ht  <- sym(ht)
  vcc <- sym(vcc)
  vsc <- sym(vsc)
  
    df  %>%                             # define data frame utilizado
    na_if(0) %>%                        # Transforma zeros em NA
    group_by_at( vars(.groups) )  %>%   # definicao da chave
    summarize(                          # Funcao que compila os df
      DAP  = mean(!!dap, na.rm = TRUE), # Media de DAP
      HT   = mean(!!ht,  na.rm = TRUE), # media de HT
      VCC  = sum(!!vcc,  na.rm = TRUE), # Soma de volume com casca
      VSC  = sum(!!vsc,  na.rm = TRUE), # Soma de volume sem casca
      AS   = pi * DAP^2 / 4000 * HT   , # Area Seccional
      FFCC = VCC / AS * HT            , # Fator de forma com casca
      FFSC = VSC / AS * HT ) %>%        # Fator de forma sem casca
   mutate_at(                           # Funcao que cria novas variaveis utilizando as variaveis
     vars(contains("FF")),              # especificadas por vars
     funs(medio = mean)    ) %>%        # Fator de forma medio
      na_if(0) %>%                      # Se vsc nao for informado, variaveis que o utilizam serao 0, portanto, deve-se converte-las para NA, para depois remove-las
   select_if(Negate(anyNA))             # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
    
}
