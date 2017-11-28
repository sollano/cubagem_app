#' @export

hubersc <- function(df, di, comp_secao, es, .groups, di_mm_to_cm=FALSE, es_mm_to_cm=FALSE   ){

  # se df nao for fornecido, nulo, ou  nao for dataframe, parar
  if(  missing(df) || is.null(df) || is.na(df) || !is.data.frame(df) ){  
    stop("df not set", call. = F) 
  }
  
  # se di nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(di) || is.null(di) || is.na(di) || di == "" || is.null(df[di] ) ){  
    stop("di not set", call. = F) 
  }
  
  # se comp_secao nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(comp_secao) || is.null(comp_secao) || is.na(comp_secao) || comp_secao == "" || is.null(df[comp_secao] ) ){  
    stop("comp_secao not set", call. = F) 
  }
  
  # se es nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(es) || is.null(es) || is.na(es) || es == "" || is.null(df[es] ) ){  
    stop("es not set", call. = F) 
  }
  
  # se di_mm_to_cm nao for igual a TRUE ou FALSE
  if(!   di_mm_to_cm %in% c(TRUE, FALSE, "") ){ 
    stop("di_mm_to_cm must be equal to TRUE or FALSE", call. = F) 
  }

  # se es_mm_to_cm nao for igual a TRUE ou FALSE
  if(!   es_mm_to_cm %in% c(TRUE, FALSE, "") ){ 
    stop("es_mm_to_cm must be equal to TRUE or FALSE", call. = F) 
  }
  
  # se .groups nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(.groups) || is.null(.groups) || is.na(.groups) || .groups == "" || is.null(df[.groups] ) ){  
    stop(".groups not set", call. = F) 
  }
  
  # Converter diametro da secao de milimetro para centimetro
    if(di_mm_to_cm){
    
    df[[di]] <- df[[di]]/100
    
  }
  
  # Converter espessura da casca de milimetro para centimetro
  if(es_mm_to_cm){
    
    df[[es]] <- df[[es]]/10
    
  }
  
  
  df %>% 
    group_by_(.dots = .groups) %>% 
    mutate_(
      .dots = 
        setNames(
          list(
            lazyeval::interp( ~ di-2*(es) , di= as.name(di), es = as.name(es) ),
            lazyeval::interp( ~  (di_sc^2* pi) / 40000 , di_sc = as.name("di_sc")), 
            lazyeval::interp( ~ comp_secao , AS = as.name("AS_SC"),  comp_secao = as.name(comp_secao))
          ),
          nm=c("di_sc","AS_SC", "VSC")
        )
    ) %>% 
    ungroup
}
