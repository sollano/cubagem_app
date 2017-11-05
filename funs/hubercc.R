#' @export

hubercc <- function(df, di, comp_secao, .groups, di_mm_to_cm=FALSE){

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
  
  # se di_mm_to_cm nao for igual a TRUE ou FALSE
  if(!   di_mm_to_cm %in% c(TRUE, FALSE, "") ){ 
    stop("di_mm_to_cm must be equal to TRUE or FALSE", call. = F) 
  }
  
  # se .groups nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(.groups) || is.null(.groups) || is.na(.groups) || .groups == "" || is.null(df[.groups] ) ){  
    stop(".groups not set", call. = F) 
  }
  
  
  # Converter diametro da secao de milimetro para centimetro
  if(di_mm_to_cm){
    
    df[[di]] <- df[[di]]/10
    
  }
  
  df %>% 
    group_by_(.dots = .groups) %>% 
    mutate_(
      .dots = 
        setNames(
          list( 
            lazyeval::interp( ~ ( (di^2* pi) / 40000) , di = as.name(di)), 
            lazyeval::interp( ~ AS_CC * comp_secao  , AS = as.name("AS_CC"),  comp_secao = as.name(comp_secao))
          ),
          nm=c("AS_CC", "VCC")
        )
    )
}
