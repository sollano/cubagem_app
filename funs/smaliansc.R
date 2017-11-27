#' @title Volume sem casca pelo metodo de Smalian
#' 
#' @description Calculo do volume sem casca em nivel de secao utilizando o metodo de Smalian.
#' 
#' @details 
#' Funcao utilizada para calcular o volume sem casca pelo metodo de Smalian. A funcao possui
#' integracao com dplyr, podendo ser utilizada dentro de um pipe, em conjunto com a funcao
#' group_by.
#' 
#' @param df Data frame a ser utilizado.
#' @param di Nome entre aspas da variavel diametro da secao em centimetros.
#' @param hi Nome entre aspas da variavel altura da secao em metros.
#' @param es Nome entre aspas da variavel espessura da casca em milimetros.
#' @param .groups Nome(s) entre aspas da(s) variavel(s) classificatoria(s) utilizadas para identificar as arvores. Caso este argumento seja \code{NULL}, serao utilizados grupos ja definidos no dataframe. Padrao: \code{NULL}.
#' @return Dataframe contendo os valores em nivel de secao.
#' 
#' @references 
#' CAMPOS, J. C. C.; LEITE, H. G. Mensuracao florestal: perguntas e respostas. 3a. ed. Vicosa: Editora UFV, 2013. 605 p.
#'
#' @seealso Funcao complementar:
#'   \code{\link{ smaliancc }}, para o calculo do volume com casca.
#'   
#' @export
#' @examples
#' library(forestr)
#' data("ex7_mfr")
#'
#' # Calcular o volume sem casca pelo metodo de Smalian:
#' smaliansc(ex7_mfr,"di_cc", "hi", "e_casca", "ARVORE")
#' 
#' # ou, utilizando pipes:
#'  ex7_mfr %>% 
#'  group_by(ARVORE) %>% 
#'  smaliansc("di_cc", "hi", "e_casca")
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

smaliansc <- function(df, di, hi, es, .groups, di_mm_to_cm=FALSE, hi_cm_to_m=FALSE, es_mm_to_cm=FALSE ){
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, parar
  if(  missing(df) || is.null(df) || is.na(df) || !is.data.frame(df) ){  
    stop("df not set", call. = F) 
  }
  
  # se di nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(di) || is.null(di) || is.na(di) || di == "" || is.null(df[di] ) ){  
    stop("di not set", call. = F) 
  }
  
  # se hi nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(hi) || is.null(hi) || is.na(hi) || hi == "" || is.null(df[hi] ) ){  
    stop("hi not set", call. = F) 
  }
  
  # se es nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(es) || is.null(es) || is.na(es) || es == "" || is.null(df[es] ) ){  
    stop("es not set", call. = F) 
  }
  
  # se di_mm_to_cm nao for igual a TRUE ou FALSE
  if(!   di_mm_to_cm %in% c(TRUE, FALSE, "") ){ 
    stop("di_mm_to_cm must be equal to TRUE or FALSE", call. = F) 
  }
  
  # se hi_cm_to_m nao for igual a TRUE ou FALSE
  if(!   hi_cm_to_m %in% c(TRUE, FALSE, "") ){ 
    stop("hi_cm_to_m must be equal to TRUE or FALSE", call. = F) 
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
    
    df[[di]] <- df[[di]]/10
    
  }
  
  # Converter altura da secao de centrometro para metro
  if(hi_cm_to_m){
    
    df[[hi]] <- df[[hi]]/10
    
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
            lazyeval::interp( ~ di-2*(es/10) , di= as.name(di), es = as.name(es) ),
            lazyeval::interp( ~  (di_sc^2* pi) / 40000 , di_sc = as.name("di_sc")), 
            lazyeval::interp( ~ ((AS + lead(AS) )/2 ) * (lead(hi) - hi) , AS = as.name("AS_SC"),  hi = as.name(hi))
          ),
          nm=c("di_sc", "AS_SC", "VSC")
        )
    )  %>% 
    ungroup
}
