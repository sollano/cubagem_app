#' @title Volume com casca pelo metodo de Smalian
#' 
#' @description Calculo do volume com casca em nivel de secao utilizando o metodo de Smalian.
#' 
#' @details 
#' Funcao utilizada para calcular o volume com casca pelo metodo de Smalian. A funcao possui
#' integracao com dplyr, podendo ser utilizada dentro de um pipe, em conjunto com a funcao
#' group_by.
#' 
#' @param df Data frame a ser utilizado.
#' @param di Nome entre aspas da variavel diametro da secao em centimetros.
#' @param hi Nome entre aspas da variavel altura da secao em metros.
#' @param .groups Nome(s) entre aspas da(s) variavel(s) classificatoria(s) utilizadas para identificar as arvores. Caso este argumento seja \code{NULL}, serao utilizados grupos ja definidos no dataframe. Padrao: \code{NULL}.
#' @return Dataframe contendo os valores em nivel de secao.
#' 
#' @references 
#' CAMPOS, J. C. C.; LEITE, H. G. Mensuracao florestal: perguntas e respostas. 3a. ed. Vicosa: Editora UFV, 2013. 605 p.
#'
#' @seealso Funcao complementar:
#'   \code{\link{ smaliansc }}, para o calculo do volume sem casca.
#'   
#' @export
#' @examples
#' library(forestr)
#' data("ex7_mfr")
#' 
#' # Calcular o volume com casca pelo metodo de Smalian:
#' smaliancc(ex7_mfr,"di_cc", "hi", "ARVORE")
#' 
#' # ou, utilizando pipes:
#'  ex7_mfr %>% 
#'  group_by(ARVORE) %>% 
#'  smaliancc("di_cc", "hi")
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

smaliancc <- function(df, di, hi, .groups, di_mm_to_cm=FALSE, hi_cm_to_m=FALSE ){
  
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
  
  # se di_mm_to_cm nao for igual a TRUE ou FALSE
  if(!   di_mm_to_cm %in% c(TRUE, FALSE, "") ){ 
    stop("di_mm_to_cm must be equal to TRUE or FALSE", call. = F) 
  }
  
  # se hi_cm_to_m nao for igual a TRUE ou FALSE
  if(!   hi_cm_to_m %in% c(TRUE, FALSE, "") ){ 
    stop("hi_cm_to_m must be equal to TRUE or FALSE", call. = F) 
  }

  # se .groups nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(.groups) || is.null(.groups) || is.na(.groups) || .groups == "" || is.null(df[.groups] ) ){  
    stop(".groups not set", call. = F) 
  }
  
  # Converter diametro da secao de milimetro para centimetro
  if(di_mm_to_cm){
    
    df[[di]] <- df[[di]]/10
    
  }
  
  # Converter altura da secao de centimetro para metro
  if(hi_cm_to_m){
    
    df[[hi]] <- df[[hi]]/100
    
  }
  
   df %>% 
    group_by_(.dots = .groups) %>% 
    mutate_(
      .dots = 
        setNames(
          list( 
            lazyeval::interp( ~ ( (di^2* pi) / 40000) , di = as.name(di)), 
            lazyeval::interp( ~ ((AS + lead(AS) )/2 ) * (lead(hi) - hi) , AS = as.name("AS_CC"),  hi = as.name(hi))
          ),
          nm=c("AS_CC", "VCC")
        )
    ) %>% 
     ungroup
}
