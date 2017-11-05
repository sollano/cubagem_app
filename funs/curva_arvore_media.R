#' @export

curva_arvore_media <- function(df, d, dap, h, ht, facet){
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, parar
  if(  missing(df) || is.null(df) || is.na(df) || !is.data.frame(df) ){  
    stop("df not set", call. = F) 
  }
  
  # se d nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(d) || is.null(d) || is.na(d) || d == "" || is.null(df[d]) ){  
    stop("d not set", call. = F) 
  }
  
  # se dap nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(dap) || is.null(dap) || is.na(dap) || dap == "" || is.null(df[dap]) ){  
    stop("dap not set", call. = F) 
  }
  
  # se h nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(h) || is.null(h) || is.na(h) || h == "" || is.null(df[h]) ){  
    stop("h not set", call. = F) 
  }
  
  # se ht nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, parar
  if(  missing(ht) || is.null(ht) || is.na(ht) || ht == "" || is.null(df[ht]) ){  
    stop("ht not set", call. = F) 
  }
  
  sym <- rlang::sym
  d <- sym(d)
  dap <- sym(dap)
  h <- sym(h)
  ht <- sym(ht)
  
  # se facet nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, definir como nulo
  if(  missing(facet) || is.null(facet) || is.na(facet) || facet == "" || is.null(df[facet]) ){  
    facet <- NULL
  }
  
 p <- df %>% 
    mutate(d_sob_dap = (!!d)/(!!dap),h_sob_ht = (!!h)/(!!ht), h_sob_ht_quad = h_sob_ht^2 ) %>% 
    ggplot2::ggplot(aes(x=d_sob_dap, y=h_sob_ht)) + 
    ggplot2::geom_point(size = 2, alpha = .4) + 
    # coord_fixed(ratio=2) +
    ggplot2::labs(x=expression(italic(frac(d,DAP))), 
         y=expression(italic(frac(h,HT)))
    ) +
    ggpmisc::stat_poly_eq(
      formula = x ~ poly(y, 2, raw=T),
      size = 3,
      eq.x.rhs    = "italic(frac(h,HT))",
      eq.with.lhs = "italic(hat(frac(d,DAP)))~`=`~", 
      ggplot2::aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")),
      label.x.npc="right",
      parse = TRUE  ) +
    ggthemes::theme_igray(base_family = "serif") +
    theme(
      axis.title.y = element_text(angle = 0, vjust =.5),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.title   = element_text(size = 14,face="bold"), 
      axis.text    = element_text(size = 14),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      strip.text.x = element_text(size = 14)   )
  
 if(!is.null(facet) ){p <- p + facet_wrap(facet) }

 return(p)  
  
  
  
}
