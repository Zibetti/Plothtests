#' Plot Chi-Square test for one populational variance
#'
#' @param alpha significance level for the critical limits
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @param df degrees of freedom for the sample (default = 10)
#' @param va sample variance (default = 10)
#' @param vp populational variance under null hypothesis (default = 10)
#' @param annotations logical, show annotations in plot figure, TRUE ou FALSE (default = TRUE)
#' @param color integer, color theme for plot, from 1 to 7 so far (default = 3)
#'
#' @export
#'
#' @importFrom utils getFromNamespace
#'
#' @examples
#' plot_chi_test(alpha = 0.05,alternative = "two.sided",
#'               df = 20, va = 20,
#'               vp = 15,
#'               annotations = TRUE,
#'               color = 3)
#'

plot_chi_test = function(alpha = 0.05, alternative = "two.sided", df, va, vp, annotations = TRUE, color = 3){

  ## color function
  tema = color_theme(color)
  corCurva        = tema$corCurva
  corPValue       = tema$corPValue
  corET           = tema$corET
  corLinhaLimCrit = tema$corLinhaLimCrit
  corAreaLimCrit  = tema$corAreaLimCrit

  # set function to arrow
  #iArrows <- igraph:::igraph.Arrows
  iArrows = getFromNamespace("igraph.Arrows", "igraph")

  if (df < 22) {
    x    = seq(0,50,by=0.01)
  } else {
    x    = seq(0,300,by=0.01)
  }

  #browser()


  # Test Statistic - ET
  ET  = df*va/vp         #chi-quadrado amostral

  # Valores das Density
  DenCHI = dchisq(x,df)   # Densidade chi-quadrado - valores do eixo x
  DenET = dchisq(ET,df)   # Densidade na estatistica amostral

  # limites do gráfico de acordo com os graus de liberdade
  plim = 0.0001
  plim_inf = qchisq(plim,df)
  plim_sup = qchisq(1-plim,df)

  ## Bilateral - Two.sided ============================================================================================================================
  if(alternative == "two.sided"){

    lic  = qchisq(alpha/2,df)   # Limite inferior crítico
    lsc  = qchisq(1-alpha/2,df) # Limite superior crítico
    dlic = dchisq(lic,df)   # Densidade no Limite inferior crítico
    dlsc = dchisq(lsc,df)   # Densidade no Limite superior crítico

    # Plot da distribuicao H0
    delta = 0.035 # 3.5% acima ou abaixo do limite
    if(ET >= lsc){
      plot(x,DenCHI, type="l", xlab = "Statistic values", xlim = c(plim_inf , (1+delta)*ET),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main =  bquote("Null Distribution :" ~ sigma^2 == .(vp) ~ ",  " ~ df == ~ .(df)) )

      # lista de unicodes para serem subsituidos no sprintf, sprintf está sendo usado por causa do CairoWin
      # https://stackoverflow.com/questions/27690729/greek-letters-symbols-and-line-breaks-inside-a-ggplot-legend-label

    }else if(ET <= lic){
      plot(x,DenCHI, type="l", xlab = "Statistic values", xlim = c((1-delta)*ET , plim_sup),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main =  bquote("Null Distribution :" ~ sigma^2 == .(vp) ~ ",  " ~ df == ~ .(df)) )

    }else{
      plot(x,DenCHI, type="l", xlab = "Statistic values", xlim = c(plim_inf,plim_sup),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main =  bquote("Null Distribution :" ~ sigma^2 == .(vp) ~ ",  " ~ df == ~ .(df)) )
    }


    # limites criticos alpha/2
    polygon(x = c(lsc, lsc     , x[x>lsc]   , tail(x, n=1)), # X = conjunto dos valores de lsc até o final
            y = c(0  , dlsc    , DenCHI[x>lsc], 0),            # Y = conjunto das Density de lsc até o final
            col = corAreaLimCrit,                            # cor do preenchimento
            border = corAreaLimCrit)                         # cor da borda

    polygon(x   = c(0 ,0       , x[x<lic]    ,lic  ,lic), # X = conjunto dos valores de lsc até o final
            y   = c(0 ,DenCHI[1] , DenCHI[x<lic] ,dlic,0),    # Y = conjunto das Density de lsc até o final
            col = corAreaLimCrit,                         # cor do preenchimento
            border = corAreaLimCrit)                      # cor da borda

    abline(v = lsc, lty = "dashed", lwd = 2, col = "darkgray")
    abline(v = lic, lty = "dashed", lwd = 2, col = "darkgray")
    abline(h = 0  , col = "gray"  , lwd = 0.5)
    points(ET, 0  , col = corET   , pch = 16 , cex=1.4)

    # plotar novamente a curva por cima
    lines(x, DenCHI, type="l", ylab = "Density", lwd = 3, col = corCurva)

    if(ET > qchisq(0.5, df) ){
      area_ET_cum = 1 - pchisq(ET, df)      # 1 - (area de 0 até estatistica amostral)
      ponto_ET    = qchisq(area_ET_cum, df) # qui-quadrado amostral two.sided - extremidade oposta
      Den_ponto_ET= dchisq(ponto_ET,df)     # Densidade na estatistica amostral two.sided - extremidade oposta

      # area do  P-value superior
      polygon(x = c(ET , ET     ,      x[x>ET], tail(x, n=1)),      # X = conjunto dos valores de lsc até o final
              y = c(0  , DenET  , DenCHI[x>ET], 0           ),      # Y = conjunto das Density de lsc até o final
              col = corPValue,
              density = c(30),
              angle = c(-45))

      # area do  P-value inferior
      polygon(x = c(0 ,0         ,      x[x<ponto_ET] ,ponto_ET     ,ponto_ET),  # X = conjunto dos valores de lsc até o final
              y = c(0 ,DenCHI[1] , DenCHI[x<ponto_ET] ,Den_ponto_ET ,0       ),  # Y = conjunto das Density de lsc até o final
              col = corPValue,
              density = c(30),
              angle = c(-45))
    }
    if(ET <= qchisq(0.5, df)){

      area_ET_cum = pchisq(ET, df)          # (area de 0 até estatistica amostral)
      ponto_ET    = qchisq(area_ET_cum, df) # qui-quadrado amostral two.sided - extremidade oposta
      Den_ponto_ET= dchisq(ponto_ET,df)     # Densidade na estatistica amostral two.sided - extremidade oposta


      # area do  P-value superior
      polygon(x = c(ponto_ET, ponto_ET     ,      x[x>ponto_ET], tail(x, n=1)), # X = conjunto dos valores de lsc até o final
              y = c(0       , Den_ponto_ET , DenCHI[x>ponto_ET], 0           ), # Y = conjunto das Density de lsc até o final
              col = corPValue,
              density = c(30),
              angle = c(-45))

      # area do  P-value inferior
      polygon(x = c(0 ,0        ,      x[x<ponto_ET] ,ponto_ET     ,ponto_ET), # X = conjunto dos valores de lsc até o final
              y = c(0 ,DenCHI[1], DenCHI[x<ponto_ET] ,Den_ponto_ET ,0       ), # Y = conjunto das Density de lsc até o final
              col = corPValue,
              density = c(30),
              angle = c(-45))

    }

    # Ticks do eixo X no plot
    Map(axis, side=1, at = round(c(lic,ET, lsc),2),
        col.axis = c("gray" , corET, "gray"),
        col.ticks = c("gray", corET, "gray"),
        lwd=0, las=1,
        lwd.ticks = 2)

    # plotar novamente a curva por cima
    #lines(x, DenCHI, type="l", xlab = "Valores de F", ylab = "Density", lwd = 3, col = corCurva)

    ## Anotações - escolha
    if(annotations == TRUE){

      # posição da legenda relativa ao Plot()

      ## obtendo os limites do plot
      plot_limits = par("usr") # [1]-xini [2]-xfim [3]-yini [4]-yfim

      ## obtendo o centro do plot
      xcent = (plot_limits[2] + plot_limits[1])/2
      ycent = (plot_limits[3] + plot_limits[4])/2

      # Valor de P arrow -------------------------------------
      # arrow direita
      iArrows(xcent        , ycent,                # de  (x1,y1)
              1.025*abs(ET), plot_limits[4]/100 ,  # até (x2,y2)
              h.lwd  = 2,
              sh.lwd = 1,
              sh.col = corPValue,
              sh.lty = 2,
              h.lty = 1,
              curved  = 1/(20*abs(ET)),#0.085,
              width  =  1,
              size   = 0.7)
      # arrow esquerda
      iArrows(xcent   , ycent,   # de  (x1,y1)
              0.975*abs(ponto_ET), plot_limits[4]/100, # até (x2,y2)
              h.lwd  = 2,
              sh.lwd = 1,
              sh.col = corPValue,
              sh.lty = 2,
              h.lty = 1,
              curved  = -1/(20*abs(ET)), #-0.085,
              width  =  1,
              size   = 0.7)

      text(xcent   , ycent,
           label = paste("P-value =",round(2*area_ET_cum,2)),
           col   = corPValue,
           pos = 3)

      # fim Valor de P arrow  ------------------------------------

      # Legendas
      legenda <- list( bquote( "Test Statistic =" ~ chi^2 == ~ .(round(ET,2)) ) ,
                       bquote( alpha == ~.(alpha) ),
                       bquote( "Critical limits" ~~ "[" ~ .(round(lic,2)) ~ ";" ~ .(round(lsc,2)) ~"]") )

      mtext(side = 3, do.call(expression, legenda), line=-2:-4, adj=1, col=c(corET,corAreaLimCrit, "gray"))
    }
  }

  ## Superior - greater =====================================================================================================

  if(alternative == "greater"){

    lsc  = qchisq(1-alpha,df) # Limite superior crítico
    dlsc = dchisq(lsc,df)   # Densidade no Limite superior crítico

    # Plot da distribuicao H0
    delta = 0.035 # 3.5% acima ou abaixo do limite
    if(ET >= lsc){
      plot(x,DenCHI, type="l", xlab = "Statistic values", xlim = c(plim_inf , (1+delta)*ET),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main =  bquote("Null Distribution :" ~ sigma^2 == .(vp) ~ ",  " ~ df == ~ .(df)) )
    }else{
      plot(x,DenCHI, type="l", xlab = "Statistic values", xlim = c(plim_inf,plim_sup),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main =  bquote("Null Distribution :" ~ sigma^2 == .(vp) ~ ",  " ~ df == ~ .(df)) )
    }


    # area critica alpha
    polygon(x = c(lsc, lsc     , x[x>lsc]     , tail(x, n=1)), # X = conjunto dos valores de lsc até o final
            y = c(0  , dlsc    , DenCHI[x>lsc], 0           ), # Y = conjunto das Density de lsc até o final
            col = corAreaLimCrit,                            # cor do preenchimento
            border = corAreaLimCrit)                         # cor da borda                               # cor do preenchimento

    # limites criticos alpha
    abline(v = lsc, col = "gray", lty = "dashed", lwd = 2)

    # linha de base
    abline(h = 0, col = "gray", lwd = 0.5)

    # plotar novamente a curva por cima
    lines(x, DenCHI, type="l", lwd = 3, col = corCurva)

    # Estatistica do Teste -  observada
    points(ET,0, col = corET, pch=16 , cex=1.4)

    area_ET_cum = 1 - pchisq(ET, df)    # 1 - (area de 0 até estatistica amostral)

    # area do  P-value superior
    polygon(x = c(ET , ET   ,      x[x>ET], tail(x, n=1)), # X = conjunto dos valores de lsc até o final
            y = c(0  , DenET, DenCHI[x>ET], 0           ), # Y = conjunto das Density de lsc até o final
            col = corPValue,
            density = c(20),
            angle   = c(-45))

    # plotar novamente a curva por cima
    #lines(x, DenCHI, type="l", xlab = "Valores de F", ylab = "Density", lwd = 3, col = corCurva)

    # Ticks do eixo X no plot
    Map(axis, side=1, at = round(c(ET, lsc),2),
        col.axis = c(corET, "gray"),
        col.ticks = c(corET, "gray"),
        lwd=0,
        las=1,
        lwd.ticks = 2)

    ## Anotações - escolha
    if(annotations == TRUE){

      # posição da legenda relativa ao Plot()

      ## obtendo os limites do plot
      plot_limits = par("usr") # [1]-xini [2]-xfim [3]-yini [4]-yfim

      ## obtendo o centro do plot
      xcent = (plot_limits[2] + plot_limits[1])/2
      ycent = (plot_limits[3] + plot_limits[4])/2

      # Valor de P arrow -------------------------------------
      # arrow direita
      iArrows(xcent        , ycent,                # de  (x1,y1)
              1.025*abs(ET), plot_limits[4]/100 ,  # até (x2,y2)
              h.lwd  = 2,
              sh.lwd = 1,
              sh.col = corPValue,
              sh.lty = 2,
              h.lty = 1,
              curved  = 1/(20*abs(ET)),#0.085,
              width  =  1,
              size   = 0.7)


      text(xcent   , ycent,
           label = paste("P-value =",round(area_ET_cum,2)),
           col   = corPValue,
           pos = 3)

      # fim Valor de P arrow  ------------------------------------

      # Legendas
      legenda <- list( bquote( "Test Statistic =" ~ chi^2 == ~ .(round(ET,2)) ) ,
                       bquote( alpha == ~.(alpha) ),
                       bquote( "Critical limits" ~~ "[ -Inf ;" ~ .(round(lsc,2)) ~"]") )

      mtext(side = 3, do.call(expression, legenda), line=-2:-4, adj=1, col=c(corET,corAreaLimCrit, "gray"))

    }
  }

  ## Inferior - less ====================================================================================================
  if(alternative == "less"){

    lic  = qchisq(alpha,df) # Limite inferior crítico
    dlic = dchisq(lic,df)   # Densidade no Limite inferior crítico
    lsc  = qchisq(1-alpha,df) # Limite superior crítico
    dlsc = dchisq(lsc,df)   # Densidade no Limite superior crítico


    # Plot da distribuicao H0
    delta = 0.035 # 3.5% acima ou abaixo do limite
    if(ET >= lsc){
      plot(x,DenCHI, type="l", xlab = "Statistic values", xlim = c(plim_inf , (1+delta)*ET),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main =  bquote("Null Distribution :" ~ sigma^2 == .(vp) ~ ",  " ~ df == ~ .(df)) )
    }else if(ET <= plim_inf){
      plot(x,DenCHI, type="l", xlab = "Statistic values", xlim = c((1-delta)*ET,(1+delta)*lsc),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main =  bquote("Null Distribution :" ~ sigma^2 == .(vp) ~ ",  " ~ df == ~ .(df)) )
    }else if(ET <= lic && ET > plim_inf){
      plot(x,DenCHI, type="l", xlab = "Statistic values", xlim = c((1-delta)*ET,(1+delta)*lsc),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main =  bquote("Null Distribution :" ~ sigma^2 == .(vp) ~ ",  " ~ df == ~ .(df)) )
    }else{
      plot(x,DenCHI, type="l", xlab = "Statistic values", xlim = c(plim_inf,plim_sup),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main =  bquote("Null Distribution :" ~ sigma^2 == .(vp) ~ ",  " ~ df == ~ .(df)) )
    }


    # area critica alpha inferior
    polygon(x = c(0 ,      x[x<lic], lic  , lic), # X = conjunto dos valores de lsc até o final
            y = c(0 , DenCHI[x<lic], dlic , 0),  # Y = conjunto das Density de lsc até o final
            col = corAreaLimCrit,                            # cor do preenchimento
            border = corAreaLimCrit)                         # cor da borda                               # cor do preenchimento


    # limites criticos alpha
    abline(v = lic, col = "gray", lty = "dashed", lwd = 2)

    # linha de base
    abline(h = 0, col = "gray", lwd = 0.5)

    # plotar novamente a curva por cima
    lines(x, DenCHI, type="l", lwd = 3, col = corCurva)

    # Estatistica do Teste -  observada
    points(ET,0, col = corET, pch=16 , cex=1.4)

    area_ET_cum = pchisq(ET, df)    # 1 - (area de 0 até estatistica amostral)

    # area do  P-value Inferior
    polygon(x = c(0,     x[x<ET],    ET, ET), # X = conjunto dos valores de lsc até o final
            y = c(0,DenCHI[x<ET], DenET, 0 ), # Y = conjunto das Density de lsc até o final
            col = corPValue,
            density = c(20),
            angle   = c(-45))

    # plotar novamente a curva por cima
    #lines(x, DenCHI, type="l", xlab = "Valores de Q", ylab = "Density", lwd = 3, col = corCurva)

    # Ticks do eixo X no plot
    Map(axis, side=1, at = round(c(ET, lic),2),
        col.axis = c(corET, "gray"),
        col.ticks = c(corET, "gray"),
        lwd=0,
        las=1,
        lwd.ticks = 2)

    ## Anotações - escolha
    if(annotations == TRUE){

      # posição da legenda relativa ao Plot()

      ## obtendo os limites do plot
      plot_limits = par("usr") # [1]-xini [2]-xfim [3]-yini [4]-yfim

      ## obtendo o centro do plot
      xcent = (plot_limits[2] + plot_limits[1])/2
      ycent = (plot_limits[3] + plot_limits[4])/2

      # Valor de P arrow -------------------------------------
      # arrow esquerda
      iArrows(xcent   , ycent,   # de  (x1,y1)
              0.975*abs(ET), plot_limits[4]/100, # até (x2,y2)
              h.lwd  = 2,
              sh.lwd = 1,
              sh.col = corPValue,
              sh.lty = 2,
              h.lty = 1,
              curved  = -min( c(1/3000 , 1/(df*ceiling(ET))) ), #-0.085, curvatura diminui conforme muda a escala - graus de liberdade e ET
              width  =  1,
              size   = 0.7)


      text(xcent   , ycent,
           label = paste("P-value =",round(area_ET_cum,2)),
           col   = corPValue,
           pos = 3)

      # fim Valor de P arrow  ------------------------------------

      # Legendas
      legenda <- list( bquote( "Test Statistic =" ~ chi^2 == ~ .(round(ET,2)) ) ,
                       bquote( alpha == ~.(alpha) ),
                       bquote( "Critical limits" ~~ "[" ~ .(round(lic,2)) ~"; +Inf]") )

      mtext(side = 3, do.call(expression, legenda), line=-2:-4, adj=1, col=c(corET,corAreaLimCrit, "gray"))
    }
  }
  PVAL = switch(alternative,
                two.sided = 2*area_ET_cum,
                less      = area_ET_cum,
                greater   = area_ET_cum)
  return(list(test.statistic = ET, degree.freed = df, p.value = PVAL))
}


plot_chi_test(alpha = 0.05, alternative = "two.sided", df = 10, va = 10, vp = 10, annotations = TRUE, color = 3)
plot_chi_test(alpha = 0.05, alternative = "two.sided", df = 10, va = 10.000002, vp = 10, annotations = TRUE, color = 3)
plot_chi_test(alpha = 0.05, alternative = "two.sided", df = 10, va = 9, vp = 10, annotations = TRUE, color = 3)
plot_chi_test(alpha = 0.05, alternative = "two.sided", df = 10, va = 9.6, vp = 10, annotations = TRUE, color = 3)
plot_chi_test(alpha = 0.05, alternative = "two.sided", df = 10, va = 9.7, vp = 10, annotations = TRUE, color = 3)
plot_chi_test(alpha = 0.05, alternative = "two.sided", df = 10, va = 9.8, vp = 10, annotations = TRUE, color = 3)
plot_chi_test(alpha = 0.05, alternative = "two.sided", df = 10, va = 9.9, vp = 10, annotations = TRUE, color = 3)
plot_chi_test(alpha = 0.05, alternative = "two.sided", df = 10, va = 9.99, vp = 10, annotations = TRUE, color = 3)

# Montgomery
plot_chi_test(alpha = 0.05, alternative = "two.sided", df = 19, va = 0.0153, vp = 0.01, annotations = TRUE, color = 3)
plot_chi_test(alpha = 0.05, alternative = "greater"  , df = 19, va = 0.0153, vp = 0.01, annotations = TRUE, color = 3)

#plot_chi_test(alpha = 0.05, alternative = "greater"  , df = 10, va = 10, vp = 10, annotations = TRUE, color = 3)
#plot_chi_test(alpha = 0.05, alternative = "less"     , df = 10, va = 10, vp = 10, annotations = TRUE, color = 3)
