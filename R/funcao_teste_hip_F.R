#' Plot F Test to Compare Two Variances
#'
#' Plot F Test to Compare Two Variance, the larger sample variance is always placed in the numerator.
#'
#'
#' @param alpha significance level for the critical limits
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @param df1 degrees of freedom for the sample 1 (default = 10)
#' @param df2 degrees of freedom for the sample 2 (default = 10)
#' @param v1 sample 1 variance (default = 1.5)
#' @param v2 sample 2 variance (default = 2.5)
#' @param ratio the ratio of population variances under the null hypoyhesis (default = 1)
#' @param annotations logical, show annotations in plot figure, TRUE ou FALSE (default = TRUE)
#' @param color integer, color theme for plot, from 1 to 7 so far (default = 3)
#'
#' @export
#'
#' @importFrom utils getFromNamespace
#'
#' @examples
#' plot_F_test(alpha = 0.05, alternative = "two.sided",
#'             df1 = 10, df2 = 10,
#'             v1 = 1.5, v2 = 2.5,
#'             ratio = 1,
#'             annotations = TRUE,
#'             color = 3)
#'

plot_F_test = function(alpha = 0.05, alternative = "two.sided", df1, df2, v1, v2, ratio = 1, annotations = TRUE, color = 3){

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

  #browser()
  # Dados - Eixo x:
  if (df1 < 2 | df2 < 2) {
    x    = seq(0,300,by=0.01)
  } else {
    x    = seq(0,30,by=0.01)
  }
  # Variâncias numerador e denominador
  if(v1 > v2){
    gl_num = df1
    v_num  = v1
    gl_den = df2
    v_den  = v2
  } else {
    gl_num = df2
    v_num  = v2
    gl_den = df1
    v_den  = v1
  }
  # Estatistica do Teste - ET
  ET  = (v_num/v_den)*ratio

  # Valores das densidades
  DenF  = df(x,gl_num,gl_den)
  DenET = df(ET,gl_num,gl_den)   # Densidade na estatistica amostral

## Bilateral - two.sided ======================================================================================================
  if(alternative == "two.sided"){

    lic  = qf(alpha/2,gl_num, gl_den) # Limite inferior crítico
    lsc  = qf(1-alpha/2,gl_num, gl_den)      # Limite superior crítico
    dlic = df(lic,gl_num, gl_den)   # Densidade no Limite inferior crítico
    dlsc = df(lsc,gl_num, gl_den)   # Densidade no Limite superior crítico

    # Plot da distribuicao H0
    if(ET > lsc){
      plot(x, DenF, type="l", xlab = "Statistic values", xlim = c(0,ET+1),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
         main = bquote("Null Distribution :" ~ sigma[A]^2/sigma[B]^2 == 1 ~ ",  " ~ df[A] == ~ .(gl_num) ~~~ df[B] == ~ .(gl_den)))

      }else{
      plot(x, DenF, type="l", xlab = "Statistic values", xlim = c(0,lsc+1),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main = bquote("Null Distribution :" ~ sigma[A]^2/sigma[B]^2 == 1 ~ ",  " ~ df[A] == ~ .(gl_num) ~~~ df[B] == ~ .(gl_den)))
    }

    # limites criticos alpha/2
    polygon(x = c(lsc, lsc     , x[x>lsc]   , tail(x, n=1)), # X = conjunto dos valores de lsc até o final
            y = c(0  , dlsc    , DenF[x>lsc], 0),            # Y = conjunto das densidades de lsc até o final
            col = corAreaLimCrit,                            # cor do preenchimento
            border = corAreaLimCrit)                         # cor da borda

    polygon(x   = c(0 ,0       , x[x<lic]    ,lic  ,lic), # X = conjunto dos valores de lsc até o final
            y   = c(0 ,DenF[1] , DenF[x<lic] ,dlic,0),    # Y = conjunto das densidades de lsc até o final
            col = corAreaLimCrit,                         # cor do preenchimento
            border = corAreaLimCrit)                      # cor da borda

    abline(v = lsc, lty = "dashed", lwd = 2, col = "darkgray")
    abline(v = lic, lty = "dashed", lwd = 2, col = "darkgray")
    abline(h = 0  , col = "gray"  , lwd = 0.5)
    points(ET, 0  , col = corET   , pch = 16 , cex=1.4)



    if(ET > ratio){
      area_ET_cum = 1 - pf(ET, gl_num, gl_den)    # 1 - (area de 0 até estatistica amostral)
      ponto_ET = qf(area_ET_cum, gl_num, gl_den)  # ponto de ET na H0 - extremidade oposta
      Den_ponto_ET= df(ponto_ET, gl_num, gl_den)  # Densidade da ET estatistica amostral bilateral - extremidade oposta

      # area do  P-value superior
      polygon(x = c(ET, ET    , x[x>ET]   , tail(x, n=1)), # X = conjunto dos valores de lsc até o final
              y = c(0 , DenET , DenF[x>ET], 0),            # Y = conjunto das densidades de lsc até o final
              col = corPValue , density = c(30), angle = c(-45))

      # area do  P-value inferior
      polygon(x = c(0 ,0       , x[x<ponto_ET]    ,ponto_ET  ,ponto_ET), # X = conjunto dos valores de lsc até o final
              y = c(0 ,DenF[1] , DenF[x<ponto_ET] ,Den_ponto_ET  ,0),            # Y = conjunto das densidades de lsc até o final
              col = corPValue  , density = c(30), angle = c(-45))

    }
    if(ET <= ratio){

      area_ET_cum = pf(ET, gl_num, gl_den)           # (area de 0 até estatistica amostral)
      ponto_ET    = qf(area_ET_cum, gl_num, gl_den)  # Ponto de ET na H0
      Den_ponto_ET= df(ponto_ET, gl_num, gl_den)     # Densidade da ET estatistica amostral bilateral

      ponto_ET_2  = qf(1-area_ET_cum, gl_num, gl_den) # Ponto de ET na H0 - extremidade oposta
      Den_ponto_ET_2= df(ponto_ET_2, gl_num, gl_den)      # Densidade da ET estatistica amostral bilateral - extremidade oposta

      # area do  P-value superior
      polygon(x = c(ponto_ET  , ponto_ET    , x[x>ponto_ET]   , tail(x, n=1)), # X = conjunto dos valores de lsc até o final
              y = c(0         , Den_ponto_ET, DenF[x>ponto_ET], 0),            # Y = conjunto das densidades de lsc até o final
              col = corPValue , density = c(30), angle = c(-45))

      # area do  P-value inferior
      polygon(x = c(0 ,0       , x[x<ponto_ET_2]    ,ponto_ET_2  ,ponto_ET_2), # X = conjunto dos valores de lsc até o final
              y = c(0 ,DenF[1] , DenF[x<ponto_ET_2] ,Den_ponto_ET_2  ,0),            # Y = conjunto das densidades de lsc até o final
              col = corPValue  , density = c(30), angle = c(-45))

    }


    Map(axis, side=1, at = round(c(lic,ET, lsc),2),
        col.axis = c("gray" , corET, "gray"),
        col.ticks = c("gray", corET, "gray"),
        lwd=0, las=1,
        lwd.ticks = 2)

    # plotar novamente a curva por cima
    lines(x, DenF, type="l", xlab = "Statistic values", ylab = "Density", lwd = 3, col = corCurva)

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
      legenda <- list( bquote( "Test Statistic = F = "~ .(round(ET,2)) ) ,
                       bquote( alpha == ~.(alpha) ),
                       bquote( "Critical limits" ~~ "[" ~ .(round(lic,2)) ~ ";" ~ .(round(lsc,2)) ~"]") )

      mtext(side = 3, do.call(expression, legenda), line=-1:-3, adj=1, col=c(corET,corAreaLimCrit, "gray"))
    }
  }

  ## Superior - greater ======================================================================================================
    if(alternative == "greater"){

    lsc  = qf(1-alpha,gl_num, gl_den)      # Limite superior crítico
    dlsc = df(lsc,gl_num, gl_den)          # Densidade no Limite superior crítico

    area_ET_cum = 1-pf(ET, gl_num, gl_den)         # 1 - (area de 0 até estatistica amostral)
    ponto_ET    = qf(area_ET_cum, gl_num, gl_den)  # Ponto de ET na H0
    Den_ponto_ET= df(ponto_ET, gl_num, gl_den)     # Densidade da ET estatistica amostral bilateral

    # Plot da distribuicao H0
    if(ET > lsc){
      plot(x, DenF, type="l", xlab = "Statistic values", xlim = c(0,ET+1),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main = bquote("Null Distribution :" ~ sigma[A]^2/sigma[B]^2 == 1 ~ ",  " ~ df[A] == ~ .(gl_num) ~~~ df[B] == ~ .(gl_den)))
    }else{
      plot(x, DenF, type="l", xlab = "Statistic values", xlim = c(0,lsc+1),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main = bquote("Null Distribution :" ~ sigma[A]^2/sigma[B]^2 == 1 ~ ",  " ~ df[A] == ~ .(gl_num) ~~~ df[B] == ~ .(gl_den)))

    }


    # limite critico alpha
    polygon(x = c(lsc, lsc     , x[x>lsc]   , tail(x, n=1)), # X = conjunto dos valores de lsc até o final
            y = c(0  , dlsc    , DenF[x>lsc], 0),            # Y = conjunto das densidades de lsc até o final
            col = corAreaLimCrit,                            # cor do preenchimento
            border = corAreaLimCrit)                         # cor da borda

    # area do  P-value superior
    polygon(x = c(ET, ET    , x[x>ET]   , tail(x, n=1)), # X = conjunto dos valores de lsc até o final
            y = c(0 , DenET , DenF[x>ET], 0),            # Y = conjunto das densidades de lsc até o final
            col = corPValue, density = c(30), angle = c(-45))

    abline(v = lsc, lty = "dashed", lwd = 2, col = "darkgray")
    abline(h = 0  , col = "gray"  , lwd = 0.5)
    points(ET, 0  , col = corET   , pch = 16 , cex=1.4)

    Map(axis, side=1, at = round(c(ET, lsc),2),
        col.axis  = c(corET, "gray"),
        col.ticks = c(corET, "gray"),
        lwd=0, las=1,
        lwd.ticks = 2)

    # plotar novamente a curva por cima
    lines(x, DenF, type="l", xlab = "Statistic values", ylab = "Density", lwd = 3, col = corCurva)

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
      legenda <- list( bquote( "Test Statistic = F = "~ .(round(ET,2)) ) ,
                       bquote( alpha == ~.(alpha) ),
                       bquote( "Critical limits" ~~ "[ -Inf ;" ~ .(round(lsc,2)) ~"]") )

      mtext(side = 3, do.call(expression, legenda), line=-1:-3, adj=1, col=c(corET,corAreaLimCrit, "gray"))
    }
    }
  ## Inferior - less ==========================================================================================================
  if(alternative == "less"){

    lic  = qf(alpha,gl_num, gl_den) # Limite inferior crítico
    dlic = df(lic,gl_num, gl_den)   # Densidade no Limite inferior crítico

    area_ET_cum = pf(ET, gl_num, gl_den)           # (area de 0 até estatistica amostral)
    ponto_ET    = qf(area_ET_cum, gl_num, gl_den)  # Ponto de ET na H0
    Den_ponto_ET= df(ponto_ET, gl_num, gl_den)     # Densidade da ET estatistica amostral bilateral

    lsc  = qf(1-alpha,gl_num, gl_den)      # Limite superior crítico


    # Plot da distribuicao H0
    if(ET > lsc){
      plot(x, DenF, type="l", xlab = "Statistic values", xlim = c(0,ET+1),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main = bquote("Null Distribution :" ~ sigma[A]^2/sigma[B]^2 == 1 ~ ",  " ~ df[A] == ~ .(gl_num) ~~~ df[B] == ~ .(gl_den)))
    }else{
      plot(x, DenF, type="l", xlab = "Statistic values", xlim = c(0,lsc+1),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main = bquote("Null Distribution :" ~ sigma[A]^2/sigma[B]^2 == 1 ~ ",  " ~ df[A] == ~ .(gl_num) ~~~ df[B] == ~ .(gl_den)))

    }


    # limite critico alpha
    polygon(x   = c(0 ,0       , x[x<lic]    ,lic  ,lic), # X = conjunto dos valores de lsc até o final
            y   = c(0 ,DenF[1] , DenF[x<lic] ,dlic,0),    # Y = conjunto das densidades de lsc até o final
            col = corAreaLimCrit,                         # cor do preenchimento
            border = corAreaLimCrit)                      # cor da borda

    # area do  P-value inferior
    polygon(x = c(0 ,0       , x[x<ponto_ET]    ,ponto_ET  ,ponto_ET), # X = conjunto dos valores de lsc até o final
            y = c(0 ,DenF[1] , DenF[x<ponto_ET] ,Den_ponto_ET  ,0),    # Y = conjunto das densidades de lsc até o final
            col = corPValue, density = c(30), angle = c(-45))

    abline(v = lic, lty = "dashed", lwd = 2, col = "darkgray")
    abline(h = 0  , col = "gray"  , lwd = 0.5)
    points(ET, 0  , col =corET    , pch = 16 , cex=1.4)


    Map(axis, side=1, at = round(c(lic, ET),2),
        col.axis  = c("gray" , corET),
        col.ticks = c("gray" , corET),
        lwd=0, las=1,
        lwd.ticks = 2)


    # plotar novamente a curva por cima
    lines(x, DenF, type="l", xlab = "Statistic values", ylab = "Density", lwd = 3, col = corCurva)

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
              curved  = -min( c(1/3000 , 1/(gl_num*ceiling(ET))) ), #-0.085, curvatura diminui conforme muda a escala - graus de liberdade e ET
              width  =  1,
              size   = 0.7)


      text(xcent   , ycent,
           label = paste("P-value =",round(area_ET_cum,2)),
           col   = corPValue,
           pos = 3)

      # fim Valor de P arrow  ------------------------------------

      # Legendas
      legenda <- list( bquote( "Test Statistic = F = "~ .(round(ET,2)) ) ,
                       bquote( alpha == ~.(alpha) ),
                       bquote( "Critical limits" ~~ "[" ~ .(round(lic,2)) ~"; +Inf]") )

      mtext(side = 3, do.call(expression, legenda), line=-1:-3, adj=1, col=c(corET,corAreaLimCrit, "gray"))
    }
  }
  PVAL = switch(alternative,
                two.sided = 2*area_ET_cum,
                less      = area_ET_cum,
                greater   = area_ET_cum)
  return(list(test.statistic = ET, df.num = gl_num, df.den = gl_den, p.value = PVAL))
}


