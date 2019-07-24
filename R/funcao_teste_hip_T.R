#' Plot a two sample t-test of sample statistics
#'
#' Performs two sample t-tests for mean 1 and mean 2 (user supplied summary information). Is only applicable for the 2-sample tests (mu1-mu2).
#' alternative = "greater" is the alternative that sample mean 1 has a larger mean than sample mean 2.
#' If var.equal is "equal" then the pooled estimate of the variance is used. By default, if var.equal is "notequal" then the each sample variance is used separately and the Welch modification to the degrees of freedom is used.
#' Note: The result of Welch modification to the degrees of freedom was truncated to integer.
#'
#' @param alpha significance level for the critical limits; it must lie between zero and one
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @param var.equal a char variable indicating whether to treat the two variances as being equal or notequal. If "equal" then the pooled variance is used to estimate the variance otherwise ("notequal") the Welch (or Satterthwaite) approximation to the degrees of freedom is used
#' @param n1 a single number representing the sample size of sample 1
#' @param m1 a single number representing the sample mean of sample 1
#' @param v1 a single number representing the sample variance for sample 1
#' @param n2 a single number representing the sample size of sample 2
#' @param m2 a single number representing the sample mean of sample 2
#' @param v2 a single number representing the sample variance for sample 2
#' @param delta0 a number indicating the true value of the difference in means of the two sample test.
#' @param annotations logical, show annotations in plot figure, TRUE ou FALSE (default = TRUE)
#' @param color integer, color theme for plot, from 1 to 7 so far (default = 3)
#'
#' @export
#'
#' @importFrom utils getFromNamespace
#'
#' @examples
#' plot_T_test(alpha = 0.05, alternative = "two.sided",
#'             var.equal = "equal",
#'             n1 = 10, m1 = 3.1, v1 = 1.5,
#'             n2 = 20, m2 = 4.2, v2 = 1.7,
#'             delta0 = 0,
#'             annotations = TRUE,
#'             color = 3)



plot_T_test = function(alpha = 0.05, alternative = "two.sided", var.equal = "equal", n1, m1, v1, n2, m2, v2, delta0 = 0, annotations = TRUE, color = 3){

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

  if ((length(alpha) != 1 || !is.finite(alpha) || alpha < 0 || alpha > 1)) # para ter efetivamente o stop é necessário { curly } em toda a função
      stop("'alpha' must be a single number between 0 and 1")
  if (n1 < 1 || n2 < 1)
    stop("not enough 'n1 or n2' observations")

  # Dados - Eixo x:
  x    = seq(-30,30,by=0.01)

  # Variâncias - casos
  if(var.equal == "equal"){
    gl  = n1 + n2 - 2
    Sp2 = ( (n1 -1)*v1 + (n2-1)*v2 ) / (gl)
    ET  = ( (m1 - m2) - (delta0) ) / ( sqrt(Sp2)*sqrt(1/n1 + 1/n2) )
  }
  if(var.equal == "notequal"){
    gld = (v1/n1 + v2/n2)^2 / ( (v1/n1)^2 / (n1-1) + (v2/n2)^2 / (n2-1) )
    gl  = trunc(gld)
    ET  = ( (m1 - m2) - (delta0) ) / ( sqrt(v1/n1 + v2/n2) )
  }

  # Valores das densidades
  DenT  = dt(x,gl)    # Densidades dos valores de x
  DenET = dt(ET,gl)   # Densidade da estatistica amostral ET

  # Bilateral ========================================================================================================

  if(alternative == "two.sided"){

    lic  = qt(alpha/2,gl)           # Limite inferior crítico
    lsc  = qt(1-alpha/2,gl)         # Limite superior crítico
    dlic = dt(lic,gl)               # Densidade do Limite inferior crítico
    dlsc = dt(lsc,gl)               # Densidade do Limite superior crítico

    # Plot da distribuicao H0
    if(ET > 4){
      plot(x, DenT, type="l", xlab = "Statistic values", xlim = c(-(ET+1),ET+1),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main = bquote("Null Distribution :" ~ mu[1] - mu[2] == 0 ~ ",  " ~ gl == ~ .(gl)))

    }else if(ET < -4){
      plot(x, DenT, type="l", xlab = "Statistic values", xlim = c((ET-1),-(ET-1)),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main = bquote("Null Distribution :" ~ mu[1] - mu[2] == 0 ~ ",  " ~ gl == ~ .(gl)))

    }else{
      plot(x, DenT, type="l", xlab = "Statistic values", xlim = c(-4,4),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main = bquote("Null Distribution :" ~ mu[1] - mu[2] == 0 ~ ",  " ~ gl == ~ .(gl)))
    }

    # área dos limites criticos alpha/2 - superior
    polygon(x = c(lsc, lsc     , x[x>lsc]   , tail(x, n=1)), # X = conjunto dos valores de lsc até o final
            y = c(0  , dlsc    , DenT[x>lsc], 0),            # Y = conjunto das densidades de lsc até o final
            col = corAreaLimCrit,                            # cor do preenchimento
            border = corAreaLimCrit)                         # cor da borda

    # área dos limites criticos alpha/2 - inferior
    polygon(x = c(-4 ,-4        ,    x[x<lic] ,lic  , lic), # X = conjunto dos valores de lsc até o final
            y = c( 0 ,dt(-4,gl) , DenT[x<lic] ,dlic , 0  ), # Y = conjunto das densidades de lsc até o final
            col = corAreaLimCrit,                         # cor do preenchimento
            border = corAreaLimCrit)                      # cor da borda

    abline(v = lsc, lty = "dashed", lwd = 2, col = "darkgray")
    abline(v = lic, lty = "dashed", lwd = 2, col = "darkgray")
    abline(h = 0  , col = "gray"  , lwd = 0.5)
    points(ET, 0  , col = corET   , pch = 16 , cex=1.4)


    # Area do P-value
    if(ET >= 0){
      area_ET_cum = 1 - pt(ET, gl)                # area de cauda superior : 1 - (area de -inf até estatistica amostral)
      ponto_ET = qt(area_ET_cum, gl)              # ponto da ET na H0 - extremidade oposta
      Den_ponto_ET= dt(ponto_ET, gl)              # Densidade da ET   - extremidade oposta

      # area do  P-value superior
      polygon(x = c(ET, ET    , x[x>ET]   , tail(x, n=1)), # X = conjunto dos valores de lsc até o final
              y = c(0 , DenET , DenT[x>ET], 0),            # Y = conjunto das densidades de lsc até o final
              col = corPValue , density = c(30), angle = c(-45))

      # area do  P-value inferior
      polygon(x = c(-4 ,-4        ,    x[x<ponto_ET] ,ponto_ET     , ponto_ET), # X = conjunto dos valores de lsc até o final
              y = c( 0 ,dt(-4,gl) , DenT[x<ponto_ET] ,Den_ponto_ET , 0       ), # Y = conjunto das densidades de lsc até o final
              col = corPValue  , density = c(30), angle = c(-45))

    }
    if(ET < 0){
      area_ET_cum = pt(ET, gl)                       # area de cauda inferior :(area de 0 até estatistica amostral)
      ponto_ET    = qt(area_ET_cum, gl)              # Ponto de ET na H0
      Den_ponto_ET= dt(ponto_ET, gl)                 # Densidade da ET estatistica amostral bilateral

      ponto_ET_2     = -1*ET                        # Ponto de ET na H0 - extremidade oposta
      Den_ponto_ET_2 = dt(ponto_ET_2, gl)           # Densidade da ET estatistica amostral bilateral - extremidade oposta

      # area do  P-value superior
      polygon(x = c(ponto_ET_2, ponto_ET_2    ,    x[x>ponto_ET_2] , tail(x, n=1)), # X = conjunto dos valores de lsc até o final
              y = c(0         , Den_ponto_ET_2, DenT[x>ponto_ET_2] , 0           ), # Y = conjunto das densidades de lsc até o final
              col = corPValue , density = c(30), angle = c(-45))

      # area do  P-value inferior
      polygon(x = c(-4 ,-4        ,    x[x<ponto_ET] ,ponto_ET     , ponto_ET), # X = conjunto dos valores de lsc até o final
              y = c( 0 ,dt(-4,gl) , DenT[x<ponto_ET] ,Den_ponto_ET , 0       ), # Y = conjunto das densidades de lsc até o final
              col = corPValue  , density = c(30), angle = c(-45))
    }


    Map(axis, side=1, at = round(c(lic,ET, lsc),2),
        col.axis = c("gray" , corET, "gray"),
        col.ticks = c("gray", corET, "gray"),
        lwd = 0,
        las = 1,
        lwd.ticks = 2)

    # plotar novamente a curva por cima
    lines(x, DenT, type="l", xlab = "Statistic values", ylab = "Density", lwd = 3, col = corCurva)

    ## Anotações - escolha
    if(annotations == TRUE){

      # posição da legenda relativa ao Plot()

      ## obtendo os limites do plot
      plot_limits = par("usr") # [1]-xini [2]-xfim [3]-yini [4]-yfim

      ## obtendo o centro do plot
      xcent = (plot_limits[2] + plot_limits[1])/2
      ycent = (plot_limits[3] + plot_limits[4])/2

      ## atribuindo coordendas para o print da legenda
      xplot = plot_limits[2]/2
      yplot = plot_limits[4] - 0.10*plot_limits[4]

      # Valor de P arrow -------------------------------------
      # arrow direita
      iArrows(xcent   , ycent, # de  (x1,y1)
              abs(ET) + 0.2, 0.02,  # até (x2,y2)
              h.lwd  = 2,
              sh.lwd = 1,
              sh.col = corPValue,
              sh.lty = 2,
              h.lty = 1,
              curve  = 1/(10*abs(ET)),#0.085,
              width  =  1,
              size   = 0.7)
      # arrow esquerda
      iArrows(xcent   , ycent,   # de  (x1,y1)
              -1*abs(ET) - 0.2, 0.02, # até (x2,y2)
              h.lwd  = 2,
              sh.lwd = 1,
              sh.col = corPValue,
              sh.lty = 2,
              h.lty = 1,
              curve  = -1/(10*abs(ET)), #-0.085,
              width  =  1,
              size   = 0.7)

      text(xcent   , ycent,
           label = paste("P-value =",round(2*area_ET_cum,2)),
           col   = corPValue,
           pos = 3)

      # fim Valor de P arrow  ------------------------------------

      # Legendas
      legenda <- list( bquote( "Test Statistic = T = "~ .(round(ET,2)) ) ,
                       bquote( alpha == ~.(alpha) ),
                       bquote( "Critical limits" ~~ "[" ~ .(round(lic,2)) ~ ";" ~ .(round(lsc,2)) ~"]") )

      mtext(side = 3, do.call(expression, legenda), line=-1:-3, adj=1, col=c(corET,corAreaLimCrit, "gray"))

    }
  }

  ## superior ==========================================================================================================
  if(alternative == "greater"){

    lsc  = qt(1-alpha,gl)           # Limite superior crítico
    dlsc = dt(lsc,gl)               # Densidade do Limite superior crítico

    # Plot da distribuicao H0
    if(ET > 4){
      plot(x, DenT, type="l", xlab = "Statistic values", xlim = c(-(ET+1),ET+1),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main = bquote("Null Distribution :" ~ mu[1] - mu[2] == 0 ~ ",  " ~ gl == ~ .(gl)))

    }else if(ET < -4){
      plot(x, DenT, type="l", xlab = "Statistic values", xlim = c((ET-1),-(ET-1)),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main = bquote("Null Distribution :" ~ mu[1] - mu[2] == 0 ~ ",  " ~ gl == ~ .(gl)))

    }else{
      plot(x, DenT, type="l", xlab = "Statistic values", xlim = c(-4,4),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main = bquote("Null Distribution :" ~ mu[1] - mu[2] == 0 ~ ",  " ~ gl == ~ .(gl)))
    }

    # area do P-value
    area_ET_cum = 1 - pt(ET, gl)                # area de cauda superior : 1 - (area de -inf até estatistica amostral)
    ponto_ET = qt(area_ET_cum, gl)              # ponto da ET na H0 - extremidade oposta
    Den_ponto_ET= dt(ponto_ET, gl)              # Densidade da ET   - extremidade oposta

    # área dos limites criticos alpha - superior
    polygon(x = c(lsc, lsc     , x[x>lsc]   , tail(x, n=1)), # X = conjunto dos valores de lsc até o final
            y = c(0  , dlsc    , DenT[x>lsc], 0),            # Y = conjunto das densidades de lsc até o final
            col = corAreaLimCrit,                            # cor do preenchimento
            border = corAreaLimCrit )                         # cor da borda

    abline(v = lsc, lty = "dashed", lwd = 2, col = "darkgray")
    abline(h = 0  , col = "gray"  , lwd = 0.5)
    points(ET, 0  , col = corET   , pch = 16 , cex=1.4)

    # plotar novamente a curva por cima
    lines(x, DenT, type="l", xlab = "Statistic values", ylab = "Density", lwd = 3, col = corCurva)

    # area do  P-value superior
    polygon(x = c(ET, ET    , x[x>ET]   , tail(x, n=1)), # X = conjunto dos valores de lsc até o final
            y = c(0 , DenET , DenT[x>ET], 0),            # Y = conjunto das densidades de lsc até o final
            col     = corPValue ,
            density = c(10),
            angle   = c(-45),
            lwd     = 1.5 )

    Map(axis, side=1, at = round(c(ET, lsc),2),
        col.axis = c(corET, "gray"),
        col.ticks = c(corET, "gray"),
        lwd = 0,
        las = 1,
        lwd.ticks = 2)

    ## Anotações - escolha
    if(annotations == TRUE){

      # posição da legenda relativa ao Plot()

      ## obtendo os limites do plot
      plot_limits = par("usr") # [1]-xini [2]-xfim [3]-yini [4]-yfim

      ## obtendo o centro do plot
      xcent = (plot_limits[2] + plot_limits[1])/2
      ycent = (plot_limits[3] + plot_limits[4])/2

      ## atribuindo coordendas para o print da legenda
      xplot = plot_limits[2]/2
      yplot = plot_limits[4] - 0.10*plot_limits[4]

      # Valor de P arrow -------------------------------------
      # arrow direita
      if(ET >= 0){
      iArrows(xcent   , ycent,      # de  (x1,y1)
              ET + 0.2, 0.02,  # até (x2,y2)
              h.lwd  = 2,
              sh.lwd = 1,
              sh.col = corPValue,
              sh.lty = 2,
              h.lty = 1,
              curve  = 1/(10*abs(ET)),#0.085,
              width  =  1,
              size   = 0.7)
      }else{
        iArrows(xcent      , ycent,      # de  (x1,y1)
                xcent + 0.5, 0.02,  # até (x2,y2)
                h.lwd  = 2,
                sh.lwd = 1,
                sh.col = corPValue,
                sh.lty = 2,
                h.lty = 1,
                curve  = 1/(10*abs(ET)),#0.085,
                width  =  1,
                size   = 0.7)
      }


      text(xcent   , ycent,
           label = paste("P-value =",round(area_ET_cum,2)),
           col   = corPValue,
           pos = 3)

      # fim Valor de P arrow  ------------------------------------

      # Legendas
      legenda <- list( bquote( "Test Statistic = T = "~ .(round(ET,2)) ) ,
                       bquote( alpha == ~.(alpha) ),
                       bquote( "Critical limits" ~~ "[ -inf ;" ~ .(round(lsc,2)) ~"]") )

      mtext(side = 3, do.call(expression, legenda), line=-1:-3, adj=1, col=c(corET,corAreaLimCrit, "gray"))
    }
  }
  ## Inferior ============================================================================================================
  if(alternative == "less"){

    lic  = qt(alpha,gl) # Limite inferior crítico
    dlic = dt(lic,gl)   # Densidade no Limite inferior crítico

    area_ET_cum = pt(ET, gl)           # (area de 0 até estatistica amostral)
    ponto_ET    = qt(area_ET_cum, gl)  # Ponto de ET na H0
    Den_ponto_ET= dt(ponto_ET, gl)     # Densidade da ET estatistica amostral bilateral

    lsc  = qt(1-alpha,gl)              # Limite superior crítico

    # Plot da distribuicao H0
    if(ET > 4){
      plot(x, DenT, type="l", xlab = "Statistic values", xlim = c(-(ET+1),ET+1),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main = bquote("Null Distribution :" ~ mu[1] - mu[2] == 0 ~ ",  " ~ gl == ~ .(gl)))

    }else if(ET < -4){
      plot(x, DenT, type="l", xlab = "Statistic values", xlim = c((ET-1),-(ET-1)),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main = bquote("Null Distribution :" ~ mu[1] - mu[2] == 0 ~ ",  " ~ gl == ~ .(gl)))

    }else{
      plot(x, DenT, type="l", xlab = "Statistic values", xlim = c(-4,4),ylab = "Density", lwd = 3, col = corCurva, xaxt='n',
           main = bquote("Null Distribution :" ~ mu[1] - mu[2] == 0 ~ ",  " ~ gl == ~ .(gl)))
    }

    # área do limite critico alpha  - inferior
    polygon(x = c(x[1] ,x[1]        ,    x[x<lic] ,lic  , lic), # X = conjunto dos valores de lsc até o final
            y = c( 0   ,dt(x[1],gl) , DenT[x<lic] ,dlic , 0  ), # Y = conjunto das densidades de lsc até o final
            col = corAreaLimCrit,                         # cor do preenchimento
            border = corAreaLimCrit)                      # cor da borda

    # plotar novamente a curva por cima
    lines(x, DenT, type="l", xlab = "Statistic values", ylab = "Density", lwd = 3, col = corCurva)

    # area do  P-value inferior
    polygon(x = c(x[1] ,x[1]       ,    x[x<ponto_ET] ,ponto_ET    ,ponto_ET), # X = conjunto dos valores de lsc até o final
            y = c(0    ,dt(x[1],gl), DenT[x<ponto_ET] ,Den_ponto_ET,0       ), # Y = conjunto das densidades de lsc até o final
            col = corPValue,
            density = c(30),
            angle = c(-45),
            lwd = 1.5)

    abline(v = lic, lty = "dashed", lwd = 2, col = "darkgray")
    abline(h = 0  , col = "gray"  , lwd = 0.5)
    points(ET, 0  , col =corET    , pch = 16 , cex=1.4)


    Map(axis, side=1, at = round(c(lic, ET),2),
        col.axis  = c("gray" , corET),
        col.ticks = c("gray" , corET),
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

      ## atribuindo coordendas para o print da legenda
      xplot = plot_limits[2]/2
      yplot = plot_limits[4] - 0.10*plot_limits[4]

      # Valor de P arrow -------------------------------------
      # arrow direita
      if(ET >= 0){
        iArrows(xcent      , ycent,      # de  (x1,y1)
                xcent - 0.5, 0.02,  # até (x2,y2)
                h.lwd  = 2,
                sh.lwd = 1,
                sh.col = corPValue,
                sh.lty = 2,
                h.lty = 1,
                curve  = -1/(10*abs(ET)),#0.085,
                width  =  1,
                size   = 0.7)
      }else{
        iArrows(xcent   , ycent,      # de  (x1,y1)
                ET - 0.2, 0.02,  # até (x2,y2)
                h.lwd  = 2,
                sh.lwd = 1,
                sh.col = corPValue,
                sh.lty = 2,
                h.lty = 1,
                curve  = -1/(10*abs(ET)),#0.085,
                width  =  1,
                size   = 0.7)
      }


      text(xcent   , ycent,
           label = paste("P-value =",round(area_ET_cum,2)),
           col   = corPValue,
           pos = 3)

      # fim Valor de P arrow  ------------------------------------

      # Legendas
      legenda <- list( bquote( "Test Statistic = T = "~ .(round(ET,2)) ) ,
                       bquote( alpha == ~.(alpha) ),
                       bquote( "Critical limits" ~~ "[" ~ .(round(lic,2)) ~"; + Inf]") )

      mtext(side = 3, do.call(expression, legenda), line=-1:-3, adj=1, col=c(corET,corAreaLimCrit, "gray"))
    }
  }
  PVAL = switch(alternative,
                two.sided = 2*area_ET_cum,
                less      = area_ET_cum,
                greater   = area_ET_cum)
  return(list(test.statistic = ET, df = gl, p.value = PVAL))
}


# Testes --------------------------------------------------------------------------------------------------------------




