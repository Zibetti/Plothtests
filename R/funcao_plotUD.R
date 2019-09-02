

#' Plot Uniform Discrete Probability Distribution
#' 
#' Plot Uniform Discrete Probability Distribution and highlight probability of some values from i to f. There are some default to values
#' to each parameter in function.
#'
#' @param a lower limits of the distribution
#' @param b upper limits of the distribution
#' @param prob logical (default = FALSE), if TRUE highlight some values of the probability of random variable from 'i' to 'f'
#' @param i highlight probability from this intial value of distribution
#' @param f highlight probability from 'i' to this final value of distribution
#'
#' @return
#' @export
#'
#' @examples
#' plotUD(a = 0, b = 4, prob = TRUE, i = 2, f = 4)

plotUD = function(a = 0, b = 4, prob = FALSE, i = 2, f = 4){
  if(prob == TRUE){
    if (i < a | i > b) stop("'i' deve estar entre 'a' e 'b' ")
    if (f > b | f < i) stop("'f' deve estar entre 'a' e 'b' e menor que 'i' ")
  }
  
  x  = seq(a,b,1)
  px = rep(1/(b-a+1),b-a+1)
  plot(x, px, 
       type = "h", 
       bty="n", 
       xaxt="n", # não plota eixo x
       yaxt="n", # não plota eixo y,
       xlab = "x",
       ylab = bquote( p[X](x) ))
  
  points(x,px,pch=16)
  axis(side=1,at=seq(a-1,b+1,1),lwd=3)
  axis(side=2,at=seq(0,1,0.05),lwd=3)
  exp_value = (a+b)/2
  var_value = ((b-a+1)^2 - 1)/12
  # Legendas
  legenda <- list( bquote( "E[X] =" ~ .(exp_value)) ,
                   bquote( "V(X) =" ~ .(var_value))
                   )
  
  mtext(side = 3, do.call(expression, legenda), line=-1:-2, adj=1, col=c("gray", "gray"))
  
  if(prob == TRUE){
    par(new=TRUE)
    d = i:f
    pd = rep(1/(b-a+1),f-i+1)
    plot( d, pd, type="h", 
          bty="n", 
          xaxt="n", # não plota eixo x
          yaxt="n", # não plota eixo y,
          xlab = NA,
          ylab = NA,
          col="green",
          xlim= c(a,b))
    points(x,px,pch=16)
  }
}




#' Plot Cumultaive Uniform Discrete Probability Distribution
#'
#' @param a lower limits of the distribution
#' @param b upper limits of the distribution
#'
#' @return
#' @export
#'
#' @examples
#' plotcUD(a = 0, b = 4)

plotcUD = function(a = 0, b = 4){
  x  = seq(a,b,1)
  px = rep(1/(b-a+1),b-a+1)
  cpx = cumsum(px)
  plot(x, cpx, 
       xlim = c(a,b),
       ylim = c(0,1),
       type = "n", 
       bty="n", 
       xaxt="n", # não plota eixo x
       yaxt="n", # não plota eixo y,
       xlab = "x",
       ylab = bquote( F[X](x) ))
  axis(side=1,at=seq(a-1,b+1,1) ,lwd=3)
  axis(side=2,at=seq(0,1,0.10),lwd=3)
  segments(x,
           cpx,
           c(x[-1], x[length(x)]+1),
           cpx)
  
  points(x,cpx,pch=16)
  points(x[-1],cpx[-length(x)],pch=1)
  abline(h = 0, col = "gray", lty = "dashed")
  abline(h = 1, col = "gray", lty = "dashed")
}


plotcUD(a = 0, b = 4)
plotUD(a = 0, b = 4, prob = TRUE, i = 2, f = 4)

plotcUD(a = 1, b = 12)
plotUD(a = 1, b = 12, prob = TRUE, i = 2, f = 4)



plotBIN = function(n = 5, p = 0.15, hlt = FALSE, i = 2, f = 4){
  if(hlt == TRUE){
    if (n < 0 | p > 1) stop("'n' deve ser maior que 0, 'p' deve ser menor ou igual a 1 ")
    if (p < 0)         stop("'p' deve estar entre 0 e 1 ")
  }
  
  x  = seq(0,n,1)
  px = dbinom(x, size = n, prob = p)
  plot(x, px, 
       type = "h", 
       bty="n", 
       xaxt="n", # não plota eixo x
       yaxt="n", # não plota eixo y,
       xlab = "x",
       ylab = bquote( p[X](x) ))
  
  points(x, px, pch=16)
  axis(side=1,at=seq(0,n,1),lwd=3)
  axis(side=2,at=seq(0,1,0.05),lwd=3)
  # Calculo do valor esperado
  exp_value = n*p
  var_value = n*p*(1-p)
  # Legendas
  legenda <- list( bquote( "E[X] =" ~ .(exp_value)) ,
                   bquote( "V(X) =" ~ .(var_value))
  )
  
  mtext(side = 3, do.call(expression, legenda), line=-1:-2, adj=1, col=c("gray", "gray"))
  xl = par("usr")[1:2]
  yl = par("usr")[3:4]
  if(hlt == TRUE){
    par(new=TRUE)
    par(xaxs="i",yaxs="i")
    d  = i:f
    pd = px[(i+1):(f+1)]
    plot( d, pd, type="h", 
          bty="n", 
          xaxt="n", # não plota eixo x
          yaxt="n", # não plota eixo y,
          xlab = NA,
          ylab = NA,
          col="green",
          xlim = as.numeric(format(xl,digits = 6)),
          ylim = as.numeric(format(yl,digits = 6)))
    points(x,px,pch=16)
  }
}
plotBIN(n = 5, p = 0.15, hlt = TRUE, i = 2, f = 4)
# if you want both plots to be on the same scale, you can specify the axes
# limits a priori, or retrieve the limits from the first figure to call in the
# second by par("usr"). to use these in the second xlim, ylim call, you should
# set par(xaxs="i",yaxs="i") so they aren't padded above and below the
# specified limits.