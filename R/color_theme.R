# Color Theme
## Cores para os gráficos


#' Selection of theme color, 1 to 5 for now
#'
#' @param paleta selection of a theme color
#'
#' @return list of colors
#' @export
#'
#' @examples
#' color_theme(3) # 1 to 7 themes so far

color_theme = function(paleta){

  # setClass(Class="ColorPlot",
  #          representation(
  #            corCurva        = "character",
  #            corPValue       = "character",
  #            corET           = "character",
  #            corLinhaLimCrit = "character",
  #            corAreaLimCrit  = "character"
  #          )
  # )

  # coleção de cores extraída de:
  # http://identidade.ufsc.br/files/2010/10/manual_brasao_ufsc_web.pdf
  # https://www.canva.com/pt_br/aprenda/cores-para-sites-50-paginas-impactantes/
  # https://www.canva.com/colors/color-palettes/?src=Blog&utm_source=Blog&utm_medium=Design&utm_content=Living-Coral-Cor-2019&utm_campaign=Aprenda

  if(paleta == 1){
    # Paleta UFSC
    corCurva         = rgb(0  ,56 ,147 , maxColorValue = 255) # azulUFSC
    corPValue        = "black" # rgb(0  ,146,6   , maxColorValue = 255) # verdeUFSC
    corET            = rgb(254,255,0   , maxColorValue = 255) # amareloUFSC
    corLinhaLimCrit  = rgb(0  ,0  ,0   , maxColorValue = 255) # pretoUFSC
    corAreaLimCrit   = rgb(255,0  ,9   , maxColorValue = 255) # vermelhoUFSC
  }else if(paleta == 2){
    # Paleta 08. puro e destacado
    corCurva         = "#CAEBF2" # Sky
    corPValue        = "#A9A9A9" # carbon
    corET            = "#EFEFEF" # Neutral
    corLinhaLimCrit  = "#A9A9A9" # carbon
    corAreaLimCrit   = "#FF3B3F" # Watermelon
  }else if(paleta == 3){
    # 04. Moderno e puro
    corCurva         = "#07889B" # Teal
    corPValue        = "#E37222" # Tangerine
    corET            = "#E37222" # Tangerine
    corLinhaLimCrit  = "#66B9Bf" # Powder
    corAreaLimCrit   = "#66B9Bf" # Powder
  }else if(paleta == 4){
    # 01. Divertido e profissional
    corCurva         = "#4ABDAC" # Fresh
    corPValue        = "#F7B733" # Sunshine
    corET            = "#F7B733" # Sunshine
    corLinhaLimCrit  = "#FC4A1A" # Vermillion
    corAreaLimCrit   = "#FC4A1A" # Vermillion
  }else if(paleta == 5){
    # 20. Degradê e cores desbotadas
    corCurva         = "#015249" # Forest
    corPValue        = "#77C9D4" # Feather
    corET            = "#77C9D4" # Feather
    corLinhaLimCrit  = "#57BC90" # Marine
    corAreaLimCrit   = "#57BC90" # Marine
  }else if(paleta == 6){
    #02. Inspirado na história da arte
    corCurva         = "#262228" # Evening
    corPValue        = "#FFCE00" # SunFlowers
    corET            = "#FFCE00" # SunFlowers
    corLinhaLimCrit  = "#0375B4" # starry Night
    corAreaLimCrit   = "#0375B4" # starry Night

  }else if(paleta == 7){
    #02. Inspirado na história da arte
    corCurva         = "#565656" # BlackBoard
    corPValue =corET = "#76323F" # Oxblood
    corLinhaLimCrit  = "#C09F80" # Tan
    corAreaLimCrit   = "#D7CEC7" # Grain

  }else{
    # 04. Moderno e puro
    corCurva         = "#07889B" # Teal
    corPValue        = "#E37222" # Tangerine
    corET            = "#E37222" # Tangerine
    corLinhaLimCrit  = "#66B9Bf" # Powder
    corAreaLimCrit   = "#66B9Bf" # Powder

  }
  # return(new("ColorPlot",
  #            corCurva        = corCurva,
  #            corPValue       = corPValue,
  #            corET           = corET,
  #            corLinhaLimCrit = corLinhaLimCrit,
  #            corAreaLimCrit  = corAreaLimCrit ))
  #
  return(list(corCurva        = corCurva,
             corPValue       = corPValue,
             corET           = corET,
             corLinhaLimCrit = corLinhaLimCrit,
             corAreaLimCrit  = corAreaLimCrit ))
}

