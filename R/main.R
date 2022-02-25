usethis::use_package("quantmod")

#' Pull stock market data and plot it using Quantmod
#'
#' @export
#' @param ticker, nbmonths, theme, BollingerBands
plot_stock <- function(ticker, nbmonths, theme, BollingerBands){
  require(quantmod, quietly = T, warn.conflicts = T)

  period_nb_months <- nbmonths
  period_subset <- paste0('last ', period_nb_months, ' months')
  if ( BollingerBands == 'with Bollinger' ) {
    indicators <- "addVo();addBBands()"
  } else {
    indicators <- "addVo()"
  }
  s <- quantmod::getSymbols(ticker, src="yahoo", auto.assign = F)
  quantmod::chartSeries(s, subset=period_subset, theme=chartTheme(theme), name=ticker, TA=indicators)
}
