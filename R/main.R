usethis::use_package("quantmod")
usethis::use_package("FinancialMath")

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

# usethis::use_package("FinancialMath")
# NPV(cf0=100,cf=c(50,60,10,20),times=c(1,5,9,9),i=.045, plot = T)
# IRR(cf0=100,cf=c(1,1,30,40,50,1),times=c(1,1,3,4,5,6), plot = T)
