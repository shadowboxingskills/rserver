#' Make a random plot
#'
#' This function creates a random histogram plot.
#'
#' @export
#' @param n numer of random values
#' @param dist one of "normal" or "uniform".
randomplot <- function(n, dist=c("normal", "uniform")){
  #input validation
  dist <- match.arg(dist)
  stopifnot(n < 1e6)

  if(dist == "normal"){
    hist(rnorm(n), main="SG")
  }

  if(dist == "uniform"){
    hist(runif(n))
  }

  #return nothing
  invisible();
}

install.packages("caTools", repos = "http://cran.us.r-project.org")

#' Multiple Regression example using Boston housing dataset
#'
#' This function creates a multiple regression model trained with the Boston housing dataset.
#'
#' @export
#' @param n numer of random values
#' @param dist one of "normal" or "uniform".
regression_model <- function(n, dist=c("normal", "uniform")){
  require(caTools)

  #input validation
  dist <- match.arg(dist)
  stopifnot(n < 1e6)

  if(dist == "normal"){
    hist(rnorm(n), main="SG")
  }

  if(dist == "uniform"){
    hist(runif(n))
  }

  #return nothing
  invisible();
}


