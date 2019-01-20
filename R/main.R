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
    graphics::hist(stats::rnorm(n), main="SG")
  }

  if(dist == "uniform"){
    graphics::hist(stats::runif(n))
  }

  #return nothing
  invisible();
}

usethis::use_package("caTools")

#' Multiple Regression example using Boston housing dataset
#'
#' This function creates a multiple regression model trained with the Boston housing dataset.
#'
#' @export
#' @param n numer of random values
#' @param dist one of "normal" or "uniform".
regression_model <- function(n, dist){
  #require(caTools)

  #input validation
  #dist <- match.arg(dist, several.ok = T)
  stopifnot(n < 1e6)

  message <- paste0("dist = ", dist, ". ",
                    "dist[1] = ", dist[1], ". ",
                    "dist[2] = ", dist[2], ". ",
                    "length(dist) = ", length(dist))

  if ( length(dist) == 1 ) {
    if(dist == "normal"){
      graphics::hist(stats::rnorm(n), main=message)
    }

    if(dist == "uniform"){
      graphics::hist(stats::runif(n), main=message)
    }
  } else {
    if ( (dist[1] == "uniform") && (dist[2] == "normal") ) {
      graphics::hist(stats::rnorm(n), main="uniform + normal")
    } else {
      graphics::hist(stats::rnorm(n), main=paste0("ERROR", message))
    }
  }

  #return nothing
  invisible();
}


