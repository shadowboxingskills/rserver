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

usethis::use_package("mlbench")
usethis::use_package("caret")
usethis::use_package("lattice")
usethis::use_package("caTools")

#' Multiple Regression example using Boston housing dataset
#'
#' This function creates a multiple regression model trained with the Boston housing dataset.
#'
#' @export
#' @param n numer of random values
#' @param features one of "normal" or "uniform".
regression_model <- function(n, features){
  require(mlbench)
  require(caret)
  require(lattice)
  # require(caTools)

  data(BostonHousing)

  # - CRIM     per capita crime rate by town
  # - ZN       proportion of residential land zoned for lots over 25,000 sq.ft.
  # - INDUS    proportion of non-retail business acres per town
  # - CHAS     Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
  # - NOX      nitric oxides concentration (parts per 10 million)
  # - RM       average number of rooms per dwelling
  # - AGE      proportion of owner-occupied units built prior to 1940
  # - DIS      weighted distances to five Boston employment centres
  # - RAD      index of accessibility to radial highways
  # - TAX      full-value property-tax rate per $10,000
  # - PTRATIO  pupil-teacher ratio by town
  # - B        1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
  # - LSTAT    % lower status of the population
  # - MEDV     Median value of owner-occupied homes in $1000's

  df <- BostonHousing
  # str(df)
  # regVar <- c("age", "lstat", "tax")
  # regVar <- c("crim", "zn", "indus", "nox" ,"rm", "age", "dis", "rad", "tax", "ptratio", "b", "lstat") # chas categorical
  # remove chas from features list since it is categorical
  if ( "chas" %in% features ) {
    regVar <- features[!(features %in% "chas")]
  } else {
    regVar <- features
  }

  N_features <- length(regVar)
  layout_cols <- ifelse( N_features < 3, N_features, 3)
  layout_rows <- ceiling( N_features / 3)

  # str(df[, regVar])

  theme1 <- lattice::trellis.par.get()
  theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
  theme1$plot.symbol$pch = 16
  theme1$plot.line$col = rgb(1, 0, 0, .7)
  theme1$plot.line$lwd <- 2
  lattice::trellis.par.set(theme1)
  p <- caret::featurePlot(x = df[, regVar],
              y = df$medv,
              plot = "scatter",
              type = c("p", "smooth"),
              span = .5,
              layout = c(layout_cols, layout_rows)
              )
  print(p)

  # N_features <- dim(df)[2] - 1 # total number of features
  # all_features <- colnames(df)

  # #input validation
  # #features <- match.arg(dist, several.ok = T)
  # stopifnot(length(features) > 0)
  # stopifnot(length(features) < N_features)
  # stopifnot(features %in% all_features)

  # message <- paste0("dist=", paste(features, ' '), "/ ",
  #                   "length=", length(features))
  #
  # if ( length(features) == 1 ) {
  #   if(features == "normal"){
  #     graphics::hist(stats::rnorm(n), main=message)
  #   }
  #
  #   if(features == "uniform"){
  #     graphics::hist(stats::runif(n), main=message)
  #   }
  # } else {
  #   if ( (features[1] == "uniform") && (features[2] == "normal") ) {
  #     graphics::hist(stats::rnorm(n), main=message)
  #   }
  # }

  #return nothing
  invisible();
}


