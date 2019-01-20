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

#' Multiple Regression example using Boston housing dataset
#'
#' This function creates a multiple regression model trained with the Boston housing dataset.
#'
#' @export
#' @param features one of "normal" or "uniform".
regression_model <- function(features){
  require(mlbench, quietly = T, warn.conflicts = T) # for BostonHousing data
  require(caret, quietly = T, warn.conflicts = T)
  require(lattice, quietly = T, warn.conflicts = T)

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
  # regVar <- c("crim", "zn", "indus", "nox" ,"rm", "age", "dis", "rad", "tax", "ptratio", "b", "lstat") # chas categorical

  # remove chas from features list since it is categorical
  if ( "chas" %in% features ) {
    regVar <- features[!(features %in% "chas")]
  } else {
    regVar <- features
  }
  # str(df[, regVar])

  N_features <- length(regVar)
  layout_cols <- ifelse( N_features < 3, N_features, 3)
  layout_rows <- ceiling( N_features / 3)

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

  #return nothing
  invisible();
}


usethis::use_package("corrplot")

#' Multiple Regression example using Boston housing dataset
#'
#' This function creates a multiple regression model trained with the Boston housing dataset.
#'
#' @export
#' @param features one of "normal" or "uniform".
regression_model_correlation_plot <- function(features){
  require(mlbench, quietly = T, warn.conflicts = T) # for BostonHousing data
  require(corrplot, quietly = T, warn.conflicts = T) # for corrplot

  data(BostonHousing)
  df <- BostonHousing

  # remove chas from features list since it is categorical
  if ( "chas" %in% features ) {
    regVar <- features[!(features %in% "chas")]
  } else {
    regVar <- features
  }
  # add the target (medv)
  regVar <- c(regVar, "medv")
  x = df[, regVar]

  corrplot::corrplot( stats::cor(x) )

  #return nothing
  invisible();
}


usethis::use_package("caTools")
usethis::use_package("dplyr")

#' Multiple Regression example using Boston housing dataset
#'
#' This function creates a multiple regression model trained with the Boston housing dataset.
#'
#' @export
#' @param features one of "normal" or "uniform".
regression_model_training <- function(features){
  require(mlbench, quietly = T, warn.conflicts = T) # for BostonHousing data
  require(caTools, quietly = T, warn.conflicts = T) # for sample.split
  # require(dplyr, quietly = T, warn.conflicts = T) # for select

  data(BostonHousing)
  df <- BostonHousing

  set.seed(42)
  msk <- caTools::sample.split(df, SplitRatio = 3/4)
  t=sum( msk)  # number of elements in one class
  f=sum(!msk)  # number of elements in the other class
  stopifnot( round((t+f)*3/4) == t ) # test ratios

  # test results
  # print(paste( "All Labels numbers: total=",t+f,", train=",t,", test=",f,", ratio=", t/(t+f) ) )

  train <- base::subset(df, msk==T)
  test <- base::subset(df, msk==F)

  # train <- dplyr::select(train,-b)
  # test <- dplyr::select(test,-b)

  f <- paste0( "medv ~ ",  paste(features, collapse = " + ") )
  model <- stats::lm(formula = f , data = train)
  print(summary(model))

  # return nothing
  invisible();
}

#' Multiple Regression example using Boston housing dataset
#'
#' This function creates a multiple regression model trained with the Boston housing dataset.
#'
#' @export
#' @param features one of "normal" or "uniform".
regression_model_plot_residuals <- function(features){
  require(mlbench, quietly = T, warn.conflicts = T) # for BostonHousing data
  require(caTools, quietly = T, warn.conflicts = T) # for sample.split
  # require(dplyr, quietly = T, warn.conflicts = T) # for select

  data(BostonHousing)
  df <- BostonHousing

  set.seed(42)
  msk <- caTools::sample.split(df, SplitRatio = 3/4)
  t=sum( msk)  # number of elements in one class
  f=sum(!msk)  # number of elements in the other class
  stopifnot( round((t+f)*3/4) == t ) # test ratios

  # test results
  # print(paste( "All Labels numbers: total=",t+f,", train=",t,", test=",f,", ratio=", t/(t+f) ) )

  train <- base::subset(df, msk==T)
  test <- base::subset(df, msk==F)

  # train <- dplyr::select(train,-b)
  # test <- dplyr::select(test,-b)

  f <- paste0( "medv ~ ",  paste(features, collapse = " + ") )
  model <- stats::lm(formula = f , data = train)
  # summary(model)

  par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
  plot(model)
  # par(mfrow = c(1, 1))  # Return plotting panel to 1 section

  #return nothing
  invisible();
}


usethis::use_package("ggplot2")

#' Multiple Regression example using Boston housing dataset
#'
#' This function creates a multiple regression model trained with the Boston housing dataset.
#'
#' @export
#' @param features one of "normal" or "uniform".
regression_model_plot_residuals_distribution <- function(features){
  require(mlbench, quietly = T, warn.conflicts = T) # for BostonHousing data
  require(caTools, quietly = T, warn.conflicts = T) # for sample.split
  require(ggplot2, quietly = T, warn.conflicts = T) # for ggplot

  data(BostonHousing)
  df <- BostonHousing

  set.seed(42)
  msk <- caTools::sample.split(df, SplitRatio = 3/4)
  t=sum( msk)  # number of elements in one class
  f=sum(!msk)  # number of elements in the other class
  stopifnot( round((t+f)*3/4) == t ) # test ratios

  train <- base::subset(df, msk==T)
  test <- base::subset(df, msk==F)

  f <- paste0( "medv ~ ",  paste(features, collapse = " + ") )
  # f <- "medv ~ ."
  model <- stats::lm(formula = f , data = train)

  residuals <- residuals(model)
  residuals <- as.data.frame(residuals)

  ggplot2::ggplot( residuals, aes(residuals) ) +
    ggplot2::geom_histogram( fill='red', alpha=0.4, binwidth=1 )

  #return nothing
  invisible();
}


