usethis::use_package("ggplot2")

#' Make a random plot
#'
#' This function creates a random histogram plot.
#'
#' @export
#' @param n numer of random values
#' @param dist one of "normal" or "uniform".
randomplot <- function(n, dist=c("normal", "uniform", "test")){
  require(ggplot2, quietly = T, warn.conflicts = T)

  #input validation
  dist <- match.arg(dist)
  stopifnot(n < 1e6)
  main <- "SG"

  if(dist == "normal"){
    graphics::hist(stats::rnorm(n), main=main)
  }

  if(dist == "uniform"){
    graphics::hist(stats::runif(n), main=main)
  }

  if(dist == "test"){
    dat <- data.frame(cond = factor(rep(c("A","B"), each=n)), rating = c(rnorm(n),rnorm(n, mean=.8)))
    # Histogram overlaid with kernel density curve
    ggplot2::ggplot(dat, aes(x=rating)) +
      ggplot2::geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                     binwidth=.5,
                     colour="black", fill="white") +
      ggplot2::geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
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

  f <- paste0( "medv ~ ",  paste(features, collapse = " + ") )
  model <- stats::lm(formula = f , data = train)

  print( summary(model) )

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

  g <- ggplot2::ggplot( residuals, aes(residuals) ) +
    ggplot2::geom_histogram( fill='red', alpha=0.4, binwidth=1 ) +
    ggplot2::theme_light()
  print(g)

  #return nothing
  invisible();
}


usethis::use_package("plotly")
usethis::use_package("magrittr")
usethis::use_package("htmlwidgets")

#' Multiple Regression example using Boston housing dataset
#'
#' This function creates a multiple regression model trained with the Boston housing dataset.
#'
#' @export
#' @param features one of "normal" or "uniform".
regression_model_predict <- function(features){
  require(mlbench, quietly = T, warn.conflicts = T) # for BostonHousing data
  require(caTools, quietly = T, warn.conflicts = T) # for sample.split
  require(plotly, quietly = T, warn.conflicts = T) # for ggplotly
  require(ggplot2, quietly = T, warn.conflicts = T) # for ggplot
  require(magrittr, quietly = T, warn.conflicts = T) # for pipes
  require(htmlwidgets, quietly = T, warn.conflicts = T) # for saveWidget

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

  test$predicted.medv <- stats::predict( model, test )
  test$predicted.medv <- round( test$predicted.medv, 1 )

  p <- test %>%
    ggplot2::ggplot( aes(medv, predicted.medv) ) +
      ggplot2::geom_point( alpha=0.4, show.legend=F ) +
      ggplot2::geom_smooth( show.legend=F ) +
      ggplot2::xlab('Actual Prices') +
      ggplot2::ylab('Predicted Prices') +
      ggplot2::theme_light()

  m <- plotly::ggplotly(p)
  htmlwidgets::saveWidget(m, "mymap.html", selfcontained = F)
}


#' Multiple Regression example using Boston housing dataset
#'
#' This function creates a multiple regression model trained with the Boston housing dataset.
#'
#' @export
#' @param features one of "normal" or "uniform".
regression_model_RMSE <- function(features){
  require(mlbench, quietly = T, warn.conflicts = T) # for BostonHousing data
  require(caTools, quietly = T, warn.conflicts = T) # for sample.split
  require(caret, quietly = T, warn.conflicts = T) # for postResample

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

  test$predicted.medv <- stats::predict(model, test)

  # calculate the model Root Mean Square Error (RMSD)
  # error <- test$medv - test$predicted.medv
  # mse <- mean(error)^2
  # rmsd <- sqrt( mse )

  r <- caret::postResample(pred = test$predicted.medv, obs = test$medv)

  # print("RMSD manually calculated: ")
  # print( round(rmsd, 2) )
  print("Results from the caret library: ")
  print( round(r, 2) )

  #return nothing
  invisible();
}

usethis::use_package("leaflet")

#' Example leaflet widget
#'
#' Hello world of leaflet htmlwidget wrapped in OpenCPU. Based on example:
#' \url{http://rstudio.github.io/leaflet/}.
#'
#' @export
#' @param title label for the marker
#' @param lat lattitude coordinate
#' @param lng longintude coordinate
make_map1 <- function(title = "This is a test", lat, lng){
  m <- leaflet::leaflet()
  m <- leaflet::addTiles(m)
  m <- leaflet::addMarkers(m, lng = as.numeric(lng), lat = as.numeric(lat), popup = title)
  htmlwidgets::saveWidget(m, "mymap1.html", selfcontained = T)
}



