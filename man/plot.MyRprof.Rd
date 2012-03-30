\name{plot.MyRprof}
\alias{plot.MyRprof}
\title{Plots the call stack}
\usage{
  \method{plot}{MyRprof} (x, minlab = 0.1, add = FALSE,
    xlim = NULL, col = "grey", quiet = FALSE, ...)
}
\arguments{
  \item{x}{An object of class 'MyRprof'}

  \item{minlab}{The minimum 'length' for a call to get a
  label}

  \item{add}{logical value indicating if you want to have
  the option to click on rectangles after the initial plot
  is made to add labels}

  \item{xlim}{The limits for plotting for time}

  \item{col}{The color of the rectangles}

  \item{quiet}{logical value indicating if you want
  instructions printed if add = T}

  \item{...}{other arguments passed onto rect}
}
\description{
  Plots the call stack using back graphics
}
\details{
  Could be prettier but it doesn't have the bridge problem
}
\author{
  Dason Kurkiewicz
}

