% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weibull.R
\name{weibull}
\alias{weibull}
\alias{weibullcdf}
\alias{weibull.pdf}
\alias{weibull.orig}
\alias{weibull.inverse}
\alias{weibull.orig.cdf}
\alias{weibull.orig.pdf_x}
\alias{weibull.orig.pdf_p1}
\alias{weibull.orig.pdf_p2}
\alias{weibull.inverse_x.cdf}
\alias{weibull.inverse_x.pdf}
\title{Weibul}
\usage{
weibull(x, params)

weibull.orig(x, params)

weibull.inverse(x, params)

\method{weibull}{orig.cdf}(x, params)

\method{weibull}{orig.pdf_x}(x, params)

\method{weibull}{orig.pdf_p1}(x, params)

\method{weibull}{orig.pdf_p2}(x, params)

\method{weibull}{inverse_x.cdf}(x, params)

\method{weibull}{inverse_x.pdf}(x, params)
}
\arguments{
\item{x}{Vector of x parametres m and s}

\item{params}{Weibull function has two parameters \n m marks the x coordinate where function reaches the midpoint. \n s adjusts the slope of the fuction}
}
\value{
Vector of result vaues
}
\description{
Weibull function is a core type function.
\cr
\cr
\cr
It's CDF formula is: y = 2*m*s*(log(x) - log(m)) / log(2) + log(log(2))
\cr
It's PDF formula per x is: y = 2*m*s/log(2)/x
\cr
It's PDF formula per m is: y = 2*s/log(2)*(log(x)-log(m)+1)
\cr
It's PDF formula per s is: 2*m*(log(x) - log(m))/log(2)
}
