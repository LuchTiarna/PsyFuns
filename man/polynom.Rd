% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polynom.R
\name{polynom}
\alias{polynom}
\alias{polynom.cdf}
\alias{polynom.pdf}
\alias{polynom.orig}
\alias{polynom.inverse}
\alias{polynom.orig.cdf}
\alias{polynom.orig.pdf_x}
\alias{polynom.orig.pdf_p1}
\alias{polynom.orig.pdf_p2}
\alias{polynom.inverse_x.cdf}
\alias{polynom.inverse_x.pdf_x}
\title{Polynom}
\usage{
polynom(x, params)

polynom.orig(x, params)

polynom.inverse(x, params)

\method{polynom}{orig.cdf}(x, params)

\method{polynom}{orig.pdf_x}(x, params)

\method{polynom}{orig.pdf_p1}(x, params)

\method{polynom}{orig.pdf_p2}(x, params)

\method{polynom}{inverse_x.cdf}(x, params)

\method{polynom}{inverse_x.pdf_x}(x, params)
}
\arguments{
\item{x}{Vector of x parametres a and b}

\item{params}{polynom function has two parameters \n a \n b}

\item{b}{adjusts the curve slope}
}
\value{
Vector of result vaues
}
\description{
Polynomial function is a core type function.
\cr
\cr
\cr
It's CDF formula is: y = (x / a) ^ b
\cr
It's PDF formula per x is: y = (x / a) ^ (b- 1) * b / a
\cr
It's PDF formula per a is: y = (x / a)^(b + 1) * b * x
\cr
It's PDF formula per b is: y = (x / a)^(b) * log(x/a)
}
