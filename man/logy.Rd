% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Logy.R
\name{logy}
\alias{logy}
\alias{logy.cdf}
\alias{logy.pdf}
\alias{logy.orig}
\alias{logy.inverse}
\alias{logy.orig.cdf}
\alias{logy.orig.pdf_x}
\alias{logy.orig.pdf_p1}
\alias{logy.orig.pdf_p2}
\alias{logy.inverse_x.cdf}
\alias{logy.inverse_x.pdf_x}
\title{Logy}
\usage{
logy.orig(x, params)

logy.inverse(x, params)

\method{logy}{orig.cdf}(x, params)

\method{logy}{orig.pdf_x}(x, params)

\method{logy}{orig.pdf_p1}(x, params)

\method{logy}{orig.pdf_p2}(x, params)

\method{logy}{inverse_x.cdf}(x, params)

\method{logy}{inverse_x.pdf_x}(x, params)
}
\arguments{
\item{x}{Vector of x parametres a and b}

\item{params}{logy function has two parameters\n a - increases the gradient of the function \n b - defines the origin point}
}
\value{
Vector of result vaues
}
\description{
Log function is a core type function. It is the part of PDF.
\cr
\cr
\cr
It's CDF formula is: y = a * log(x) + b
\cr
It's PDF formula per x is: y = a / x
\cr
It's PDF formula per a is: y = log(x)
\cr
It's PDF formula per b is: y = 1
}
