% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_trans.R
\name{f_trans}
\alias{f_trans}
\title{creates a dataframe containing the points for the espiral hiperbolica mandala}
\usage{
f_trans(x, y, t, d)
}
\arguments{
\item{x}{is a vector length n with coordinate x of point}

\item{y}{is a vector length n with coordinate y of point}

\item{t}{is a vector with shifts on the x or y-axis}

\item{d}{is a direction translation, 1)x or 2)y}
}
\value{
Returns a dataframe with the original points plus the respective translation of these points.
}
\description{
Function to translation points by shifts on the x-axis or y-axis
}
\examples{
x=c(1,1)
y=c(0,1)
t=c(-3, 3)
d=1
f_trans(x,y,t,d)

}
\author{
Luciane Ferreira Alcoforado
}
