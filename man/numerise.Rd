\name{numerise}
\alias{numerise}
\title{numerise}
\usage{
numerise(data,factor)
}
\description{
Numerise dataframe column by column. Possibility to choose factor column to not numerise.
}

\arguments{

\item{data}{data table use}

\item{factor}{factor to not numerise}


\author{
Augustin Soulard

\examples{
tab = data.frame(A = c("E","F"),B=c("3","5"),C=c("4","7"))
numerise(tab,factor="A")
numerise(tab,factor=1)
tab = data.frame(A = c("E","F"),B=c("3","5"),C=c("4","7"),D = c("2018","2020"))
numerise(tab,factor=c("A","D"))
numerise(tab,factor=c(1,4))
}
