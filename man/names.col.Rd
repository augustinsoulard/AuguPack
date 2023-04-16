\name{names.col}
\alias{names.col}
\title{names.col}
\usage{
names.col(data,col=1)
}
\description{
Change column names and delete column use for this.
}

\arguments{

\item{data}{data table use}

\item{col}{number or name of target row with new names for column}


\author{
Augustin Soulard
}

\examples{
tab = data.frame(A = c("E",1),B=c("F",5),C=c("G",7))
row.names(tab)=c("newNames","data")
names.col(tab,1)
names.col(tab,"newNames")
}
