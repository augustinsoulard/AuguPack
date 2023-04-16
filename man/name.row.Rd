\name{names.row}
\alias{names.row}
\title{names.row}
\usage{
names.row(data,row=1)
}
\description{
Change row names and delete row use for this.
}

\arguments{

\item{data}{data table use}

\item{row}{number or name of target column with new names for row}


\author{
Augustin Soulard

\examples{
tab = data.frame(A = c("E","F"),B=c(3,5),C=c(3,7))
names.row(tab,1)
names.row(tab,"A")
}
