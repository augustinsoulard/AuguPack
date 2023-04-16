\name{barmod}
\alias{barmod}
\title{barmod}
\usage{
barmod(mod,formule,title,errorbar,xlab,ylab,ymin,ymax,bartitle,barcolor,barsignification,hauteurText,labelsize,pos.legend,title.legend,horiz)
}
\description{
Display graphic.
}

\arguments{
\item{mod}{modèle issue d'une fonction \code{glm()} ou similaire}

\item{formule}{Formule utilisant les noms de colonnes du tableau \code{data}. Exemple : \code{diversite~station*habitat}}

\item{title}{an overall title for the plot}

\item{errorbar}{Le type de barre d'erreur utilisé, par défaut "IC" pour l'intervalle de confiance, il ets possible d'utilise "ES" pour l'erreur standard}

\item{xlab}{a title for the x axis}

\item{ylab}{a title for the y axis}

\item{ymin}{Minimal value of the y axis}

\item{ymax}{Maximal value of the y axis}

\item{bartitle}{Title of bar in vector of factors}

\item{barcolor}{Color of bar in vector of factors}

\item{barsignification}{Signification codes in vector of factors}

\item{hauteurText}{Vertical positioning of signification codes}

\item{labelsize}{Size of signification codes}

\item{pos.legend}{Legend position if interaction. Can be right, left, bot, top, topright,botleft ...}

\item{title.legend}{Legend title}

\item{horiz}{TRUE or FALSE, allow to organize legend horizontally or vertically }
}

\author{
Augustin Soulard
}
