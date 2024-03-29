% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xegaDfCrossover.R
\name{xegaDfCrossoverFactory}
\alias{xegaDfCrossoverFactory}
\title{Configure the crossover function of a genetic algorithm.}
\usage{
xegaDfCrossoverFactory(method = "UCrossGene")
}
\arguments{
\item{method}{A string specifying the crossover function.}
}
\value{
Crossover function for genes.
}
\description{
\code{xegaDfCrossoverFactory} implements the selection
             of one of the crossover functions in this
             package by specifying a text string.
             The selection fails ungracefully (produces
             a runtime error), if the label does not match.
             The functions are specified locally.

             Current support:

             Crossover functions with one kid:
             \enumerate{
             \item "CrossGene" returns \code{CrossGene}.
             \item "UCrossGene" returns \code{UCrossGene}. Default.
             \item "PUCrossGene" returns \code{PUCrossGene}.
             }
}
\examples{
XGene<-xegaDfCrossoverFactory("UCrossGene")
gene1<-xegaDfInitGene(lFxegaDfGene)
gene2<-xegaDfInitGene(lFxegaDfGene)
XGene(gene1, gene2, lFxegaDfGene)
}
\seealso{
Other Configuration: 
\code{\link{lFxegaDfGene}},
\code{\link{xegaDfGeneMapFactory}()},
\code{\link{xegaDfMutationFactory}()},
\code{\link{xegaDfReplicationFactory}()},
\code{\link{xegaDfScaleFactorFactory}()}
}
\concept{Configuration}
