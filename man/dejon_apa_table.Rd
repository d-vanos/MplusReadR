% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dejon_apa_table.R
\name{dejon_apa_table}
\alias{dejon_apa_table}
\title{APA-style Mplus Tables for Dejonckheere Project}
\usage{
dejon_apa_table(tidy_data, model_type)
}
\arguments{
\item{tidy_data}{A tidy dataset of Mplus models, created by \code{\link[=dejon_compile]{dejon_compile()}}.}

\item{model_type}{One of 'null', 'univariate', or 'bivariate'.}
}
\value{
APA-style table of Mplus output for the Dejonckheere Project.
}
\description{
Creates APA-style tables for the Dejonckheere Project containing output from multiple Mplus objects.
}
