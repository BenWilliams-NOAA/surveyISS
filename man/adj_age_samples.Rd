% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_sampling.R
\name{adj_age_samples}
\alias{adj_age_samples}
\title{Test age sampling magnitude}
\usage{
adj_age_samples(data, rate = NULL, grp = c("year", "species_code"))
}
\arguments{
\item{data}{age specimen sample dataframe}

\item{rate}{proportion of age samples (default = NULL)}

\item{grp}{grouping variable(s) (default is to change survey-level sampling)}
}
\description{
Function to test changes in age sampling for whatever grouping is desired (default is changes at survey-level sampling)
}
