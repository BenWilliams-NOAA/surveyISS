% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query_data.R
\name{query_data_t3}
\alias{query_data_t3}
\title{Wrapper function to query data}
\usage{
query_data_t3(query = FALSE)
}
\arguments{
\item{query}{Boolean. If query = TRUE then will connect to AKFIN and query data, otherwise will read list of previously queried data. (default = FALSE)}
}
\value{
a list of necessary data sources, dataframes are also written to csv files within the 'data/region' folder.
}
\description{
Function that queries data for all Tier 3 and above stock assessments performed by AFSC
}
