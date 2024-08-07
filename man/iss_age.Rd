% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comp_stats.R
\name{iss_age}
\alias{iss_age}
\title{Calculate age composition input sampled size (iss)}
\usage{
iss_age(rss_age, specimen_data, survey_region = NULL)
}
\arguments{
\item{rss_age}{iterated age composition realized sample size}

\item{specimen_data}{age-length specimen input dataframe}

\item{survey_region}{overall region, i.e., goa, ai, ebs, nbs... If defined, will compute
statistics for spatially-explicit results. (default = NULL)}
}
\description{
Function to calculate age composition iss (harmonic mean of realized sample sizes) and add nominal sample
size (the number of ages actually aged) and the number of sampled hauls to output. Will compute for regional
or spatially-explicit cases (depending on whether survey_region is defined, see arguments below)
}
