% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/srvy_comps_ai_rebs.R
\name{srvy_comps_ai_rebs}
\alias{srvy_comps_ai_rebs}
\title{primary survey age/length pop'n numbers function (customized for ai rebs complex)}
\usage{
srvy_comps_ai_rebs(
  lfreq_data,
  specimen_data,
  cpue_data,
  strata_data,
  r_t,
  yrs,
  boot_hauls,
  boot_lengths,
  boot_ages,
  al_var,
  age_err
)
}
\arguments{
\item{lfreq_data}{length frequency data}

\item{specimen_data}{age-length specimen data}

\item{cpue_data}{abundance by length data}

\item{strata_data}{strata and associated area}

\item{r_t}{reader/tester ageing data}

\item{yrs}{age filter returns years >= (default = NULL)}

\item{boot_hauls}{switch for resampling hauls (default = FALSE)}

\item{boot_lengths}{switch for resampling lengths (default = FALSE)}

\item{boot_ages}{switch for resampling ages (default = FALSE)}

\item{al_var}{switch for including age-length variability (default = FALSE)}

\item{age_err}{switch for including ageing error (default = FALSE)}
}
\description{
primary survey age/length pop'n numbers function (customized for ai rebs complex)
}
\examples{
swo(lfreq, specimen, cpue, strata_data, yrs = 2015, boot_hauls = TRUE,
    boot_lengths = TRUE, length_samples = 100)
}
