% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/srvy_iss.R
\name{srvy_iss_caal}
\alias{srvy_iss_caal}
\title{Survey input sample size function}
\usage{
srvy_iss_caal(
  iters = 1,
  specimen_data,
  cpue_data,
  yrs = NULL,
  bin = 1,
  boot_hauls = FALSE,
  boot_ages = FALSE,
  al_var = FALSE,
  al_var_ann = FALSE,
  age_err = FALSE,
  age_samples = NULL,
  plus_len = NULL,
  plus_age = NULL,
  region = "goa",
  save_interm = FALSE,
  save_stats = FALSE,
  save
)
}
\arguments{
\item{iters}{number of iterations (min of 500 recommended for full run)}

\item{specimen_data}{age-length specimen input dataframe}

\item{cpue_data}{catch-per-unit effort input dataframe}

\item{yrs}{any year filter >= (default = NULL)}

\item{bin}{bin size (default = 1 cm), also can use custom length bins following ss3 bin convention}

\item{boot_hauls}{Boolean. Resample hauls w/replacement? (default = FALSE)}

\item{boot_ages}{Boolean. Resample ages w/replacement? (default = FALSE)}

\item{al_var}{Boolean. Include age-length variability in resampled age data? (default = FALSE)}

\item{al_var_ann}{Boolean. Resample age-length variability annually or pooled across years? (default = FALSE)}

\item{age_err}{Boolean. Include ageing error in resampled age data? (default = FALSE)}

\item{age_samples}{If set at a value, tests reductions (and increases) in survey-level number of ages collected. To test, set at a proportion of ages collected, i.e., 0.8 or 1.2 (default = NULL)}

\item{plus_len}{If set at a value, computes length expansion with a plus-length group (default = FALSE)}

\item{plus_age}{If set at a value, computes age expansion with a plus-age group (default = FALSE)}

\item{region}{Region will create a folder and place results in said folder. (default = 'goa')}

\item{save_interm}{Boolean. Save the intermediate results: resampled age/length comps and realized sample size per iteration? (default = FALSE)}

\item{save_stats}{Boolean. Save other statistics: base age/length comps without resampling, mean length-at-age, bootstrap bias? (default = FALSE)}

\item{save}{Name to attach to and identify output files.}
}
\value{
Dataframe of input sample size by year, species (using RACE species codes), sex (0 - combined sex,
1 - males, 2 - females; all with short description IDs in sex_desc column), and length for conditional age-at-length
(output saved with 'iss_caal' in file name). For comparison, nominal sample size ('nss' - the number of age-length samples
actually taken) is included. Will also produce other dataframes if desired (see save_intern and save_stats argument descriptions).
}
\description{
Bootstrap data sources to replicate bottom trawl conditional age-at-length.
}
