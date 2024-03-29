% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_refrates.R
\name{calc_refrates}
\alias{calc_refrates}
\title{Calculate age-, sex-, cohort-, region-specific incidence rates from a cohort}
\usage{
calc_refrates(
  df,
  dattype = NULL,
  count_var,
  refpop_df,
  calc_totals = FALSE,
  fill_sites = "no",
  region_var = NULL,
  age_var = NULL,
  sex_var = NULL,
  year_var = NULL,
  race_var = NULL,
  site_var = NULL,
  quiet = FALSE
)
}
\arguments{
\item{df}{dataframe in long format}

\item{dattype}{can be "zfkd" or "seer" or NULL. Will set default variable names if dattype is "seer" or "zfkd". Default is NULL.}

\item{count_var}{variable to be counted as observed case. Should be 1 for case to be counted.}

\item{refpop_df}{df where reference population data is defined. Only required if option futime = "refpop" is chosen. It is assumed that refpop_df has the columns
"region" for region, "sex" for biological sex, "age" for age-groups (can be single ages or 5-year brackets), "year" for time period (can be single year or 5-year brackets),
"population_pyar" for person-years at risk in the respective age/sex/year cohort.
refpop_df must use the same category coding of age, sex, region, year and site as age_var, sex_var, region_var, year_var and site_var.}

\item{calc_totals}{option to calculate totals for all age-groups, all sexes, all years, all races, all sites. Default is FALSE.}

\item{fill_sites}{option to fill missing sites in observed with incidence rate of 0. Needs to define the coding system used.
Can be either "no" for not filling missing sites. "icd2d" for ICD-O-3 2 digit (C00-C80), "icd3d" for ICD-O-3 3digit,
"icd10gm2d" for ICD-10-GM 2-digit (C00-C97), "sitewho" for Site SEER WHO coding (no 1-89 categories), "sitewho_b"  for Site SEER WHO B recoding (no. 1-111 categories),
"sitewho_epi" for SITE SEER WHO coding with additional sums,  "sitewhogen" for SITE WHO coding with less categories to make compatible for international rates,
"sitewho_num" for numeric coding of Site SEER WHO coding (no 1-89 categories), "sitewho_b_num"  for numeric coding of Site SEER WHO B recoding (no. 1-111 categories),
"sitewhogen_num" for numeric international rates, c("manual", char_vector) of sites manually defined}

\item{region_var}{variable in df that contains information on region where case was incident. Default is set if dattype is given.}

\item{age_var}{variable in df that contains information on age-group. Default is set if dattype is given.}

\item{sex_var}{variable in df that contains information on sex. Default is set if dattype is given.}

\item{year_var}{variable in df that contains information on year or year-period when case was incident. Default is set if dattype is given.}

\item{race_var}{optional argument, if rates should be calculated stratified by race. If you want to use this option, provide variable name of df that contains race information. If race_var is provided refpop_df needs to contain the variable "race".}

\item{site_var}{variable in df that contains information on ICD code of case diagnosis. Cases are usually the second cancers. Default is set if dattype is given.}

\item{quiet}{If TRUE, warnings and messages will be suppressed. Default is FALSE.}
}
\value{
df
}
\description{
Calculate age-, sex-, cohort-, region-specific incidence rates from a cohort
}
\examples{
#load sample data
data("us_second_cancer")
data("population_us")

us_second_cancer \%>\%
  #create variable to indicate to be counted as case
  dplyr::mutate(is_case = 1) \%>\%
  #calculate refrates - warning: these are not realistic numbers, just showing functionality
  calc_refrates(dattype = "seer", , count_var = "is_case", refpop_df = population_us,
               region_var = "registry", age_var = "fc_agegroup", sex_var = "sex", 
               site_var = "t_site_icd")
}
