% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acs.R
\name{get_acs}
\alias{get_acs}
\title{Obtain data and feature geometry for the American Community Survey}
\usage{
get_acs(
  geography,
  variables = NULL,
  table = NULL,
  cache_table = FALSE,
  year = 2023,
  output = "tidy",
  state = NULL,
  county = NULL,
  zcta = NULL,
  geometry = FALSE,
  keep_geo_vars = FALSE,
  shift_geo = FALSE,
  summary_var = NULL,
  key = NULL,
  moe_level = 90,
  survey = "acs5",
  show_call = FALSE,
  ...
)
}
\arguments{
\item{geography}{The geography of your data.}

\item{variables}{Character string or vector of character strings of variable
IDs. tidycensus automatically returns the estimate and the margin of error
associated with the variable.}

\item{table}{The ACS table for which you would like to request all
variables. Uses lookup tables to identify the variables; performs faster
when variable table already exists through \code{load_variables(cache =
TRUE)}. Only one table may be requested per call.}

\item{cache_table}{Whether or not to cache table names for faster future
access. Defaults to FALSE; if TRUE, only needs to be called once per
dataset.  If variables dataset is already cached via the
\code{load_variables} function, this can be bypassed.}

\item{year}{The year, or endyear, of the ACS sample. 5-year ACS data is
available from 2009 through 2023; 1-year ACS data is available from 2005
through 2023, with the exception of 2020.  Defaults to 2023.}

\item{output}{One of "tidy" (the default) in which each row represents an
enumeration unit-variable combination, or "wide" in which each row
represents an enumeration unit and the variables are in the columns.}

\item{state}{An optional vector of states for which you are requesting data.
State names, postal codes, and FIPS codes are accepted. Defaults to NULL.}

\item{county}{The county for which you are requesting data. County names and
FIPS codes are accepted. Must be combined with a value supplied to `state`.
Defaults to NULL.}

\item{zcta}{The zip code tabulation area(s) for which you are requesting
data. Specify a single value or a vector of values to get data for more
than one ZCTA. Numeric or character ZCTA GEOIDs are accepted. When
specifying ZCTAs, geography must be set to `"zcta"` and `state` must be specified with
`county` left as `NULL`. Defaults to NULL.}

\item{geometry}{if FALSE (the default), return a regular tibble of ACS data.
if TRUE, uses the tigris package to return an sf tibble with simple feature
geometry in the `geometry` column.}

\item{keep_geo_vars}{if TRUE, keeps all the variables from the Census
shapefile obtained by tigris.  Defaults to FALSE.}

\item{shift_geo}{(deprecated) if TRUE, returns geometry with Alaska and Hawaii shifted for
thematic mapping of the entire US. Geometry was originally obtained from
the albersusa R package. As of May 2021, we recommend using \code{tigris::shift_geometry()}
instead.}

\item{summary_var}{Character string of a "summary variable" from the ACS to
be included in your output. Usually a variable (e.g. total population) that
you'll want to use as a denominator or comparison.}

\item{key}{Your Census API key. Obtain one at
\url{https://api.census.gov/data/key_signup.html}}

\item{moe_level}{The confidence level of the returned margin of error.  One
of 90 (the default), 95, or 99.}

\item{survey}{The ACS contains one-year, three-year, and five-year surveys
expressed as "acs1", "acs3", and "acs5". The default selection is "acs5."}

\item{show_call}{if TRUE, display call made to Census API. This can be very
useful in debugging and determining if error messages returned are due to
tidycensus or the Census API. Copy to the API call into a browser and see
what is returned by the API directly. Defaults to FALSE.}

\item{...}{Other keyword arguments}
}
\value{
A tibble or sf tibble of ACS data
}
\description{
Obtain data and feature geometry for the American Community Survey
}
\examples{
\dontrun{
library(tidycensus)
library(tidyverse)
library(viridis)
census_api_key("YOUR KEY GOES HERE")

tarr <- get_acs(geography = "tract", variables = "B19013_001",
                state = "TX", county = "Tarrant", geometry = TRUE, year = 2020)

ggplot(tarr, aes(fill = estimate, color = estimate)) +
  geom_sf() +
  coord_sf(crs = 26914) +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(option = "magma")


vt <- get_acs(geography = "county", variables = "B19013_001", state = "VT", year = 2019)

vt \%>\%
mutate(NAME = gsub(" County, Vermont", "", NAME)) \%>\%
 ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbar(aes(xmin = estimate - moe, xmax = estimate + moe), width = 0.3, size = 0.5) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income by county in Vermont",
       subtitle = "2015-2019 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")

}
}
