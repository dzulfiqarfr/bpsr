% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset.R
\name{bps_dataset}
\alias{bps_dataset}
\alias{bps_get_dataset}
\alias{bps_get_datasets}
\title{Look up and request dataset}
\usage{
bps_dataset(
  subject_id = NULL,
  vertical_var_group_id = NULL,
  domain_id = "0000",
  page = NULL,
  lang = c("ind", "eng")
)

bps_get_dataset(
  dataset_id,
  vertical_var_id = NULL,
  derived_var_id = NULL,
  year_id = NULL,
  period_id = NULL,
  domain_id = "0000",
  lang = c("ind", "eng"),
  keep = "none"
)

bps_get_datasets(
  dataset_id,
  domain_id = "0000",
  lang = c("ind", "eng"),
  keep = "none"
)
}
\arguments{
\item{subject_id}{The subject ID. Use \code{\link[=bps_subject]{bps_subject()}} to see the list of
subject IDs.}

\item{vertical_var_group_id}{The vertical variable group ID. Use
\code{\link[=bps_vertical_var]{bps_vertical_var()}} to see the list of vertical variable group IDs.}

\item{domain_id}{The domain ID of a BPS bureau. Defaults to "0000", which
is the domain ID of the central bureau. Use \code{\link[=bps_domain]{bps_domain()}} to see the
list of domain IDs.}

\item{page}{The page number of the result. Defaults to page one. Set to \code{Inf}
to request all pages.}

\item{lang}{The result's language. Must be either "ind" for Indonesian or
"eng" for English. Defaults to Indonesian.}

\item{dataset_id}{The dataset ID. To request multiple datasets, supply a
vector of the IDs to \code{bps_get_datasets()}. Use \code{bps_dataset()} to see
the list of dataset IDs.}

\item{vertical_var_id}{The vertical variable ID. Use \code{\link[=bps_vertical_var]{bps_vertical_var()}}
to see the list of vertical variable IDs.}

\item{derived_var_id}{The derived variable ID. Use \code{\link[=bps_derived_var]{bps_derived_var()}} to
see the list of derived variable IDs.}

\item{year_id}{The year ID. Use \code{\link[=bps_year]{bps_year()}} to see the list of year IDs.}

\item{period_id}{The period ID. Use \code{\link[=bps_period]{bps_period()}} to see the list of
period IDs.}

\item{keep}{ID columns to keep. Defaults to "none", which drops all of them.
Set to "all" to keep all ID columns. Otherwise, must be "vertical_var_id",
"derived_var_id", "year_id" or "period_id". To keep multiple ID columns,
supply a vector of them.}
}
\value{
\itemize{
\item \code{bps_dataset()} returns a tibble containing dataset IDs and titles.
\item \code{bps_get_dataset()} returns a tibble with the \code{bpsr_tbl} subclass, which
has a \code{metadata} attribute. Use \code{\link[=bps_metadata]{bps_metadata()}} to read it.
\item \code{bps_get_datasets()} returns a list with the \code{bpsr_multiple_tbl} class,
which contains tibbles with the \code{bpsr_tbl} subclass.
}
}
\description{
Search and get datasets that BPS distributes in JSONs. The API calls them
dynamic tables.
\itemize{
\item \code{bps_dataset()} requests the dataset table. This table contains dataset
IDs, which are needed to request datasets
\item \code{bps_get_dataset()} requests a dataset
\item \code{bps_get_datasets()} requests multiple datasets
}
}
\section{Request filter}{
Filter a request by multiple values of \code{vertical_var_id},
\code{derived_var_id}, \code{year_id} or \code{period_id} by supplying a vector of the IDs,
to \code{bps_get_dataset()}. For example, \code{vertical_var_id = c("1100", "1200")}.

For \code{year_id} and \code{period_id}, filter by a range of values by concatenating
the start and the end of the range using a colon, e.g.
\code{year_id = "118:121"} for observations from 2018 to 2021.
}

\examples{
\dontshow{if (httr2::secret_has_key("BPSR_KEY")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Get the dataset on consumer price index of food category
bps_get_dataset("1905", lang = "eng")

# Get some Human Development Index datasets
bps_get_datasets(c("414", "415", "416", "417"), lang = "eng")
\dontshow{\}) # examplesIf}
}
