# Internal data

# bpsr uses these data to primarily to check function inputs, such as dataset ID


## Packages ----

library(conflicted)
library(tidyverse)
conflict_prefer("dplyr", "filter")
library(bpsr)


## Helper ----

get_across_domain <- function(fun,
                              domain_id,
                              page = Inf,
                              progress = TRUE) {
  purrr::pmap(
    list(domain_id = domain_id),
    fun,
    page = page,
    .progress = progress
  )
}


scrape_bps_hs_code <- function(year = 2022, parse = TRUE) {
  stopifnot(is.numeric(year), is.logical(parse))

  if (!any(year %in% c(2008, 2009, 2012, 2017, 2022))) {
    abort("`year` must be either 2008, 2009, 2012, 2017 or 2022.")
  }

  html <- rvest::read_html(
    "https://www.bps.go.id/exim/masterhs.html#subjekViewTab5"
  )

  table <- html |>
    rvest::html_element(str_c("table#hs", year)) |>
    rvest::html_table()

  if (!parse) {
    return(table)
  }

  table |>
    dplyr::rename(description = DESKRIPSI) |>
    dplyr::mutate(
      year = year,
      hs_code = str_extract(description, "\\[\\d*\\]"),
      hs_code = str_remove_all(hs_code, "[:punct:]"),
      description = str_remove(description, "\\[\\d*\\]\\s"),
      hs_chapter = str_sub(hs_code, 1, 2)
    ) |>
    dplyr::select(year, hs_chapter, hs_code, description)
}


## Data ----

### Domain lookup table ----

domain_lookup <- bps_domain() |>
  select(domain_name, domain_id) |>
  deframe()


### Dataset ID list ----

# Split the request into parts to prevent some unexpected failed requests
# from interrupting the whole request
dataset_1 <- get_across_domain(bps_dataset, domain_lookup[seq(1, 200)])

dataset_2 <- get_across_domain(bps_dataset, domain_lookup[seq(201, 400)])

dataset_3 <- get_across_domain(
  bps_dataset,
  domain_lookup[seq(401, length(domain_lookup))]
)

dataset_all <- c(dataset_1, dataset_2, dataset_3)

dataset_id_list <- dataset_all |>
  set_names(domain_lookup) |>
  map(pluck, "dataset_id")


### Spreadsheet dataset ID list ----

spreadsheet_1 <- get_across_domain(
  bps_dataset_spreadsheet,
  domain_lookup[seq(1, 200)]
)

spreadsheet_2 <- get_across_domain(
  bps_dataset_spreadsheet,
  domain_lookup[seq(201, 400)]
)

spreadsheet_3 <- get_across_domain(
  bps_dataset_spreadsheet,
  domain_lookup[seq(401, length(domain_lookup))]
)

spreadsheet_all <- c(spreadsheet_1, spreadsheet_2, spreadsheet_3)

spreadsheet_id_list <- spreadsheet_all |>
  set_names(domain_lookup) |>
  map(pluck, "dataset_id")


### Period ----

period_table <- bps_period(page = Inf)


### HS code table ----

hs_table <- scrape_bps_hs_code()


### Write ----

usethis::use_data(
  domain_lookup,
  dataset_id_list,
  spreadsheet_id_list,
  period_table,
  hs_table,
  internal = TRUE,
  overwrite = TRUE
)
