test_that("`bps_dataset()` works", {
  expect_s3_class(bps_dataset(subject_id = "26"), "tbl_df")
  expect_s3_class(bps_dataset(vertical_var_group_id = "1"), "tbl_df")
})


test_that("`bps_get_dataset()` works", {
  expect_s3_class(
    bps_get_dataset(
      "1905",
      vertical_var_id = c("3100", "3273"),
      derived_var_id = c("1553", "1666"),
      year_id = c("120", "121"),
      period_id = c("1", "12")
    ),
    "bpsr_tbl"
  )

  expect_s3_class(
    bps_get_dataset("1906", year_id = "120:122", period_id = "1:6"),
    "bpsr_tbl"
  )
})


test_that("`keep` in `bps_get_dataset()` works", {
  data_all <- bps_get_dataset("417", keep = "all")
  id_column <- c("vertical_var_id", "derived_var_id", "year_id", "period_id")
  expect_true(all(id_column %in% names(data_all)))

  some_id_column <- c("vertical_var_id", "period_id")
  data_vervar_period <- bps_get_dataset("1927", keep = some_id_column)
  expect_true(all(some_id_column %in% names(data_vervar_period)))
})


test_that("`bps_get_datasets()` works", {
  data <- bps_get_datasets(c("1910", "1956"))
  expect_s3_class(data[[1]], "bpsr_tbl")
  expect_s3_class(data[[2]], "bpsr_tbl")
})


test_that("`composite_id` column gets parsed correctly", {
  # Test against datasets with similar IDs across variables. Phone ownership,
  # for example, has "111" as the dataset ID, which is the same with the year ID
  # for 2011
  phone_ownership <- bps_get_dataset("111")

  phone_ownership_aceh_2011 <- dplyr::filter(
    phone_ownership,
    vertical_var == "ACEH",
    year == 2011
  )

  phone_ownership_aceh_2011_overall <- phone_ownership_aceh_2011 |>
    dplyr::filter(derived_var == "Perkotaan+Perdesaan") |>
    dplyr::pull(var)

  phone_ownership_aceh_2011_urban <- phone_ownership_aceh_2011 |>
    dplyr::filter(derived_var == "Perkotaan") |>
    dplyr::pull(var)

  phone_ownership_aceh_2011_rural <- phone_ownership_aceh_2011 |>
    dplyr::filter(derived_var == "Perdesaan") |>
    dplyr::pull(var)

  expect_equal(phone_ownership_aceh_2011_overall, 3.29)
  expect_equal(phone_ownership_aceh_2011_urban, 6.25)
  expect_equal(phone_ownership_aceh_2011_rural, 2.13)

  # Test against the monthly inflation dataset, as `parse_year_n_period()`
  # previously failed to correctly parse the two variables of this dataset. It
  # resulted in mislabelled periods for the 2021 observations
  inflation <- bps_get_dataset("1708")

  inflation_national_2021 <- dplyr::filter(
    inflation,
    vertical_var == "INDONESIA",
    year == 2021
  )

  inflation_national_2021_period <- dplyr::pull(inflation_national_2021, period)

  period <- c(
    "Januari",
    "Februari",
    "Maret",
    "April",
    "Mei",
    "Juni",
    "Agustus",
    "September",
    "Oktober",
    "November",
    "Desember",
    "Tahunan"
  )

  expect_true(all(period %in% inflation_national_2021_period))
})


test_that("`bps_get_dataset()` parses Indonesian, English data correctly", {
  ind <- bps_get_dataset("1955", keep = "all")
  eng <- bps_get_dataset("1955", lang = "eng", keep = "all")

  ind_bare <- ind |>
    dplyr::select(
      !c(vertical_var, derived_var, derived_var_id, period, year)
    ) |>
    unclass_bpsr_tbl()


  eng_bare <- eng |>
    dplyr::select(
      !c(vertical_var, derived_var, derived_var_id, period, year)
    ) |>
    unclass_bpsr_tbl()

  expect_true(all.equal(ind_bare, eng_bare))
})


test_that("`bps_get_dataset()` fails due to invalid `dataset_id", {
  expect_error(bps_get_dataset("abcd"))
  expect_error(bps_get_dataset(c("1905", "1906")))
})


test_that("`bps_get_dataset()` fails due to invalid `vertical_var_id`", {
  expect_error(bps_get_dataset("1905", vertical_var_id = "abcd"))

  expect_error(
    bps_get_dataset("1905", vertical_var_id = c("3100", "3273", "abcd"))
  )

  expect_error(bps_get_dataset("1905", vertical_var_id = "3100;3273"))
})


test_that("`bps_get_dataset()` fails due to invalid `derived_var_id`", {
  expect_error(bps_get_dataset("1905", derived_var_id = "abcd"))

  expect_error(
    bps_get_dataset("1905", derived_var_id = c("1553", "1666", "abcd"))
  )

  expect_error(bps_get_dataset("1905", derived_var_id = "1553;1666"))
})


test_that("`bps_get_dataset()` fails due to invalid `year_id`", {
  expect_error(bps_get_dataset("416", year_id = c("114", "abcd")))
  expect_error(bps_get_dataset("416", year_id = "114;115;116"))
})


test_that("`bps_get_dataset()` fails due to invalid `period_id`", {
  expect_error(bps_get_dataset("1905", period_id = c("1", "abcd")))
  expect_error(bps_get_dataset("1905", period_id = "1;2;3"))
})


test_that("`bps_get_datasets()` fails due to invalid `dataset_id`", {
  expect_error(bps_get_datasets("1905"))
  expect_error(bps_get_dataset(c("416", "abcd")))
})


test_that("`bps_dataset()` fails due to invalid `vertical_var_group_id`", {
  expect_error(bps_dataset(vertical_var_group_id = "abcd"))
  expect_error(bps_dataset(vertical_var_group_id = c("1", "5")))
})
